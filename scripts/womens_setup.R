# =========================================================
# WOMEN'S SETUP
# Requires: source("common_helpers.R")
# =========================================================

if (!exists("common_logistic")) {
  stop("Please source('common_helpers.R') before source('womens_setup.R').")
}

# ---------------------------------------------------------
# WOMEN'S INPUT FILES
# ---------------------------------------------------------

w_file_teams <- "WTeams.csv"
w_file_slots <- "WNCAATourneySlots.csv"
w_file_seeds <- "WNCAATourneySeeds.csv"
w_file_tourn_det <- "WNCAATourneyDetailedResults.csv"
w_file_reg_det <- "WRegularSeasonDetailedResults.csv"
w_file_ratings <- "WomensRatings.csv"   # change this if needed

# ratings file column names
w_rating_season_col <- "year"
w_rating_team_col <- "team"
w_rating_value_col <- "badj_em"

w_rating_name_max_dist <- 0.15

cat("Loading women's data...\n")

# ---------------------------------------------------------
# LOAD WOMEN'S DATA
# ---------------------------------------------------------

w_teams <- read_csv(w_file_teams, show_col_types = FALSE) %>% clean_names()
w_slots <- read_csv(w_file_slots, show_col_types = FALSE) %>% clean_names()
w_seeds <- read_csv(w_file_seeds, show_col_types = FALSE) %>% clean_names()
w_tourn_det <- read_csv(w_file_tourn_det, show_col_types = FALSE) %>% clean_names()
w_reg_det <- read_csv(w_file_reg_det, show_col_types = FALSE) %>% clean_names()
w_ratings_raw <- read_csv(w_file_ratings, show_col_types = FALSE) %>% clean_names()

# ---------------------------------------------------------
# BUILD WOMEN'S RATINGS TABLE
# ---------------------------------------------------------

cat("Mapping women's ratings to team IDs...\n")

w_ratings_all <- common_map_teamnames_to_ids(
  df = w_ratings_raw,
  teams_df = w_teams,
  season_col = w_rating_season_col,
  team_col = w_rating_team_col,
  value_col = w_rating_value_col,
  max_dist = w_rating_name_max_dist
)

if (nrow(w_ratings_all) == 0) {
  stop("No women's ratings mapped. Check women's rating file and column names.")
}

# ---------------------------------------------------------
# WOMEN'S USABLE SEASONS
# ---------------------------------------------------------

w_years_ratings <- sort(unique(w_ratings_all$season))
w_years_slots   <- sort(unique(w_slots$season))
w_years_seeds   <- sort(unique(w_seeds$season))
w_years_tourn   <- sort(unique(w_tourn_det$season))
w_years_reg     <- sort(unique(w_reg_det$season))

w_seasons <- Reduce(intersect, list(
  w_years_ratings,
  w_years_slots,
  w_years_seeds,
  w_years_tourn,
  w_years_reg
))

if (length(w_seasons) == 0) {
  stop("No overlapping women's seasons found.")
}

# ---------------------------------------------------------
# FIT WOMEN'S K SLOPE
# ---------------------------------------------------------

cat("Fitting women's k_slope...\n")

w_reg_games <- w_reg_det %>%
  filter(season %in% w_seasons) %>%
  transmute(
    season,
    team_a = w_team_id,
    team_b = l_team_id,
    y = 1L
  ) %>%
  bind_rows(
    w_reg_det %>%
      filter(season %in% w_seasons) %>%
      transmute(
        season,
        team_a = l_team_id,
        team_b = w_team_id,
        y = 0L
      )
  ) %>%
  left_join(
    w_ratings_all %>% rename(team_a = team_id, r_a = rating),
    by = c("season", "team_a")
  ) %>%
  left_join(
    w_ratings_all %>% rename(team_b = team_id, r_b = rating),
    by = c("season", "team_b")
  ) %>%
  mutate(diff = r_a - r_b) %>%
  filter(!is.na(diff))

if (nrow(w_reg_games) == 0) {
  stop("No women's regular-season rows matched ratings.")
}

w_fit_slope <- glm(y ~ diff, data = w_reg_games, family = binomial())
w_k_slope <- unname(coef(w_fit_slope)[["diff"]])

# ---------------------------------------------------------
# WOMEN'S HELPERS
# ---------------------------------------------------------

w_seed_team <- function(seed_str, seeds_s) {
  seeds_s$team_id[match(seed_str, seeds_s$seed)]
}

w_make_seed_lookup <- function(season, seeds_df = w_seeds) {
  seeds_df %>%
    filter(season == !!season) %>%
    transmute(
      team_id,
      seed,
      seed_num = common_extract_seed_num(seed)
    )
}

w_make_rating_lookup <- function(rmap_season, season, seeds_df = w_seeds) {
  seeds_s <- seeds_df %>%
    filter(season == !!season) %>%
    transmute(team_id)
  
  min_r <- suppressWarnings(min(rmap_season$rating, na.rm = TRUE))
  if (!is.finite(min_r)) min_r <- 0
  
  missing <- setdiff(unique(seeds_s$team_id), unique(rmap_season$team_id))
  
  if (length(missing) > 0) {
    rmap_season <- bind_rows(
      rmap_season,
      tibble(season = season, team_id = missing, rating = min_r)
    )
  }
  
  rvec <- rmap_season$rating
  names(rvec) <- as.character(rmap_season$team_id)
  
  list(
    rvec = rvec,
    min_r = min_r,
    n_missing = length(missing)
  )
}

w_make_season_rating_lookup <- function(season, ratings_all = w_ratings_all) {
  rmap <- ratings_all %>% filter(season == !!season)
  rvec <- rmap$rating
  names(rvec) <- as.character(rmap$team_id)
  rvec
}

w_p_win <- function(teamA, teamB, rvec, k_slope = w_k_slope) {
  ra <- unname(rvec[as.character(teamA)])
  rb <- unname(rvec[as.character(teamB)])
  if (is.na(ra) || is.na(rb)) return(0.5)
  common_logistic(k_slope * (ra - rb))
}

w_resolve_entry_info <- function(entry, winners, seeds_s, rvec, k_slope = w_k_slope) {
  if (!is.null(names(winners)) && entry %in% names(winners)) {
    return(list(team_id = winners[[entry]]))
  }
  
  idx <- match(entry, seeds_s$seed)
  if (!is.na(idx)) {
    return(list(team_id = seeds_s$team_id[idx]))
  }
  
  idx_a <- match(paste0(entry, "a"), seeds_s$seed)
  idx_b <- match(paste0(entry, "b"), seeds_s$seed)
  
  ta <- if (!is.na(idx_a)) seeds_s$team_id[idx_a] else NA_integer_
  tb <- if (!is.na(idx_b)) seeds_s$team_id[idx_b] else NA_integer_
  
  if (!is.na(ta) && !is.na(tb)) {
    p_a <- w_p_win(ta, tb, rvec, k_slope)
    return(list(team_id = ifelse(p_a >= 0.5, ta, tb)))
  }
  
  list(team_id = NA_integer_)
}

# ---------------------------------------------------------
# WOMEN'S BRACKET / POOL FUNCTIONS
# ---------------------------------------------------------

w_beam_map_bracket_full <- function(season, rvec, seeds_df = w_seeds, slots_df = w_slots,
                                    beam_width = 150, k_slope = w_k_slope) {
  slots_s <- slots_df %>%
    filter(season == !!season) %>%
    mutate(rnd = common_parse_slot_round(slot)) %>%
    arrange(rnd, slot)
  
  seeds_s <- seeds_df %>%
    filter(season == !!season) %>%
    transmute(seed, team_id)
  
  beam <- list(list(
    winners = list(),
    picks = tibble(
      slot = character(),
      rnd = integer(),
      team_id = integer(),
      opp_team_id = integer(),
      pick_p = double(),
      pick_is_model_upset = logical()
    ),
    logp = 0
  ))
  
  for (i in seq_len(nrow(slots_s))) {
    slot_i <- slots_s$slot[i]
    rnd_i <- slots_s$rnd[i]
    s1 <- slots_s$strong_seed[i]
    s2 <- slots_s$weak_seed[i]
    
    new_beam <- list()
    
    for (st in beam) {
      e1 <- w_resolve_entry_info(s1, st$winners, seeds_s, rvec, k_slope)
      e2 <- w_resolve_entry_info(s2, st$winners, seeds_s, rvec, k_slope)
      
      t1 <- e1$team_id
      t2 <- e2$team_id
      if (is.na(t1) || is.na(t2)) next
      
      p1 <- common_clamp_prob(w_p_win(t1, t2, rvec, k_slope))
      
      st1 <- st
      st1$winners[[slot_i]] <- t1
      st1$picks <- bind_rows(
        st1$picks,
        tibble(
          slot = slot_i,
          rnd = rnd_i,
          team_id = t1,
          opp_team_id = t2,
          pick_p = p1,
          pick_is_model_upset = p1 < 0.5
        )
      )
      st1$logp <- st1$logp + log(p1)
      
      st2 <- st
      st2$winners[[slot_i]] <- t2
      st2$picks <- bind_rows(
        st2$picks,
        tibble(
          slot = slot_i,
          rnd = rnd_i,
          team_id = t2,
          opp_team_id = t1,
          pick_p = 1 - p1,
          pick_is_model_upset = (1 - p1) < 0.5
        )
      )
      st2$logp <- st2$logp + log(1 - p1)
      
      new_beam[[length(new_beam) + 1]] <- st1
      new_beam[[length(new_beam) + 1]] <- st2
    }
    
    if (length(new_beam) == 0) {
      stop(paste("Women's beam collapsed at slot", slot_i, "for season", season))
    }
    
    ord <- order(map_dbl(new_beam, "logp"), decreasing = TRUE)
    new_beam <- new_beam[ord]
    
    if (length(new_beam) > beam_width) {
      new_beam <- new_beam[seq_len(beam_width)]
    }
    
    beam <- new_beam
  }
  
  list(
    picks = beam[[1]]$picks,
    logp = beam[[1]]$logp
  )
}

w_generate_chalk_bracket <- function(season, base_rvec, beam_width = 150, k_slope = w_k_slope) {
  w_beam_map_bracket_full(
    season = season,
    rvec = base_rvec,
    seeds_df = w_seeds,
    slots_df = w_slots,
    beam_width = beam_width,
    k_slope = k_slope
  )
}

w_compute_bracket_features <- function(season, picks, logp, seeds_df = w_seeds, chalk_picks) {
  seed_lu <- w_make_seed_lookup(season, seeds_df)
  
  picks2 <- picks %>%
    left_join(seed_lu %>% select(team_id, seed_num), by = "team_id")
  
  main_picks <- picks2 %>%
    filter(rnd >= 1, rnd <= 6)
  
  max_rnd <- max(main_picks$rnd)
  champ <- main_picks %>% filter(rnd == max_rnd) %>% slice(1)
  ff <- main_picks %>% filter(rnd == max_rnd - 1)
  
  chalk_dist <- main_picks %>%
    select(slot, team_id) %>%
    rename(this = team_id) %>%
    left_join(
      chalk_picks %>%
        filter(rnd >= 1, rnd <= 6) %>%
        select(slot, team_id) %>%
        rename(chalk = team_id),
      by = "slot"
    ) %>%
    summarise(dist = sum(this != chalk, na.rm = TRUE)) %>%
    pull(dist)
  
  tibble(
    season = season,
    logp = logp,
    mean_pick_p = mean(main_picks$pick_p),
    min_pick_p = min(main_picks$pick_p),
    n_low_picks_35 = sum(main_picks$pick_p < 0.35),
    n_coinflip_picks = sum(main_picks$pick_p >= 0.45 & main_picks$pick_p <= 0.55),
    n_low_late_picks_35 = sum(main_picks$pick_p < 0.35 & main_picks$rnd >= 4),
    champ_seed = champ$seed_num,
    champ_pick_p = champ$pick_p,
    ff_seed_sum = sum(ff$seed_num),
    dist_from_chalk = chalk_dist
  )
}

w_generate_candidate_pool <- function(season,
                                      sigma,
                                      n_candidates = 100,
                                      beam_width = 150,
                                      k_slope = w_k_slope) {
  rmap_season <- w_ratings_all %>% filter(season == !!season)
  
  if (nrow(rmap_season) < 20) {
    stop(paste("Too few women's ratings for season", season))
  }
  
  lk <- w_make_rating_lookup(rmap_season, season, w_seeds)
  base_rvec <- lk$rvec
  
  chalk <- w_generate_chalk_bracket(
    season = season,
    base_rvec = base_rvec,
    beam_width = beam_width,
    k_slope = k_slope
  )
  
  picks_all <- vector("list", n_candidates)
  feats_all <- vector("list", n_candidates)
  
  for (i in seq_len(n_candidates)) {
    rvec_sim <- common_make_noisy_rvec(base_rvec, sigma)
    
    br <- w_beam_map_bracket_full(
      season = season,
      rvec = rvec_sim,
      seeds_df = w_seeds,
      slots_df = w_slots,
      beam_width = beam_width,
      k_slope = k_slope
    )
    
    picks_all[[i]] <- br$picks %>%
      mutate(bracket_id = i, season = season)
    
    feats_all[[i]] <- w_compute_bracket_features(
      season = season,
      picks = br$picks,
      logp = br$logp,
      seeds_df = w_seeds,
      chalk_picks = chalk$picks
    ) %>%
      mutate(bracket_id = i)
  }
  
  list(
    picks = bind_rows(picks_all),
    features = bind_rows(feats_all)
  )
}

w_add_pool_features <- function(picks_df, features_df) {
  champions <- picks_df %>%
    filter(rnd == 6) %>%
    select(bracket_id, champ_team_id = team_id)
  
  champion_support <- champions %>%
    count(champ_team_id, name = "champ_count") %>%
    mutate(champion_support_share = champ_count / sum(champ_count))
  
  features_out <- features_df %>%
    left_join(champions, by = "bracket_id") %>%
    left_join(
      champion_support %>% select(champ_team_id, champion_support_share),
      by = "champ_team_id"
    )
  
  slot_consensus <- picks_df %>%
    filter(rnd >= 1, rnd <= 6) %>%
    group_by(slot, team_id) %>%
    summarise(pick_count = n(), .groups = "drop") %>%
    group_by(slot) %>%
    mutate(pick_share = pick_count / sum(pick_count)) %>%
    ungroup()
  
  bracket_consensus <- picks_df %>%
    filter(rnd >= 1, rnd <= 6) %>%
    left_join(
      slot_consensus %>% select(slot, team_id, pick_share),
      by = c("slot", "team_id")
    ) %>%
    group_by(bracket_id) %>%
    summarise(
      mean_game_consensus = mean(pick_share, na.rm = TRUE),
      .groups = "drop"
    )
  
  features_out <- features_out %>%
    left_join(bracket_consensus, by = "bracket_id")
  
  pick_matrix <- picks_df %>%
    filter(rnd >= 1, rnd <= 6) %>%
    select(bracket_id, slot, team_id) %>%
    pivot_wider(names_from = slot, values_from = team_id) %>%
    arrange(bracket_id)
  
  bracket_ids <- pick_matrix$bracket_id
  pick_mat <- as.matrix(pick_matrix %>% select(-bracket_id))
  n_brackets <- nrow(pick_mat)
  distances <- numeric(n_brackets)
  
  for (i in seq_len(n_brackets)) {
    this <- pick_mat[i, ]
    others <- pick_mat[-i, , drop = FALSE]
    
    diff_counts <- rowSums(
      others != matrix(this, nrow = nrow(others), ncol = ncol(others), byrow = TRUE)
    )
    
    distances[i] <- mean(diff_counts)
  }
  
  uniqueness_df <- tibble(
    bracket_id = bracket_ids,
    bracket_uniqueness = distances
  )
  
  features_out %>%
    left_join(uniqueness_df, by = "bracket_id")
}

# ---------------------------------------------------------
# WOMEN'S REALITY / SELECTION FUNCTIONS
# ---------------------------------------------------------

w_score_bracket_against_reality <- function(season, picks_df,
                                            slots_df = w_slots,
                                            seeds_df = w_seeds,
                                            tourn_df = w_tourn_det) {
  slots_s <- slots_df %>%
    filter(season == !!season) %>%
    mutate(rnd = common_parse_slot_round(slot)) %>%
    filter(rnd >= 1, rnd <= 6) %>%
    arrange(rnd, slot)
  
  seeds_s <- seeds_df %>%
    filter(season == !!season) %>%
    transmute(seed, team_id)
  
  actual_games <- tourn_df %>%
    filter(season == !!season) %>%
    transmute(
      a = pmin(w_team_id, l_team_id),
      b = pmax(w_team_id, l_team_id),
      winner = w_team_id
    )
  
  actual_winners <- list()
  actual_by_slot <- vector("list", nrow(slots_s))
  
  resolve_actual_entry <- function(entry, winners, seeds_s) {
    if (!is.null(names(winners)) && entry %in% names(winners)) {
      return(winners[[entry]])
    }
    
    tid <- w_seed_team(entry, seeds_s)
    if (!is.na(tid)) return(tid)
    
    ta <- w_seed_team(paste0(entry, "a"), seeds_s)
    tb <- w_seed_team(paste0(entry, "b"), seeds_s)
    
    if (!is.na(ta) && !is.na(tb)) {
      key_a <- min(ta, tb)
      key_b <- max(ta, tb)
      
      w <- actual_games$winner[
        match(paste(key_a, key_b), paste(actual_games$a, actual_games$b))
      ]
      
      if (!is.na(w)) return(w)
      return(ta)
    }
    
    NA_integer_
  }
  
  for (i in seq_len(nrow(slots_s))) {
    slot_i <- slots_s$slot[i]
    s1 <- slots_s$strong_seed[i]
    s2 <- slots_s$weak_seed[i]
    
    t1 <- resolve_actual_entry(s1, actual_winners, seeds_s)
    t2 <- resolve_actual_entry(s2, actual_winners, seeds_s)
    
    key_a <- min(t1, t2)
    key_b <- max(t1, t2)
    
    w <- actual_games$winner[
      match(paste(key_a, key_b), paste(actual_games$a, actual_games$b))
    ]
    
    actual_winners[[slot_i]] <- w
    actual_by_slot[[i]] <- tibble(slot = slot_i, actual_winner = w)
  }
  
  actual_by_slot <- bind_rows(actual_by_slot)
  
  eval_df <- picks_df %>%
    filter(rnd >= 1, rnd <= 6) %>%
    select(slot, pred_winner = team_id) %>%
    left_join(actual_by_slot, by = "slot") %>%
    mutate(correct = pred_winner == actual_winner)
  
  sum(eval_df$correct, na.rm = TRUE)
}

w_select_best_logp <- function(feat_df) {
  feat_df %>% arrange(desc(logp), n_coinflip_picks, champ_seed, bracket_id) %>% slice(1)
}

w_select_lowest_fragility <- function(feat_df) {
  feat_df %>% arrange(n_coinflip_picks, n_low_late_picks_35, champ_seed, desc(logp), bracket_id) %>% slice(1)
}

w_select_balanced_chalk <- function(feat_df) {
  target_dist <- median(feat_df$dist_from_chalk, na.rm = TRUE)
  feat_df %>%
    mutate(dist_to_target = abs(dist_from_chalk - target_dist)) %>%
    arrange(dist_to_target, desc(logp), n_coinflip_picks, bracket_id) %>%
    slice(1)
}

w_select_best_non1_champion <- function(feat_df) {
  non1 <- feat_df %>% filter(champ_seed > 1)
  if (nrow(non1) == 0) return(w_select_best_logp(feat_df))
  non1 %>% arrange(desc(logp), n_coinflip_picks, champ_seed, bracket_id) %>% slice(1)
}

w_select_regret_min <- function(feat_df) {
  feat_df %>%
    mutate(
      regret_score =
        2.0 * n_coinflip_picks +
        3.0 * n_low_late_picks_35 +
        1.5 * pmax(champ_seed - 1, 0) +
        4.0 * (1 - champ_pick_p) -
        1.0 * logp
    ) %>%
    arrange(regret_score, bracket_id) %>%
    slice(1)
}

w_select_consensus_champion <- function(feat_df) {
  feat_df %>% arrange(desc(champion_support_share), desc(logp), n_coinflip_picks, bracket_id) %>% slice(1)
}

w_select_anti_consensus_champion <- function(feat_df) {
  feat_df %>%
    mutate(anti_consensus_score = logp - 8.0 * champion_support_share) %>%
    arrange(desc(anti_consensus_score), n_coinflip_picks, bracket_id) %>%
    slice(1)
}

w_select_stability_cluster <- function(feat_df) {
  feat_df %>% arrange(bracket_uniqueness, desc(logp), n_coinflip_picks, bracket_id) %>% slice(1)
}

w_select_balanced_contrarian <- function(feat_df) {
  target_consensus <- quantile(feat_df$mean_game_consensus, probs = 0.35, na.rm = TRUE)
  target_unique <- median(feat_df$bracket_uniqueness, na.rm = TRUE)
  
  feat_df %>%
    mutate(
      dist_consensus = abs(mean_game_consensus - target_consensus),
      dist_unique = abs(bracket_uniqueness - target_unique),
      balanced_contrarian_score = dist_consensus + 0.5 * dist_unique - 0.05 * logp
    ) %>%
    arrange(balanced_contrarian_score, bracket_id) %>%
    slice(1)
}

w_select_high_prob_contrarian <- function(feat_df) {
  feat_df %>%
    mutate(
      high_prob_contrarian_score =
        logp - 12.0 * mean_game_consensus - 0.75 * n_coinflip_picks
    ) %>%
    arrange(desc(high_prob_contrarian_score), bracket_id) %>%
    slice(1)
}

w_evaluate_selection_rules <- function(season, feature_df, picks_df,
                                       slots_df = w_slots,
                                       seeds_df = w_seeds,
                                       tourn_df = w_tourn_det) {
  rules <- list(
    best_logp = w_select_best_logp,
    lowest_fragility = w_select_lowest_fragility,
    balanced_chalk = w_select_balanced_chalk,
    best_non1_champion = w_select_best_non1_champion,
    regret_min = w_select_regret_min,
    consensus_champion = w_select_consensus_champion,
    anti_consensus_champion = w_select_anti_consensus_champion,
    stability_cluster = w_select_stability_cluster,
    balanced_contrarian = w_select_balanced_contrarian,
    high_prob_contrarian = w_select_high_prob_contrarian
  )
  
  results <- vector("list", length(rules))
  idx <- 1
  
  for (rule_name in names(rules)) {
    chosen <- rules[[rule_name]](feature_df)
    chosen_id <- chosen$bracket_id[[1]]
    
    chosen_picks <- picks_df %>% filter(bracket_id == chosen_id)
    
    slot_correct <- w_score_bracket_against_reality(
      season = season,
      picks_df = chosen_picks,
      slots_df = slots_df,
      seeds_df = seeds_df,
      tourn_df = tourn_df
    )
    
    results[[idx]] <- chosen %>%
      mutate(rule = rule_name, slot_correct = slot_correct)
    
    idx <- idx + 1
  }
  
  bind_rows(results) %>%
    arrange(desc(slot_correct), desc(logp))
}

w_classify_season_strategy <- function(champion_support_max,
                                       mean_pool_consensus,
                                       mean_pool_uniqueness,
                                       top1_minus_top8,
                                       top1_minus_top16,
                                       top4_minus_top16_mean,
                                       cutoffs) {
  concentrated_score <- 0L
  chaotic_score <- 0L
  
  if (champion_support_max >= cutoffs$champion_support_max) concentrated_score <- concentrated_score + 1L
  if (mean_pool_consensus >= cutoffs$mean_pool_consensus) concentrated_score <- concentrated_score + 1L
  if (top1_minus_top8 >= cutoffs$top1_minus_top8) concentrated_score <- concentrated_score + 1L
  if (top1_minus_top16 >= cutoffs$top1_minus_top16) concentrated_score <- concentrated_score + 1L
  if (top4_minus_top16_mean >= cutoffs$top4_minus_top16_mean) concentrated_score <- concentrated_score + 1L
  
  if (mean_pool_uniqueness >= cutoffs$mean_pool_uniqueness) chaotic_score <- chaotic_score + 1L
  if (champion_support_max < cutoffs$champion_support_max) chaotic_score <- chaotic_score + 1L
  if (mean_pool_consensus < cutoffs$mean_pool_consensus) chaotic_score <- chaotic_score + 1L
  if (top1_minus_top8 < cutoffs$top1_minus_top8) chaotic_score <- chaotic_score + 1L
  if (top1_minus_top16 < cutoffs$top1_minus_top16) chaotic_score <- chaotic_score + 1L
  if (top4_minus_top16_mean < cutoffs$top4_minus_top16_mean) chaotic_score <- chaotic_score + 1L
  
  if (concentrated_score >= 4 && chaotic_score <= 2) {
    strategy <- "consensus_champion"
    season_type <- "concentrated"
  } else if (chaotic_score >= 4) {
    if (mean_pool_uniqueness >= cutoffs$mean_pool_uniqueness &&
        mean_pool_consensus < cutoffs$mean_pool_consensus) {
      strategy <- "balanced_contrarian"
      season_type <- "chaotic"
    } else {
      strategy <- "stability_cluster"
      season_type <- "flat"
    }
  } else {
    strategy <- "stability_cluster"
    season_type <- "middle"
  }
  
  tibble(
    season_type = season_type,
    recommended_rule = strategy,
    concentrated_score = concentrated_score,
    chaotic_score = chaotic_score
  )
}

cat("Women's setup ready.\n")
cat("w_k_slope =", w_k_slope, "\n")
cat("Women's seasons loaded:", paste(range(w_seasons), collapse = " to "), "\n")