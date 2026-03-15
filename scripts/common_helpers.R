suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(janitor)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(stringdist)
})

# =========================================================
# COMMON HELPERS
# Shared by men's and women's setup scripts
# =========================================================

common_logistic <- function(x) 1 / (1 + exp(-x))

common_clamp_prob <- function(p, eps = 1e-12) {
  pmin(pmax(p, eps), 1 - eps)
}

common_norm_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", "and") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

common_parse_slot_round <- function(slot_vec) {
  ifelse(
    str_detect(slot_vec, "^R\\d"),
    as.integer(str_extract(slot_vec, "(?<=^R)\\d+")),
    0L
  )
}

common_extract_seed_num <- function(seed_str) {
  as.integer(str_extract(seed_str, "\\d+"))
}

common_make_noisy_rvec <- function(base_rvec, sigma) {
  out <- base_rvec + rnorm(length(base_rvec), mean = 0, sd = sigma)
  names(out) <- names(base_rvec)
  out
}

common_map_teamnames_to_ids <- function(df, teams_df,
                                        season_col,
                                        team_col,
                                        value_col,
                                        max_dist = 0.15) {
  df2 <- df %>%
    mutate(
      season = as.integer(.data[[season_col]]),
      team_name_src = .data[[team_col]],
      team_norm = common_norm_name(team_name_src),
      val_raw = .data[[value_col]]
    ) %>%
    transmute(season, team_name_src, team_norm, value = val_raw) %>%
    filter(!is.na(season), !is.na(team_norm), !is.na(value)) %>%
    mutate(value = suppressWarnings(as.numeric(value))) %>%
    filter(!is.na(value)) %>%
    distinct()
  
  teams_lu <- teams_df %>%
    transmute(
      team_id = team_id,
      team_name = team_name,
      team_norm_k = common_norm_name(team_name)
    ) %>%
    distinct()
  
  mapped <- df2 %>%
    crossing(teams_lu) %>%
    mutate(dist = stringdist::stringdist(team_norm, team_norm_k, method = "jw")) %>%
    group_by(season, team_name_src) %>%
    slice_min(dist, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    filter(dist < max_dist) %>%
    group_by(season, team_id) %>%
    slice_min(dist, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(season, team_id, rating = value)
  
  mapped
}

common_log_loss <- function(y, p) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

common_parse_submission_id <- function(id_vec) {
  parts <- stringr::str_split_fixed(id_vec, "_", 3)
  
  tibble(
    ID = id_vec,
    season = as.integer(parts[, 1]),
    team1 = as.integer(parts[, 2]),
    team2 = as.integer(parts[, 3])
  )
}

common_mc_prob_from_diff <- function(diff_vec, k_slope, sigma, n_draws = 200, seed = 123) {
  set.seed(seed)
  
  n <- length(diff_vec)
  noise_sd <- sqrt(2) * sigma
  
  diff_mat <- matrix(
    rep(diff_vec, each = n_draws),
    nrow = n_draws,
    ncol = n,
    byrow = FALSE
  )
  
  noise_mat <- matrix(
    rnorm(n_draws * n, mean = 0, sd = noise_sd),
    nrow = n_draws,
    ncol = n
  )
  
  p_mat <- common_logistic(k_slope * (diff_mat + noise_mat))
  p <- colMeans(p_mat)
  
  common_clamp_prob(p)
}