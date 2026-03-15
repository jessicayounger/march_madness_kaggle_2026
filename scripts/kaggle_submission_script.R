suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

# =========================================================
# KAGGLE SUBMISSION BUILDER (2026)
# Uses the work from common_helpers.R, mens_setup.R, womens_setup.R
# Sigma is set to 2 for both men's and women's, per your instruction.
# =========================================================

# ---- user settings ----
submission_season <- 2026
sigma_m <- 2
sigma_w <- 2
n_draws <- 400
mc_seed <- 20260315
sample_submission_file <- "SampleSubmissionStage2.csv"
out_file <- "submission_2026_sigma2_mw.csv"

# ---- load project scripts ----
source("common_helpers.R")
source("mens_setup.R")
source("womens_setup.R")

# ---- sanity checks ----
if (!submission_season %in% m_ratings_all$season) {
  stop(sprintf("Men's ratings do not include season %s.", submission_season))
}
if (!submission_season %in% w_ratings_all$season) {
  stop(sprintf("Women's ratings do not include season %s.", submission_season))
}

# ---- helpers ----
make_full_rating_lookup <- function(teams_df, ratings_all, season) {
  season_teams <- teams_df %>%
    distinct(team_id)
  
  season_ratings <- ratings_all %>%
    filter(season == !!season) %>%
    select(team_id, rating)
  
  min_rating <- suppressWarnings(min(season_ratings$rating, na.rm = TRUE))
  if (!is.finite(min_rating)) {
    stop(sprintf("No finite ratings found for season %s.", season))
  }
  
  full_lookup <- season_teams %>%
    left_join(season_ratings, by = "team_id") %>%
    mutate(rating = if_else(is.na(rating), min_rating, rating))
  
  rvec <- full_lookup$rating
  names(rvec) <- as.character(full_lookup$team_id)
  
  list(
    rvec = rvec,
    min_rating = min_rating,
    n_missing_filled = sum(is.na(full_lookup$rating))
  )
}

predict_block <- function(df, rvec, k_slope, sigma, n_draws, seed) {
  if (nrow(df) == 0) return(df %>% mutate(Pred = numeric(0)))
  
  ra <- unname(rvec[as.character(df$team1)])
  rb <- unname(rvec[as.character(df$team2)])
  
  missing_mask <- is.na(ra) | is.na(rb)
  diff <- ra - rb
  
  pred <- rep(0.5, nrow(df))
  if (any(!missing_mask)) {
    pred[!missing_mask] <- common_mc_prob_from_diff(
      diff_vec = diff[!missing_mask],
      k_slope = k_slope,
      sigma = sigma,
      n_draws = n_draws,
      seed = seed
    )
  }
  
  df %>% mutate(Pred = pred)
}

# ---- build season rating lookups ----
m_lookup <- make_full_rating_lookup(
  teams_df = m_teams,
  ratings_all = m_ratings_all,
  season = submission_season
)

w_lookup <- make_full_rating_lookup(
  teams_df = w_teams,
  ratings_all = w_ratings_all,
  season = submission_season
)

m_team_ids <- as.integer(names(m_lookup$rvec))
w_team_ids <- as.integer(names(w_lookup$rvec))

overlap_ids <- intersect(m_team_ids, w_team_ids)
if (length(overlap_ids) > 0) {
  stop("Men's and women's team IDs overlap; expected no overlap for submission split.")
}

# ---- read sample submission ----
sample_sub <- read_csv(sample_submission_file, show_col_types = FALSE)

if (!all(c("ID", "Pred") %in% names(sample_sub))) {
  stop("Sample submission must contain columns: ID, Pred")
}

sub_ids <- common_parse_submission_id(sample_sub$ID)

if (any(sub_ids$season != submission_season)) {
  bad_seasons <- sort(unique(sub_ids$season[sub_ids$season != submission_season]))
  stop(sprintf(
    "Sample submission contains season(s) other than %s: %s",
    submission_season,
    paste(bad_seasons, collapse = ", ")
  ))
}

# Rows are lower TeamId vs higher TeamId by competition rules.
if (any(sub_ids$team1 >= sub_ids$team2)) {
  stop("Found submission IDs where team1 is not lower than team2.")
}

# ---- split men / women ----
is_men <- sub_ids$team1 %in% m_team_ids & sub_ids$team2 %in% m_team_ids
is_women <- sub_ids$team1 %in% w_team_ids & sub_ids$team2 %in% w_team_ids

if (any(!(is_men | is_women))) {
  bad_rows <- sub_ids %>%
    filter(!(is_men | is_women)) %>%
    slice_head(n = 10)
  
  print(bad_rows)
  stop("Some submission IDs could not be classified as men's or women's matchups.")
}

if (any(is_men & is_women)) {
  stop("Some submission IDs were classified as both men's and women's matchups.")
}

men_rows <- sub_ids %>% filter(is_men)
women_rows <- sub_ids %>% filter(is_women)

# ---- predict ----
men_pred <- predict_block(
  df = men_rows,
  rvec = m_lookup$rvec,
  k_slope = m_k_slope,
  sigma = sigma_m,
  n_draws = n_draws,
  seed = mc_seed
)

women_pred <- predict_block(
  df = women_rows,
  rvec = w_lookup$rvec,
  k_slope = w_k_slope,
  sigma = sigma_w,
  n_draws = n_draws,
  seed = mc_seed + 1
)

submission <- bind_rows(men_pred, women_pred) %>%
  select(ID, Pred) %>%
  right_join(sample_sub %>% select(ID), by = "ID") %>%
  mutate(Pred = pmin(pmax(Pred, 1e-12), 1 - 1e-12)) %>%
  arrange(ID)

if (nrow(submission) != nrow(sample_sub)) {
  stop("Final submission row count does not match the sample submission row count.")
}

if (any(is.na(submission$Pred))) {
  stop("Final submission contains NA predictions.")
}
submission$Pred <- pmin(pmax(submission$Pred, 1e-6), 1 - 1e-6)
write_csv(submission, out_file)

cat("Done. Wrote:", out_file, "\n")
cat("Rows:", nrow(submission), "\n")
cat("Men rows:", nrow(men_rows), "\n")
cat("Women rows:", nrow(women_rows), "\n")
cat("m_k_slope:", m_k_slope, "| sigma_m:", sigma_m, "\n")
cat("w_k_slope:", w_k_slope, "| sigma_w:", sigma_w, "\n")
cat("Prediction summary:\n")
print(summary(submission$Pred))

range(submission$Pred)
submission %>% 
  arrange(desc(Pred)) %>% 
  head(10)%>%
  mutate(Pred = sprintf("%.8f", Pred))
