# SKUBAL: AN ANNOTATED PUBLIC-DATA RESEARCH NOTEBOOK
#
# PURPOSE
# This is a teaching version of the exploratory R script behind the HTML report.
# Each section explains what the code does, why the step matters, and where
# the interpretation needs caution. The original sequence is retained so
# readers can see the actual analytical decisions and revisions.
#
# DATA AND SCOPE
# Data come from publicly accessible Statcast, MLB game feeds, FanGraphs
# pitcher seasons and public free-agent contract trackers.
# This is not a proprietary team projection or an audited contract database.
#
# HOW TO USE THIS NOTEBOOK
# 1. Install the required R packages and choose your own project folder.
# 2. Run sections in order, checking the printed diagnostics at each stage.
# 3. Some exploratory sections repeat earlier work or require manual checks.
# 4. Do not assume the entire notebook runs unattended from a fresh session.
# 5. The final valuation is conditional on explicit modeling assumptions.
#
# INTERPRETATION GUIDE
# A regression coefficient describes an estimated association, not causation.
# A 95% confidence interval and a 95% prediction interval answer different
# questions. The latter describes uncertainty for an individual new outcome.
# An unconvincing p-value is not proof that an effect is exactly zero.
#



# ==========================================================================
# 01 | Project setup and data collection
# ==========================================================================
# Start with public Statcast pitch data. Cache each season so the analysis
# can be repeated without downloading everything again.
#
# SKUBAL PROJECT - DATA COLLECTION


library(baseballr)
library(tidyverse)
library(lubridate)

# Set project directory
project_dir <- ""
# Set project_dir above to a directory on your computer before running.
if (!nzchar(project_dir)) stop("Set project_dir before running this notebook.")

# Create directory if needed
dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)

# Set working directory
setwd(project_dir)

# Create project folders
dir.create("data", showWarnings = FALSE)
dir.create("scripts", showWarnings = FALSE)
dir.create("models", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# Verify
getwd()
list.dirs(recursive = FALSE)


dir.create("data", showWarnings = FALSE)

skubal_id <- 669373

# Download one season at a time
# to reduce request size and allow caching.

for (yr in 2021:2026) {
  
  file <- paste0(
    "data/skubal_", yr, ".rds"
  )
  
  if (!file.exists(file)) {
    
    message("Downloading ", yr)
    
    pitches <- statcast_search_pitchers(
      start_date = paste0(yr, "-03-01"),
      end_date   = paste0(yr, "-11-15"),
      pitcherid  = skubal_id
    )
    
    saveRDS(pitches, file)
  }
}

# Combine all seasons
skubal <- list.files(
  "data",
  pattern = "^skubal_[0-9]{4}\\.rds$",
  full.names = TRUE
) |>
  purrr::map_dfr(readRDS) |>
  mutate(
    game_date = as.Date(game_date),
    season = year(game_date)
  ) |>
  filter(game_type == "R")

# Confirm data
skubal |>
  count(season, name = "pitches")

glimpse(skubal)



# ==========================================================================
# 02 | Describe Skubal's pitch arsenal
# ==========================================================================
# Measure pitch usage, velocity, spin and movement by season. This is the
# descriptive baseline before testing any hypothesis.
#
# Season-by-season pitch characteristics

# WHY: Group individual pitches into seasons and pitch types. Usage is a share of pitches, not a measure of pitch quality.
pitch_profile <- skubal |>
  filter(
    !is.na(pitch_type),
    !is.na(release_speed)
  ) |>
  group_by(season, pitch_type, pitch_name) |>
  summarise(
    pitches = n(),
    avg_velocity = mean(release_speed, na.rm = TRUE),
    velocity_sd = sd(release_speed, na.rm = TRUE),
    avg_spin = mean(release_spin_rate, na.rm = TRUE),
    avg_extension = mean(release_extension, na.rm = TRUE),
    horizontal_break = mean(pfx_x, na.rm = TRUE),
    vertical_break = mean(pfx_z, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(season) |>
  mutate(
    usage_pct = round(100 * pitches / sum(pitches), 1)
  ) |>
  ungroup() |>
  arrange(season, desc(pitches))

print(pitch_profile, n = Inf)


# ==========================================================================
# 03 | American League East opponents
# ==========================================================================
# Look at Skubal's experience against teams Toronto plays frequently. These
# summaries describe exposure, not future success against those teams.
#
# AL-East
al_east <- c("NYY", "BOS", "BAL", "TB", "TOR")


# WHY: Identify opponents from the game record, then limit the sample to AL East clubs.
skubal_al_east <- skubal |>
  mutate(
    opponent = if_else(
      home_team == "DET",
      away_team,
      home_team
    )
  ) |>
  filter(opponent %in% al_east)

skubal_al_east |>
  group_by(season, opponent) |>
  summarise(
    games = n_distinct(game_pk),
    pitches = n(),
    avg_fastball_velocity = mean(
      release_speed[pitch_type == "FF"],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(opponent, season)

al_east_summary <- skubal_al_east |>
  group_by(opponent) |>
  summarise(
    games = n_distinct(game_pk),
    pitches = n(),
    seasons = n_distinct(season),
    first_season = min(season),
    last_season = max(season),
    .groups = "drop"
  ) |>
  arrange(desc(pitches))

print(al_east_summary, n = Inf)

skubal_al_east |>
  filter(season >= 2024) |>
  group_by(opponent) |>
  summarise(
    games = n_distinct(game_pk),
    pitches = n(),
    ff_velocity = mean(
      release_speed[pitch_type == "FF"],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  print(n = Inf)


# ==========================================================================
# 04 | Ballparks and roof classifications
# ==========================================================================
# Identify where Skubal pitched and classify playing conditions. Check the
# game records because stadium names and weather labels are not always enough
# to establish roof status.
#
# Identify every stadium and game
stadium_summary <- skubal |>
  group_by(home_team) |>
  summarise(
    games = n_distinct(game_pk),
    pitches = n(),
    first_game = min(game_date),
    last_game = max(game_date),
    .groups = "drop"
  ) |>
  arrange(desc(games))

print(stadium_summary, n = Inf)

# Games played in Toronto or Tampa Bay
roof_candidates <- skubal |>
  filter(home_team %in% c("TOR", "TB")) |>
  group_by(game_pk, game_date, season, home_team, away_team) |>
  summarise(
    pitches = n(),
    ff_velocity = mean(
      release_speed[pitch_type == "FF"],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(game_date)

print(roof_candidates, n = Inf)

# Create game-level dataset

# WHY: Collapse pitch records to one row per game. This avoids treating thousands of pitches as thousands of independent games.
skubal_games <- skubal |>
  mutate(
    opponent = if_else(
      home_team == "DET",
      away_team,
      home_team
    )
  ) |>
  group_by(
    game_pk, game_date, season,
    home_team, away_team, opponent
  ) |>
  summarise(
    pitches = n(),
    ff_velocity = mean(
      release_speed[pitch_type == "FF"],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(game_date)

# Save for future sessions
saveRDS(skubal_games, "data/skubal_games.rds")

# Verify
nrow(skubal_games)

library(jsonlite)

roof_teams <- c(
  "AZ", "SEA", "MIL", "TOR",
  "TB", "HOU", "TEX", "MIA"
)

roof_games <- skubal_games |>
  filter(home_team %in% roof_teams) |>
  distinct(game_pk, game_date, home_team, away_team)

get_game_weather <- function(game_id) {
  
  url <- paste0(
    "https://statsapi.mlb.com/api/v1.1/game/",
    game_id,
    "/feed/live"
  )
  
  tryCatch({
    
    game <- jsonlite::fromJSON(url)
    
    tibble(
      game_pk = game_id,
      venue = game$gameData$venue$name,
      condition = game$gameData$weather$condition,
      temperature = game$gameData$weather$temp,
      wind = game$gameData$weather$wind
    )
    
  }, error = function(e) {
    
    tibble(
      game_pk = game_id,
      venue = NA_character_,
      condition = NA_character_,
      temperature = NA_real_,
      wind = NA_character_
    )
    
  })
}

roof_weather <- purrr::map_dfr(
  roof_games$game_pk,
  get_game_weather
)


# WHY: Classify roof status from public game information. The manual overrides below should be reviewed against source records.
roof_results <- roof_games |>
  left_join(roof_weather, by = "game_pk") |>
  mutate(
    roof_status = case_when(
      home_team == "TB" &
        venue == "Tropicana Field" ~ "Closed",
      
      str_detect(
        coalesce(condition, ""),
        regex("roof closed", ignore_case = TRUE)
      ) ~ "Closed",
      
      str_detect(
        coalesce(condition, ""),
        regex("roof open", ignore_case = TRUE)
      ) ~ "Open",
      
      TRUE ~ "Unknown"
    )
  ) |>
  arrange(game_date)

print(roof_results, n = Inf)

write_csv(
  roof_results,
  "data/skubal_roof_conditions.csv"
)

# Set working directory
setwd("")

# Load saved datasets

# WHY: Collapse pitch records to one row per game. This avoids treating thousands of pitches as thousands of independent games.
skubal_games <- readRDS("data/skubal_games.rds")


# WHY: Classify roof status from public game information. The manual overrides below should be reviewed against source records.
roof_results <- read_csv(
  "data/skubal_roof_conditions.csv",
  show_col_types = FALSE
)

# Identify covered stadiums
roof_teams <- c(
  "AZ", "SEA", "MIL", "TOR",
  "TB", "HOU", "TEX", "MIA"
)

# Build environment dataset

# WHY: Attach the game-level roof classification to Skubal's starts.
skubal_environment <- skubal_games |>
  left_join(
    roof_results |>
      select(game_pk, roof_status),
    by = "game_pk"
  ) |>
  mutate(
    environment = case_when(
      roof_status == "Closed" ~ "Indoor",
      roof_status == "Open" ~ "Outdoor",
      home_team %in% roof_teams ~ "Unknown",
      TRUE ~ "Outdoor"
    )
  )

# Save so we don't have to recreate it
saveRDS(
  skubal_environment,
  "data/skubal_environment.rds"
)

# Verify classifications
skubal_environment |>
  count(season, environment) |>
  print(n = Inf)

# Check total games and duplicate game IDs
skubal_games |>
  summarise(
    rows = n(),
    unique_games = n_distinct(game_pk),
    duplicate_rows = n() - n_distinct(game_pk)
  )

# Identify any duplicated game IDs
skubal_games |>
  count(game_pk) |>
  filter(n > 1)


# WHY: Classify roof status from public game information. The manual overrides below should be reviewed against source records.
roof_results <- roof_results |>
  mutate(
    roof_status = if_else(
      game_pk == 825106,
      "Open",
      roof_status
    )
  )

write_csv(
  roof_results,
  "data/skubal_roof_conditions.csv"
)


# WHY: Classify roof status from public game information. The manual overrides below should be reviewed against source records.
roof_results <- roof_results |>
  mutate(
    roof_status = if_else(
      game_pk == 825106,
      "Open",
      roof_status
    )
  )

write_csv(
  roof_results,
  "data/skubal_roof_conditions.csv"
)


# WHY: Attach the game-level roof classification to Skubal's starts.
skubal_environment <- skubal_environment |>
  mutate(
    roof_status = if_else(
      game_pk == 825106,
      "Open",
      roof_status
    ),
    environment = if_else(
      game_pk == 825106,
      "Outdoor",
      environment
    )
  )

saveRDS(
  skubal_environment,
  "data/skubal_environment.rds"
)

open_games <- c(634046, 745223, 825106)


# WHY: Classify roof status from public game information. The manual overrides below should be reviewed against source records.
roof_results <- roof_results |>
  mutate(
    roof_status = if_else(
      game_pk %in% open_games,
      "Open",
      roof_status
    )
  )

write_csv(
  roof_results,
  "data/skubal_roof_conditions.csv"
)


# WHY: Attach the game-level roof classification to Skubal's starts.
skubal_environment <- skubal_environment |>
  mutate(
    roof_status = if_else(
      game_pk %in% open_games,
      "Open",
      roof_status
    ),
    environment = if_else(
      game_pk %in% open_games,
      "Outdoor",
      environment
    )
  )

saveRDS(
  skubal_environment,
  "data/skubal_environment.rds"
)

# ==========================================================================
# 05 | Indoor and outdoor comparisons
# ==========================================================================
# Compare fastball velocity and movement across environments. Include season
# in the regression so a change in Skubal's repertoire over time is not
# automatically attributed to the roof.
#

skubal_environment |>
  count(environment)

velocity_summary <- skubal_environment |>
  filter(!is.na(ff_velocity)) |>
  group_by(season, environment) |>
  summarise(
    games = n(),
    mean_velocity = mean(ff_velocity),
    sd_velocity = sd(ff_velocity),
    .groups = "drop"
  )

print(velocity_summary, n = Inf)

velocity_difference <- velocity_summary |>
  select(season, environment, mean_velocity) |>
  pivot_wider(
    names_from = environment,
    values_from = mean_velocity
  ) |>
  mutate(
    difference_mph = Indoor - Outdoor
  )

print(velocity_difference, n = Inf)

velocity_model_data <- skubal_environment |>
  filter(!is.na(ff_velocity)) |>
  mutate(
    environment = factor(
      environment,
      levels = c("Outdoor", "Indoor")
    ),
    season = factor(season)
  )


# WHY: Use a linear model to estimate the indoor-outdoor difference after adjusting for season.
velocity_model <- lm(
  ff_velocity ~ environment + season,
  data = velocity_model_data
)

summary(velocity_model)

confint(
  velocity_model,
  "environmentIndoor",
  level = 0.95
)

skubal <- bind_rows(
  lapply(
    list.files(
      "data",
      pattern = "^skubal.*\\.rds$",
      full.names = TRUE
    ),
    readRDS
  )
)

list.files("data", pattern = "\\.rds$")

# Inspect the loaded dataset
dim(skubal)

# Check the number of pitches per season
skubal |>
  count(season)

# List saved RDS files
list.files("data", pattern = "\\.rds$")


# Load each season and explicitly add its year
skubal <- map_dfr(2021:2026, function(yr) {
  
  readRDS(
    file.path("data", paste0("skubal_", yr, ".rds"))
  ) |>
    mutate(season = yr)
  
})

# Verify the dataset
skubal |>
  summarise(
    pitches = n(),
    games = n_distinct(game_pk),
    missing_seasons = sum(is.na(season))
  )

# Verify seasonal counts
skubal |>
  count(season) |>
  print(n = Inf)

skubal |>
  count(season, game_type) |>
  print(n = Inf)

skubal |>
  summarise(
    total_pitches = n(),
    unique_games = n_distinct(game_pk),
    unique_dates = n_distinct(game_date)
  )

skubal |>
  distinct(game_pk, season) |>
  anti_join(
    skubal_environment |> distinct(game_pk),
    by = "game_pk"
  ) |>
  count(season)

# Restrict to regular season
skubal <- skubal |>
  filter(game_type == "R")

# Add environment classification
skubal <- skubal |>
  left_join(
    skubal_environment |>
      select(game_pk, environment),
    by = "game_pk"
  )

# Validate
skubal |>
  summarise(
    pitches = n(),
    games = n_distinct(game_pk),
    missing_environment = sum(is.na(environment))
  )


# WHY: Convert Statcast movement from feet to inches and average within games.
ff_movement <- skubal |>
  filter(
    pitch_type == "FF",
    !is.na(pfx_x),
    !is.na(pfx_z)
  ) |>
  group_by(game_pk, season, environment) |>
  summarise(
    pitches = n(),
    horizontal_inches = mean(pfx_x) * 12,
    vertical_inches = mean(pfx_z) * 12,
    .groups = "drop"
  )

movement_summary <- ff_movement |>
  group_by(season, environment) |>
  summarise(
    games = n(),
    pitches = sum(pitches),
    horizontal_inches = mean(horizontal_inches),
    vertical_inches = mean(vertical_inches),
    .groups = "drop"
  )

print(movement_summary, n = Inf)

# Set outdoor as the reference category

# WHY: Convert Statcast movement from feet to inches and average within games.
ff_movement <- ff_movement |>
  mutate(
    environment = factor(
      environment,
      levels = c("Outdoor", "Indoor")
    ),
    season = factor(season)
  )

# Horizontal movement
horizontal_model <- lm(
  horizontal_inches ~ environment + season,
  data = ff_movement
)

# Vertical movement
vertical_model <- lm(
  vertical_inches ~ environment + season,
  data = ff_movement
)

# Examine indoor coefficients and 95% CIs
library(broom)

movement_results <- bind_rows(
  tidy(horizontal_model, conf.int = TRUE) |>
    mutate(metric = "Horizontal"),
  tidy(vertical_model, conf.int = TRUE) |>
    mutate(metric = "Vertical")
) |>
  filter(term == "environmentIndoor") |>
  select(
    metric,
    estimate,
    std.error,
    p.value,
    conf.low,
    conf.high
  )

print(movement_results)

library(broom)

indoor_games <- ff_movement |>
  filter(environment == "Indoor") |>
  pull(game_pk)


# WHY: Repeat the analysis while excluding each indoor game in turn. This tests whether a small number of games drive the result.
sensitivity_results <- map_dfr(
  indoor_games,
  function(excluded_game) {
    
    model <- lm(
      vertical_inches ~ environment + season,
      data = ff_movement |>
        filter(game_pk != excluded_game)
    )
    
    tidy(model, conf.int = TRUE) |>
      filter(term == "environmentIndoor") |>
      mutate(excluded_game = excluded_game)
  }
) |>
  select(
    excluded_game,
    estimate,
    p.value,
    conf.low,
    conf.high
  )

print(sensitivity_results, n = Inf)

sensitivity_results |>
  summarise(
    min_effect = min(estimate),
    max_effect = max(estimate),
    significant_models = sum(p.value < 0.05),
    total_models = n()
  )

ff_characteristics <- skubal |>
  filter(pitch_type == "FF") |>
  group_by(game_pk, season, environment) |>
  summarise(
    pitches = n(),
    vertical_inches = mean(pfx_z * 12, na.rm = TRUE),
    horizontal_inches = mean(pfx_x * 12, na.rm = TRUE),
    velocity = mean(release_speed, na.rm = TRUE),
    spin_rate = mean(release_spin_rate, na.rm = TRUE),
    extension = mean(release_extension, na.rm = TRUE),
    release_x = mean(release_pos_x, na.rm = TRUE),
    release_z = mean(release_pos_z, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(vertical_inches, horizontal_inches, velocity,
        spin_rate, extension, release_x, release_z),
      ~ if_else(is.nan(.x), NA_real_, .x)
    )
  )

# Examine completeness
ff_characteristics |>
  summarise(
    games = n(),
    indoor_games = sum(environment == "Indoor"),
    across(
      c(vertical_inches, velocity, spin_rate,
        extension, release_x, release_z),
      ~ sum(is.na(.x)),
      .names = "missing_{.col}"
    )
  )


# WHY: Add pitch velocity, spin and extension as covariates. These can be related to movement, but controlling for them changes the question being estimated.
adjusted_movement_model <- lm(
  vertical_inches ~ environment + factor(season) +
    velocity + spin_rate + extension,
  data = ff_characteristics |>
    mutate(
      environment = factor(
        environment,
        levels = c("Outdoor", "Indoor")
      )
    )
)

library(broom)

# ==========================================================================
# 06 | Fastball outcomes
# ==========================================================================
# Look beyond pitch shape to swinging strikes, called strikes and contact
# quality. Use game-clustered uncertainty because pitches thrown in the same
# game are not independent observations.
#

tidy(adjusted_movement_model, conf.int = TRUE) |>
  filter(term == "environmentIndoor")

ff_effectiveness <- skubal |>
  filter(pitch_type == "FF") |>
  mutate(
    is_whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip"
    ),
    is_swing = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip",
      "foul",
      "foul_bunt",
      "hit_into_play",
      "hit_into_play_no_out",
      "hit_into_play_score"
    ),
    is_called_strike = description == "called_strike",
    is_contact = !is.na(
      estimated_woba_using_speedangle
    )
  ) |>
  group_by(game_pk, season, environment) |>
  summarise(
    pitches = n(),
    swings = sum(is_swing),
    whiffs = sum(is_whiff),
    called_strikes = sum(is_called_strike),
    whiff_rate = if_else(
      swings > 0,
      whiffs / swings,
      NA_real_
    ),
    csw_rate = (whiffs + called_strikes) / pitches,
    contact_events = sum(is_contact),
    xwoba_contact = mean(
      estimated_woba_using_speedangle,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    xwoba_contact = if_else(
      is.nan(xwoba_contact),
      NA_real_,
      xwoba_contact
    )
  )

effectiveness_summary <- ff_effectiveness |>
  group_by(season, environment) |>
  summarise(
    games = n(),
    pitches = sum(pitches),
    whiff_rate = sum(whiffs) / sum(swings),
    csw_rate = sum(whiffs + called_strikes) /
      sum(pitches),
    xwoba_contact = weighted.mean(
      xwoba_contact,
      w = contact_events,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(effectiveness_summary, n = Inf)


swing_descriptions <- c(
  "swinging_strike",
  "swinging_strike_blocked",
  "foul_tip",
  "foul",
  "foul_bunt",
  "hit_into_play",
  "hit_into_play_no_out",
  "hit_into_play_score",
  "missed_bunt",
  "bunt_foul_tip"
)

ff_outcomes <- skubal |>
  filter(pitch_type == "FF") |>
  mutate(
    swing = description %in% swing_descriptions,
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip",
      "missed_bunt"
    ),
    called_strike = description == "called_strike",
    batted_ball = !is.na(
      estimated_woba_using_speedangle
    )
  ) |>
  group_by(season, environment) |>
  summarise(
    games = n_distinct(game_pk),
    pitches = n(),
    swings = sum(swing),
    whiffs = sum(whiff),
    called_strikes = sum(called_strike),
    batted_balls = sum(batted_ball),
    whiff_rate = whiffs / swings,
    csw_rate = (whiffs + called_strikes) / pitches,
    xwoba_contact = mean(
      estimated_woba_using_speedangle,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(ff_outcomes, n = Inf)

install.packages('sandwich')
install.packages('lmtest')

library(sandwich)
library(lmtest)

# Use the existing validated pitch-level dataset

# WHY: Create consistent swing and whiff indicators before fitting the outcome models.
ff_model_data <- skubal |>
  filter(pitch_type == "FF") |>
  mutate(
    environment = factor(
      environment,
      levels = c("Outdoor", "Indoor")
    ),
    season = factor(season),
    
    swing = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip",
      "foul",
      "foul_bunt",
      "hit_into_play",
      "hit_into_play_no_out",
      "hit_into_play_score",
      "missed_bunt",
      "bunt_foul_tip"
    ),
    
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip",
      "missed_bunt"
    ),
    
    csw = whiff | description == "called_strike"
  )

# Whiff probability among swings

# WHY: Model whiff probability among swings rather than among all pitches.
whiff_model <- glm(
  whiff ~ environment + season,
  data = ff_model_data |> filter(swing),
  family = binomial()
)

# Called strike or whiff probability per pitch

# WHY: Model the probability of a called strike or whiff on each pitch.
csw_model <- glm(
  csw ~ environment + season,
  data = ff_model_data,
  family = binomial()
)

# Cluster-robust inference by game
whiff_results <- coeftest(
  whiff_model,
  vcov. = vcovCL(
    whiff_model,
    cluster = ff_model_data |>
      filter(swing) |>
      pull(game_pk)
  )
)

csw_results <- coeftest(
  csw_model,
  vcov. = vcovCL(
    csw_model,
    cluster = ff_model_data$game_pk
  )
)

print(whiff_results)
print(csw_results)


# ==========================================================================
# 07 | Skubal's 2026 season and return from the injured list
# ==========================================================================
# Compare his pitch characteristics before and after the selected absence.
# These are observational comparisons and cannot establish that the absence
# caused any change.
#

# WHY: Define the comparison periods using the dates chosen for this analysis. Check the dates against the injury record before publication.
skubal_2026 <- skubal |>
  filter(season == 2026) |>
  mutate(
    game_date = as.Date(game_date),
    period = case_when(
      game_date < as.Date("2026-05-01") ~ "Pre-IL",
      game_date >= as.Date("2026-06-13") ~ "Post-IL",
      TRUE ~ "IL period"
    )
  )

# Verify appearances and dates
skubal_2026 |>
  group_by(period) |>
  summarise(
    games = n_distinct(game_pk),
    pitches = n(),
    first_game = min(game_date),
    last_game = max(game_date),
    .groups = "drop"
  )

ff_2026_games <- skubal_2026 |>
  filter(pitch_type == "FF", period != "IL period") |>
  group_by(game_pk, game_date, period) |>
  summarise(
    pitches = n(),
    velocity = mean(release_speed, na.rm = TRUE),
    spin_rate = mean(release_spin_rate, na.rm = TRUE),
    vertical_inches = mean(pfx_z, na.rm = TRUE) * 12,
    horizontal_inches = mean(pfx_x, na.rm = TRUE) * 12,
    extension = mean(release_extension, na.rm = TRUE),
    release_height = mean(release_pos_z, na.rm = TRUE),
    .groups = "drop"
  )

ff_2026_summary <- ff_2026_games |>
  group_by(period) |>
  summarise(
    games = n(),
    ff_pitches = sum(pitches),
    velocity = mean(velocity),
    spin_rate = mean(spin_rate),
    vertical_inches = mean(vertical_inches),
    horizontal_inches = mean(horizontal_inches),
    extension = mean(extension),
    release_height = mean(release_height),
    .groups = "drop"
  )

print(ff_2026_summary, width = Inf)

metrics <- c(
  "velocity",
  "spin_rate",
  "vertical_inches",
  "horizontal_inches",
  "extension",
  "release_height"
)


# WHY: Compare game-level means and show 95% confidence intervals. Multiple tests increase the chance of an apparently significant result by chance.
pre_post_tests <- purrr::map_dfr(metrics, function(metric) {
  
  pre <- ff_2026_games |>
    filter(period == "Pre-IL") |>
    pull(all_of(metric))
  
  post <- ff_2026_games |>
    filter(period == "Post-IL") |>
    pull(all_of(metric))
  
  test <- t.test(post, pre, conf.level = 0.95)
  
  tibble(
    metric = metric,
    difference = unname(mean(post) - mean(pre)),
    ci_low = test$conf.int[1],
    ci_high = test$conf.int[2],
    p_value = test$p.value
  )
})

print(pre_post_tests, n = Inf, width = Inf)

ff_2026_timeline <- ff_2026_games |>
  arrange(game_date) |>
  mutate(
    start_number = row_number(),
    starts_since_return = if_else(
      period == "Post-IL",
      cumsum(period == "Post-IL"),
      NA_integer_
    )
  ) |>
  select(
    start_number,
    game_date,
    period,
    pitches,
    velocity,
    spin_rate,
    vertical_inches,
    horizontal_inches
  )

print(ff_2026_timeline, n = Inf, width = Inf)

horizontal_trend <- lm(
  horizontal_inches ~ start_number,
  data = ff_2026_timeline
)

summary(horizontal_trend)
confint(horizontal_trend)

horizontal_adjusted <- lm(
  horizontal_inches ~ start_number +
    velocity + spin_rate,
  data = ff_2026_timeline
)

summary(horizontal_adjusted)
confint(horizontal_adjusted)

horizontal_mechanics <- ff_2026_games |>
  arrange(game_date) |>
  mutate(start_number = row_number())

mechanics_model <- lm(
  horizontal_inches ~ start_number +
    velocity +
    spin_rate +
    extension +
    release_height,
  data = horizontal_mechanics
)

summary(mechanics_model)
confint(mechanics_model)

release_x_2026 <- skubal_2026 |>
  filter(pitch_type == "FF") |>
  group_by(game_pk) |>
  summarise(
    release_x = mean(release_pos_x, na.rm = TRUE),
    .groups = "drop"
  )

horizontal_mechanics <- horizontal_mechanics |>
  left_join(release_x_2026, by = "game_pk")

mechanics_full_model <- lm(
  horizontal_inches ~ start_number +
    velocity +
    spin_rate +
    extension +
    release_height +
    release_x,
  data = horizontal_mechanics
)

summary(mechanics_full_model)
confint(mechanics_full_model)

pitch_mix_2026 <- skubal_2026 |>
  filter(period != "IL period") |>
  count(period, pitch_type, name = "pitches") |>
  group_by(period) |>
  mutate(
    total_pitches = sum(pitches),
    usage_pct = 100 * pitches / total_pitches
  ) |>
  ungroup() |>
  arrange(pitch_type, period)

print(pitch_mix_2026, n = Inf)

arsenal_2026 <- skubal_2026 |>
  filter(
    period != "IL period",
    !is.na(pitch_type)
  ) |>
  group_by(period, pitch_type) |>
  summarise(
    pitches = n(),
    velocity = mean(release_speed, na.rm = TRUE),
    spin_rate = mean(release_spin_rate, na.rm = TRUE),
    horizontal_inches = mean(pfx_x, na.rm = TRUE) * 12,
    vertical_inches = mean(pfx_z, na.rm = TRUE) * 12,
    .groups = "drop"
  ) |>
  arrange(pitch_type, period)

print(arsenal_2026, n = Inf, width = Inf)


# ==========================================================================
# 08 | Fastball and sinker movement over time
# ==========================================================================
# Check whether movement changes are shared across pitch types and whether
# release position helps explain them. A statistical association is not proof
# of a mechanical cause.
#
fastball_trends <- skubal_2026 |>
  filter(pitch_type %in% c("FF", "SI")) |>
  group_by(game_pk, game_date, pitch_type) |>
  summarise(
    pitches = n(),
    horizontal_inches = mean(pfx_x, na.rm = TRUE) * 12,
    vertical_inches = mean(pfx_z, na.rm = TRUE) * 12,
    velocity = mean(release_speed, na.rm = TRUE),
    spin_rate = mean(release_spin_rate, na.rm = TRUE),
    release_x = mean(release_pos_x, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(game_date) |>
  group_by(pitch_type) |>
  mutate(start_number = row_number()) |>
  ungroup()


# WHY: Estimate movement trends separately for four-seamers and sinkers.
fastball_trend_results <- fastball_trends |>
  group_by(pitch_type) |>
  group_modify(~ {
    model <- lm(
      horizontal_inches ~ start_number,
      data = .x
    )
    
    broom::tidy(model, conf.int = TRUE) |>
      filter(term == "start_number") |>
      select(estimate, conf.low, conf.high, p.value)
  }) |>
  ungroup()

print(fastball_trend_results, width = Inf)

fastball_comparison <- fastball_trends |>
  select(
    game_pk,
    game_date,
    pitch_type,
    horizontal_inches,
    release_x
  ) |>
  pivot_wider(
    names_from = pitch_type,
    values_from = c(horizontal_inches, release_x)
  ) |>
  arrange(game_date) |>
  mutate(start_number = row_number())

# Correlation between FF and SI horizontal movement
cor.test(
  fastball_comparison$horizontal_inches_FF,
  fastball_comparison$horizontal_inches_SI
)

# Release-position trends
release_trends <- fastball_trends |>
  group_by(pitch_type) |>
  group_modify(~ {
    broom::tidy(
      lm(release_x ~ start_number, data = .x),
      conf.int = TRUE
    ) |>
      filter(term == "start_number") |>
      select(estimate, conf.low, conf.high, p.value)
  }) |>
  ungroup()

print(release_trends)

ff_residuals <- resid(
  lm(
    horizontal_inches_FF ~ start_number,
    data = fastball_comparison
  )
)

si_residuals <- resid(
  lm(
    horizontal_inches_SI ~ start_number,
    data = fastball_comparison
  )
)

cor.test(ff_residuals, si_residuals)

ff_model <- lm(
  horizontal_inches_FF ~ start_number + release_x_FF,
  data = fastball_comparison
)

si_model <- lm(
  horizontal_inches_SI ~ start_number + release_x_SI,
  data = fastball_comparison
)

cor.test(
  resid(ff_model),
  resid(si_model)
)

fastball_effectiveness <- ff_model_data |>
  filter(pitch_type %in% c("FF", "SI")) |>
  group_by(game_pk, pitch_type) |>
  summarise(
    swings = sum(swing),
    whiffs = sum(whiff),
    pitches = n(),
    csw = sum(csw),
    .groups = "drop"
  ) |>
  mutate(
    whiff_rate = if_else(
      swings > 0,
      whiffs / swings,
      NA_real_
    ),
    csw_rate = csw / pitches
  )

fastball_effectiveness_2026 <- fastball_trends |>
  left_join(
    fastball_effectiveness,
    by = c("game_pk", "pitch_type"),
    suffix = c("_movement", "_outcome")
  )

fastball_effectiveness_2026 |>
  group_by(pitch_type) |>
  summarise(
    games = n(),
    pitches = sum(pitches_outcome),
    swings = sum(swings),
    whiffs = sum(whiffs),
    whiff_rate = sum(whiffs) / sum(swings),
    csw_rate = sum(csw) / sum(pitches_outcome),
    .groups = "drop"
  ) |>
  print(width = Inf)


# ==========================================================================
# 09 | Build the Rogers Centre comparison data
# ==========================================================================
# Download public Statcast records from Toronto home games and collect game-
# level roof information. This is a separate ballpark sample, not a record of
# Skubal pitching in every condition.
#
rogers_test <- baseballr::statcast_search(
  start_date = "2026-04-01",
  end_date = "2026-04-30",
  player_type = "pitcher"
) |>
  filter(
    game_type == "R",
    home_team == "TOR"
  )

saveRDS(rogers_test, "data/rogers_test_2026_april.rds")

names(formals(baseballr::statcast_search))
packageVersion("baseballr")
baseballr::statcast_search
getAnywhere("statcast_search")

dir.create("data", showWarnings = FALSE)


# WHY: Break large Statcast downloads into months to make failed requests easier to retry.
rogers_months <- tidyr::crossing(
  season = 2024:2026,
  month = 3:9
) |>
  mutate(
    start_date = as.Date(
      sprintf("%d-%02d-01", season, month)
    ),
    end_date = lubridate::ceiling_date(
      start_date, "month"
    ) - 1
  )

rogers_download_log <- vector(
  "list",
  nrow(rogers_months)
)

for (i in seq_len(nrow(rogers_months))) {
  
  yr <- rogers_months$season[i]
  mo <- rogers_months$month[i]
  
  file <- sprintf(
    "data/rogers_stadium_%d_%02d.rds",
    yr, mo
  )
  
  if (file.exists(file)) {
    dat <- readRDS(file)
    status <- "existing"
  } else {
    
    url <- paste0(
      "https://baseballsavant.mlb.com/statcast_search/csv?",
      "all=true",
      "&type=details",
      "&player_type=pitcher",
      "&hfGT=R%7C",
      "&hfSea=", yr, "%7C",
      "&stadium=14",
      "&game_date_gt=", rogers_months$start_date[i],
      "&game_date_lt=", rogers_months$end_date[i]
    )
    
    dat <- tryCatch(
      readr::read_csv(
        url,
        show_col_types = FALSE,
        progress = FALSE
      ),
      error = function(e) {
        message("FAILED: ", yr, "-", mo, ": ", conditionMessage(e))
        NULL
      }
    )
    
    status <- if (is.null(dat)) "failed" else "downloaded"
    
    if (!is.null(dat) && "game_pk" %in% names(dat)) {
      saveRDS(dat, file)
    }
  }
  
  rogers_download_log[[i]] <- tibble(
    season = yr,
    month = mo,
    status = status,
    games = if (!is.null(dat) && "game_pk" %in% names(dat))
      n_distinct(dat$game_pk) else NA_integer_,
    pitches = if (!is.null(dat)) nrow(dat) else NA_integer_
  )
  
  message(yr, "-", sprintf("%02d", mo), ": ", status)
  
  Sys.sleep(2)
}

rogers_download_log <- bind_rows(rogers_download_log)

print(rogers_download_log, n = Inf)


rogers_files <- list.files(
  "data",
  pattern = "^rogers_stadium_20(24|25|26)_[0-9]{2}\\.rds$",
  full.names = TRUE
)

# Standardize all columns as character before combining

# WHY: Combine cached files and deduplicate pitches using game, at-bat and pitch identifiers.
rogers <- purrr::map_dfr(
  rogers_files,
  function(file) {
    readRDS(file) |>
      mutate(across(everything(), as.character))
  }
) |>
  mutate(
    game_date = as.Date(game_date),
    across(
      c(
        release_speed,
        release_spin_rate,
        release_extension,
        release_pos_x,
        release_pos_z,
        pfx_x,
        pfx_z
      ),
      readr::parse_double
    ),
    across(
      c(
        game_pk,
        game_year,
        pitcher,
        batter,
        at_bat_number,
        pitch_number
      ),
      readr::parse_integer
    )
  ) |>
  filter(
    game_type == "R",
    home_team == "TOR"
  ) |>
  distinct(
    game_pk,
    at_bat_number,
    pitch_number,
    .keep_all = TRUE
  )

saveRDS(rogers, "data/rogers_2024_2026.rds")

rogers |>
  group_by(game_year) |>
  summarise(
    games = n_distinct(game_pk),
    pitches = n(),
    pitchers = n_distinct(pitcher),
    missing_movement = sum(is.na(pfx_x) | is.na(pfx_z)),
    .groups = "drop"
  ) |>
  print(width = Inf)

rogers_games <- rogers |>
  group_by(game_pk, game_date, game_year, away_team) |>
  summarise(
    pitches = n(),
    pitchers = n_distinct(pitcher),
    .groups = "drop"
  ) |>
  arrange(game_date)

saveRDS(
  rogers_games,
  "data/rogers_games_2024_2026.rds"
)

rogers_games |>
  count(game_year, name = "games")

# Test MLB's game feed for roof information
game_id <- rogers_games$game_pk[1]

game_feed <- jsonlite::fromJSON(
  paste0(
    "https://statsapi.mlb.com/api/v1.1/game/",
    game_id,
    "/feed/live"
  ),
  simplifyVector = FALSE
)

# Inspect venue and weather information
game_feed$gameData$venue

game_feed$gameData$weather

roof_test_games <- rogers_games |>
  
  roof_test_games <- rogers_games |>
  arrange(game_year, game_date) |>
  group_by(game_year) |>
  mutate(game_number = row_number()) |>
  filter(game_number %in% c(1, 20, 40, 60, 81)) |>
  ungroup()

roof_test <- purrr::map_dfr(
  seq_len(nrow(roof_test_games)),
  function(i) {
    
    game_id <- roof_test_games$game_pk[i]
    
    feed <- tryCatch(
      jsonlite::fromJSON(
        paste0(
          "https://statsapi.mlb.com/api/v1.1/game/",
          game_id,
          "/feed/live"
        ),
        simplifyVector = FALSE
      ),
      error = function(e) NULL
    )
    
    condition <- if (is.null(feed)) {
      NA_character_
    } else {
      feed$gameData$weather$condition
    }
    
    tibble(
      game_pk = game_id,
      game_date = roof_test_games$game_date[i],
      game_year = roof_test_games$game_year[i],
      roof_condition = if (is.null(condition)) NA_character_ else condition
    )
  }
)

print(roof_test, n = Inf) 

names(rogers_games)


# WHY: Retrieve and cache roof-condition records. Weather descriptions may be imperfect proxies for roof status.
rogers_roof <- purrr::map_dfr(
  seq_len(nrow(rogers_games)),
  function(i) {
    
    game_id <- rogers_games$game_pk[i]
    
    feed <- tryCatch(
      jsonlite::fromJSON(
        paste0(
          "https://statsapi.mlb.com/api/v1.1/game/",
          game_id,
          "/feed/live"
        ),
        simplifyVector = FALSE
      ),
      error = function(e) NULL
    )
    
    condition <- if (is.null(feed)) {
      NA_character_
    } else {
      feed$gameData$weather$condition
    }
    
    tibble(
      game_pk = game_id,
      game_date = rogers_games$game_date[i],
      game_year = rogers_games$game_year[i],
      roof_condition = if (is.null(condition)) {
        NA_character_
      } else {
        condition
      }
    )
  }
)

saveRDS(
  rogers_roof,
  "data/rogers_roof_2024_2026.rds"
)


options(timeout = 15)

roof_file <- "data/rogers_roof_2024_2026.rds"


# WHY: Retrieve and cache roof-condition records. Weather descriptions may be imperfect proxies for roof status.
rogers_roof <- if (file.exists(roof_file)) {
  readRDS(roof_file)
} else {
  tibble(
    game_pk = integer(),
    game_date = as.Date(character()),
    game_year = integer(),
    roof_condition = character()
  )
}

remaining_games <- rogers_games |>
  filter(!game_pk %in% rogers_roof$game_pk)

for (i in seq_len(nrow(remaining_games))) {
  
  game_id <- remaining_games$game_pk[i]
  
  cat(
    "Processing", i, "of", nrow(remaining_games),
    "| Game:", game_id, "\n"
  )
  flush.console()
  
  condition <- tryCatch({
    
    url <- paste0(
      "https://statsapi.mlb.com/api/v1.1/game/",
      game_id,
      "/feed/live"
    )
    
    feed <- jsonlite::fromJSON(
      url,
      simplifyVector = FALSE
    )
    
    feed$gameData$weather$condition
    
  }, error = function(e) NA_character_)
  
  if (is.null(condition) || length(condition) == 0) {
    condition <- NA_character_
  }
  
  rogers_roof <- bind_rows(
    rogers_roof,
    tibble(
      game_pk = game_id,
      game_date = remaining_games$game_date[i],
      game_year = remaining_games$game_year[i],
      roof_condition = condition
    )
  )
  
  saveRDS(rogers_roof, roof_file)
  
  Sys.sleep(0.25)
}

rogers_roof |>
  count(game_year, roof_condition) |>
  print(n = Inf)


# WHY: Retrieve and cache roof-condition records. Weather descriptions may be imperfect proxies for roof status.
rogers_roof <- rogers_roof |>
  mutate(
    roof_status = case_when(
      roof_condition == "Roof Closed" ~ "Closed",
      roof_condition %in% c(
        "Clear", "Cloudy", "Partly Cloudy",
        "Sunny", "Overcast"
      ) ~ "Open",
      TRUE ~ "Unknown"
    )
  )

rogers_environment <- rogers |>
  left_join(
    rogers_roof |>
      select(game_pk, roof_status, roof_condition),
    by = "game_pk"
  )

rogers_environment |>
  group_by(game_year, roof_status) |>
  summarise(
    games = n_distinct(game_pk),
    pitches = n(),
    pitchers = n_distinct(pitcher),
    .groups = "drop"
  ) |>
  print(n = Inf)


saveRDS(
  rogers_environment,
  "data/rogers_environment_2024_2026.rds"
)

rogers_roof |>
  filter(roof_status == "Unknown") |>
  print(width = Inf)

feed <- jsonlite::fromJSON(
  "https://statsapi.mlb.com/api/v1.1/game/744888/feed/live",
  simplifyVector = FALSE
)

list(
  weather = feed$gameData$weather,
  game_info = feed$gameData$game,
  status = feed$gameData$status
)


# WHY: Retrieve and cache roof-condition records. Weather descriptions may be imperfect proxies for roof status.
rogers_roof <- rogers_roof |>
  mutate(
    roof_condition = if_else(
      game_pk == 744888,
      "Roof Closed",
      roof_condition
    ),
    roof_status = if_else(
      game_pk == 744888,
      "Closed",
      roof_status
    )
  )

rogers_environment <- rogers |>
  left_join(
    rogers_roof |>
      select(game_pk, roof_status, roof_condition),
    by = "game_pk"
  )

saveRDS(
  rogers_roof,
  "data/rogers_roof_2024_2026.rds"
)

saveRDS(
  rogers_environment,
  "data/rogers_environment_2024_2026.rds"
)

rogers_environment |>
  group_by(roof_status) |>
  summarise(
    games = n_distinct(game_pk),
    pitches = n(),
    .groups = "drop"
  )


# ==========================================================================
# 10 | Rogers Centre roof analysis
# ==========================================================================
# Compare pitch characteristics under open and closed roof conditions. Focus
# on left-handed pitchers and within-pitcher comparisons to reduce, but not
# eliminate, differences between pitchers.
#
rogers_fastballs <- rogers_environment |>
  filter(
    pitch_type %in% c("FF", "SI"),
    roof_status %in% c("Open", "Closed")
  ) |>
  mutate(
    horizontal_inches = pfx_x * 12,
    vertical_inches = pfx_z * 12
  )

rogers_fastball_summary <- rogers_fastballs |>
  group_by(pitch_type, roof_status) |>
  summarise(
    games = n_distinct(game_pk),
    pitchers = n_distinct(pitcher),
    pitches = n(),
    velocity = mean(release_speed, na.rm = TRUE),
    horizontal_inches = mean(horizontal_inches, na.rm = TRUE),
    vertical_inches = mean(vertical_inches, na.rm = TRUE),
    spin_rate = mean(release_spin_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(pitch_type, roof_status)

print(rogers_fastball_summary, width = Inf)

rogers_handedness <- rogers_fastballs |>
  group_by(
    pitch_type,
    p_throws,
    roof_status
  ) |>
  summarise(
    pitchers = n_distinct(pitcher),
    pitches = n(),
    velocity = mean(release_speed, na.rm = TRUE),
    horizontal_inches = mean(horizontal_inches, na.rm = TRUE),
    vertical_inches = mean(vertical_inches, na.rm = TRUE),
    spin_rate = mean(release_spin_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(pitch_type, p_throws, roof_status)

print(rogers_handedness, n = Inf, width = Inf)

rogers_left <- rogers_fastballs |>
  filter(p_throws == "L") |>
  mutate(
    roof_status = factor(
      roof_status,
      levels = c("Open", "Closed")
    )
  )

# Restrict to pitchers observed under both roof conditions
eligible_pitchers <- rogers_left |>
  distinct(pitch_type, pitcher, roof_status) |>
  count(pitch_type, pitcher, name = "conditions") |>
  filter(conditions == 2)


# WHY: Keep left-handed pitchers observed under both conditions, reducing between-pitcher confounding.
rogers_left_matched <- rogers_left |>
  semi_join(
    eligible_pitchers,
    by = c("pitch_type", "pitcher")
  )

rogers_left_matched |>
  group_by(pitch_type) |>
  summarise(
    pitchers = n_distinct(pitcher),
    games = n_distinct(game_pk),
    pitches = n(),
    .groups = "drop"
  )


# WHY: Identify pitcher-season combinations with both roof states. This is a stricter comparison than pooling all pitchers.
rogers_within_season <- rogers_left |>
  group_by(
    pitch_type,
    pitcher,
    game_year
  ) |>
  summarise(
    roof_conditions = n_distinct(roof_status),
    games = n_distinct(game_pk),
    pitches = n(),
    .groups = "drop"
  ) |>
  filter(roof_conditions == 2)

rogers_within_season |>
  group_by(pitch_type) |>
  summarise(
    pitchers = n_distinct(pitcher),
    pitcher_seasons = n(),
    games = sum(games),
    pitches = sum(pitches),
    .groups = "drop"
  ) |>
  print(width = Inf)

metrics <- c(
  "release_speed",
  "horizontal_inches",
  "vertical_inches"
)


# WHY: Estimate roof associations using pitcher-season controls and standard errors clustered by game.
roof_models <- purrr::map_dfr(
  c("FF", "SI"),
  function(pt) {
    
    purrr::map_dfr(
      metrics,
      function(metric) {
        
        dat <- rogers_model_data |>
          filter(
            pitch_type == pt,
            !is.na(.data[[metric]])
          )
        
        formula <- stats::as.formula(
          paste(metric, "~ roof_closed + pitcher_season")
        )
        
        model <- lm(formula, data = dat)
        
        robust_vcov <- sandwich::vcovCL(
          model,
          cluster = dat$game_pk,
          type = "HC1"
        )
        
        test <- lmtest::coeftest(
          model,
          vcov. = robust_vcov
        )
        
        estimate <- unname(test["roof_closed", "Estimate"])
        se <- unname(test["roof_closed", "Std. Error"])
        
        tibble(
          pitch_type = pt,
          metric = metric,
          pitches = nrow(dat),
          games = n_distinct(dat$game_pk),
          estimate = estimate,
          lower_95 = estimate - 1.96 * se,
          upper_95 = estimate + 1.96 * se,
          p_value = test["roof_closed", "Pr(>|t|)"]
        )
      }
    )
  }
)

print(roof_models, n = Inf, width = Inf)

# REPRODUCIBILITY NOTE: rogers_model_data is referenced here, but the
# uploaded exploratory script does not define it explicitly.
# Reconstruct and verify that analysis sample before running this block.
rogers_whiffs <- rogers_model_data |>
  mutate(
    swing = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip",
      "foul",
      "foul_bunt",
      "hit_into_play",
      "hit_into_play_no_out",
      "hit_into_play_score",
      "missed_bunt",
      "bunt_foul_tip"
    ),
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip",
      "missed_bunt"
    )
  ) |>
  filter(swing)

rogers_whiffs |>
  group_by(pitch_type, roof_status) |>
  summarise(
    games = n_distinct(game_pk),
    swings = n(),
    whiffs = sum(whiff),
    whiff_rate = mean(whiff),
    .groups = "drop"
  ) |>
  print(width = Inf)


# WHY: Fit logistic models for whiffs and express effects as odds ratios, not percentage-point differences.
whiff_models <- purrr::map_dfr(
  c("FF", "SI"),
  function(pt) {
    
    dat <- rogers_whiffs |>
      filter(pitch_type == pt) |>
      mutate(whiff = as.integer(whiff))
    
    model <- glm(
      whiff ~ roof_closed + pitcher_season,
      data = dat,
      family = binomial()
    )
    
    robust_vcov <- sandwich::vcovCL(
      model,
      cluster = dat$game_pk,
      type = "HC1"
    )
    
    test <- lmtest::coeftest(
      model,
      vcov. = robust_vcov
    )
    
    estimate <- unname(test["roof_closed", "Estimate"])
    se <- unname(test["roof_closed", "Std. Error"])
    
    tibble(
      pitch_type = pt,
      swings = nrow(dat),
      games = n_distinct(dat$game_pk),
      odds_ratio = exp(estimate),
      lower_95 = exp(estimate - 1.96 * se),
      upper_95 = exp(estimate + 1.96 * se),
      p_value = test["roof_closed", "Pr(>|z|)"]
    )
  }
)

print(whiff_models, width = Inf)


# ==========================================================================
# 11 | How Skubal compares with pitchers at Rogers Centre
# ==========================================================================
# Compare Skubal's fastball and sinker characteristics with the public Rogers
# Centre sample. Similar pitch traits do not guarantee similar outcomes.
#
skubal_comparison <- skubal_2026 |>
  filter(pitch_type %in% c("FF", "SI")) |>
  mutate(
    horizontal_inches = pfx_x * 12,
    vertical_inches = pfx_z * 12
  )

rogers_reference <- rogers_fastballs |>
  filter(p_throws == "L")

metrics <- c(
  "release_speed",
  "horizontal_inches",
  "vertical_inches",
  "release_spin_rate"
)

skubal_profile <- purrr::map_dfr(
  c("FF", "SI"),
  function(pt) {

    purrr::map_dfr(
      metrics,
      function(metric) {

        reference <- rogers_reference |>
          filter(pitch_type == pt) |>
          pull(all_of(metric))

        player <- skubal_comparison |>
          filter(pitch_type == pt) |>
          pull(all_of(metric))

        reference <- reference[is.finite(reference)]
        player <- player[is.finite(player)]

        player_mean <- mean(player)

        tibble(
          pitch_type = pt,
          metric = metric,
          skubal_mean = player_mean,
          rogers_mean = mean(reference),
          rogers_sd = sd(reference),
          difference = player_mean - mean(reference),
          percentile = mean(reference <= player_mean) * 100
        )
      }
    )
  }
)

print(skubal_profile, n = Inf, width = Inf)

rogers_pitcher_profiles <- rogers_reference |>
  group_by(
    game_year,
    pitcher,
    pitch_type
  ) |>
  summarise(
    pitches = n(),
    velocity = mean(release_speed, na.rm = TRUE),
    horizontal_inches = mean(horizontal_inches, na.rm = TRUE),
    vertical_inches = mean(vertical_inches, na.rm = TRUE),
    spin_rate = mean(release_spin_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(pitches >= 50)

rogers_pitcher_profiles |>
  group_by(pitch_type) |>
  summarise(
    pitcher_seasons = n(),
    unique_pitchers = n_distinct(pitcher),
    median_pitches = median(pitches),
    .groups = "drop"
  ) |>
  print(width = Inf)

skubal_targets <- skubal_profile |>
  select(pitch_type, metric, skubal_mean) |>
  pivot_wider(
    names_from = metric,
    values_from = skubal_mean
  ) |>
  rename(
    target_velocity = release_speed,
    target_horizontal = horizontal_inches,
    target_vertical = vertical_inches
  )

rogers_comparable <- rogers_reference |>
  left_join(
    skubal_targets,
    by = "pitch_type"
  ) |>
  mutate(
    velocity_match =
      abs(release_speed - target_velocity) <= 2,
    
    movement_match =
      abs(horizontal_inches - target_horizontal) <= 4 &
      abs(vertical_inches - target_vertical) <= 4,
    
    full_match = velocity_match & movement_match
  )

rogers_comparable |>
  group_by(pitch_type) |>
  summarise(
    total_pitches = n(),
    velocity_matches = sum(velocity_match, na.rm = TRUE),
    movement_matches = sum(movement_match, na.rm = TRUE),
    full_matches = sum(full_match, na.rm = TRUE),
    matching_pitchers = n_distinct(pitcher[full_match %in% TRUE]),
    .groups = "drop"
  ) |>
  print(width = Inf)

rogers_match_effectiveness <- rogers_comparable |>
  mutate(
    swing = description %in% swing_descriptions,
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip",
      "missed_bunt"
    )
  ) |>
  group_by(pitch_type, roof_status) |>
  summarise(
    games = n_distinct(game_pk),
    pitchers = n_distinct(pitcher),
    comparable_pitches = sum(full_match, na.rm = TRUE),
    comparable_swings = sum(swing & full_match, na.rm = TRUE),
    comparable_whiffs = sum(whiff & full_match, na.rm = TRUE),
    whiff_rate = if_else(
      comparable_swings > 0,
      comparable_whiffs / comparable_swings,
      NA_real_
    ),
    .groups = "drop"
  )

print(rogers_match_effectiveness, n = Inf, width = Inf)

rogers_comparable |>
  filter(full_match) |>
  group_by(pitch_type, roof_status) |>
  summarise(
    pitches = n(),
    pitchers = n_distinct(pitcher),
    games = n_distinct(game_pk),
    pitcher_seasons = n_distinct(
      interaction(pitcher, game_year)
    ),
    .groups = "drop"
  ) |>
  print(width = Inf)

rogers_model_sample <- rogers_reference |>
  mutate(
    swing = description %in% swing_descriptions,
    whiff = description %in% c(
      "swinging_strike",
      "swinging_strike_blocked",
      "foul_tip",
      "missed_bunt"
    )
  ) |>
  filter(
    swing,
    !is.na(release_speed),
    !is.na(horizontal_inches),
    !is.na(vertical_inches),
    !is.na(release_spin_rate),
    !is.na(release_extension),
    !is.na(plate_x),
    !is.na(plate_z)
  )

rogers_model_sample |>
  group_by(pitch_type, roof_status) |>
  summarise(
    swings = n(),
    whiffs = sum(whiff),
    whiff_rate = mean(whiff),
    pitchers = n_distinct(pitcher),
    games = n_distinct(game_pk),
    .groups = "drop"
  ) |>
  print(width = Inf)

rogers_model_sample <- rogers_environment |>
  dplyr::filter(
    p_throws == "L",
    pitch_type %in% c("FF", "SI"),
    roof_status %in% c("Open", "Closed")
  ) |>
  dplyr::mutate(
    horizontal_inches = pfx_x * 12,
    vertical_inches = pfx_z * 12,
    swing = description %in% c(
      "swinging_strike", "swinging_strike_blocked",
      "foul_tip", "foul", "foul_bunt",
      "hit_into_play", "hit_into_play_no_out",
      "hit_into_play_score", "missed_bunt",
      "bunt_foul_tip"
    ),
    whiff = as.integer(description %in% c(
      "swinging_strike", "swinging_strike_blocked",
      "foul_tip", "missed_bunt"
    ))
  ) |>
  dplyr::filter(
    swing,
    dplyr::if_all(
      c(release_speed, horizontal_inches,
        vertical_inches, release_spin_rate,
        release_extension, plate_x, plate_z),
      ~ !is.na(.x)
    )
  )

rogers_model_sample |>
  dplyr::count(pitch_type, roof_status)

rogers_model_sample |>
  dplyr::group_by(pitch_type) |>
  dplyr::summarise(
    min_plate_x = min(plate_x),
    max_plate_x = max(plate_x),
    min_plate_z = min(plate_z),
    max_plate_z = max(plate_z),
    min_extension = min(release_extension),
    max_extension = max(release_extension),
    .groups = "drop"
  ) |>
  print(width = Inf)

rogers_model_sample <- rogers_model_sample |>
  dplyr::mutate(
    plate_x = as.numeric(plate_x),
    plate_z = as.numeric(plate_z)
  )

sapply(
  rogers_model_sample[
    c("release_speed", "horizontal_inches",
      "vertical_inches", "release_extension",
      "plate_x", "plate_z")
  ],
  class
)

for (pt in c("FF", "SI")) {
  
  dat <- rogers_model_sample |>
    dplyr::filter(pitch_type == pt) |>
    dplyr::mutate(
      roof_closed = as.integer(roof_status == "Closed"),
      season = factor(game_year)
    )
  
  fit <- glm(
    whiff ~ roof_closed +
      release_speed +
      horizontal_inches +
      vertical_inches +
      release_extension +
      plate_x +
      plate_z +
      season,
    data = dat,
    family = binomial()
  )
  
  cat(
    "\nPitch:", pt,
    "\nConverged:", fit$converged,
    "\nIterations:", fit$iter,
    "\n"
  )
  
  if (fit$converged) {
    
    robust_se <- sandwich::vcovCL(
      fit,
      cluster = dat$game_pk,
      type = "HC1"
    )
    
    print(
      lmtest::coeftest(
        fit,
        vcov. = robust_se
      )
    )
  }
}


# ==========================================================================
# 12 | Early contract scenario exploration
# ==========================================================================
# Explore what combinations of future WAR and contract size would imply.
# These preliminary scenarios are useful for learning, but the later aging
# model replaces them as the primary projection.
#

# WHY: Specify hypothetical future production scenarios before introducing the historical projection.
skubal_scenarios <- tibble::tibble(
  scenario = c("Optimistic", "Baseline", "Pessimistic"),
  initial_war = c(6.0, 5.2, 4.0),
  annual_decline = c(0.35, 0.50, 0.65)
)

skubal_war_projection <- tidyr::crossing(
  skubal_scenarios,
  year = 2027:2034
) |>
  dplyr::mutate(
    age = 30 + year - 2027,
    projected_war = pmax(
      0,
      initial_war - annual_decline * (year - 2027)
    )
  )

skubal_war_summary <- skubal_war_projection |>
  dplyr::group_by(scenario) |>
  dplyr::summarise(
    total_war = sum(projected_war),
    average_war = mean(projected_war),
    final_year_war = dplyr::last(projected_war),
    .groups = "drop"
  )

print(skubal_war_summary, width = Inf)

contract_scenarios <- tidyr::crossing(
  skubal_war_summary,
  contract_millions = c(320, 360, 400),
  dollars_per_war = c(9, 10, 11)
) |>
  dplyr::mutate(
    war_value_millions = total_war * dollars_per_war,
    surplus_millions =
      war_value_millions - contract_millions,
    cost_per_war =
      contract_millions / total_war
  )

contract_summary <- contract_scenarios |>
  dplyr::filter(dollars_per_war == 10) |>
  dplyr::select(
    scenario,
    contract_millions,
    total_war,
    cost_per_war,
    surplus_millions
  )

print(contract_summary, n = Inf, width = Inf)

break_even <- tidyr::crossing(
  contract_millions = c(320, 360, 400),
  dollars_per_war = c(9, 10, 11)
) |>
  dplyr::mutate(
    required_war = contract_millions / dollars_per_war,
    annual_war = required_war / 8,
    required_2027_war = required_war / 8 +
      0.50 * 3.5
  )

print(break_even, n = Inf, width = Inf)

list.files("data", pattern = "\\.rds$", full.names = FALSE)


packageVersion("baseballr")

args(baseballr::fg_pitcher_leaders)


# ==========================================================================
# 13 | Download historical FanGraphs pitcher seasons
# ==========================================================================
# Build a historical record of pitcher performance and workload. Check
# downloaded seasons and missing values before making comparisons.
#
pitcher_test <- baseballr::fg_pitcher_leaders(
  startseason = "2024",
  endseason = "2024",
  qual = "0",
  ind = "1",
  pageitems = "100"
)

dim(pitcher_test)
names(pitcher_test)

dir.create("data/aging", showWarnings = FALSE)

for (yr in 2010:2026) {
  
  cat("Downloading", yr, "...\n")
  
  dat <- baseballr::fg_pitcher_leaders(
    startseason = as.character(yr),
    endseason = as.character(yr),
    qual = "0",
    ind = "1",
    pageitems = "2000"
  )
  
  if (is.data.frame(dat) && nrow(dat) > 0) {
    
    dat <- dat |>
      dplyr::select(
        playerid, PlayerName, Season,
        Age, GS, G, IP, WAR, ERA, FIP
      )
    
    saveRDS(
      dat,
      paste0("data/aging/pitchers_", yr, ".rds")
    )
    
    cat("Saved", nrow(dat), "records\n")
    
  } else {
    cat("FAILED:", yr, "\n")
  }
  
  Sys.sleep(1)
}

aging_files <- list.files(
  "data/aging",
  pattern = "^pitchers_[0-9]{4}\\.rds$",
  full.names = TRUE
)

length(aging_files)


# WHY: Combine FanGraphs seasons into a consistent pitcher-year panel.
pitcher_history <- purrr::map_dfr(
  aging_files,
  readRDS
) |>
  dplyr::mutate(
    Season = as.integer(Season),
    Age = as.integer(Age),
    GS = as.numeric(GS),
    IP = as.numeric(IP),
    WAR = as.numeric(WAR)
  )

pitcher_history |>
  dplyr::summarise(
    records = dplyr::n(),
    unique_pitchers = dplyr::n_distinct(playerid),
    seasons = dplyr::n_distinct(Season),
    earliest_season = min(Season),
    latest_season = max(Season),
    missing_age = sum(is.na(Age)),
    missing_war = sum(is.na(WAR)),
    duplicate_player_seasons =
      nrow(dplyr::cur_data()) -
      dplyr::n_distinct(playerid, Season)
  ) |>
  print(width = Inf)


# ==========================================================================
# 14 | Define the historical aging cohort
# ==========================================================================
# Select pitchers with meaningful recent performance around the same age as
# Skubal. Follow them across the next eight years, including seasons with no
# MLB production.
#

# WHY: Choose a comparable historical starting cohort using age, innings and recent WAR thresholds.
aging_candidates <- pitcher_history |>
  dplyr::filter(Age %in% 27:29) |>
  dplyr::group_by(playerid, PlayerName) |>
  dplyr::summarise(
    seasons_observed = dplyr::n_distinct(Age),
    total_ip = sum(IP),
    total_war = sum(WAR),
    total_starts = sum(GS),
    age29_season = max(Season[Age == 29], na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(
    seasons_observed == 3,
    total_ip >= 400,
    total_war >= 8,
    age29_season >= 2012,
    age29_season <= 2018
  ) |>
  dplyr::arrange(dplyr::desc(total_war))

print(
  aging_candidates |>
    dplyr::select(
      PlayerName,
      age29_season,
      total_ip,
      total_war,
      total_starts
    ),
  n = Inf,
  width = Inf
)


# WHY: Track every selected pitcher for eight seasons, including years without an MLB record.
aging_panel <- aging_candidates |>
  dplyr::select(
    playerid,
    PlayerName,
    age29_season,
    total_war,
    total_ip
  ) |>
  tidyr::crossing(age = 30:37) |>
  dplyr::mutate(
    Season = age29_season + (age - 29)
  ) |>
  dplyr::left_join(
    pitcher_history |>
      dplyr::select(
        playerid,
        Season,
        Age,
        IP,
        GS,
        WAR
      ),
    by = c("playerid", "Season")
  ) |>
  dplyr::mutate(
    appeared = !is.na(WAR),
    WAR = tidyr::replace_na(WAR, 0),
    IP = tidyr::replace_na(IP, 0),
    GS = tidyr::replace_na(GS, 0)
  )

aging_panel |>
  dplyr::group_by(age) |>
  dplyr::summarise(
    pitchers = dplyr::n_distinct(playerid),
    appeared = sum(appeared),
    appearance_rate = mean(appeared),
    mean_war = mean(WAR),
    median_war = median(WAR),
    mean_ip = mean(IP),
    .groups = "drop"
  ) |>
  print(n = Inf, width = Inf)


# ==========================================================================
# 15 | What happened to comparable pitchers?
# ==========================================================================
# Summarize eight-year outcomes and fit an initial relationship between
# recent WAR and future WAR. Averages alone can hide the wide spread of
# pitching careers.
#
aging_summary <- aging_panel |>
  dplyr::group_by(age) |>
  dplyr::summarise(
    pitchers = dplyr::n_distinct(playerid),
    appeared = sum(appeared),
    appearance_rate = mean(appeared),
    mean_war = mean(WAR),
    median_war = median(WAR),
    mean_ip = mean(IP),
    .groups = "drop"
  )

career_outcomes <- aging_panel |>
  dplyr::group_by(playerid, PlayerName) |>
  dplyr::summarise(
    eight_year_war = sum(WAR),
    eight_year_ip = sum(IP),
    seasons_appeared = sum(appeared),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(eight_year_war))

print(aging_summary, n = Inf, width = Inf)

career_outcomes |>
  dplyr::summarise(
    pitchers = dplyr::n(),
    mean_war = mean(eight_year_war),
    median_war = median(eight_year_war),
    p10_war = unname(quantile(eight_year_war, .10)),
    p90_war = unname(quantile(eight_year_war, .90)),
    mean_ip = mean(eight_year_ip),
    median_seasons_appeared = median(seasons_appeared)
  ) |>
  print(width = Inf)

career_outcomes |>
  dplyr::left_join(
    aging_candidates |>
      dplyr::select(playerid, total_war),
    by = "playerid"
  ) |>
  dplyr::select(
    PlayerName,
    total_war,
    eight_year_war,
    eight_year_ip,
    seasons_appeared
  ) |>
  dplyr::arrange(dplyr::desc(eight_year_war)) |>
  print(n = Inf, width = Inf)

war_relationship <- career_outcomes |>
  dplyr::left_join(
    aging_candidates |>
      dplyr::select(playerid, total_war),
    by = "playerid"
  )


# WHY: Estimate the association between pre-contract performance and subsequent eight-year WAR.
war_model <- lm(
  eight_year_war ~ total_war,
  data = war_relationship
)

summary(war_model)

confint(war_model, level = 0.95)

loocv_results <- purrr::map_dfr(
  seq_len(nrow(war_relationship)),
  function(i) {
    
    training <- war_relationship[-i, ]
    testing <- war_relationship[i, ]
    
    model <- lm(
      eight_year_war ~ total_war,
      data = training
    )
    
    tibble::tibble(
      PlayerName = testing$PlayerName,
      actual = testing$eight_year_war,
      predicted = as.numeric(
        predict(model, newdata = testing)
      ),
      baseline = mean(training$eight_year_war)
    )
  }
)

loocv_results |>
  dplyr::summarise(
    model_rmse = sqrt(mean((actual - predicted)^2)),
    baseline_rmse = sqrt(mean((actual - baseline)^2)),
    model_mae = mean(abs(actual - predicted)),
    baseline_mae = mean(abs(actual - baseline)),
    model_bias = mean(predicted - actual)
  ) |>
  print(width = Inf)


# ==========================================================================
# 16 | Check comparability and workload
# ==========================================================================
# Compare Skubal's starting point with the cohort and test whether additional
# workload information improves the projection.
#
skubal_baseline <- pitcher_history |>
  dplyr::filter(
    PlayerName == "Tarik Skubal",
    Age %in% 27:29
  ) |>
  dplyr::select(
    PlayerName, Season, Age,
    GS, IP, WAR, ERA, FIP
  ) |>
  dplyr::arrange(Season)

print(skubal_baseline, width = Inf)

skubal_baseline |>
  dplyr::summarise(
    seasons = dplyr::n(),
    total_ip = sum(IP),
    total_war = sum(WAR),
    total_starts = sum(GS)
  ) |>
  print(width = Inf)

skubal_comps <- war_relationship |>
  dplyr::filter(
    abs(total_war - 17.96) <= 4
  ) |>
  dplyr::arrange(
    abs(total_war - 17.96)
  ) |>
  dplyr::select(
    PlayerName,
    total_war,
    eight_year_war,
    eight_year_ip,
    seasons_appeared
  )

print(skubal_comps, n = Inf, width = Inf)

skubal_comps |>
  dplyr::summarise(
    pitchers = dplyr::n(),
    mean_war = mean(eight_year_war),
    median_war = median(eight_year_war),
    min_war = min(eight_year_war),
    max_war = max(eight_year_war)
  ) |>
  print(width = Inf)

workload_analysis <- aging_panel |>
  dplyr::filter(
    playerid %in% (
      war_relationship |>
        dplyr::filter(abs(total_war - 17.96) <= 4) |>
        dplyr::pull(playerid)
    )
  ) |>
  dplyr::group_by(PlayerName) |>
  dplyr::summarise(
    total_war = sum(WAR),
    total_ip = sum(IP),
    war_per_180_ip = dplyr::if_else(
      total_ip > 0,
      total_war / total_ip * 180,
      NA_real_
    ),
    seasons_appeared = sum(appeared),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(total_war))

print(workload_analysis, n = Inf, width = Inf)

aging_components <- aging_panel |>
  dplyr::group_by(age) |>
  dplyr::summarise(
    pitchers = dplyr::n(),
    pitchers_active = sum(IP > 0),
    appearance_rate = mean(IP > 0),
    mean_ip_all = mean(IP),
    mean_ip_active = mean(IP[IP > 0]),
    total_war = sum(WAR),
    total_ip = sum(IP),
    war_per_180_ip = total_war / total_ip * 180,
    .groups = "drop"
  )

print(aging_components, n = Inf, width = Inf)

aging_panel |>
  dplyr::group_by(Season) |>
  dplyr::summarise(
    pitcher_seasons = dplyr::n(),
    active_pitchers = sum(IP > 0),
    total_ip = sum(IP),
    total_war = sum(WAR),
    mean_ip = mean(IP),
    mean_war = mean(WAR),
    .groups = "drop"
  ) |>
  dplyr::arrange(Season) |>
  print(n = Inf, width = Inf)


# ==========================================================================
# 17 | Pandemic-season sensitivity
# ==========================================================================
# Check how the shortened 2020 season affects historical totals. Replacing
# 2020 with a neighboring-season estimate is a sensitivity assumption, not
# observed production.
#

# WHY: Replace shortened-season production with an explicit counterfactual for sensitivity testing only.
pandemic_sensitivity <- aging_panel |>
  dplyr::left_join(
    pitcher_history |>
      dplyr::filter(Season %in% c(2019, 2021)) |>
      dplyr::select(playerid, Season, WAR) |>
      tidyr::pivot_wider(
        names_from = Season,
        values_from = WAR,
        names_prefix = "war_"
      ),
    by = "playerid"
  ) |>
  dplyr::mutate(
    war_2019 = tidyr::replace_na(war_2019, 0),
    war_2021 = tidyr::replace_na(war_2021, 0),
    estimated_2020_war = (war_2019 + war_2021) / 2,
    adjusted_war = dplyr::if_else(
      Season == 2020,
      estimated_2020_war,
      WAR
    )
  ) |>
  dplyr::group_by(playerid, PlayerName) |>
  dplyr::summarise(
    actual_eight_year_war = sum(WAR),
    adjusted_eight_year_war = sum(adjusted_war),
    difference = adjusted_eight_year_war -
      actual_eight_year_war,
    .groups = "drop"
  )

pandemic_sensitivity |>
  dplyr::summarise(
    pitchers = dplyr::n(),
    actual_mean = mean(actual_eight_year_war),
    adjusted_mean = mean(adjusted_eight_year_war),
    actual_median = median(actual_eight_year_war),
    adjusted_median = median(adjusted_eight_year_war),
    mean_difference = mean(difference)
  ) |>
  print(width = Inf)


# ==========================================================================
# 18 | Fit and validate the aging projection
# ==========================================================================
# Compare alternative models, examine leave-one-out prediction errors and
# test sensitivity to individual pitchers. Predictive accuracy matters more
# than finding an impressive in-sample fit.
#
adjusted_model_data <- war_relationship |>
  dplyr::select(
    playerid, PlayerName, total_war
  ) |>
  dplyr::left_join(
    pandemic_sensitivity |>
      dplyr::select(
        playerid, adjusted_eight_year_war
      ),
    by = "playerid"
  )


# WHY: Fit the final WAR-only projection after the pandemic sensitivity adjustment.
adjusted_model <- lm(
  adjusted_eight_year_war ~ total_war,
  data = adjusted_model_data
)

summary(adjusted_model)

predict(
  adjusted_model,
  newdata = data.frame(total_war = 17.96),
  interval = "prediction",
  level = 0.95
)

workload_model <- lm(
  adjusted_eight_year_war ~ total_war + total_ip,
  data = adjusted_model_data |>
    dplyr::left_join(
      aging_candidates |>
        dplyr::select(playerid, total_ip),
      by = "playerid"
    )
)

summary(workload_model)

anova(adjusted_model, workload_model)

recent_performance_data <- adjusted_model_data |>
  dplyr::left_join(
    aging_candidates |>
      dplyr::select(playerid, age29_season),
    by = "playerid"
  ) |>
  dplyr::left_join(
    pitcher_history |>
      dplyr::filter(Age == 29) |>
      dplyr::select(
        playerid,
        Season,
        age29_war = WAR
      ),
    by = c(
      "playerid",
      "age29_season" = "Season"
    )
  )

recent_model <- lm(
  adjusted_eight_year_war ~ total_war + age29_war,
  data = recent_performance_data
)

summary(recent_model)

anova(adjusted_model, recent_model)


# WHY: Compare out-of-sample prediction error across candidate aging models.
cv_comparison <- purrr::map_dfr(
  seq_len(nrow(recent_performance_data)),
  function(i) {
    
    train <- recent_performance_data[-i, ]
    test <- recent_performance_data[i, ]
    
    model_1 <- lm(
      adjusted_eight_year_war ~ total_war,
      data = train
    )
    
    model_2 <- lm(
      adjusted_eight_year_war ~ total_war + age29_war,
      data = train
    )
    
    tibble::tibble(
      actual = test$adjusted_eight_year_war,
      war_only = as.numeric(predict(model_1, test)),
      war_recent = as.numeric(predict(model_2, test))
    )
  }
)

cv_comparison |>
  dplyr::summarise(
    war_only_rmse = sqrt(mean((actual - war_only)^2)),
    war_recent_rmse = sqrt(mean((actual - war_recent)^2)),
    war_only_mae = mean(abs(actual - war_only)),
    war_recent_mae = mean(abs(actual - war_recent))
  ) |>
  print(width = Inf)

prediction_sensitivity <- purrr::map_dfr(
  seq_len(nrow(adjusted_model_data)),
  function(i) {
    
    model <- lm(
      adjusted_eight_year_war ~ total_war,
      data = adjusted_model_data[-i, ]
    )
    
    tibble::tibble(
      excluded_pitcher = adjusted_model_data$PlayerName[i],
      skubal_prediction = as.numeric(
        predict(
          model,
          newdata = data.frame(total_war = 17.96)
        )
      )
    )
  }
)

prediction_sensitivity |>
  dplyr::summarise(
    minimum_prediction = min(skubal_prediction),
    maximum_prediction = max(skubal_prediction),
    median_prediction = median(skubal_prediction),
    prediction_range = max(skubal_prediction) -
      min(skubal_prediction)
  ) |>
  print(width = Inf)

prediction_sensitivity |>
  dplyr::arrange(skubal_prediction) |>
  dplyr::slice(c(1:3, (dplyr::n()-2):dplyr::n())) |>
  print(n = Inf, width = Inf)


# ==========================================================================
# 19 | Allocate future WAR by contract year
# ==========================================================================
# Distribute the eight-year WAR estimate using historical age patterns. These
# annual values are an allocation of one total forecast, not eight
# independently validated predictions.
#

# WHY: Allocate projected WAR across ages using historical average age patterns.
skubal_annual_projection <- aging_panel |>
  dplyr::group_by(age) |>
  dplyr::summarise(
    historical_mean_war = mean(WAR),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    Season = 2027 + (age - 30),
    historical_share =
      historical_mean_war / sum(historical_mean_war),
    projected_war = historical_share * 24.61681,
    cumulative_war = cumsum(projected_war)
  ) |>
  dplyr::select(
    Season,
    age,
    historical_mean_war,
    projected_war,
    cumulative_war
  )

print(skubal_annual_projection, n = Inf, width = Inf)


# WHY: Apply the assumed WAR price and discount future values to a common starting year.
contract_value <- skubal_annual_projection |>
  dplyr::mutate(
    contract_year = Season - 2027 + 1,
    value_millions = projected_war * 10,
    discount_factor = 1 / (1.05 ^ contract_year),
    present_value_millions =
      value_millions * discount_factor
  )

contract_value |>
  dplyr::summarise(
    total_war = sum(projected_war),
    undiscounted_value_m = sum(value_millions),
    present_value_m = sum(present_value_millions)
  ) |>
  print(width = Inf)

contract_scenarios <- tidyr::expand_grid(
  contract_millions = c(320, 360, 400),
  contract_year = 1:8
) |>
  dplyr::mutate(
    annual_salary_m = contract_millions / 8,
    discount_factor = 1 / (1.05 ^ contract_year),
    pv_salary_m = annual_salary_m * discount_factor
  ) |>
  dplyr::group_by(contract_millions) |>
  dplyr::summarise(
    annual_salary_m = dplyr::first(annual_salary_m),
    pv_contract_m = sum(pv_salary_m),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    pv_production_m = sum(contract_value$present_value_millions),
    pv_surplus_m = pv_production_m - pv_contract_m,
    break_even_dollars_per_war_m =
      pv_contract_m /
      sum(
        contract_value$projected_war *
          contract_value$discount_factor
      )
  )

print(contract_scenarios, width = Inf)

grep(
  "contract|salary|free.agent",
  getNamespaceExports("baseballr"),
  value = TRUE,
  ignore.case = TRUE
)

lapply(
  c(
    "espn_mlb_player_contracts",
    "espn_mlb_player_contract",
    "mlb_people_free_agents"
  ),
  function(fn) {
    list(
      function_name = fn,
      arguments = names(formals(getExportedValue("baseballr", fn)))
    )
  }
)

utils::help(
  "espn_mlb_player_contract",
  package = "baseballr"
)

utils::help(
  "espn_mlb_player_contracts",
  package = "baseballr"
)

utils::help(
  "espn_mlb_player_contract",
  package = "baseballr"
)


grep(
  "spotrac|cot|salary|contract",
  getNamespaceExports("baseballr"),
  value = TRUE,
  ignore.case = TRUE
)


# ==========================================================================
# 20 | Collect public free-agent contract information
# ==========================================================================
# Use publicly reported contract terms and compare signing classes. Reported
# headline totals may omit economically important details such as deferrals,
# options and incentives.
#
url <- paste0(
  "https://www.fangraphs.com/roster-resource/",
  "free-agent-tracker?season=2025&pos=sp"
)

tables <- tryCatch(
  rvest::read_html(url) |>
    rvest::html_elements("table") |>
    rvest::html_table(),
  error = function(e) {
    message(conditionMessage(e))
    list()
  }
)

length(tables)

purrr::imap_dfr(
  tables,
  function(tbl, i) {
    tibble::tibble(
      table_number = i,
      rows = nrow(tbl),
      columns = ncol(tbl),
      column_names = paste(names(tbl), collapse = " | ")
    )
  }
) |>
  print(n = Inf, width = Inf)

names(tables[[10]])

tables[[10]] |>
  dplyr::select(
    dplyr::any_of(c(
      "Name", "Pos", "Age",
      "2024 WAR", "Proj WAR",
      "Med Years", "Med Total", "Med AAV",
      "Years", "Total", "AAV", "Team"
    ))
  ) |>
  head(10) |>
  print(width = Inf)

contracts_2025 <- tables[[10]]

names(contracts_2025) <- make.names(
  names(contracts_2025),
  unique = TRUE
)

contracts_2025 |>
  dplyr::slice(3:12) |>
  print(width = Inf)

contracts_2025_clean <- contracts_2025 |>
  dplyr::filter(
    Pos == "SP",
    Name != "",
    Name != "Name"
  ) |>
  dplyr::mutate(
    Age = as.numeric(Age),
    prior_war = as.numeric(X2024.WAR),
    contract_years = as.numeric(Years),
    contract_total_m = readr::parse_number(Total.Salary),
    reported_aav_m = readr::parse_number(AAV),
    calculated_aav_m = contract_total_m / contract_years,
    projected_war = readr::parse_number(Proj.WAR)
  )

contracts_2025_clean |>
  dplyr::summarise(
    pitchers = dplyr::n(),
    signed_contracts = sum(!is.na(contract_total_m)),
    missing_contracts = sum(is.na(contract_total_m)),
    available_projections = sum(!is.na(projected_war)),
    total_guaranteed_m = sum(contract_total_m, na.rm = TRUE)
  ) |>
  print(width = Inf)

fa_table_inventory <- purrr::map_dfr(
  2022:2026,
  function(season) {
    
    url <- paste0(
      "https://www.fangraphs.com/roster-resource/",
      "free-agent-tracker?season=", season,
      "&pos=sp"
    )
    
    page_tables <- tryCatch(
      rvest::read_html(url) |>
        rvest::html_elements("table") |>
        rvest::html_table(),
      error = function(e) list()
    )
    
    tibble::tibble(
      season = season,
      tables_found = length(page_tables),
      contract_tables = sum(
        purrr::map_lgl(
          page_tables,
          ~ all(c(
            "Name", "Signing Team",
            "Years", "Total Salary"
          ) %in% names(.x))
        )
      ),
      contract_rows = sum(
        purrr::map_int(
          page_tables,
          ~ if (all(c(
            "Name", "Signing Team",
            "Years", "Total Salary"
          ) %in% names(.x))) nrow(.x) else 0L
        )
      )
    )
  }
)

print(fa_table_inventory, n = Inf, width = Inf)

season_check <- purrr::map_dfr(
  2022:2026,
  function(season) {
    
    url <- paste0(
      "https://www.fangraphs.com/roster-resource/",
      "free-agent-tracker?season=", season,
      "&pos=sp"
    )
    
    tbl <- rvest::read_html(url) |>
      rvest::html_elements("table") |>
      rvest::html_table()
    
    contract_tbl <- tbl[[10]]
    
    names(contract_tbl) <- make.names(
      names(contract_tbl),
      unique = TRUE
    )
    
    contract_tbl |>
      dplyr::filter(
        Pos == "SP",
        Name != "",
        Name != "Name"
      ) |>
      dplyr::slice_head(n = 5) |>
      dplyr::transmute(
        requested_season = season,
        Name,
        previous_war_column = names(contract_tbl)[8]
      )
  }
)

print(season_check, n = Inf, width = Inf)


# WHY: Combine public free-agent contracts across signing classes and standardize salary fields.
fa_contracts <- purrr::map_dfr(
  2022:2026,
  function(season) {
    
    url <- paste0(
      "https://www.fangraphs.com/roster-resource/",
      "free-agent-tracker?season=", season,
      "&pos=sp"
    )
    
    page_tables <- rvest::read_html(url) |>
      rvest::html_elements("table") |>
      rvest::html_table()
    
    tbl <- page_tables[[10]]
    
    names(tbl) <- make.names(
      names(tbl),
      unique = TRUE
    )
    
    prior_war_col <- paste0("X", season - 1, ".WAR")
    
    tbl |>
      dplyr::filter(
        Pos == "SP",
        Name != "",
        Name != "Name"
      ) |>
      dplyr::transmute(
        signing_class = season,
        player = Name,
        age = readr::parse_number(Age),
        previous_team = Prev.Team,
        signing_team = Signing.Team,
        prior_war = readr::parse_number(
          .data[[prior_war_col]]
        ),
        projected_war = readr::parse_number(Proj.WAR),
        contract_years = readr::parse_number(Years),
        contract_total_m = readr::parse_number(Total.Salary),
        reported_aav_m = readr::parse_number(AAV),
        crowd_years = readr::parse_number(Med.Years),
        crowd_total_m = readr::parse_number(Med.Total)
      )
  }
)

fa_contracts |>
  dplyr::group_by(signing_class) |>
  dplyr::summarise(
    pitchers = dplyr::n(),
    contracts_reported = sum(!is.na(contract_total_m)),
    projections_available = sum(!is.na(projected_war)),
    total_contract_value_m = sum(
      contract_total_m,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  print(n = Inf, width = Inf)

fa_long_term <- fa_contracts |>
  dplyr::filter(
    !is.na(contract_total_m),
    !is.na(contract_years),
    contract_years >= 3,
    contract_total_m > 0
  ) |>
  dplyr::mutate(
    nominal_aav_m = contract_total_m / contract_years
  )

fa_long_term |>
  dplyr::group_by(signing_class) |>
  dplyr::summarise(
    contracts = dplyr::n(),
    median_years = median(contract_years),
    median_total_m = median(contract_total_m),
    median_aav_m = median(nominal_aav_m),
    maximum_total_m = max(contract_total_m),
    .groups = "drop"
  ) |>
  print(n = Inf, width = Inf)

fa_long_term |>
  dplyr::arrange(dplyr::desc(contract_total_m)) |>
  dplyr::select(
    signing_class,
    player,
    age,
    contract_years,
    contract_total_m,
    nominal_aav_m
  ) |>
  dplyr::slice_head(n = 15) |>
  print(n = Inf, width = Inf)


# ==========================================================================
# 21 | Compare elite starting-pitcher contracts
# ==========================================================================
# Focus on more comparable free agents and examine their recent performance
# before signing. The sample is small, so correlations should not be treated
# as pricing rules.
#
elite_fa <- fa_long_term |>
  dplyr::filter(
    age >= 28,
    age <= 33,
    contract_years >= 5
  ) |>
  dplyr::arrange(dplyr::desc(contract_total_m))

elite_fa |>
  dplyr::select(
    signing_class,
    player,
    age,
    prior_war,
    contract_years,
    contract_total_m,
    reported_aav_m,
    nominal_aav_m
  ) |>
  print(n = Inf, width = Inf)


# WHY: Attach the three MLB seasons preceding each elite contract.
elite_war_history <- elite_fa |>
  dplyr::filter(player != "Kodai Senga") |>
  dplyr::mutate(
    last_season = signing_class - 1
  ) |>
  dplyr::left_join(
    pitcher_history |>
      dplyr::select(PlayerName, Season, WAR, IP),
    by = c("player" = "PlayerName"),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(
    Season >= last_season - 2,
    Season <= last_season
  ) |>
  dplyr::group_by(
    signing_class, player, age,
    contract_years, contract_total_m
  ) |>
  dplyr::summarise(
    seasons_found = dplyr::n_distinct(Season),
    three_year_war = sum(WAR),
    three_year_ip = sum(IP),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(three_year_war))

print(elite_war_history, n = Inf, width = Inf)

elite_war_history |>
  dplyr::mutate(
    contract_per_prior_war =
      contract_total_m / three_year_war
  ) |>
  dplyr::select(
    player,
    three_year_war,
    contract_years,
    contract_total_m,
    contract_per_prior_war
  ) |>
  dplyr::arrange(
    dplyr::desc(contract_per_prior_war)
  ) |>
  print(n = Inf, width = Inf)

cor.test(
  elite_war_history$three_year_war,
  elite_war_history$contract_total_m,
  method = "pearson"
)

elite_market_test <- elite_war_history |>
  dplyr::mutate(
    nominal_aav_m = contract_total_m / contract_years
  )

cor.test(
  elite_market_test$three_year_war,
  elite_market_test$nominal_aav_m,
  method = "pearson"
)

elite_market_test |>
  dplyr::select(
    player,
    three_year_war,
    contract_years,
    contract_total_m,
    nominal_aav_m
  ) |>
  dplyr::arrange(dplyr::desc(nominal_aav_m)) |>
  print(n = Inf, width = Inf)

cor.test(
  elite_market_test$three_year_war,
  elite_market_test$nominal_aav_m,
  method = "pearson"
)

fa_long_term |>
  dplyr::group_by(signing_class) |>
  dplyr::summarise(
    contracts = dplyr::n(),
    median_aav_m = median(nominal_aav_m),
    mean_aav_m = mean(nominal_aav_m),
    p75_aav_m = as.numeric(
      quantile(nominal_aav_m, 0.75)
    ),
    max_aav_m = max(nominal_aav_m),
    .groups = "drop"
  ) |>
  print(n = Inf, width = Inf)

fa_contracts |>
  dplyr::filter(
    !is.na(contract_total_m),
    !is.na(contract_years),
    contract_years >= 2,
    contract_total_m > 0
  ) |>
  dplyr::mutate(
    nominal_aav_m = contract_total_m / contract_years
  ) |>
  dplyr::group_by(signing_class) |>
  dplyr::summarise(
    contracts = dplyr::n(),
    median_aav_m = median(nominal_aav_m),
    p25_aav_m = as.numeric(quantile(nominal_aav_m, .25)),
    p75_aav_m = as.numeric(quantile(nominal_aav_m, .75)),
    median_prior_war = median(prior_war, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(n = Inf, width = Inf)


# ==========================================================================
# 22 | Model the free-agent market
# ==========================================================================
# Fit a salary model using contract year, prior WAR, age and contract length.
# Inspect influential contracts because a few unusual deals can move a small
# regression.
#
market_model_data <- fa_contracts |>
  dplyr::filter(
    !is.na(contract_total_m),
    !is.na(contract_years),
    contract_years >= 2,
    contract_total_m > 0,
    !is.na(prior_war),
    !is.na(age)
  ) |>
  dplyr::mutate(
    nominal_aav_m = contract_total_m / contract_years,
    years_since_2022 = signing_class - 2022
  )


# WHY: Fit a log-salary regression. Exponentiating its fitted value estimates a conditional median on the original salary scale.
market_model <- lm(
  log(nominal_aav_m) ~
    years_since_2022 +
    prior_war +
    age +
    contract_years,
  data = market_model_data
)

summary(market_model)

nobs(market_model)


# WHY: Identify observations with high leverage or influence. Do not remove them automatically.
market_influence <- market_model_data |>
  dplyr::mutate(
    cooks_distance = cooks.distance(market_model),
    leverage = hatvalues(market_model),
    studentized_residual = rstudent(market_model)
  ) |>
  dplyr::arrange(dplyr::desc(cooks_distance))

market_influence |>
  dplyr::select(
    player,
    signing_class,
    prior_war,
    contract_years,
    nominal_aav_m,
    cooks_distance,
    leverage,
    studentized_residual
  ) |>
  dplyr::slice_head(n = 10) |>
  print(n = Inf, width = Inf)

cat(
  "\nCook's D reference threshold:",
  4 / nrow(market_model_data),
  "\n"
)


# ==========================================================================
# 23 | Test model sensitivity and out-of-sample error
# ==========================================================================
# Restrict to productive pitchers as one sensitivity check, vary the cutoff
# and use leave-one-out validation. A model that fits history can still miss
# expensive contracts badly.
#
productive_market <- market_model_data |>
  dplyr::filter(prior_war >= 2)


# WHY: Refit among pitchers above the chosen prior-WAR threshold to test sample composition.
productive_model <- lm(
  log(nominal_aav_m) ~
    years_since_2022 +
    prior_war +
    age +
    contract_years,
  data = productive_market
)

summary(productive_model)

cat(
  "\nObservations:", nobs(productive_model),
  "\nOriginal WAR coefficient:",
  coef(market_model)["prior_war"],
  "\nProductive-starter WAR coefficient:",
  coef(productive_model)["prior_war"],
  "\n"
)

cutoff_sensitivity <- purrr::map_dfr(
  c(0, 1, 1.5, 2, 2.5, 3),
  function(cutoff) {
    
    data <- market_model_data |>
      dplyr::filter(prior_war >= cutoff)
    
    model <- lm(
      log(nominal_aav_m) ~
        years_since_2022 +
        prior_war +
        age +
        contract_years,
      data = data
    )
    
    coefficients <- summary(model)$coefficients
    
    tibble::tibble(
      minimum_war = cutoff,
      n = nobs(model),
      annual_market_change_pct =
        100 * (exp(coef(model)["years_since_2022"]) - 1),
      market_p_value =
        coefficients["years_since_2022", "Pr(>|t|)"],
      war_coefficient = coef(model)["prior_war"],
      adjusted_r2 = summary(model)$adj.r.squared
    )
  }
)

print(cutoff_sensitivity, n = Inf, width = Inf)

productive_market <- productive_market |>
  dplyr::mutate(
    actual_aav_m = nominal_aav_m
  )

loocv_results <- purrr::map_dfr(
  seq_len(nrow(productive_market)),
  function(i) {
    
    training <- productive_market[-i, ]
    testing <- productive_market[i, ]
    
    model <- lm(
      log(nominal_aav_m) ~
        years_since_2022 +
        prior_war +
        age +
        contract_years,
      data = training
    )
    
    tibble::tibble(
      player = testing$player,
      actual_aav_m = testing$actual_aav_m,
      predicted_aav_m = exp(
        predict(model, newdata = testing)
      )
    )
  }
)

loocv_results |>
  dplyr::summarise(
    n = dplyr::n(),
    mae_m = mean(abs(actual_aav_m - predicted_aav_m)),
    rmse_m = sqrt(mean(
      (actual_aav_m - predicted_aav_m)^2
    )),
    median_absolute_error_m = median(
      abs(actual_aav_m - predicted_aav_m)
    )
  ) |>
  print(width = Inf)

loocv_results |>
  dplyr::mutate(
    absolute_error_m =
      abs(actual_aav_m - predicted_aav_m)
  ) |>
  dplyr::arrange(dplyr::desc(absolute_error_m)) |>
  dplyr::slice_head(n = 10) |>
  print(n = Inf, width = Inf)

baseline_results <- productive_market |>
  dplyr::mutate(
    baseline_aav_m = purrr::map_dbl(
      seq_len(dplyr::n()),
      function(i) {
        median(
          nominal_aav_m[
            signing_class == signing_class[i] &
              seq_along(nominal_aav_m) != i
          ],
          na.rm = TRUE
        )
      }
    )
  )

tibble::tibble(
  model = c("Regression", "Signing-class median"),
  mae_m = c(
    mean(abs(
      loocv_results$actual_aav_m -
        loocv_results$predicted_aav_m
    )),
    mean(abs(
      baseline_results$nominal_aav_m -
        baseline_results$baseline_aav_m
    ))
  ),
  rmse_m = c(
    sqrt(mean(
      (loocv_results$actual_aav_m -
         loocv_results$predicted_aav_m)^2
    )),
    sqrt(mean(
      (baseline_results$nominal_aav_m -
         baseline_results$baseline_aav_m)^2
    ))
  )
) |>
  print(width = Inf)

skubal_salary_prediction |>
  exp() |>
  as.data.frame() |>
  dplyr::mutate(
    contract_total_fit_m = fit * 8,
    contract_total_lower_m = lwr * 8,
    contract_total_upper_m = upr * 8
  ) |>
  tibble::as_tibble() |>
  print(width = Inf)

productive_market |>
  dplyr::summarise(
    n = dplyr::n(),
    max_prior_war = max(prior_war),
    p90_prior_war = as.numeric(quantile(prior_war, .90)),
    max_contract_years = max(contract_years),
    p90_contract_years = as.numeric(
      quantile(contract_years, .90)
    ),
    max_aav_m = max(nominal_aav_m),
    max_contract_total_m = max(contract_total_m)
  ) |>
  print(width = Inf)

productive_market |>
  dplyr::mutate(
    war_distance = abs(prior_war - 5.21),
    years_distance = abs(contract_years - 8),
    combined_distance = sqrt(
      ((prior_war - 5.21) /
         sd(productive_market$prior_war))^2 +
        ((contract_years - 8) /
           sd(productive_market$contract_years))^2
    )
  ) |>
  dplyr::arrange(combined_distance) |>
  dplyr::select(
    player,
    signing_class,
    age,
    prior_war,
    contract_years,
    contract_total_m,
    nominal_aav_m,
    combined_distance
  ) |>
  dplyr::slice_head(n = 10) |>
  print(n = Inf, width = Inf)

elite_war_history |>
  dplyr::mutate(
    nominal_aav_m = contract_total_m / contract_years,
    three_year_war_per_season = three_year_war / 3
  ) |>
  dplyr::select(
    player,
    age,
    three_year_war,
    three_year_war_per_season,
    contract_years,
    nominal_aav_m,
    contract_total_m
  ) |>
  dplyr::arrange(dplyr::desc(three_year_war)) |>
  print(n = Inf, width = Inf)


# ==========================================================================
# 24 | One-year versus three-year performance
# ==========================================================================
# Reconstruct recent WAR histories and compare whether one or three seasons
# predict salary better. Missing MLB seasons are coded as zero only after
# checking the historical records.
#

# WHY: Collect three pre-signing MLB seasons for each free agent.
market_three_year_war <- market_model_data |>
  dplyr::mutate(
    last_season = signing_class - 1
  ) |>
  dplyr::left_join(
    pitcher_history |>
      dplyr::group_by(PlayerName, Season) |>
      dplyr::summarise(
        season_war = sum(WAR, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("player" = "PlayerName"),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(
    Season >= last_season - 2,
    Season <= last_season
  ) |>
  dplyr::group_by(
    player, signing_class, age,
    contract_years, contract_total_m,
    nominal_aav_m, prior_war,
    years_since_2022
  ) |>
  dplyr::summarise(
    seasons_found = dplyr::n_distinct(Season),
    three_year_war = sum(season_war),
    .groups = "drop"
  )

market_three_year_war |>
  dplyr::summarise(
    contracts_matched = dplyr::n(),
    complete_three_seasons =
      sum(seasons_found == 3),
    incomplete_histories =
      sum(seasons_found < 3),
    median_three_year_war =
      median(three_year_war)
  ) |>
  print(width = Inf)

market_three_year_war |>
  dplyr::filter(seasons_found < 3) |>
  dplyr::select(
    player,
    signing_class,
    age,
    prior_war,
    seasons_found,
    three_year_war,
    contract_years,
    contract_total_m
  ) |>
  dplyr::arrange(seasons_found, player) |>
  print(n = Inf, width = Inf)

missing_seasons <- market_model_data |>
  dplyr::mutate(
    last_season = signing_class - 1
  ) |>
  tidyr::expand_grid(year_offset = 0:2) |>
  dplyr::mutate(
    expected_season = last_season - year_offset
  ) |>
  dplyr::anti_join(
    pitcher_history |>
      dplyr::distinct(PlayerName, Season),
    by = c(
      "player" = "PlayerName",
      "expected_season" = "Season"
    )
  ) |>
  dplyr::select(
    player,
    signing_class,
    expected_season,
    prior_war
  ) |>
  dplyr::arrange(player, expected_season)

print(missing_seasons, n = Inf, width = Inf)


# WHY: Create the complete comparison table used by both salary models.
market_war_complete <- market_model_data |>
  dplyr::mutate(
    last_season = signing_class - 1
  ) |>
  tidyr::expand_grid(year_offset = 0:2) |>
  dplyr::mutate(
    Season = last_season - year_offset
  ) |>
  dplyr::left_join(
    pitcher_history |>
      dplyr::group_by(PlayerName, Season) |>
      dplyr::summarise(
        season_war = sum(WAR, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c(
      "player" = "PlayerName",
      "Season" = "Season"
    )
  ) |>
  dplyr::mutate(
    season_war = tidyr::replace_na(season_war, 0)
  ) |>
  dplyr::group_by(
    player, signing_class, age,
    contract_years, contract_total_m,
    nominal_aav_m, prior_war,
    years_since_2022
  ) |>
  dplyr::summarise(
    three_year_war = sum(season_war),
    .groups = "drop"
  )

market_war_complete |>
  dplyr::summarise(
    contracts = dplyr::n(),
    median_three_year_war = median(three_year_war),
    mean_three_year_war = mean(three_year_war),
    minimum_three_year_war = min(three_year_war),
    maximum_three_year_war = max(three_year_war)
  ) |>
  print(width = Inf)


# ==========================================================================
# 25 | Compare salary models fairly
# ==========================================================================
# Evaluate both salary specifications on the same contracts with leave-one-
# out predictions. Neither wins decisively, so report both rather than
# choosing the more dramatic estimate.
#
formulas <- list(
  one_year = log(nominal_aav_m) ~
    years_since_2022 + prior_war +
    age + contract_years,
  
  three_year = log(nominal_aav_m) ~
    years_since_2022 + three_year_war +
    age + contract_years
)


# WHY: Use identical leave-one-out splits for a fair comparison of salary specifications.
model_comparison <- purrr::imap_dfr(
  formulas,
  function(formula, model_name) {
    
    predictions <- purrr::map_dbl(
      seq_len(nrow(market_war_complete)),
      function(i) {
        model <- lm(
          formula,
          data = market_war_complete[-i, ]
        )
        
        exp(as.numeric(predict(
          model,
          newdata = market_war_complete[i, ]
        )))
      }
    )
    
    tibble::tibble(
      model = model_name,
      n = length(predictions),
      mae_m = mean(abs(
        market_war_complete$nominal_aav_m -
          predictions
      )),
      rmse_m = sqrt(mean(
        (market_war_complete$nominal_aav_m -
           predictions)^2
      ))
    )
  }
)

print(model_comparison, width = Inf)


# ==========================================================================
# 26 | Apply market models to Skubal
# ==========================================================================
# Translate the fitted models into hypothetical eight-year contract
# estimates. Skubal exceeds the observed three-year WAR range, so these
# figures involve extrapolation.
#
skubal_input <- tibble::tibble(
  years_since_2022 = 5,
  prior_war = 5.21,
  three_year_war = 17.96,
  age = 30,
  contract_years = 8
)


# WHY: Apply both models to Skubal, while noting that the prediction intervals are wide and extrapolation is substantial.
skubal_market_estimates <- purrr::imap_dfr(
  formulas,
  function(formula, model_name) {
    
    model <- lm(
      formula,
      data = market_war_complete
    )
    
    prediction <- predict(
      model,
      newdata = skubal_input,
      interval = "prediction",
      level = 0.95
    )
    
    tibble::tibble(
      model = model_name,
      annual_aav_m = exp(prediction[1, "fit"]),
      lower_aav_m = exp(prediction[1, "lwr"]),
      upper_aav_m = exp(prediction[1, "upr"]),
      eight_year_total_m =
        8 * exp(prediction[1, "fit"])
    )
  }
)

print(skubal_market_estimates, width = Inf)


# ==========================================================================
# 27 | Final contract economics
# ==========================================================================
# Compare salary with projected WAR value, then discount future production
# and payments. The assumed $10 million per WAR is a scenario input, not a
# verified market price.
#

# WHY: Calculate undiscounted surplus and the WAR price needed to break even.
final_valuation <- tibble::tibble(
  contract_m = c(320, 360, 400, 451, 595)
) |>
  dplyr::mutate(
    annual_aav_m = contract_m / 8,
    projected_war = 24.61681,
    nominal_value_m = projected_war * 10,
    nominal_surplus_m = nominal_value_m - contract_m,
    break_even_dollars_per_war_m =
      contract_m / projected_war
  )

print(final_valuation, width = Inf)


# WHY: Discount both production value and salary payments. The result depends on the assumed discount rate, WAR price and payment schedule.
final_pv <- final_valuation |>
  dplyr::mutate(
    production_pv_m = sum(
      contract_value$present_value_millions
    ),
    salary_pv_m = purrr::map_dbl(
      contract_m,
      function(total) {
        sum(
          (total / 8) *
            contract_value$discount_factor
        )
      }
    ),
    surplus_pv_m = production_pv_m - salary_pv_m,
    break_even_war_price_pv_m =
      salary_pv_m /
      sum(
        contract_value$projected_war *
          contract_value$discount_factor
      )
  ) |>
  dplyr::select(
    contract_m,
    production_pv_m,
    salary_pv_m,
    surplus_pv_m,
    break_even_war_price_pv_m
  )

print(final_pv, width = Inf)


# ==========================================================================
# 28 | Bayesian extension: uncertainty in eight-year WAR and contract surplus
# ==========================================================================
# WHAT: Fit a Bayesian regression to the same 31 historical pitchers used in
# the frequentist aging analysis. Draw possible eight-year WAR outcomes for
# Skubal, then pass each draw through the contract calculation.
# WHY: A single expected WAR value hides the uncertainty in an individual
# pitcher's future. The posterior predictive distribution includes both
# parameter uncertainty and the unexplained pitcher-to-pitcher variation.
#
# This is a deliberately modest Bayesian extension, NOT a hierarchical model.
# With only one eight-year outcome per pitcher, a pitcher-level random
# intercept would not be separately identified from residual variation.
# A richer hierarchical annual-WAR model is a possible later project.
#
# RUNNING THIS SECTION
# Install brms and its Stan backend separately before running. This section
# may take several minutes. It depends on war_relationship and the annual
# WAR allocation created in earlier sections of this script.
#
# IMPORTANT: No Bayesian numerical results are asserted in the HTML report
# until this section has been run and its diagnostics have been reviewed.

if (!requireNamespace("brms", quietly = TRUE)) {
  stop("Install the 'brms' package and a working Stan backend to run section 28.")
}

# Standardize the predictor using ONLY the historical cohort. This makes
# the prior on the regression coefficient easier to interpret.
bayes_history <- war_relationship |>
  dplyr::filter(is.finite(total_war), is.finite(eight_year_war)) |>
  dplyr::mutate(total_war_z = as.numeric(scale(total_war)))

stopifnot(nrow(bayes_history) >= 20)
war_center <- mean(bayes_history$total_war)
war_scale <- stats::sd(bayes_history$total_war)

# PRIOR CHOICES (in eight-year WAR units)
# Intercept: Student-t centered near the historical mean, with wide tails.
# Slope: Normal(0, 10) for one SD of recent WAR. This regularizes the
# relationship without assuming that recent WAR has no predictive value.
# Sigma: Exponential(0.1), placing positive prior mass on a range of
# residual standard deviations. These are choices to test, not facts.
# Student-t residuals reduce sensitivity to unusually extreme outcomes.
bayes_priors <- c(
  brms::prior(student_t(3, 12, 10), class = "Intercept"),
  brms::prior(normal(0, 10), class = "b"),
  brms::prior(exponential(0.1), class = "sigma")
)

set.seed(20261120)
bayes_fit <- brms::brm(
  formula = eight_year_war ~ total_war_z,
  data = bayes_history,
  family = brms::student(),
  prior = bayes_priors,
  chains = 4,
  iter = 3000,
  warmup = 1000,
  cores = min(4L, parallel::detectCores()),
  seed = 20261120,
  refresh = 250
)

# DIAGNOSTICS: Inspect R-hat, effective sample sizes, divergences,
# posterior predictive checks and prior sensitivity before interpreting
# any probabilities. Convergence is necessary but not sufficient.
print(summary(bayes_fit))
plot(bayes_fit)
brms::pp_check(bayes_fit, ndraws = 100)

# The supplied analysis reports Skubal's 2024-2026 total as 17.96 fWAR.
# Verify this value against the downloaded FanGraphs data if refreshed.
skubal_prior_three_year_war <- 17.96
bayes_newdata <- data.frame(
  total_war_z = (skubal_prior_three_year_war - war_center) / war_scale
)

# posterior_epred is uncertainty in the expected outcome.
# posterior_predict also adds the individual pitcher's outcome variation.
# For contract risk, use posterior_predict, NOT posterior_epred.
bayes_expected <- as.numeric(brms::posterior_epred(bayes_fit, newdata = bayes_newdata))
bayes_predictive <- as.numeric(brms::posterior_predict(bayes_fit, newdata = bayes_newdata))

# This is an aggregate eight-year model. To discount the WAR, we must
# allocate each draw across seasons. We use the existing deterministic
# eight-year allocation as a fixed shape, not as eight new forecasts.
annual_shape <- c(6.73, 4.31, 3.08, 3.09, 2.21, 2.56, 1.63, 1.02)
annual_weights <- annual_shape / sum(annual_shape)

# The following assumes the same annual WAR shape for EVERY draw. It does
# not model uncertainty about when production arrives, or season-by-season
# correlation. Negative aggregate WAR draws remain negative; truncating
# them at zero would artificially improve the economic outcomes.
# Price growth begins in the second contract year (2028).
bayes_contract_grid <- tidyr::crossing(
  contract_millions = c(320, 360, 400, 451, 595),
  initial_millions_per_war = c(10, 12, 15),
  annual_war_price_growth = c(0, 0.03, 0.05)
)

bayes_contract_results <- purrr::pmap_dfr(
  bayes_contract_grid,
  function(contract_millions, initial_millions_per_war, annual_war_price_growth) {
    discount_factors <- 1.05 ^ (1:8)
    production_pv_per_total_war <- sum(
      annual_weights *
        initial_millions_per_war *
        (1 + annual_war_price_growth) ^ (0:7) /
        discount_factors
    )
    salary_pv <- sum((contract_millions / 8) / discount_factors)
    surplus_draws <- bayes_predictive * production_pv_per_total_war - salary_pv
    tibble::tibble(
      contract_millions = contract_millions,
      initial_millions_per_war = initial_millions_per_war,
      annual_war_price_growth = annual_war_price_growth,
      median_surplus_pv = stats::median(surplus_draws),
      lower_95_surplus_pv = unname(stats::quantile(surplus_draws, 0.025)),
      upper_95_surplus_pv = unname(stats::quantile(surplus_draws, 0.975)),
      probability_positive_surplus = mean(surplus_draws > 0)
    )
  }
)

bayes_war_summary <- tibble::tibble(
  quantity = c("Expected WAR (posterior)", "Individual WAR (predictive)"),
  median = c(stats::median(bayes_expected), stats::median(bayes_predictive)),
  lower_95 = c(stats::quantile(bayes_expected, .025), stats::quantile(bayes_predictive, .025)),
  upper_95 = c(stats::quantile(bayes_expected, .975), stats::quantile(bayes_predictive, .975))
)

print(bayes_war_summary)
print(bayes_contract_results, n = Inf)

# NEXT CHECKS BEFORE PUBLICATION
# 1. Refit with weaker and stronger slope/intercept priors.
# 2. Compare Bayesian leave-one-out predictive accuracy with the original
#    model, using brms::add_criterion(bayes_fit, "loo") and loo diagnostics.
# 3. Inspect Skubal's position in the historical predictor distribution.
# 4. Consider a richer annual-WAR model only with defensible longitudinal
#    data structure, validated priors and a genuine out-of-sample check.
# 5. Publish actual numerical posterior summaries only after completing
#    these diagnostics. A posterior probability is conditional on the
#    selected cohort, likelihood, priors and contract assumptions.
