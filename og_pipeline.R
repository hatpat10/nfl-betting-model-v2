# ============================================================================
# NFL BETTING MODEL - ENHANCED MASTER PIPELINE
# ============================================================================
# Enhanced with: Weather, Injuries, Schedule Factors, Situational Stats, Pace

# Clear environment
rm(list = ls())
gc()

# ============================================================================
# CONFIGURATION
# ============================================================================

SEASON <- 2025
WEEK <- 9
BASE_DIR <- 'C:/Users/Patsc/Documents/nfl/'

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║        NFL BETTING MODEL - ENHANCED PIPELINE                   ║\n")
cat("║        Season:", SEASON, "| Week:", WEEK, "                                  ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# LOAD PACKAGES
# ============================================================================

cat("Loading required packages...\n")

required_packages <- c(
  "nflverse", "nflreadr", "nflfastR", "nflplotR", "nflseedR", "nfl4th",
  "dplyr", "tidyr", "writexl", "openxlsx", "ggplot2", "gt", "gtExtras",
  "gridExtra", "zoo", "ggrepel", "geosphere"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  Installing", pkg, "...\n")
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat("✓ All packages loaded successfully!\n\n")

# ============================================================================
# LOAD TEAM VISUAL DATA
# ============================================================================

cat("Loading team visual data...\n")
teams_colors_logos <- nflreadr::load_teams() %>%
  select(team_abbr, team_name, team_color, team_color2, team_logo_espn, 
         team_wordmark, team_conf, team_division)
cat("✓ Team visual data loaded for", nrow(teams_colors_logos), "teams\n\n")

# ============================================================================
# PHASE 1: DATA CLEANING & PREPARATION
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PHASE 1: CLEANING & PREPARING DATA                           ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

start_time_phase1 <- Sys.time()

# Create output directory
output_dir <- file.path(BASE_DIR, paste0('week', WEEK))
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Load raw play-by-play data
cat("[1/12] Loading play-by-play data...\n")
pbp_raw <- nflreadr::load_pbp(SEASON)
cat("      Loaded:", nrow(pbp_raw), "plays\n")

# Add 4th down decision analysis
cat("[2/12] Adding 4th down analytics...\n")
pbp_raw <- pbp_raw %>% nfl4th::add_4th_probs()

# Filter to relevant plays
cat("[3/12] Filtering to relevant plays...\n")
pbp_clean <- pbp_raw %>%
  filter(
    season_type == "REG",
    !is.na(week),
    week <= !!WEEK,
    !is.na(play_type),
    play_type %in% c("pass", "run", "field_goal", "punt", "qb_kneel", "qb_spike"),
    !(qtr %in% c(2, 4) & game_seconds_remaining <= 120 & score_differential_post > 8),
    is.na(penalty_team) | penalty_type %in% c("Defensive Offside", "Neutral Zone Infraction", "Defensive Delay of Game")
  )
cat("      Remaining:", nrow(pbp_clean), "plays\n")

# Create clean variables
cat("[4/12] Creating clean variables...\n")
pbp_analysis <- pbp_clean %>%
  mutate(
    game_id = as.character(game_id),
    season = as.numeric(season),
    week = as.numeric(week),
    game_date = as.Date(game_date),
    offense_team = posteam,
    defense_team = defteam,
    play_type_clean = case_when(
      play_type == "pass" ~ "pass",
      play_type == "run" & qb_scramble == 1 ~ "qb_scramble",
      play_type == "run" ~ "run",
      play_type == "field_goal" ~ "field_goal",
      play_type == "punt" ~ "punt",
      TRUE ~ "other"
    ),
    down = as.integer(down),
    ydstogo = as.integer(ydstogo),
    yards_gained = as.numeric(yards_gained),
    yardline_100 = as.numeric(yardline_100),
    red_zone = if_else(yardline_100 <= 20 & yardline_100 > 0, 1, 0),
    success = as.numeric(success),
    epa = as.numeric(epa),
    wpa = as.numeric(wpa),
    explosive_pass = if_else(play_type == "pass" & yards_gained >= 20, 1, 0),
    explosive_run = if_else(play_type == "run" & yards_gained >= 10, 1, 0),
    explosive_play = if_else(explosive_pass == 1 | explosive_run == 1, 1, 0),
    stuffed = if_else(yards_gained <= 0, 1, 0),
    sack = as.numeric(sack),
    interception = as.numeric(interception),
    fumble_lost = as.numeric(fumble_lost),
    turnover = if_else(interception == 1 | fumble_lost == 1, 1, 0),
    touchdown = as.numeric(touchdown),
    pass_touchdown = as.numeric(pass_touchdown),
    rush_touchdown = as.numeric(rush_touchdown),
    score_differential = as.numeric(score_differential),
    quarter = as.integer(qtr),
    # NEW: Situational flags
    is_third_down = if_else(down == 3, 1, 0),
    is_fourth_down = if_else(down == 4, 1, 0),
    two_minute_drill = if_else(half_seconds_remaining <= 120, 1, 0)
  ) %>%
  mutate(across(c(success, explosive_pass, explosive_run, explosive_play, stuffed, 
                  turnover, touchdown, is_third_down, is_fourth_down, two_minute_drill), 
                ~replace_na(., 0)))

# ============================================================================
# NEW: LOAD WEATHER DATA
# ============================================================================

cat("[5/12] Loading weather data...\n")

weather_data <- pbp_raw %>%
  filter(!is.na(week), week <= !!WEEK) %>%
  select(game_id, week, home_team, away_team, temp, wind, weather) %>%
  distinct() %>%
  mutate(
    temp = as.numeric(temp),
    wind = as.numeric(wind),
    has_weather = !is.na(weather) & weather != "",
    is_dome = is.na(weather) | weather == "DOME",
    is_cold = temp < 40 & !is.na(temp),
    is_hot = temp > 85 & !is.na(temp),
    is_windy = wind > 15 & !is.na(wind),
    weather_impact_score = case_when(
      is_dome ~ 0,
      is_windy & is_cold ~ 3,  # Worst conditions
      is_windy | is_cold ~ 2,   # Moderate impact
      is_hot ~ 1,               # Minor impact
      TRUE ~ 0                  # Good conditions
    )
  )

cat("      Weather data loaded for", n_distinct(weather_data$game_id), "games\n")
cat("      Dome games:", sum(weather_data$is_dome), "\n")
cat("      High wind games:", sum(weather_data$is_windy, na.rm = TRUE), "\n")
cat("      Cold weather games:", sum(weather_data$is_cold, na.rm = TRUE), "\n")

# ============================================================================
# NEW: LOAD INJURY DATA
# ============================================================================

cat("[6/12] Loading injury data...\n")

injuries_raw <- tryCatch({
  nflreadr::load_injuries(SEASON)
}, error = function(e) {
  cat("      ⚠ Could not load injury data:", e$message, "\n")
  return(NULL)
})

if (!is.null(injuries_raw) && nrow(injuries_raw) > 0) {
  injuries_summary <- injuries_raw %>%
    filter(!is.na(week), week <= !!WEEK) %>%
    group_by(team, week) %>%
    summarise(
      total_injuries = n(),
      out_count = sum(report_status == "Out", na.rm = TRUE),
      questionable_count = sum(report_status == "Questionable", na.rm = TRUE),
      doubtful_count = sum(report_status == "Doubtful", na.rm = TRUE),
      # Key position injuries (higher impact)
      qb_injuries = sum(position == "QB" & report_status %in% c("Out", "Doubtful"), na.rm = TRUE),
      ol_injuries = sum(position %in% c("T", "G", "C") & report_status == "Out", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      injury_impact_score = (out_count * 2) + (doubtful_count * 1.5) + 
        (questionable_count * 0.5) + 
        (qb_injuries * 5) + (ol_injuries * 1.5)
    )
  
  cat("      Injury data loaded for", n_distinct(injuries_summary$team), "teams\n")
  cat("      Total injuries tracked:", nrow(injuries_summary), "team-weeks\n")
} else {
  injuries_summary <- data.frame()
  cat("      ⚠ No injury data available\n")
}

# ============================================================================
# NEW: LOAD SCHEDULE DATA WITH REST CALCULATIONS
# ============================================================================

cat("[7/12] Loading schedule with rest days...\n")

schedule_full <- nflreadr::load_schedules(SEASON)

schedule_rest <- schedule_full %>%
  filter(!is.na(gameday)) %>%
  mutate(gameday = as.Date(gameday)) %>%  # Convert to Date first
  arrange(home_team, gameday) %>%
  group_by(home_team) %>%
  mutate(
    home_days_rest = as.numeric(gameday - lag(gameday)),
    home_days_rest = if_else(is.na(home_days_rest), 7, home_days_rest)
  ) %>%
  ungroup() %>%
  arrange(away_team, gameday) %>%
  group_by(away_team) %>%
  mutate(
    away_days_rest = as.numeric(gameday - lag(gameday)),
    away_days_rest = if_else(is.na(away_days_rest), 7, away_days_rest)
  ) %>%
  ungroup() %>%
  mutate(
    is_thursday = weekday == "Thursday",
    is_primetime = if_else(!is.na(gametime), 
                           as.numeric(substr(gametime, 1, 2)) >= 20, 
                           FALSE),
    rest_advantage = home_days_rest - away_days_rest,
    # Short week disadvantage (< 6 days rest)
    home_short_week = home_days_rest < 6,
    away_short_week = away_days_rest < 6
  )

cat("      Schedule data loaded\n")
cat("      Thursday games:", sum(schedule_rest$is_thursday, na.rm = TRUE), "\n")

# ============================================================================
# AGGREGATE TEAM STATS (ENHANCED)
# ============================================================================

cat("[8/12] Aggregating enhanced team statistics...\n")

team_offense_weekly <- pbp_analysis %>%
  filter(play_type_clean %in% c("pass", "run", "qb_scramble")) %>%
  group_by(season, week, offense_team) %>%
  summarise(
    games_played = n_distinct(game_id),
    total_plays = n(),
    pass_plays = sum(play_type_clean == "pass"),
    run_plays = sum(play_type_clean %in% c("run", "qb_scramble")),
    pass_rate = pass_plays / total_plays,
    
    # Core metrics
    avg_yards_per_play = mean(yards_gained, na.rm = TRUE),
    avg_yards_per_pass = mean(if_else(play_type_clean == "pass", yards_gained, NA_real_), na.rm = TRUE),
    avg_yards_per_run = mean(if_else(play_type_clean %in% c("run", "qb_scramble"), yards_gained, NA_real_), na.rm = TRUE),
    avg_epa_per_play = mean(epa, na.rm = TRUE),
    avg_epa_pass = mean(if_else(play_type_clean == "pass", epa, NA_real_), na.rm = TRUE),
    avg_epa_run = mean(if_else(play_type_clean %in% c("run", "qb_scramble"), epa, NA_real_), na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    explosive_play_rate = mean(explosive_play, na.rm = TRUE),
    stuff_rate = mean(stuffed, na.rm = TRUE),
    sack_rate = sum(sack) / pass_plays,
    turnovers_lost = sum(turnover, na.rm = TRUE),
    turnover_rate = turnovers_lost / total_plays,
    touchdowns = sum(touchdown, na.rm = TRUE),
    
    # NEW: Situational stats
    third_down_attempts = sum(is_third_down),
    third_down_conversions = sum(is_third_down & first_down == 1, na.rm = TRUE),
    third_down_rate = if_else(third_down_attempts > 0, 
                              third_down_conversions / third_down_attempts, 
                              NA_real_),
    
    # NEW: Red zone efficiency
    red_zone_plays = sum(red_zone),
    red_zone_tds = sum(red_zone & touchdown == 1, na.rm = TRUE),
    red_zone_td_rate = if_else(red_zone_plays > 0, 
                               red_zone_tds / red_zone_plays, 
                               NA_real_),
    
    # NEW: Two-minute drill
    two_min_plays = sum(two_minute_drill),
    two_min_epa = mean(if_else(two_minute_drill == 1, epa, NA_real_), na.rm = TRUE),
    
    # NEW: Pace metrics
    plays_per_game = total_plays / games_played,
    seconds_per_play = mean(play_clock, na.rm = TRUE),
    
    .groups = "drop"
  )

team_defense_weekly <- pbp_analysis %>%
  filter(play_type_clean %in% c("pass", "run", "qb_scramble")) %>%
  group_by(season, week, defense_team) %>%
  summarise(
    def_total_plays = n(),
    def_avg_yards_per_play = mean(yards_gained, na.rm = TRUE),
    def_avg_epa_per_play = mean(epa, na.rm = TRUE),
    def_success_rate_allowed = mean(success, na.rm = TRUE),
    def_sacks = sum(sack, na.rm = TRUE),
    def_stuff_rate = mean(stuffed, na.rm = TRUE),
    def_turnovers_forced = sum(turnover, na.rm = TRUE),
    def_turnover_rate = def_turnovers_forced / def_total_plays,
    def_explosive_rate_allowed = mean(explosive_play, na.rm = TRUE),
    
    # NEW: Defensive situational stats
    def_third_down_stops = sum(is_third_down & first_down == 0, na.rm = TRUE),
    def_third_down_rate = if_else(sum(is_third_down) > 0,
                                  def_third_down_stops / sum(is_third_down),
                                  NA_real_),
    def_red_zone_tds_allowed = sum(red_zone & touchdown == 1, na.rm = TRUE),
    def_red_zone_stops = sum(red_zone & touchdown == 0, na.rm = TRUE),
    def_red_zone_td_rate = if_else(sum(red_zone) > 0,
                                   def_red_zone_tds_allowed / sum(red_zone),
                                   NA_real_),
    
    .groups = "drop"
  )

# Calculate rolling averages
cat("[9/12] Calculating rolling averages...\n")

offense_rolling <- team_offense_weekly %>%
  arrange(offense_team, week) %>%
  group_by(offense_team) %>%
  mutate(
    roll3_epa = zoo::rollmean(avg_epa_per_play, k = 3, fill = NA, align = "right"),
    roll3_success_rate = zoo::rollmean(success_rate, k = 3, fill = NA, align = "right"),
    roll3_explosive_rate = zoo::rollmean(explosive_play_rate, k = 3, fill = NA, align = "right"),
    roll3_third_down = zoo::rollmean(third_down_rate, k = 3, fill = NA, align = "right"),
    roll3_red_zone_td = zoo::rollmean(red_zone_td_rate, k = 3, fill = NA, align = "right"),
    roll3_pace = zoo::rollmean(plays_per_game, k = 3, fill = NA, align = "right")
  ) %>%
  ungroup()

defense_rolling <- team_defense_weekly %>%
  arrange(defense_team, week) %>%
  group_by(defense_team) %>%
  mutate(
    def_roll3_epa = zoo::rollmean(def_avg_epa_per_play, k = 3, fill = NA, align = "right"),
    def_roll3_success_rate = zoo::rollmean(def_success_rate_allowed, k = 3, fill = NA, align = "right"),
    def_roll3_third_down = zoo::rollmean(def_third_down_rate, k = 3, fill = NA, align = "right")
  ) %>%
  ungroup()

# Create rankings
available_weeks <- unique(offense_rolling$week)
analysis_week <- max(available_weeks)

cat("      Creating rankings for week", analysis_week, "...\n")

offense_rankings <- offense_rolling %>%
  filter(week == analysis_week) %>%
  mutate(
    composite_epa = (0.6 * avg_epa_pass) + (0.4 * avg_epa_run),
    epa_rank = rank(-composite_epa),
    pass_epa_rank = rank(-avg_epa_pass),
    run_epa_rank = rank(-avg_epa_run),
    success_rank = rank(-success_rate),
    third_down_rank = rank(-third_down_rate),
    red_zone_rank = rank(-red_zone_td_rate)
  )

defense_rankings <- defense_rolling %>%
  filter(week == analysis_week) %>%
  mutate(
    composite_epa_allowed = def_avg_epa_per_play,
    def_epa_rank = rank(composite_epa_allowed),
    def_success_rank = rank(def_success_rate_allowed),
    def_turnover_rank = rank(-def_turnover_rate),
    def_third_down_rank = rank(-def_third_down_rate)
  )

# Game summaries
game_summaries <- pbp_analysis %>%
  group_by(game_id, season, week, game_date, home_team, away_team) %>%
  summarise(
    home_score = max(if_else(posteam == home_team, posteam_score, defteam_score), na.rm = TRUE),
    away_score = max(if_else(posteam == away_team, posteam_score, defteam_score), na.rm = TRUE),
    total_points = home_score + away_score,
    .groups = "drop"
  )

# Export Phase 1 data
cat("[10/12] Exporting cleaned data...\n")

write.csv(offense_rolling, file.path(output_dir, 'offense_weekly.csv'), row.names = FALSE)
write.csv(defense_rolling, file.path(output_dir, 'defense_weekly.csv'), row.names = FALSE)
write.csv(offense_rankings, file.path(output_dir, 'offense_rankings.csv'), row.names = FALSE)
write.csv(defense_rankings, file.path(output_dir, 'defense_rankings.csv'), row.names = FALSE)
write.csv(weather_data, file.path(output_dir, 'weather_data.csv'), row.names = FALSE)

if (nrow(injuries_summary) > 0) {
  write.csv(injuries_summary, file.path(output_dir, 'injuries_summary.csv'), row.names = FALSE)
}

write.csv(schedule_rest, file.path(output_dir, 'schedule_rest.csv'), row.names = FALSE)

end_time_phase1 <- Sys.time()
cat("\n✓ Phase 1 complete in", round(difftime(end_time_phase1, start_time_phase1, units = "secs"), 1), "seconds\n")
cat("  Data through Week", analysis_week, "\n")
cat("  Teams:", n_distinct(offense_rankings$offense_team), "\n\n")

# ============================================================================
# DEFINE VISUALIZATION FUNCTIONS
# ============================================================================

create_epa_rankings_plot <- function(offense_ranks, defense_ranks, teams_data) {
  combined_ranks <- offense_ranks %>%
    inner_join(defense_ranks, 
               by = c("season", "week", "offense_team" = "defense_team"),
               suffix = c("_off", "_def")) %>%
    rename(team = offense_team) %>%
    mutate(
      overall_score = ((33 - epa_rank) + (33 - def_epa_rank)) / 2,
      overall_rank = rank(-overall_score)
    ) %>%
    arrange(overall_rank) %>%
    head(16) %>%
    left_join(teams_data, by = c("team" = "team_abbr"))
  
  ggplot(combined_ranks, aes(x = reorder(team, -overall_rank), y = overall_score)) +
    geom_col(aes(fill = team_color), color = "white", linewidth = 0.8) +
    scale_fill_identity() +
    geom_text(aes(label = paste0("#", as.integer(overall_rank))), 
              vjust = -0.5, fontface = "bold", size = 4) +
    labs(
      title = "NFL Power Rankings (Enhanced Model)",
      subtitle = paste("Combined Offensive & Defensive EPA - Week", max(combined_ranks$week)),
      x = NULL, y = "Overall Power Score"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.text.x = element_text(face = "bold", size = 11),
      panel.grid.major.x = element_blank()
    )
}

create_offense_defense_scatter <- function(offense_ranks, defense_ranks, teams_data) {
  combined <- offense_ranks %>%
    inner_join(defense_ranks, 
               by = c("season", "week", "offense_team" = "defense_team"),
               suffix = c("_off", "_def")) %>%
    rename(team = offense_team) %>%
    left_join(teams_data, by = c("team" = "team_abbr"))
  
  ggplot(combined, aes(x = composite_epa, y = -composite_epa_allowed)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_point(aes(fill = team_color), size = 5, shape = 21, color = "white", stroke = 1.5) +
    scale_fill_identity() +
    ggrepel::geom_text_repel(
      aes(label = team), size = 3.5, fontface = "bold",
      box.padding = 0.5, point.padding = 0.3
    ) +
    labs(
      title = "NFL Team Efficiency Matrix",
      subtitle = paste("Offensive EPA vs Defensive EPA - Week", max(combined$week)),
      x = "Offensive EPA per Play (Better →)",
      y = "Defensive EPA per Play (Better →)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
}

# ============================================================================
# PHASE 2: ENHANCED MATCHUP ANALYSIS
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PHASE 2: ENHANCED MATCHUP ANALYSIS                           ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

start_time_phase2 <- Sys.time()

# Load schedule for target week
schedule <- nflreadr::load_schedules(SEASON) %>%
  filter(week == !!WEEK, !is.na(away_team), !is.na(home_team))

cat("Analyzing", nrow(schedule), "games for Week", WEEK, "\n\n")

# Create matchup analysis directory
matchup_dir <- file.path(output_dir, 'matchup_analysis')
if (!dir.exists(matchup_dir)) {
  dir.create(matchup_dir, recursive = TRUE)
}

# Enhanced matchup calculation
cat("[11/12] Calculating enhanced matchup advantages...\n")

calculate_enhanced_matchup <- function(team_a, team_b, offense_data, defense_data, 
                                       weather_df, injury_df, schedule_df) {
  
  # Get team stats
  team_a_offense <- offense_data %>% filter(offense_team == team_a) %>% arrange(desc(week)) %>% slice(1)
  team_b_defense <- defense_data %>% filter(defense_team == team_b) %>% arrange(desc(week)) %>% slice(1)
  
  if (nrow(team_a_offense) == 0 || nrow(team_b_defense) == 0) {
    return(NULL)
  }
  
  # Base EPA advantage
  base_epa_adv <- team_a_offense$avg_epa_per_play - team_b_defense$def_avg_epa_per_play
  
  # Get game-specific factors
  game_info <- schedule_df %>% 
    filter((home_team == team_a & away_team == team_b) | 
             (home_team == team_b & away_team == team_a)) %>%
    slice(1)
  
  is_home <- if(nrow(game_info) > 0) game_info$home_team == team_a else TRUE
  
  # Weather adjustment
  weather_adj <- 0
  if (nrow(game_info) > 0) {
    game_weather <- weather_df %>% filter(game_id == game_info$game_id)
    if (nrow(game_weather) > 0 && !is.na(game_weather$weather_impact_score)) {
      # Bad weather hurts passing offenses more
      pass_heavy <- team_a_offense$pass_rate > 0.6
      weather_adj <- if_else(pass_heavy, 
                             -game_weather$weather_impact_score * 0.02,
                             -game_weather$weather_impact_score * 0.01)
    }
  }
  
  # Injury adjustment
  injury_adj <- 0
  if (nrow(injury_df) > 0) {
    team_injuries <- injury_df %>% 
      filter(team == team_a, week == WEEK - 1) %>%
      slice(1)
    
    if (nrow(team_injuries) > 0 && !is.na(team_injuries$injury_impact_score)) {
      injury_adj <- -team_injuries$injury_impact_score * 0.01
    }
  }
  
  # Rest adjustment
  rest_adj <- 0
  if (nrow(game_info) > 0) {
    if (is_home && !is.na(game_info$home_short_week) && game_info$home_short_week) {
      rest_adj <- -0.02
    } else if (!is_home && !is.na(game_info$away_short_week) && game_info$away_short_week) {
      rest_adj <- -0.02
    }
  }
  
  # Total adjusted advantage
  adjusted_epa_adv <- base_epa_adv + weather_adj + injury_adj + rest_adj
  
  tibble(
    offense_team = team_a,
    defense_team = team_b,
    base_epa_advantage = base_epa_adv,
    weather_adjustment = weather_adj,
    injury_adjustment = injury_adj,
    rest_adjustment = rest_adj,
    adjusted_epa_advantage = adjusted_epa_adv
  )
}

matchup_summaries <- list()

for (i in 1:nrow(schedule)) {
  game <- schedule[i, ]
  home <- game$home_team
  away <- game$away_team
  
  home_adv <- calculate_enhanced_matchup(
    home, away, offense_rolling, defense_rolling, 
    weather_data, injuries_summary, schedule_rest
  )
  
  away_adv <- calculate_enhanced_matchup(
    away, home, offense_rolling, defense_rolling,
    weather_data, injuries_summary, schedule_rest
  )
  
  if (!is.null(home_adv) && !is.null(away_adv)) {
    # Get pace for total projection
    home_off <- offense_rolling %>% filter(offense_team == home) %>% arrange(desc(week)) %>% slice(1)
    away_off <- offense_rolling %>% filter(offense_team == away) %>% arrange(desc(week)) %>% slice(1)
    
    avg_pace <- if(nrow(home_off) > 0 && nrow(away_off) > 0) {
      (home_off$plays_per_game + away_off$plays_per_game) / 2
    } else {
      65  # NFL average
    }
    
    # Get weather and rest info for summary
    game_weather <- weather_data %>% filter(home_team == home, away_team == away, week == WEEK)
    game_rest <- schedule_rest %>% filter(home_team == home, away_team == away, week == WEEK)
    
    summary <- tibble(
      game = paste(away, "at", home),
      home_team = home,
      away_team = away,
      home_base_epa = home_adv$base_epa_advantage,
      away_base_epa = away_adv$base_epa_advantage,
      home_adjusted_epa = home_adv$adjusted_epa_advantage,
      away_adjusted_epa = away_adv$adjusted_epa_advantage,
      net_home_advantage = home_adv$adjusted_epa_advantage - away_adv$adjusted_epa_advantage,
      projected_margin = (home_adv$adjusted_epa_advantage - away_adv$adjusted_epa_advantage) * 15 + 2.5,
      
      # Context factors
      estimated_pace = avg_pace,
      projected_total = 45 + (net_home_advantage * 10),  # Base 45 pts + advantage
      
      # Weather/rest flags
      weather_impact = if(nrow(game_weather) > 0) game_weather$weather_impact_score else 0,
      is_dome = if(nrow(game_weather) > 0) game_weather$is_dome else FALSE,
      home_days_rest = if(nrow(game_rest) > 0) game_rest$home_days_rest else 7,
      away_days_rest = if(nrow(game_rest) > 0) game_rest$away_days_rest else 7,
      is_thursday = if(nrow(game_rest) > 0) game_rest$is_thursday else FALSE,
      
      # Adjustment contributions
      total_weather_adj = home_adv$weather_adjustment + away_adv$weather_adjustment,
      total_injury_adj = home_adv$injury_adjustment + away_adv$injury_adjustment,
      total_rest_adj = home_adv$rest_adjustment + away_adv$rest_adjustment
    )
    
    matchup_summaries[[i]] <- summary
  }
}

matchup_summary_df <- bind_rows(matchup_summaries)

# Create visualizations
cat("[12/12] Creating enhanced visualizations...\n")

# Power rankings
power_plot <- create_epa_rankings_plot(offense_rankings, defense_rankings, teams_colors_logos)
ggsave(file.path(matchup_dir, "power_rankings_colored.png"), 
       power_plot, width = 14, height = 8, dpi = 300)
cat("      ✓ Power rankings plot saved\n")

# Efficiency matrix
efficiency_plot <- create_offense_defense_scatter(offense_rankings, defense_rankings, teams_colors_logos)
ggsave(file.path(matchup_dir, "efficiency_matrix.png"), 
       efficiency_plot, width = 12, height = 10, dpi = 300)
cat("      ✓ Efficiency matrix saved\n")

# Enhanced week overview with adjustments
overview_plot <- matchup_summary_df %>%
  arrange(desc(abs(net_home_advantage))) %>%
  head(10) %>%
  mutate(
    game = reorder(game, net_home_advantage),
    has_weather = weather_impact > 1,
    has_rest_issue = is_thursday | abs(home_days_rest - away_days_rest) > 3
  ) %>%
  ggplot(aes(x = game, y = net_home_advantage, fill = has_weather)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#0066CC"),
    labels = c("Normal Conditions", "Weather Impact")
  ) +
  coord_flip() +
  labs(
    title = paste("Week", WEEK, "- Net Matchup Advantages (Adjusted)"),
    subtitle = "Includes weather, injuries, and rest adjustments",
    x = NULL, 
    y = "Net EPA Advantage (Adjusted)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(file.path(matchup_dir, "week_overview_enhanced.png"), 
       overview_plot, width = 12, height = 10, dpi = 300)
cat("      ✓ Enhanced week overview saved\n")

# NEW: Situational strength comparison
situational_plot <- offense_rankings %>%
  select(offense_team, third_down_rank, red_zone_rank, success_rank) %>%
  pivot_longer(cols = ends_with("_rank"), names_to = "metric", values_to = "rank") %>%
  mutate(
    metric = case_when(
      metric == "third_down_rank" ~ "3rd Down",
      metric == "red_zone_rank" ~ "Red Zone",
      metric == "success_rank" ~ "Overall Success"
    )
  ) %>%
  group_by(metric) %>%
  slice_min(rank, n = 10) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(offense_team, -rank), y = 33 - rank, fill = metric)) +
  geom_col() +
  facet_wrap(~metric, scales = "free_x") +
  scale_fill_manual(values = c("3rd Down" = "#3498db", 
                               "Red Zone" = "#e74c3c", 
                               "Overall Success" = "#2ecc71")) +
  labs(
    title = "Top 10 Teams by Situational Efficiency",
    x = NULL,
    y = "Efficiency Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave(file.path(matchup_dir, "situational_rankings.png"), 
       situational_plot, width = 14, height = 6, dpi = 300)
cat("      ✓ Situational rankings saved\n")

# Export enhanced data
write.csv(matchup_summary_df, file.path(matchup_dir, "matchup_summary_enhanced.csv"), row.names = FALSE)

betting_recs <- matchup_summary_df %>%
  mutate(
    spread_lean = case_when(
      net_home_advantage > 0.15 ~ paste("HOME", home_team, "has significant edge"),
      net_home_advantage < -0.15 ~ paste("AWAY", away_team, "has significant edge"),
      TRUE ~ "Even matchup"
    ),
    confidence = case_when(
      abs(net_home_advantage) > 0.20 ~ "VERY HIGH",
      abs(net_home_advantage) > 0.15 ~ "HIGH",
      abs(net_home_advantage) > 0.10 ~ "MEDIUM",
      abs(net_home_advantage) > 0.05 ~ "LOW",
      TRUE ~ "VERY LOW"
    ),
    notes = case_when(
      weather_impact >= 2 ~ "⚠️ Significant weather impact",
      is_thursday ~ "⚠️ Thursday game (short rest)",
      abs(home_days_rest - away_days_rest) > 3 ~ "⚠️ Rest advantage",
      TRUE ~ ""
    )
  ) %>%
  arrange(desc(abs(net_home_advantage)))

write.csv(betting_recs, file.path(matchup_dir, "betting_recommendations_enhanced.csv"), row.names = FALSE)

# Create detailed matchup report
matchup_report <- matchup_summary_df %>%
  left_join(
    offense_rankings %>% select(offense_team, third_down_rate, red_zone_td_rate, plays_per_game),
    by = c("home_team" = "offense_team")
  ) %>%
  left_join(
    offense_rankings %>% select(offense_team, third_down_rate, red_zone_td_rate, plays_per_game),
    by = c("away_team" = "offense_team"),
    suffix = c("_home", "_away")
  ) %>%
  left_join(
    defense_rankings %>% select(defense_team, def_third_down_rate, def_red_zone_td_rate),
    by = c("home_team" = "defense_team")
  ) %>%
  left_join(
    defense_rankings %>% select(defense_team, def_third_down_rate, def_red_zone_td_rate),
    by = c("away_team" = "defense_team"),
    suffix = c("_home_def", "_away_def")
  )

write.csv(matchup_report, file.path(matchup_dir, "detailed_matchup_report.csv"), row.names = FALSE)

end_time_phase2 <- Sys.time()
cat("\n✓ Phase 2 complete in", round(difftime(end_time_phase2, start_time_phase2, units = "secs"), 1), "seconds\n\n")

# ============================================================================
# FINAL SUMMARY WITH ENHANCEMENTS
# ============================================================================

total_time <- difftime(Sys.time(), start_time_phase1, units = "secs")

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  ENHANCED PIPELINE COMPLETE!                                   ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("Total execution time:", round(total_time, 1), "seconds\n\n")

cat("📊 ENHANCEMENTS INCLUDED:\n")
cat("  ✓ Weather impact analysis (", n_distinct(weather_data$game_id), "games)\n", sep = "")
cat("  ✓ Injury tracking (", if(nrow(injuries_summary) > 0) nrow(injuries_summary) else 0, "team-weeks)\n", sep = "")
cat("  ✓ Rest & schedule factors (Thursday games, short weeks)\n")
cat("  ✓ Situational stats (3rd down, red zone, 2-min drill)\n")
cat("  ✓ Pace of play metrics\n")
cat("  ✓ Adjusted EPA projections\n\n")

cat("🎯 TOP MATCHUPS FOR WEEK", WEEK, ":\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

if (nrow(betting_recs) > 0) {
  for (i in 1:min(5, nrow(betting_recs))) {
    bet <- betting_recs[i, ]
    cat(sprintf("%d. %s\n", i, bet$game))
    cat(sprintf("   Projected Margin: %.1f (favoring %s)\n", 
                abs(bet$projected_margin), 
                if_else(bet$projected_margin > 0, bet$home_team, bet$away_team)))
    cat(sprintf("   Projected Total: %.1f points\n", bet$projected_total))
    cat(sprintf("   Model Confidence: %s\n", bet$confidence))
    
    if (bet$weather_impact > 0) {
      cat(sprintf("   ⛈️  Weather Impact: %d/3\n", bet$weather_impact))
    }
    if (bet$is_thursday) {
      cat("   📅 Thursday Night Game (short rest)\n")
    }
    if (abs(bet$home_days_rest - bet$away_days_rest) > 2) {
      cat(sprintf("   😴 Rest Advantage: %s has %d more days\n",
                  if_else(bet$home_days_rest > bet$away_days_rest, bet$home_team, bet$away_team),
                  abs(bet$home_days_rest - bet$away_days_rest)))
    }
    
    cat(sprintf("   EPA Advantage: %.3f (before) → %.3f (after adjustments)\n",
                bet$home_base_epa - bet$away_base_epa,
                bet$net_home_advantage))
    cat("\n")
  }
}

cat("═══════════════════════════════════════════════════════════════\n\n")

cat("📁 OUTPUT FILES:\n")
cat("   Main Directory:", output_dir, "\n")
cat("   Matchup Analysis:", matchup_dir, "\n\n")

cat("📄 KEY FILES CREATED:\n")
cat("   • offense_weekly.csv (enhanced with situational stats)\n")
cat("   • defense_weekly.csv (enhanced with situational stats)\n")
cat("   • weather_data.csv (weather conditions & impact scores)\n")
if (nrow(injuries_summary) > 0) {
  cat("   • injuries_summary.csv (injury impact by team)\n")
}
cat("   • schedule_rest.csv (rest days & schedule factors)\n")
cat("   • matchup_summary_enhanced.csv (full projections)\n")
cat("   • betting_recommendations_enhanced.csv (with context)\n")
cat("   • detailed_matchup_report.csv (all metrics combined)\n\n")

cat("📊 VISUALIZATIONS CREATED:\n")
cat("   • power_rankings_colored.png\n")
cat("   • efficiency_matrix.png\n")
cat("   • week_overview_enhanced.png\n")
cat("   • situational_rankings.png\n\n")

cat("🔄 NEXT STEPS:\n")
cat("   1. Run nfl_odds_integration.R to compare with Vegas lines\n")
cat("   2. Run nfl_player_props_model.R for player projections\n")
cat("   3. Launch nfl_dashboard_app.R to visualize results\n")
cat("   4. After games complete, track prediction accuracy!\n\n")

cat("✅ Ready to make informed betting decisions!\n\n")