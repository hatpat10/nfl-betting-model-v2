# ============================================================================
# NFL BETTING MODEL - THE ODDS API INTEGRATION (ENHANCED)
# ============================================================================
# Purpose: Fetch real-time odds and compare against model predictions
# Run AFTER nfl_master_pipeline.R completes
# ============================================================================

# Clear environment
rm(list = ls())
gc()

# ============================================================================
# CONFIGURATION
# ============================================================================

ODDS_API_KEY <- "a35f7d5b7f91dbca7a9364b72c1b1cf2"
SEASON <- 2025
WEEK <- 9
BASE_DIR <- 'C:/Users/Patsc/Documents/nfl'
MIN_EDGE_THRESHOLD <- 1.5

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║     NFL BETTING MODEL - THE ODDS API INTEGRATION               ║\n")
cat("║     Week:", WEEK, "                                                    ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# LOAD PACKAGES
# ============================================================================

cat("Loading required packages...\n")

required_packages <- c("httr", "jsonlite", "dplyr", "tidyr", "writexl", "ggplot2")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

cat("✓ Packages loaded!\n\n")

# ============================================================================
# LOAD MODEL PREDICTIONS (ENHANCED VERSION)
# ============================================================================

cat("Loading model predictions...\n")

matchup_dir <- file.path(BASE_DIR, paste0('week', WEEK), 'matchup_analysis')

# Try to load enhanced version first, fall back to standard
enhanced_file <- file.path(matchup_dir, "betting_recommendations_enhanced.csv")
standard_file <- file.path(matchup_dir, "betting_recommendations.csv")

if (file.exists(enhanced_file)) {
  model_predictions <- read.csv(enhanced_file)
  cat("✓ Using ENHANCED predictions with weather/injury adjustments\n")
  using_enhanced <- TRUE
} else if (file.exists(standard_file)) {
  model_predictions <- read.csv(standard_file)
  cat("✓ Using standard predictions\n")
  using_enhanced <- FALSE
} else {
  stop("\n❌ ERROR: Run nfl_master_pipeline.R first!\n")
}

# Load matchup summary for additional context
matchup_summary_enhanced <- file.path(matchup_dir, "matchup_summary_enhanced.csv")
matchup_summary_standard <- file.path(matchup_dir, "matchup_summary.csv")

if (file.exists(matchup_summary_enhanced)) {
  matchup_summary <- read.csv(matchup_summary_enhanced)
  cat("✓ Loaded enhanced matchup data\n")
} else if (file.exists(matchup_summary_standard)) {
  matchup_summary <- read.csv(matchup_summary_standard)
  cat("✓ Loaded standard matchup data\n")
} else {
  matchup_summary <- NULL
  cat("⚠ Matchup summary not found\n")
}

cat("✓ Loaded", nrow(model_predictions), "predictions\n\n")

# ============================================================================
# FETCH ODDS FROM THE ODDS API
# ============================================================================

cat("Fetching odds from The Odds API...\n")

fetch_theoddsapi_odds <- function(api_key, regions = "us", markets = "spreads,totals") {
  
  # The Odds API endpoint for NFL
  base_url <- "https://api.the-odds-api.com/v4/sports/americanfootball_nfl/odds/"
  
  # Build query parameters
  params <- list(
    apiKey = api_key,
    regions = regions,
    markets = markets,
    oddsFormat = "american",
    dateFormat = "iso"
  )
  
  response <- httr::GET(
    url = base_url,
    query = params
  )
  
  # Check remaining requests in headers
  remaining_requests <- httr::headers(response)$`x-requests-remaining`
  if (!is.null(remaining_requests)) {
    cat("   API requests remaining:", remaining_requests, "\n")
  }
  
  if (httr::status_code(response) != 200) {
    stop("API Error: Status ", httr::status_code(response), 
         "\nMessage: ", httr::content(response, "text"))
  }
  
  # Parse response
  odds_data <- httr::content(response, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  return(odds_data)
}

tryCatch({
  odds_raw <- fetch_theoddsapi_odds(ODDS_API_KEY)
  
  if (is.null(odds_raw) || length(odds_raw) == 0 || nrow(odds_raw) == 0) {
    stop("No data returned from API")
  }
  
  cat("✓ Fetched odds data for", nrow(odds_raw), "upcoming NFL games\n\n")
  
}, error = function(e) {
  stop("\n❌ Error fetching odds:\n", e$message, 
       "\n\nTroubleshooting:\n",
       "  1. Check your API key is valid\n",
       "  2. Visit: https://the-odds-api.com/account/ to verify your quota\n",
       "  3. Ensure NFL season is active (odds only available for upcoming games)\n")
})

# ============================================================================
# PROCESS THE ODDS API DATA
# ============================================================================

cat("Processing odds data...\n")

# The Odds API returns a different structure - extract spreads and totals
process_theoddsapi_game <- function(game_row) {
  
  # Basic game info
  game_info <- list(
    game_id = game_row$id,
    home_team = game_row$home_team,
    away_team = game_row$away_team,
    game_date = game_row$commence_time
  )
  
  # Extract bookmaker odds
  if (is.null(game_row$bookmakers) || length(game_row$bookmakers) == 0) {
    return(NULL)
  }
  
  bookmakers <- game_row$bookmakers
  if (!is.data.frame(bookmakers)) {
    bookmakers <- as.data.frame(bookmakers)
  }
  
  # Priority order for bookmakers
  priority_books <- c("draftkings", "fanduel", "betmgm", "williamhill_us", 
                      "barstool", "pointsbetus", "bovada")
  
  best_book <- NULL
  best_book_name <- NULL
  
  for (book in priority_books) {
    book_data <- bookmakers[bookmakers$key == book, ]
    if (nrow(book_data) > 0) {
      best_book <- book_data[1, ]
      best_book_name <- book_data$title[1]
      break
    }
  }
  
  # If no priority book found, use first available
  if (is.null(best_book) && nrow(bookmakers) > 0) {
    best_book <- bookmakers[1, ]
    best_book_name <- bookmakers$title[1]
  }
  
  if (is.null(best_book)) {
    return(NULL)
  }
  
  game_info$sportsbook <- best_book_name
  
  # Extract markets (spreads and totals)
  markets <- best_book$markets[[1]]
  if (!is.data.frame(markets)) {
    return(NULL)
  }
  
  # Get spread data
  spread_market <- markets[markets$key == "spreads", ]
  if (nrow(spread_market) > 0 && !is.null(spread_market$outcomes[[1]])) {
    outcomes <- spread_market$outcomes[[1]]
    
    home_outcome <- outcomes[outcomes$name == game_info$home_team, ]
    away_outcome <- outcomes[outcomes$name == game_info$away_team, ]
    
    if (nrow(home_outcome) > 0 && nrow(away_outcome) > 0) {
      game_info$home_spread <- as.numeric(home_outcome$point[1])
      game_info$away_spread <- as.numeric(away_outcome$point[1])
      game_info$home_spread_odds <- as.numeric(home_outcome$price[1])
      game_info$away_spread_odds <- as.numeric(away_outcome$price[1])
    }
  }
  
  # Get totals data
  totals_market <- markets[markets$key == "totals", ]
  if (nrow(totals_market) > 0 && !is.null(totals_market$outcomes[[1]])) {
    outcomes <- totals_market$outcomes[[1]]
    over_outcome <- outcomes[outcomes$name == "Over", ]
    if (nrow(over_outcome) > 0) {
      game_info$over_under <- as.numeric(over_outcome$point[1])
    }
  }
  
  # Validate we got spread data
  if (is.null(game_info$home_spread) || is.na(game_info$home_spread)) {
    return(NULL)
  }
  
  return(game_info)
}

# Process all games
all_games <- list()
failed_games <- 0

for (i in 1:nrow(odds_raw)) {
  game_odds <- process_theoddsapi_game(odds_raw[i, ])
  if (!is.null(game_odds)) {
    all_games[[length(all_games) + 1]] <- game_odds
  } else {
    failed_games <- failed_games + 1
  }
}

cat("Processed", nrow(odds_raw), "games -", length(all_games), "with spreads,", 
    failed_games, "without spreads\n")

if (length(all_games) == 0) {
  stop("\n❌ No spread data available. Check if NFL games are scheduled.\n")
}

# Convert to dataframe
odds_df <- do.call(rbind, lapply(all_games, function(x) {
  data.frame(
    game_id = if (is.null(x$game_id)) NA else x$game_id,
    home_team = if (is.null(x$home_team)) NA else x$home_team,
    away_team = if (is.null(x$away_team)) NA else x$away_team,
    sportsbook = if (is.null(x$sportsbook)) "Unknown" else x$sportsbook,
    home_spread = if (is.null(x$home_spread)) NA else as.numeric(x$home_spread),
    away_spread = if (is.null(x$away_spread)) NA else as.numeric(x$away_spread),
    home_spread_odds = if (is.null(x$home_spread_odds)) NA else as.numeric(x$home_spread_odds),
    away_spread_odds = if (is.null(x$away_spread_odds)) NA else as.numeric(x$away_spread_odds),
    over_under = if (is.null(x$over_under)) NA else as.numeric(x$over_under),
    game_date = if (is.null(x$game_date)) NA else x$game_date,
    stringsAsFactors = FALSE
  )
}))

# Remove rows with missing critical data
odds_df <- odds_df %>%
  filter(!is.na(home_team), !is.na(away_team), !is.na(home_spread))

cat("✓ Extracted spreads for", nrow(odds_df), "games from", 
    length(unique(odds_df$sportsbook)), "sportsbook(s)\n")
cat("   Using:", paste(unique(odds_df$sportsbook), collapse = ", "), "\n\n")

# ============================================================================
# STANDARDIZE TEAM NAMES FOR MATCHING
# ============================================================================

cat("Matching team names...\n")

# The Odds API uses full team names, need to map to abbreviations
# Map full team names to NFL abbreviations (matching model format)
team_name_to_abbr <- c(
  "Arizona Cardinals" = "ARI",
  "Atlanta Falcons" = "ATL",
  "Baltimore Ravens" = "BAL",
  "Buffalo Bills" = "BUF",
  "Carolina Panthers" = "CAR",
  "Chicago Bears" = "CHI",
  "Cincinnati Bengals" = "CIN",
  "Cleveland Browns" = "CLE",
  "Dallas Cowboys" = "DAL",
  "Denver Broncos" = "DEN",
  "Detroit Lions" = "DET",
  "Green Bay Packers" = "GB",
  "Houston Texans" = "HOU",
  "Indianapolis Colts" = "IND",
  "Jacksonville Jaguars" = "JAX",
  "Kansas City Chiefs" = "KC",
  "Las Vegas Raiders" = "LV",
  "Los Angeles Chargers" = "LAC",
  "Los Angeles Rams" = "LAR",
  "Miami Dolphins" = "MIA",
  "Minnesota Vikings" = "MIN",
  "New England Patriots" = "NE",
  "New Orleans Saints" = "NO",
  "New York Giants" = "NYG",
  "New York Jets" = "NYJ",
  "Philadelphia Eagles" = "PHI",
  "Pittsburgh Steelers" = "PIT",
  "San Francisco 49ers" = "SF",
  "Seattle Seahawks" = "SEA",
  "Tampa Bay Buccaneers" = "TB",
  "Tennessee Titans" = "TEN",
  "Washington Commanders" = "WAS"
)

# Apply mapping to convert full names to abbreviations
odds_df <- odds_df %>%
  mutate(
    home_team_abbr = ifelse(home_team %in% names(team_name_to_abbr), 
                            team_name_to_abbr[home_team], home_team),
    away_team_abbr = ifelse(away_team %in% names(team_name_to_abbr), 
                            team_name_to_abbr[away_team], away_team),
    matchup_key = paste(away_team_abbr, "at", home_team_abbr)
  )

cat("✓ Team names standardized\n")
cat("   Sample matchups from odds:\n")
for (i in 1:min(3, nrow(odds_df))) {
  cat("   -", odds_df$matchup_key[i], "(from", odds_df$away_team[i], "at", odds_df$home_team[i], ")\n")
}
cat("\n")

# Check for any unmapped teams
unmapped_home <- unique(odds_df$home_team[!(odds_df$home_team %in% names(team_name_to_abbr))])
unmapped_away <- unique(odds_df$away_team[!(odds_df$away_team %in% names(team_name_to_abbr))])
if (length(unmapped_home) > 0 || length(unmapped_away) > 0) {
  cat("⚠️  Warning: Some teams couldn't be mapped:\n")
  if (length(unmapped_home) > 0) cat("   Home teams:", paste(unmapped_home, collapse = ", "), "\n")
  if (length(unmapped_away) > 0) cat("   Away teams:", paste(unmapped_away, collapse = ", "), "\n")
  cat("\n")
}

# ============================================================================
# MERGE WITH MODEL PREDICTIONS
# ============================================================================

cat("Merging with model predictions...\n")

model_predictions <- model_predictions %>%
  mutate(matchup_key = game)

betting_analysis <- model_predictions %>%
  inner_join(odds_df, by = "matchup_key")

if (nrow(betting_analysis) == 0) {
  cat("\n⚠️  Could not match odds with model predictions.\n\n")
  cat("   Model predictions (", nrow(model_predictions), " games):\n", sep = "")
  for (i in 1:min(10, nrow(model_predictions))) {
    cat("   ", i, ". ", model_predictions$matchup_key[i], "\n", sep = "")
  }
  cat("\n   Odds matchups (", nrow(odds_df), " games):\n", sep = "")
  for (i in 1:min(10, nrow(odds_df))) {
    cat("   ", i, ". ", odds_df$matchup_key[i], "\n", sep = "")
  }
  cat("\n❌ No matching games found. Possible issues:\n")
  cat("   1. Week number mismatch (check if odds are for a different week)\n")
  cat("   2. Team abbreviation mismatch (check the debug output above)\n")
  cat("   3. Different game schedules (bye weeks, postponements, etc.)\n\n")
  stop("Team names don't match. Manual adjustment needed.\n")
}

cat("✓ Matched", nrow(betting_analysis), "games\n\n")

# Add enhanced context if available
if (!is.null(matchup_summary) && nrow(matchup_summary) > 0) {
  # Check if enhanced columns exist
  if ("weather_impact" %in% names(matchup_summary)) {
    betting_analysis <- betting_analysis %>%
      left_join(
        matchup_summary %>% select(game, weather_impact, is_dome, is_thursday, 
                                   home_days_rest, away_days_rest),
        by = c("matchup_key" = "game")
      )
    cat("✓ Added weather and rest context\n\n")
  }
}

betting_analysis <- betting_analysis %>%
  mutate(
    vegas_line = home_spread,
    model_line = projected_margin,
    edge = model_line - vegas_line,
    abs_edge = abs(edge),
    
    bet_side = case_when(
      edge >= MIN_EDGE_THRESHOLD ~ home_team_abbr,
      edge <= -MIN_EDGE_THRESHOLD ~ away_team_abbr,
      TRUE ~ "PASS"
    ),
    
    bet_spread = case_when(
      edge >= MIN_EDGE_THRESHOLD ~ home_spread,
      edge <= -MIN_EDGE_THRESHOLD ~ away_spread,
      TRUE ~ NA_real_
    ),
    
    bet_odds = case_when(
      edge >= MIN_EDGE_THRESHOLD ~ home_spread_odds,
      edge <= -MIN_EDGE_THRESHOLD ~ away_spread_odds,
      TRUE ~ NA_real_
    ),
    
    ev_tier = case_when(
      abs_edge >= 5 ~ "🔥 ELITE",
      abs_edge >= 3.5 ~ "⭐ STRONG",
      abs_edge >= MIN_EDGE_THRESHOLD ~ "✓ GOOD",
      TRUE ~ "❌ PASS"
    ),
    
    bet_recommendation = if_else(
      bet_side != "PASS",
      paste0("BET ", bet_side, " ", sprintf("%+.1f", bet_spread), " (", sprintf("%+.0f", bet_odds), ")"),
      "NO BET"
    )
  )

# ============================================================================
# FILTER PROFITABLE BETS WITH CONTEXT
# ============================================================================

profitable_bets <- betting_analysis %>%
  filter(ev_tier != "❌ PASS") %>%
  arrange(desc(abs_edge))

# Add context notes if available
if ("weather_impact" %in% names(profitable_bets)) {
  profitable_bets <- profitable_bets %>%
    mutate(
      context_notes = case_when(
        weather_impact >= 2 ~ "⛈️ Weather impact",
        is_thursday ~ "📅 Thursday game",
        abs(home_days_rest - away_days_rest) > 3 ~ "😴 Rest mismatch",
        TRUE ~ ""
      )
    )
}

profitable_bets <- profitable_bets %>%
  select(
    matchup_key, ev_tier, confidence,
    bet_recommendation, edge, vegas_line, model_line,
    sportsbook, home_team_abbr, away_team_abbr, over_under, game_date,
    contains("context"), contains("weather"), contains("rest"), contains("notes")
  )

cat("✓ Found", nrow(profitable_bets), "profitable bets\n\n")

# ============================================================================
# CREATE SUMMARY
# ============================================================================

summary_stats <- data.frame(
  Metric = c(
    "Total Games Analyzed",
    "Profitable Bets Found",
    "Elite Value Bets",
    "Strong Value Bets",
    "Good Value Bets",
    "Average Edge (Profitable)",
    "Largest Edge"
  ),
  Value = c(
    nrow(betting_analysis),
    nrow(profitable_bets),
    sum(betting_analysis$ev_tier == "🔥 ELITE"),
    sum(betting_analysis$ev_tier == "⭐ STRONG"),
    sum(betting_analysis$ev_tier == "✓ GOOD"),
    if(nrow(profitable_bets) > 0) round(mean(profitable_bets$abs_edge, na.rm = TRUE), 2) else 0,
    round(max(betting_analysis$abs_edge), 2)
  )
)

# ============================================================================
# EXPORT RESULTS
# ============================================================================

cat("Exporting results...\n")

odds_output_dir <- file.path(BASE_DIR, paste0('week', WEEK), 'odds_analysis')
if (!dir.exists(odds_output_dir)) {
  dir.create(odds_output_dir, recursive = TRUE)
}

# Excel file
write_xlsx(
  list(
    Summary = summary_stats,
    Profitable_Bets = profitable_bets,
    All_Games = betting_analysis %>% 
      select(matchup_key, ev_tier, edge, vegas_line, model_line, 
             bet_recommendation, confidence, sportsbook, over_under, game_date,
             contains("weather"), contains("rest")) %>%
      arrange(desc(abs(edge)))
  ),
  file.path(odds_output_dir, paste0("betting_picks_week", WEEK, ".xlsx"))
)

# CSV files
write.csv(profitable_bets, 
          file.path(odds_output_dir, "profitable_bets.csv"), 
          row.names = FALSE)

write.csv(betting_analysis %>% 
            select(matchup_key, ev_tier, edge, vegas_line, model_line, 
                   bet_recommendation, confidence, sportsbook, game_date) %>%
            arrange(desc(abs(edge))), 
          file.path(odds_output_dir, "all_games_analysis.csv"), 
          row.names = FALSE)

cat("✓ Files exported\n\n")

# ============================================================================
# CREATE ENHANCED VISUALIZATION
# ============================================================================

cat("Creating visualization...\n")

if (nrow(betting_analysis) > 0) {
  # Add weather/context markers if available
  if ("weather_impact" %in% names(betting_analysis)) {
    betting_analysis <- betting_analysis %>%
      mutate(has_context = weather_impact > 0 | is_thursday)
  } else {
    betting_analysis <- betting_analysis %>%
      mutate(has_context = FALSE)
  }
  
  edge_plot <- betting_analysis %>%
    mutate(
      game_label = paste0(away_team_abbr, " @ ", home_team_abbr),
      has_value = abs_edge >= MIN_EDGE_THRESHOLD
    ) %>%
    arrange(desc(abs(edge))) %>%
    head(min(12, nrow(betting_analysis))) %>%
    ggplot(aes(x = reorder(game_label, edge), y = edge, fill = has_value)) +
    geom_col() +
    geom_hline(yintercept = c(-MIN_EDGE_THRESHOLD, MIN_EDGE_THRESHOLD), 
               linetype = "dashed", color = "red", linewidth = 1) +
    geom_hline(yintercept = 0, color = "black") +
    scale_fill_manual(
      values = c("TRUE" = "#00AA00", "FALSE" = "#CCCCCC"),
      labels = c("TRUE" = "Value Bet", "FALSE" = "No Value")
    ) +
    coord_flip() +
    labs(
      title = paste("Week", WEEK, "- Model vs Vegas Spreads (Enhanced)"),
      subtitle = paste("Positive = Bet Home | Negative = Bet Away | Min Edge:", MIN_EDGE_THRESHOLD, "pts"),
      x = NULL,
      y = "Edge (Model Line - Vegas Line)",
      fill = NULL,
      caption = paste("Source: The Odds API |", unique(betting_analysis$sportsbook)[1], "| Includes weather/rest adjustments")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave(file.path(odds_output_dir, "betting_edges.png"), 
         edge_plot, width = 12, height = 8, dpi = 300)
  
  cat("✓ Visualization saved\n\n")
}

# ============================================================================
# PRINT RESULTS
# ============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  BETTING ANALYSIS COMPLETE                                     ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 SUMMARY:\n")
print(summary_stats, row.names = FALSE)

cat("\n\n🎯 TOP BETTING PICKS:\n")
cat("════════════════════════════════════════════════════════════════\n\n")

if (nrow(profitable_bets) > 0) {
  for (i in 1:min(5, nrow(profitable_bets))) {
    bet <- profitable_bets[i, ]
    cat(sprintf("%s  %s\n", bet$ev_tier, bet$matchup_key))
    cat(sprintf("   %s\n", bet$bet_recommendation))
    cat(sprintf("   Edge: %.1f pts | Vegas: %.1f | Model: %.1f\n", 
                bet$edge, bet$vegas_line, bet$model_line))
    cat(sprintf("   Model Confidence: %s | Sportsbook: %s\n", 
                bet$confidence, bet$sportsbook))
    if (!is.na(bet$over_under)) {
      cat(sprintf("   Over/Under: %.1f\n", bet$over_under))
    }
    
    # Show context if available
    if ("weather_impact" %in% names(bet)) {
      if (!is.na(bet$weather_impact) && bet$weather_impact > 0) {
        cat(sprintf("   ⛈️  Weather Impact: %d/3\n", bet$weather_impact))
      }
      if (!is.na(bet$is_thursday) && bet$is_thursday) {
        cat("   📅 Thursday Night Game\n")
      }
    }
    if ("context_notes" %in% names(bet) && !is.na(bet$context_notes) && bet$context_notes != "") {
      cat(sprintf("   ℹ️  %s\n", bet$context_notes))
    }
    cat("\n")
  }
} else {
  cat("No profitable bets found this week.\n")
  cat("The model and Vegas are in close agreement.\n\n")
}

cat("════════════════════════════════════════════════════════════════\n")
cat("\n📁 Files saved to:", odds_output_dir, "\n")
cat("   - betting_picks_week", WEEK, ".xlsx\n", sep = "")
cat("   - profitable_bets.csv\n")
cat("   - all_games_analysis.csv\n")
cat("   - betting_edges.png\n")

if (using_enhanced) {
  cat("\n✨ Using ENHANCED model with weather, injuries, and rest adjustments!\n")
} else {
  cat("\n⚠️  Using standard model. Run enhanced pipeline for better predictions.\n")
}

cat("\n💡 The Odds API usage note:\n")
cat("   - Free tier: 500 requests/month\n")
cat("   - Each run uses 1 request\n")
cat("   - Check usage at: https://the-odds-api.com/account/\n")

cat("\n✓ Done! Good luck! 🏈💰\n\n")