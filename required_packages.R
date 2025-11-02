# ============================================================================
# NFL BETTING DASHBOARD - PACKAGE INSTALLER
# ============================================================================
# Run this script once to install all required packages for the dashboard

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║     NFL BETTING DASHBOARD - PACKAGE INSTALLER                  ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# List of required packages
required_packages <- c(
  # Shiny packages
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  
  # Data manipulation
  "dplyr",
  "tidyr",
  
  # Visualization
  "ggplot2",
  "plotly",
  "DT",
  "scales",
  "gt",
  "gtExtras",
  
  # NFL-specific packages
  "nflreadr",
  "nflplotR",
  "nflfastR",
  "nflverse",
  "nfl4th",
  
  # others
  "xgboost",
  "gt",
  "ggrepel",
  "writexl"
)

cat("Checking and installing required packages...\n\n")

# Function to install and load packages
install_and_load <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    cat(paste0("📦 Installing ", package, "...\n"))
    tryCatch({
      install.packages(package, dependencies = TRUE)
      cat(paste0("✓ ", package, " installed successfully\n"))
    }, error = function(e) {
      cat(paste0("❌ Error installing ", package, ": ", e$message, "\n"))
      return(FALSE)
    })
  } else {
    cat(paste0("✓ ", package, " already installed\n"))
  }
  
  # Try to load it
  tryCatch({
    suppressPackageStartupMessages(library(package, character.only = TRUE))
    return(TRUE)
  }, error = function(e) {
    cat(paste0("⚠ Warning: Could not load ", package, "\n"))
    return(FALSE)
  })
}

# Install and check each package
success_count <- 0
fail_count <- 0

for (pkg in required_packages) {
  result <- install_and_load(pkg)
  if (result) {
    success_count <- success_count + 1
  } else {
    fail_count <- fail_count + 1
  }
}

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║     INSTALLATION SUMMARY                                       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat(paste0("✓ Successfully installed/verified: ", success_count, " packages\n"))

if (fail_count > 0) {
  cat(paste0("❌ Failed to install: ", fail_count, " packages\n"))
  cat("\nPlease try installing failed packages manually:\n")
  cat("install.packages('package_name')\n")
} else {
  cat("\n🎉 All packages installed successfully!\n")
}

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("\n")
cat("📝 NEXT STEPS:\n")
cat("   1. Make sure you've run og_pipeline.R to generate predictions\n")
cat("   2. Make sure you've run og_odds.R to fetch current odds\n")
cat("   3. Run the dashboard:\n")
cat("      shiny::runApp('nfl_betting_dashboard.R')\n")
cat("\n")
cat("📖 For more information, see README_DASHBOARD.md\n")
cat("\n")

# Check if nflverse packages are working
cat("🔍 Testing NFL package functionality...\n")
test_result <- tryCatch({
  teams <- nflreadr::load_teams()
  cat(paste0("✓ Successfully loaded ", nrow(teams), " NFL teams\n"))
  cat("✓ Team logos and colors are available\n")
  TRUE
}, error = function(e) {
  cat("⚠ Warning: Could not load NFL team data\n")
  cat("   The dashboard may not display team logos correctly\n")
  FALSE
})

cat("\n")
if (test_result && fail_count == 0) {
  cat("✅ Everything is ready! You can now run the dashboard.\n")
} else if (fail_count == 0) {
  cat("⚠ Packages installed but there may be issues with NFL data\n")
  cat("   Try running: nflreadr::load_teams() to diagnose\n")
} else {
  cat("⚠ Some packages failed to install. Please resolve before running dashboard.\n")
}

cat("\n")