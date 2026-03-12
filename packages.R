# packages.R
# Install and load all R packages required for this project.

pkgs <- c(
  # Data manipulation
  "dplyr",
  "tidyr",
  "tidyverse",
  "readr",

  # Date/time
  "lubridate",

  # Visualisation
  "ggplot2",
  "scales",
  "gridExtra",
  "corrplot",
  "maps",

  # Spatial
  "sf",
  "ggmap",

  # Machine learning
  "caret",
  "e1071",        # SVM
  "ranger",       # Fast Random Forest
  "randomForest", # Feature importance compatibility
  "xgboost",      # Gradient boosting (RQ2 Method 1)
  "pROC",         # ROC / AUC
  "dbscan",       # Density-based clustering

  # Reporting
  "knitr",
  "tinytex"
)

# Install any missing packages
installed <- rownames(installed.packages())
to_install <- pkgs[!pkgs %in% installed]
if (length(to_install) > 0) {
  install.packages(to_install)
}

# Load all packages
invisible(lapply(pkgs, library, character.only = TRUE))
