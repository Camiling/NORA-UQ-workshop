# Run this from the workshop project folder during Exercise 1.
# The default checks local data and cached predictions without installing packages.
install_ranger = FALSE  # Optional: TRUE prepares fresh model fitting later.
if (getRversion() < "4.2.0") stop("Please use R 4.2 or newer.")
if (install_ranger && !requireNamespace("ranger", quietly = TRUE)) {
  tryCatch(install.packages("ranger", repos = "https://cloud.r-project.org"),
           error = function(e) message("Package installation: ", conditionMessage(e)))
}
source("R/helpers.R")
data = read_ames()
stopifnot(nrow(data) == 2930, !anyDuplicated(data$property_id))
reference = load_reference()
check_split(reference$split)
if (requireNamespace("ranger", quietly = TRUE)) {
  cat("Optional fresh fitting is available: R", as.character(getRversion()),
      "and ranger", as.character(packageVersion("ranger")), "\n")
} else {
  cat("Optional fresh fitting needs ranger.\n")
}
cat("Ready: both exercises use included predictions by default and work in base R.\n")
cat("Open 01_intervals.R for Exercise 1. Work from the project directory.\n")
