# EXERCISE 1: How useful are the prediction intervals? --------------------
# Work in pairs. Open nora_uq_workshop.Rproj first.
# Run this script section by section, filling in the two NA values.
# For ready-to-run calculations, use the worked solution instead:
# source("solutions/01_intervals_solution.R")
# Finish with two numbers, one finding and one question.

# A. Predict ------------------------------------------------------------
# What would 90% coverage mean across a new group of houses?
# Your prediction: <TO DO>

# B. Inspect the supplied predictions -----------------------------------
# One quantile regression forest supplies the estimated 5th and 95th
# percentiles: the endpoints of each nominal 90% prediction interval.
# The target is the sale price of a house with these recorded features.
# Note: these intervals have not yet been conformally calibrated.
source("R/helpers.R") # Load some help functions (you do not need to study this script).
reference = load_reference() # Load pre-done calculations
intervals = reference$test_intervals # Predicted intervals for test houses.
y = reference$split$test$price # True prices for test houses.

# Each row is one test house. Prices and interval endpoints are in USD.
head(data.frame(price = y, intervals), 3) # The first three houses.
dev.new() # make new plotting window
plot_intervals(y, intervals) # See if predicted intervals contain the true test house price for first 24 houses 

# C. Calculate two summaries using all 586 test houses -------------------
# Coverage: the fraction of observed prices inside their intervals. See START_HERE.html for hints.
coverage = NA_real_ # <TO DO> 

# Mean width: average interval width.
mean_width = NA_real_ # <TO DO> 

print(c(coverage = coverage, mean_width_USD = mean_width))
# Coverage is a fraction; multiply it by 100 to express it as a percentage.

# D. Discuss ------------------------------------------------------------
# How does observed coverage compare with the 90% target?
# Would these ranges be useful to someone deciding how much to bid?
# Your finding: <TO DO> 
# One question you would investigate next: <TO DO> 

# Stop here when your two numbers, finding and question are ready.
# We will discuss the results together, then use 03_calibration_demo.R
# to adjust the intervals using the separate calibration data.
