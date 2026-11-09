# EXERCISE 1: fully worked route, available from the start.
# Open nora_uq_workshop.Rproj, then run this file section by section.
# You can also run: source("solutions/01_intervals_solution.R")
# Everything below runs as written. Read the comments and discuss the results.
# This route uses the included forest predictions and needs only base R.

# A. Load the prepared predictions and observed test prices.
source("R/helpers.R")
reference = load_reference()
split = reference$split
intervals = reference$test_intervals
y = split$test$price
head(data.frame(price = y, intervals), 3)
dev.new() # make new plotting window
plot_intervals(y, intervals)

# Before continuing: what do you expect 90% coverage to mean?
# Prediction (write a few words):

# B. Check each house. TRUE means its observed price is inside its interval.
covered = y >= intervals$lower & y <= intervals$upper
# mean() treats TRUE as 1 and FALSE as 0, giving the fraction covered.
coverage = mean(covered)
# Calculate a width for each house, then average those widths.
mean_width = mean(intervals$upper - intervals$lower)
cat(sprintf("Coverage: %.1f%% (%d of %d houses)\nMean interval width: $%.2f\n",
            100 * coverage, sum(covered), length(y), mean_width))

# C. Pause here and write your own interpretation before reading the example.
# Finding (how useful are these ranges for a housing decision?):
# Question (what would you check next?):
# Both routes have the same finish point: two numbers, a finding and a question.

# Example explanation: these intervals cover about 96% of the test prices,
# against a nominal 90% target. Their mean width is about $112,000.
# Usefulness depends on the housing decision and the acceptable price range.
# A follow-up check could assess coverage and width within a relevant subgroup.
