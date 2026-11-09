# EXERCISE 2: fully worked route, available from the start.
# Open nora_uq_workshop.Rproj, then run this file section by section.
# You can also run: source("solutions/02_refits_solution.R")
# Everything below runs as written. Read the comments and discuss the results.
# This route uses the included forest refits and needs only base R.

# A. Load summaries for the same 100 houses at both sample sizes.
source("R/helpers.R")
reference = load_reference()
refits = reference$refits
small = refits[["300"]]$summary
large = refits[["1500"]]$summary
head(data.frame(sd_300 = small$prediction_sd,
                sd_1500 = large$prediction_sd), 3)
plot_refit_summary(refits)

# Before continuing: will every house become more stable?
# Prediction (write a few words, and say which side of the diagonal you expect):

# B. Each row has a house's SD across 20 fitted median predictions.
# Summarize those house-specific SDs with their median at each sample size.
median_sd_small = median(small$prediction_sd)
median_sd_large = median(large$prediction_sd)

# C. TRUE means this house has a lower SD with the larger sample.
more_stable = large$prediction_sd < small$prediction_sd
fraction_more_stable = mean(more_stable)
cat(sprintf("Median refit SD, 300 rows: $%.2f\nMedian refit SD, 1,500 rows: $%.2f\n",
            median_sd_small, median_sd_large))
cat(sprintf("More stable with 1,500 rows: %.0f%% (%d of %d houses)\n",
            100 * fraction_more_stable, sum(more_stable), length(more_stable)))

# D. Pause here and write your own interpretation before reading the example.
# Finding (what changed between fits and what stayed fixed?):
# Question (what evidence could help reveal a bias shared by every fitted model?):
# Both routes have the same finish point: three numbers, a finding and a question.

# Example explanation: target houses stay fixed while sampled training rows
# change. Refit SD summarizes the sensitivity of fitted medians to those rows.
# Larger resamples gave lower SD for 87 of these 100 houses in this experiment.
# Shared bias and limitations of the model class need additional checks.
# The outcome noise level can also affect the difficulty of learning.
# Twenty resamples give an approximate diagnostic for this training procedure.

# Transfer question: a buyer needs a 90% range for a future sale.
# Which output from the workshop would you report, and how would you check it?
# Refit SD has a different target and no automatic 90% outcome interpretation.
