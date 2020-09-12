source('./analysis.R')

# This file list all the experiments to be run.
# NOMENCLATURE
# source_ : means one interpolation per source is made
# _ci: 'confidence' interval is used for model uncertainty
# _pi: 'prediction' interval is used for model uncertainty

# Original
fn_aggregate_omi = mean
formula = value_original ~ value_original_mean + (r50 + r200):(SOURCETY + ELEVATION)
interval = 'confidence'
cutoff_date = "0000-09-01"
experiment_name = "original_ci"
lm_per_source = F
run_all(formula, interval, experiment_name, cutoff_date, lm_per_source)

# Same with prediction interval instead
fn_aggregate_omi = mean
interval = 'prediction'
experiment_name = "original_pi"
run_all(formula, interval, experiment_name, cutoff_date, lm_per_source, fn_aggregate_omi)

# ~ One interpolation per source - V1
formula = value_original ~ factor(NUMBER) + factor(NUMBER):(r10 + r50 + r100 + r200)
interval = 'confidence'
experiment_name = "source_based_ci_v1"
lm_per_source=F
run_all(formula, interval, experiment_name, cutoff_date, lm_per_source, fn_aggregate_omi)

# Same with prediction interval instead
interval = 'prediction'
experiment_name = "source_based_pi_v1"
run_all(formula, interval, experiment_name, cutoff_date, lm_per_source, fn_aggregate_omi)

# ~ One interpolation per source - V2 (should give similar results as V1)
formula = value_original ~ value_original_mean + r10 + r50 + r100 + r200
interval = 'confidence'
experiment_name = "source_based_ci_v2"
lm_per_source=T
run_all(formula, interval, experiment_name, cutoff_date, lm_per_source, fn_aggregate_omi)

# Same with prediction interval instead
interval = 'prediction'
experiment_name = "source_based_pi_v2"
run_all(formula, interval, experiment_name, cutoff_date, lm_per_source, fn_aggregate_omi)

# Using sum as an aggregation function per source/year
fn_aggregate_omi = sum
interval = 'confidence'
experiment_name = "source_based_ci_v2_sum"
run_all(formula, interval, experiment_name, cutoff_date, lm_per_source, fn_aggregate_omi)

# Benchmark: using mean only as a predictor already gives a high R2, hence maybe our initial confidence
# in the model
fn_aggregate_omi = mean
formula = value_original ~ value_original_mean
interval = 'confidence'
experiment_name = "mean_only_ci"
lm_per_source = F
run_all(formula, interval, experiment_name, cutoff_date, lm_per_source)


# Sanity check: no change in year, to compare with NASA uncertainty calculation

# fn_aggregate_omi = mean
# formula = value_original ~ value_original_mean + r10 + r50 + r100 + r200
# cutoff_date = "0000-01-01"
# interval = 'confidence'
# cutoff_date = NULL
# experiment_name = "normalyear_source_ci"
# lm_per_source = T
# run_all(formula, interval, experiment_name, cutoff_date, lm_per_source)
#
# interval = 'prediction'
# experiment_name = "normalyear_source_pi"
# run_all(formula, interval, experiment_name, cutoff_date, lm_per_source)
