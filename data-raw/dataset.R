library(tidyverse)
library(labelled)

iipt_dataset <- read_rds("data-raw/complete_effect_sizes.rds") %>%
  set_variable_labels(
    study_id = "Study ID",
    study_label = "Study Label",
    year = "Year Published",
    intervention_id = "Intervention ID",
    country = "Country",
    domain = "Outcome Domain",
    description = "Outcome Description",
    instrument = "Outcome Instrument",
    outcome = "Outcome",
    type = "Study Type",
    analysis_time = "Analysis Comparison Time",
    time_1 = "Time 1",
    time_2 = "Time 2",
    mean_1 = "Mean 1",
    mean_2 = "Mean 2",
    sd_1 = "SD 1",
    sd_2 = "SD 2",
    n_pairs = "Number of Pairs",
    n_1 = "n 1",
    n_2 = "n 2",
    correlation = "Correlation",
    g = "Hedges' g",
    var_g = "Variance of g",
    se_g = "SE of g",
    r = "Effect size r",
    var_r = "Variance of r",
    se_r = "SE of r",
  )

use_data(iipt_dataset, overwrite = TRUE)
