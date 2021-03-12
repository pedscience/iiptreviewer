#' Fixed Effects Meta-Analysis
#'
#' Calculate a fixed effects meta-analysis. This is mainly used for Baujat plots
#'
#' @importFrom metafor `rma.uni`
#'
#' @param data A dataframe containing the effect sizes and their associated variance
#'
#' @export
calc_fixed_meta_analysis <- function(data) {
  g <- var_g <- study_label <- . <- NULL

  data %>%
    rma.uni(yi = g, vi = var_g, slab = study_label, data = .)
}
