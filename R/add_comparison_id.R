#' Add Comparison ID
#'
#' Add a comparison ID (`comparison_id`) to keep track of studies in case of
#' identical `study_label`
#'
#' @importFrom dplyr mutate row_number
#'
#' @param data A data frame containing data for a meta-analysis
#'
#' @export
#'
#' @examples
#' iipt_dataset %>%
#'   add_comparison_id()
add_comparison_id <- function(data) {
  data %>%
    mutate(comparison_id = as.character(row_number()))
}
