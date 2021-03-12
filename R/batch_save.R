#' Save Plots in Batch
#'
#' Save all plots in a list at once to a given directory.
#'
#' @importFrom dplyr pull
#' @importFrom purrr walk2
#'
#' @param data A dataframe containing a list column of plots
#' @param plot_list A string indicating the plot column
#' @param file_name_list A string indicating the file name column
#' @param path A string to a directory where plots should be saved
#' @param width Numeric, plot width
#' @param height Numeric, plot height
#'
#' @export
batch_save <- function(data, plot_list = plots, file_name_list = file_names, path, width = NA, height = NA) {
  plots <- file_names <- NULL

  # Pull plots and file names from plot tibble
  plot_list       <- data %>% pull({{ plot_list }})
  file_name_list <- data %>% pull({{ file_name_list }})


  # Save all line plots at once. For some outcomes, there is only one study and
  # thus no line to draw. In this case, ggplot2 will throw a warning This is to be
  # expected.
  walk2(plot_list, file_name_list, ~ ggsave(filename = .y, plot = .x, path = path, width = width, height = height))
}
