#' Plot a Confusion Matrix Heatmap
#'
#' This function takes a confusion matrix object (e.g., from `caret::confusionMatrix`)
#' or a similar structure containing a table of predictions versus actuals,
#' and generates a visually appealing heatmap using ggplot2.
#' It displays counts in each cell and uses percentages for fill intensity.
#'
#' @param conf_mat A list-like object. This object is expected to have an element
#'   named "table". `conf_mat$table` should be a `table` object or a matrix
#'   (convertible to a tibble) where `as_tibble(conf_mat$table)` results in
#'   columns for predicted values, actual values, and a column of counts (typically
#'   named 'n' if `conf_mat$table` is a `table` object).
#'   The function assumes the classes are "nondiabetic" and "diabetic".
#'
#' @param title A character string for the title of the plot. Default is "Confusion Matrix".
#'
#' @param palette A character string specifying the color palette to use for the heatmap. Default is "YlOrRd". Additional palettes avaliable via `RColorBrewer::display.brewer.all(type = "seq")`.
#'
#' @param class_one A character string for the first class label. Default is "Class1".
#'
#' @param class_two A character string for the second class label. Default is "Class2".
#'
#' @return A ggplot object representing the confusion matrix heatmap.
#'
#' @importFrom dplyr %>% mutate rename
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 ggplot aes geom_tile geom_label scale_fill_distiller theme_minimal scale_x_discrete theme element_text element_blank
#' @importFrom rlang .data sym
#' @importFrom yardstick conf_mat
#'
#' @examples
#' # Ensure modeldata and yardstick are available for the example
#' if (rlang::is_installed("modeldata") && rlang::is_installed("yardstick")) {
#'   data("two_class_example", package = "modeldata")
#'   cm <-
#'     yardstick::conf_mat(
#'       two_class_example,
#'       truth = truth, # 'truth' is a column name in two_class_example
#'       estimate = predicted # 'predicted' is a column name in two_class_example
#'     )
#'   plot_conf_matrix(cm,
#'                    title = "Sample Confusion Matrix",
#'                    palette = "Blues",
#'                    class_one = "Class1", # Matches levels in two_class_example
#'                    class_two = "Class2"  # Matches levels in two_class_example
#'                   )
#' }
#'
#'
#' @export
plot_conf_matrix <- function(
    conf_mat,
    title = "Confusion Matrix",
    palette = "YlOrRd",
    class_one = "Class1",
    class_two = "Class2"
    ) {
  # Validate input structure
  if (!is.list(conf_mat) || !("table" %in% names(conf_mat))) {
    stop("'conf_mat' must be a list with an element named 'table'.", call. = FALSE)
  }

  # Convert the input table to a tibble
  conf_df_raw <- tibble::as_tibble(conf_mat[["table"]])

  # Calculate total and percentage for heatmap fill
  conf_df <- conf_df_raw %>%
    dplyr::mutate(total = sum(.data$n)) %>%
    dplyr::mutate(pct = .data$n / .data$total)

  # Rename dimension columns to 'Predicted' and 'Actual'
    names(conf_df)[1] <- "Predicted"
    names(conf_df)[2] <- "Actual"

  # Define and apply factor levels for consistent plotting order
  expected_levels <- c(class_one, class_two)
  current_pred_levels <- unique(as.character(conf_df$Predicted))
  current_act_levels <- unique(as.character(conf_df$Actual))

  conf_df$Predicted <- factor(conf_df$Predicted, levels = expected_levels)
  conf_df$Actual <- factor(conf_df$Actual, levels = rev(expected_levels))

  # Create the heatmap with ggplot2
  p <- ggplot(conf_df, aes(x = .data$Predicted, y = .data$Actual, fill = .data$pct)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_label(aes(label = .data$n), vjust = 0.5, hjust = 0.5, size = 8, fill = "white", label.padding = ggplot2::unit(0.15, "lines")) +
    scale_fill_distiller(palette = palette, type = "seq", direction = 1, name = "Percentage") +
    theme_minimal(base_size = 12) +
    scale_x_discrete(position = "top") +
    ggplot2::labs(x = "Predicted", y = "Actual", title = title) +
    theme(
      axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = ggplot2::rel(1.1)),
      axis.text.x.top = element_text(vjust = 0.5, margin = ggplot2::margin(b = 10)),
      axis.text.y = element_text(size = ggplot2::rel(1.1)),
      axis.title = element_text(size = ggplot2::rel(1.2), face = "bold"),
      plot.title = element_text(size = ggplot2::rel(1.4), hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      aspect.ratio = 1
    )
  p
  return(p)
}
