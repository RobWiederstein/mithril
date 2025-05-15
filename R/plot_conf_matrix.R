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
#' @return A ggplot object representing the confusion matrix heatmap.
#'
#' @importFrom dplyr %>% mutate rename
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 ggplot aes geom_tile geom_label scale_fill_distiller theme_minimal scale_x_discrete theme element_text element_blank
#' @importFrom rlang .data sym
#'
#' @export
plot_conf_matrix <- function(conf_mat) {
  # Validate input structure
  if (!is.list(conf_mat) || !("table" %in% names(conf_mat))) {
    stop("'conf_mat' must be a list with an element named 'table'.", call. = FALSE)
  }

  # Convert the input table to a tibble
  conf_df_raw <- tibble::as_tibble(conf_mat[["table"]])

  # Identify and rename the count column (usually 'n' or 'Freq')
  if ("n" %in% names(conf_df_raw)) {
    conf_df <- dplyr::rename(conf_df_raw, count_col = .data$n)
  } else if ("Freq" %in% names(conf_df_raw)) {
    conf_df <- dplyr::rename(conf_df_raw, count_col = .data$Freq)
  } else if (ncol(conf_df_raw) >= 3 && is.numeric(conf_df_raw[[3]])){
    warning("Count column not named 'n' or 'Freq'. Assuming the third column contains counts.")
    count_col_name <- names(conf_df_raw)[3]
    conf_df <- dplyr::rename(conf_df_raw, count_col = !!sym(count_col_name))
  } else {
    stop("Could not find a suitable count column in conf_mat$table.", call. = FALSE)
  }

  # Calculate total and percentage for heatmap fill
  conf_df <- conf_df %>%
    dplyr::mutate(total = sum(.data$count_col)) %>%
    dplyr::mutate(pct = .data$count_col / .data$total)

  # Rename dimension columns to 'Predicted' and 'Actual'
  if (ncol(conf_df) >= 2) {
    names(conf_df)[1] <- "Predicted"
    names(conf_df)[2] <- "Actual"
  } else {
    stop("The confusion matrix table does not have at least two dimension columns.", call. = FALSE)
  }

  # Define and apply factor levels for consistent plotting order
  expected_levels <- c("nondiabetic", "diabetic")
  current_pred_levels <- unique(as.character(conf_df$Predicted))
  current_act_levels <- unique(as.character(conf_df$Actual))

  if (!all(current_pred_levels %in% expected_levels)) {
    warning("Unexpected levels in 'Predicted' column: ",
            paste(setdiff(current_pred_levels, expected_levels), collapse=", "))
  }
  if (!all(current_act_levels %in% expected_levels)) {
    warning("Unexpected levels in 'Actual' column: ",
            paste(setdiff(current_act_levels, expected_levels), collapse=", "))
  }

  conf_df$Predicted <- factor(conf_df$Predicted, levels = expected_levels)
  conf_df$Actual <- factor(conf_df$Actual, levels = rev(expected_levels)) # Reversed for typical display

  # Create the heatmap with ggplot2
  p <- ggplot(conf_df, aes(x = .data$Predicted, y = .data$Actual, fill = .data$pct)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_label(aes(label = .data$count_col), vjust = 0.5, hjust = 0.5, size = 8, fill = "white", label.padding = ggplot2::unit(0.15, "lines")) +
    scale_fill_distiller(palette = "YlOrRd", type = "seq", direction = 1, name = "Percentage") +
    theme_minimal(base_size = 12) +
    scale_x_discrete(position = "top") +
    ggplot2::labs(x = "Predicted Label", y = "Actual Label", title = "Confusion Matrix") +
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
  return(p)
}
