utils::globalVariables(c("row"))

#' Plot a mixed-integer linear program
#'
#' @param A the constraint matrix or something that can be coerced to a matrix (e.g. a sparse matrix).
#' @param b the right-hand side vector b.
#' @param cv the objective vector c.
#'
#' @return a ggplot2 object
#'
#' @export
#' @import ggplot2
milp_plot <- function(A, b, cv) {
  A <- as.matrix(A)
  b <- as.numeric(b)
  cv <- as.numeric(cv)

  offset <- 10L

  df_A <- matrix_to_df(A)
  df_A$type <- "A"
  df_A$row <- df_A$row + offset

  df_b <- matrix_to_df(matrix(b, ncol = 1, nrow = ncol(A)))
  df_b$column <- ncol(A) + offset
  df_b$row <- df_b$row + offset
  df_b$type <- "b"

  df_cv <- matrix_to_df(matrix(cv, ncol = ncol(A), nrow = 1))
  df_cv$type <- "c"

  plot_df <- rbind(df_A, df_b, df_cv)
  plot_df <- plot_df[plot_df$coefficient != 0, ]
  ggplot(plot_df) +
    aes_string("column", "row") +
    geom_point(aes_string(color = "coefficient", shape = "type"), size = 2) +
    hrbrthemes::theme_ipsum() +
    scale_color_viridis_c() +
    scale_x_continuous(position = "top") +
    scale_y_continuous(position = "left", trans = "reverse") +
    ggtitle(label = paste0(ncol(A), " variables, ",
                           nrow(A), " constraints, ",
                           sum(A > 0), " non-zeros"))
}

matrix_to_df <- function(mat) {
  df <- as.data.frame(mat)
  df$row <- seq_len(nrow(df))
  df <- tidyr::gather(df,
                key = "column",
                value = "coefficient",
                ... = -row)
  df$column <- gsub(x = df$column, pattern = "V", replacement = "")
  df$column <- as.integer(df$column)
  df$row <- as.integer(df$row)
  df
}
