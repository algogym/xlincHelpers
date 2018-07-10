#' A contrast matrix for a forward difference coding scheme
#'
#' The function returns a contrast matrix for a forward difference coding scheme.
#'
#' @param k An integer indicating the number of factor levels.
#'
#' @return A matrix with the coding scheme
#' @export
#'
#' @examples
#'
#' # Returns a contrast matrix for a 4-level factor
#'
#' contr_forward(4)
#'
contr_forward <- function(k) {
  contrast_matrix <- matrix(nrow = k, ncol = k - 1)

  for (col_index in 1:(k - 1)) {
    contrast_matrix[, col_index] <- c(
      rep((k - col_index) / k, col_index),
      rep(-1 * col_index / k, k - col_index)
    )
  }
  contrast_matrix
}


