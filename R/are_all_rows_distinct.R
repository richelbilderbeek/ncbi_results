#' Are all rows distinct
#' @param t a tibble
#' @export
are_all_rows_distinct <- function(t) {
  testthat::expect_true(tibble::is_tibble(t))
  nrow(t) == nrow(dplyr::distinct(t))
}
