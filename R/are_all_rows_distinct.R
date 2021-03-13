#' Are all rows distinct
#' @export
are_all_rows_distinct <- function(t) {
  testthat::expect_true(tibble::is_tibble(t))
  nrow(t) == nrow(dplyr::distinct(t))
}
