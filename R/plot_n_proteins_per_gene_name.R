#' The number of proteins per gene name
#' @inheritParams default_params_doc
#' @export
plot_n_proteins_per_gene_name <- function(folder_name = folder_name) {

  variations_csv_filenames <- list.files(
    path = folder_name,
    pattern = ".*_variations\\.csv$",
    full.names = TRUE
  )
  testthat::expect_true(length(variations_csv_filenames) > 0)
  t_variations <- ncbiperegrine::read_variations_csv_files(
    variations_csv_filenames
  )
  t_variations$name <- stringr::str_match(
    string = t_variations$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]
  # Get distinct gene_id, name
  if (1 == 2) {
    library(dplyr)
    t_dinstict <- t_variations %>% dplyr::select(gene_id, name) %>% dplyr::distinct()
    ns <- t_dinstict %>% group_by(gene_id) %>% summarise(n = n(), .groups = "keep")
  }
  t_dinstict <- dplyr::distinct(dplyr::select(t_variations, gene_id, name))
  ns <- dplyr::summarise(
    dplyr::group_by(t_dinstict, gene_id),
    n = dplyr::n(), .groups = "keep"
  )
  ggplot2::ggplot() +
    ggplot2::aes(ns$n) +
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::scale_x_continuous("Number of proteins per gene name") +
  ggplot2::labs(
    title = "Number of proteins per gene name",
    caption = paste0(
      "Number of gene IDs: ", nrow(ns), "\n",
      "Number of proteins: ", sum(ns$n), "\n",
      "Average number of proteins per gene ID: ", mean(ns$n), "\n",
      "Highest number of proteins per gene ID: ", max(ns$n), "\n"
    )
  )
}
