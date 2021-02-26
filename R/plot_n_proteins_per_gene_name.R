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
  n_protein_variations <- nrow(t_variations)
  testthat::expect_equal(60683, n_protein_variations)
  t_variations$name <- stringr::str_match(
    string = t_variations$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]
  t_distict_variations <- dplyr::distinct(dplyr::select(t_variations, gene_id, name))
  tally_variations <- dplyr::summarise(
    dplyr::group_by(t_distict_variations, gene_id),
    n = dplyr::n(), .groups = "keep"
  )
  n_gene_names <- nrow(tally_variations)
  testthat::expect_equal(n_gene_names, 951)
  n_proteins <- sum(tally_variations$n)
  testthat::expect_equal(n_proteins, 5203)
  tally_variations$type <- "protein variation"

  t_distict_snps <- dplyr::filter(t_variations, ncbi::are_snps(variation)) %>%
    dplyr::select(gene_id, name) %>%
    dplyr::distinct()
  tally_snps <- dplyr::summarise(
    dplyr::group_by(t_distict_snps, gene_id),
    n = dplyr::n(), .groups = "keep"
  )
  n_snps_gene_names <- nrow(tally_snps)
  testthat::expect_equal(n_snps_gene_names, 912)
  n_snps_proteins <- sum(tally_snps$n)
  testthat::expect_equal(n_snps_proteins, 4861)
  tally_snps$type <- "SNP"

  tally <- dplyr::bind_rows(tally_variations, tally_snps)
  tally$type <- as.factor(tally$type)

  ggplot2::ggplot(tally, ggplot2::aes(n)) +
  ggplot2::geom_histogram(binwidth = 1) +
  ggplot2::scale_x_continuous("Number of proteins with variations/SNPs per gene name") +
  ggplot2::facet_grid(. ~ type) +
  ggplot2::labs(
    title = "Number of proteins with variations/SNPs per gene name",
    caption = paste0(
      "Number of genes with protein variations: ", n_gene_names, "\n",
      "Number of proteins with protein variations: ", n_proteins, "\n",
      "Number of genes with SNPs: ", n_snps_gene_names, "\n",
      "Number of proteins with SNPs: ", n_snps_proteins, "\n"
    )
  )
}
