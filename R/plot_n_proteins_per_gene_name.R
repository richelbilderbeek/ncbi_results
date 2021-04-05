#' The number of proteins per gene name
#' @inheritParams default_params_doc
#' @export
plot_n_proteins_per_gene_name <- function(folder_name = folder_name) {

  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    dplyr::filter(t_results, !is.na(p_in_tmh)),
    ncbi::are_snps(variation)
  )
  testthat::expect_equal(ncbiresults::get_n_variations(), nrow(t_results_snps))
  t_results_snps$name <- stringr::str_match(
    string = t_results_snps$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]
  n_gene_names <- length(unique(t_results_snps$gene_name))
  testthat::expect_equal(911, n_gene_names)
  n_proteins <- length(unique(t_results_snps$name))
  testthat::expect_equal(4780, n_proteins)

  t_proteins_per_gene <- dplyr::summarise(
    dplyr::group_by(
      dplyr::distinct(dplyr::select(t_results_snps, gene_name, name)),
      gene_name
    ),
    n = dplyr::n(), .groups = "keep"
  )
  testthat::expect_equal(911, nrow(t_proteins_per_gene))
  proteins_in_multiple_genes <- 47
  testthat::expect_equal(
    4780 + proteins_in_multiple_genes,
    sum(t_proteins_per_gene$n)
  )

  t_snps_per_gene <- dplyr::summarise(
    dplyr::group_by(
      dplyr::distinct(dplyr::select(t_results_snps, gene_name, variation)),
      gene_name
    ),
    n = dplyr::n(), .groups = "keep"
  )
  testthat::expect_equal(911, nrow(t_snps_per_gene))
  testthat::expect_equal(
    37825,
    sum(t_snps_per_gene$n)
  )

  t_proteins_per_gene$type <- "Isoforms"
  t_snps_per_gene$type <- "SNPs"

  tally <- dplyr::bind_rows(t_proteins_per_gene, t_snps_per_gene)
  tally$type <- as.factor(tally$type)

  ggplot2::ggplot(tally, ggplot2::aes(n)) +
  ggplot2::geom_histogram(fill = "#BBBBBB", binwidth = 1) +
  ggplot2::scale_x_continuous(
    "Number isoforms/SNPs"
  ) +
  ggplot2::scale_y_continuous(
    "Number of gene names"
  ) +
  ggplot2::facet_grid(
    . ~ type,
    scales = "free"
  ) + bbbq::get_bbbq_theme()
}
