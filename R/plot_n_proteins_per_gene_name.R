#' The number of proteins per gene name
#' @inheritParams default_params_doc
#' @export
plot_n_proteins_per_gene_name <- function(folder_name = folder_name) {

  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)
  n_variations <- nrow(t_results)
  testthat::expect_equal(get_n_variations_raw(), n_variations)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    dplyr::filter(t_results, !is.na(p_in_tmh)),
    ncbi::are_snps(variation)
  )
  n_snps <- nrow(t_results_snps)
  testthat::expect_equal(ncbiresults::get_n_variations(), n_snps)
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
  ggplot2::geom_histogram(col = "black", fill = "white", binwidth = 1) +
  ggplot2::scale_x_continuous(
    "Number of proteins with variations/SNPs per gene name"
  ) +
  ggplot2::facet_grid(. ~ type, scales = "free") +
  ggplot2::labs(
    title = "Number of isoforms/SNPs per gene name",
    caption = paste0(
      "Number of genes with SNPs: ", n_gene_names, "\n",
      "Number of proteins with SNPs: ", n_proteins
    )
  )
}
