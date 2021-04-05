#' Create a plot that shows the number of SNPs per gene name
#' found in the NCBI database
#' @inheritParams default_params_doc
#' @export
plot_snps_per_gene_name_ncbi <- function(folder_name = folder_name) {

  gene_id <- NULL; rm(gene_id) # nolint, fixes warning: no visible binding for global variable
  snp_id <- NULL; rm(snp_id) # nolint, fixes warning: no visible binding for global variable
  n_snps <- NULL; rm(n_snps) # nolint, fixes warning: no visible binding for global variable

  snps_filenames <- list.files(
    path = folder_name,
    pattern = ".*_snps\\.csv$",
    full.names = TRUE
  )
  testthat::expect_true(length(snps_filenames) > 0)
  n_snps_filenames <- length(unique(snps_filenames))
  testthat::expect_equal(1131, n_snps_filenames)
  t_snps <- ncbiperegrine::read_snps_files(
    snps_filenames = snps_filenames
  )
  testthat::expect_equal(nrow(t_snps), 20799305)
  t <- dplyr::summarise(
    dplyr::group_by(t_snps, gene_id),
    n_snps = dplyr::n()
  )
  testthat::expect_equal(nrow(t), 1077)
  ggplot2::ggplot(
    t,
    ggplot2::aes(x = n_snps)
  ) +
  ggplot2::geom_histogram(fill = "#BBBBBB", bins = 50) +
  ggplot2::scale_x_log10(name = "Number of SNPs") +
  ggplot2::scale_y_continuous(name = "Number of gene names") +
    bbbq::get_bbbq_theme()
}
