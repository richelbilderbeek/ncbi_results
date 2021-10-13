#' Plot the number of SNPs per protein
#' @export
plot_n_snps_per_tmp <- function(
  folder_name
) {
  p_in_tmh <- NULL; rm(p_in_tmh) # nolint, fixes warning: no visible binding for global variable

  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    t_results,
    ncbi::are_snps(variation)
  )
  testthat::expect_equal(ncbiresults::get_n_variations(), nrow(t_results_snps))
  testthat::expect_equal(
    ncbiresults::get_n_unique_snp_ids(),
    length(unique(t_results_snps$snp_id))
  )

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  testthat::expect_equal(
    nrow(t_results_tmps),
    ncbiresults::get_n_variations_tmp()
  )
  testthat::expect_equal(
    ncbiresults::get_n_unique_snp_ids_tmp(),
    length(unique(t_results_tmps$snp_id))
  )
  t_snps_per_tmp <- dplyr::summarise(
    dplyr::group_by(t_results_tmps, name),
    n = dplyr::n()
  )
  testthat::expect_equal(
    ncbiresults::get_n_unique_protein_names_tmp(),
    nrow(t_snps_per_tmp)
  )


  ggplot2::ggplot(t_snps_per_tmp, ggplot2::aes(x = n)) +
    ggplot2::geom_histogram(fill = "#BBBBBB", binwidth = 1) +
    ggplot2::scale_x_continuous("Number of SNPs in TMP") +
    ggplot2::scale_y_continuous("Occurance") +
    bbbq::get_bbbq_theme()
}
