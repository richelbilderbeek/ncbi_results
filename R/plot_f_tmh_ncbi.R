#' Plot the distribution of the percentage TMH that the NCBI proteins are
#' @export
plot_f_tmh_ncbi <- function(
  folder_name
) {
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
    ncbiresults::get_n_unique_protein_names(),
    length(unique(t_results_snps$name))
  )

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  testthat::expect_equal(nrow(t_results_tmps), ncbiresults::get_n_variations_tmp())

  t_tmh_per_protein <- dplyr::group_by(t_results_snps, name) %>%
    dplyr::summarise(f_tmh = mean(p_in_tmh))

  ggplot2::ggplot(
    dplyr::filter(t_tmh_per_protein, f_tmh > 0.0),
    ggplot2::aes(x = f_tmh)
  ) + ggplot2::geom_histogram(fill = "#BBBBBB", binwidth = 0.01) +
  ggplot2::scale_x_continuous(
    "% TMH", labels = scales::percent
  ) +
  ggplot2::scale_y_continuous("Number of proteins") +
  bbbq::get_bbbq_theme()
}
