#' Plot the distribution of the percentage TMH that the NCBI proteins are
#' @inheritParams default_params_doc
#' @export
plot_f_tmh_ncbi <- function(
  folder_name
) {
  p_in_tmh <- NULL; rm(p_in_tmh) # nolint, fixes warning: no visible binding for global variable
  variation <- NULL; rm(variation) # nolint, fixes warning: no visible binding for global variable
  name <- NULL; rm(name) # nolint, fixes warning: no visible binding for global variable
  f_tmh <- NULL; rm(f_tmh) # nolint, fixes warning: no visible binding for global variable

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
  testthat::expect_equal(
    nrow(t_results_tmps),
    ncbiresults::get_n_variations_tmp()
  )

  t_tmh_per_protein <- dplyr::summarise(
    dplyr::group_by(t_results_snps, name),
    f_tmh = mean(p_in_tmh)
  )

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
