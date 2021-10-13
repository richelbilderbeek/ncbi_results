#' Plot the distribution of the percentage TMH that the NCBI proteins are
#' @inheritParams default_params_doc
#' @export
plot_f_tmh_ncbi_per_spanner <- function(
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
    ncbiresults::get_n_unique_protein_names(),
    length(unique(t_results_snps$name))
  )

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  testthat::expect_equal(
    nrow(t_results_tmps),
    ncbiresults::get_n_variations_tmp()
  )

  t_results_tmps$spanner <- NA
  t_results_tmps$spanner[t_results_tmps$n_tmh == 1] <- "single"
  t_results_tmps$spanner[t_results_tmps$n_tmh > 1] <- "multi"
  testthat::expect_equal(0, sum(is.na(t_results_tmps$spanner)))
  t_results_tmps$spanner <- factor(
    t_results_tmps$spanner,
    levels = c("single", "multi")
  )

  t_tmh_per_protein <- dplyr::group_by(t_results_tmps, name, spanner) %>%
    dplyr::summarise(f_tmh = mean(p_in_tmh))

  ggplot2::ggplot(
    t_tmh_per_protein,
    ggplot2::aes(x = f_tmh)
  ) +
  ggplot2::geom_histogram(binwidth = 0.01, fill = "#BBBBBB") +
  ggplot2::facet_grid(
    spanner ~ .,
    space = "free",
    scales = "free_x",
    labeller = ggplot2::as_labeller(
      c(
        single = "Single-spanner",
        multi = "Multi-spanner"
      )
    )
  ) +
  ggplot2::scale_x_continuous(
    "% TMH", labels = scales::percent
  ) +
  ggplot2::scale_y_continuous("Number of proteins") +
  bbbq::get_bbbq_theme()
}
