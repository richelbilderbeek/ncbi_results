#' Do the SNPs stats per spanner
#' @inheritParams default_params_doc
#' @export
plot_conservation_per_spanner <- function(
  folder_name
) {
  n <- NULL; rm(n) # nolint, fixes warning: no visible binding for global variable
  n_tmh <- NULL; rm(n_tmh) # nolint, fixes warning: no visible binding for global variable
  percentage <- NULL; rm(percentage) # nolint, fixes warning: no visible binding for global variable
  spanner <- NULL; rm(spanner) # nolint, fixes warning: no visible binding for global variable
  variation <- NULL; rm(variation) # nolint, fixes warning: no visible binding for global variable


  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)
  testthat::expect_equal(
    nrow(t_results),
    ncbiresults::get_n_variations_raw()
  )
  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    t_results, ncbi::are_snps(variation)
  )
  testthat::expect_equal(
    nrow(t_results_snps),
    ncbiresults::get_n_variations()
  )

  t_single <- dplyr::filter(t_results_snps, n_tmh == 1)
  t_multi <- dplyr::filter(t_results_snps, n_tmh >= 2)
  testthat::expect_equal(
    ncbiresults::get_n_variations_tmp_single(),
    nrow(t_single)
  )
  testthat::expect_equal(
    ncbiresults::get_n_variations_tmp_multi(),
    nrow(t_multi)
  )

  n_success_single <- sum(t_single$is_in_tmh)
  testthat::expect_equal(452, n_success_single)
  testthat::expect_equal(
    length(unique(t_single[t_single$is_in_tmh, ]$snp_id)),
    get_n_unique_snp_ids_single_in_tmh()
  )

  n_success_expected_single <- sum(t_single$p_in_tmh)
  testthat::expect_equal(462.1535, n_success_expected_single, tol = 0.00001)
  n_success_multi <- sum(t_multi$is_in_tmh)
  testthat::expect_equal(3351, n_success_multi)
  testthat::expect_equal(
    length(unique(t_multi[t_multi$is_in_tmh, ]$snp_id)),
    get_n_unique_snp_ids_multi_in_tmh()
  )

  n_success_expected_multi <- sum(t_multi$p_in_tmh)
  testthat::expect_equal(3678.406, n_success_expected_multi, tol = 0.001)
  f_single <- n_success_single / n_success_expected_single
  testthat::expect_equal(0.9780301, f_single)
  f_multi <- n_success_multi / n_success_expected_multi
  testthat::expect_equal(0.9109923, f_multi, tol = 0.00001)

  t <- tibble::tribble(
    ~spanner, ~conservation, ~n, ~percentage,
    "Single", "Chance", n_success_expected_single, 1.0,
    "Single", "Observed", n_success_single, f_single,
    "Multi", "Chance", n_success_expected_multi, 1.0,
    "Multi", "Observed", n_success_multi, f_multi
  )
  t$spanner <- factor(t$spanner, levels = c("Single", "Multi"))

  t$conservation <- as.factor(t$conservation)

  facet_labels <- paste0(
    levels(t$spanner), "-spanners"
  )
  names(facet_labels) <- levels(t$spanner)

  p <- ggplot2::ggplot(t, ggplot2::aes(x = conservation, y = n)) +
    ggplot2::geom_col(fill = "#BBBBBB") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(percentage)),
      vjust = -0.5,
      size = 8
    ) +
    ggplot2::scale_y_continuous(
      "Number of SNPs in TMHs",
      limits = c(0, 5000)
    ) +
    ggplot2::scale_x_discrete("") +
    ggplot2::facet_grid(
      cols = ggplot2::vars(spanner),
      labeller = ggplot2::as_labeller(facet_labels)
    ) + bbbq::get_bbbq_theme()
  p
}
