#' Do the SNPs stats per spanner
#' @param ppoisbinom_single_plot_filename file to save the poisbinom
#'   for the single-spanners to plot to
#' @param ppoisbinom_multi_plot_filename file to save the poisbinom
#'   for the single-spanners to plot to
#' @export
plot_conservation_per_spanner <- function(
  folder_name
) {
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
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_single(), nrow(t_single))
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_multi(), nrow(t_multi))

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
    "single", "chance", n_success_expected_single, 1.0,
    "single", "observed", n_success_single, f_single,
    "multi", "chance", n_success_expected_multi, 1.0,
    "multi", "observed", n_success_multi, f_multi
  )
  t$spanner <- factor(t$spanner, levels = c("single", "multi"))

  t$conservation <- as.factor(t$conservation)

  facet_labels <- paste0(
    levels(t$spanner), "-spanners" #,
    # ": ",
    # c(
    #   ncbiresults::get_n_variations_tmp_single(),
    #   ncbiresults::get_n_variations_tmp_multi()
    # ), " SNPs"
  )
  names(facet_labels) <- levels(t$spanner)

  p <- ggplot2::ggplot(t, ggplot2::aes(x = conservation, y = n)) +
    ggplot2::geom_col(fill = "#BBBBBB") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(percentage)), vjust = -0.5) +
    ggplot2::scale_y_continuous("Number of SNPs in TMHs") +
    ggplot2::scale_x_discrete("") +
    ggplot2::facet_grid(
      cols = ggplot2::vars(spanner),
      labeller = ggplot2::as_labeller(facet_labels)
    ) +
    ggplot2::labs(
      title = "Evolutionary conservation of SNPs in TMHs"
    ) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour="white", fill="#FFFFFF")
  )
  p
}
