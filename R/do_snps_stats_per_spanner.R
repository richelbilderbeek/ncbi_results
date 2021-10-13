#' Do the SNPs stats per spanner
#' @param ppoisbinom_single_plot_filename file to save the poisbinom
#'   for the single-spanners to plot to
#' @param ppoisbinom_multi_plot_filename file to save the poisbinom
#'   for the single-spanners to plot to
#' @export
do_snps_stats_per_spanner <- function(
  folder_name,
  ppoisbinom_single_plot_filename = "~/ppoisbinom_single.png",
  ppoisbinom_multi_plot_filename = "~/ppoisbinom_multi.png"
) {
  # Raw
  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    dplyr::filter(t_results, !is.na(p_in_tmh)),
    ncbi::are_snps(variation)
  )
  testthat::expect_equal(ncbiresults::get_n_variations(), nrow(t_results_snps))
  # A SNP can work on multiple isoforms
  testthat::expect_equal(
    ncbiresults::get_n_unique_snp_ids(),
    length(unique(t_results_snps$snp_id))
  )

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  testthat::expect_equal(nrow(t_results_tmps), ncbiresults::get_n_variations_tmp())
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_in_tmh(), sum(t_results_tmps$is_in_tmh))
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_in_sol(), sum(!t_results_tmps$is_in_tmh))

  # A SNP can work on multiple isoforms
  testthat::expect_equal(
    ncbiresults::get_n_unique_snp_ids_tmp(),
    length(unique(t_results_tmps$snp_id))
  )

  # Merge
  t <- t_results_tmps
  testthat::expect_equal(0, sum(is.na(t$n_tmh)))
  testthat::expect_equal(0, sum(t$n_tmh == 0))
  testthat::expect_equal(get_n_variations_tmp_single(), sum(t$n_tmh == 1))
  testthat::expect_equal(get_n_variations_tmp_multi(), sum(t$n_tmh >= 2))

  t_single <- dplyr::filter(t, n_tmh == 1)
  t_multi <- dplyr::filter(t, n_tmh >= 2)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_single(), nrow(t_single))
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_multi(), nrow(t_multi))
  testthat::expect_equal(ncbiresults::get_n_unique_variations_tmp_single(), length(unique(t_single$variation)))
  testthat::expect_equal(ncbiresults::get_n_unique_variations_tmp_multi(), length(unique(t_multi$variation)))
  testthat::expect_equal(get_n_unique_snps_in_single_spanners(), length(unique(t_single$snp_id)))
  testthat::expect_equal(get_n_unique_snps_in_multi_spanners(), length(unique(t_multi$snp_id)))
  n_snps_in_single_spanners_expected <- sum(t_single$p_in_tmh)
  testthat::expect_equal(462.6681, n_snps_in_single_spanners_expected, tol = 0.01)
  n_snps_in_multi_spanners_expected <- sum(t_multi$p_in_tmh)
  testthat::expect_equal(3678.406, n_snps_in_multi_spanners_expected, tol = 0.01)

  # Statistics, single
  statses <- list()
  for (i in seq(1, 2)) {
    if (i == 1) {
      t <- t_single
      ppoisbinom_plot_filename <- ppoisbinom_single_plot_filename
    } else {
      testthat::expect_equal(2, i)
      t <- t_multi
      ppoisbinom_plot_filename <- ppoisbinom_multi_plot_filename
    }

    n <- nrow(t)
    n_success <- sum(t$is_in_tmh)
    n_success_expected <- sum(t$p_in_tmh)
    testthat::expect_equal(
      0.5,
      poisbinom::ppoisbinom(
        q = n_success_expected,
        pp = t$p_in_tmh
      ),
      tolerance = 0.00944
    )
    p <- poisbinom::ppoisbinom(
      q = n_success,
      pp = t$p_in_tmh
    )

    xs <- seq(n_success - 1, ceiling(n_success_expected) + 1)
    ys <- poisbinom::ppoisbinom(
      q = xs,
      pp = t$p_in_tmh
    )
    points <- tibble::tibble(x = xs, y = ys)
    ggplot2::ggplot(points, ggplot2::aes(x, y));   ggplot2::geom_point();   ggplot2::geom_hline(yintercept = 1.0, lty = "dotted");   ggplot2::geom_vline(xintercept = n_success, lty = "dashed", col = "blue");   ggplot2::geom_vline(xintercept = n_success_expected, lty = "dashed", col = "red");   ggplot2::scale_y_log10("Chance the have this or fewer sucesses");   ggplot2::scale_x_continuous(
        "Number of successes"
      );   ggplot2::ggsave(ppoisbinom_plot_filename, width = 180, units = "mm", height = 7)


    t_stats <- tibble::tribble(
      ~parameter, ~value,
      "p", format(p),
      "n", format(round(n)),
      "n_success", format(round(n_success)),
      "E(n_success)", format(n_success_expected)
    )
    t_stats
    statses[[i]] <- t_stats
  }
  statses
}
