#' Do the SNPs stats
#' @param ppoisbinom_plot_filename file to save the poisbinom
#'   plot to
#' @export
do_snps_stats <- function(
  folder_name,
  ppoisbinom_plot_filename = "~/ppoisbinom.png"
) {
  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)
  n_variations <- nrow(t_results)
  testthat::expect_equal(61705, n_variations)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(t_results, !is.na(p_in_tmh))
  testthat::expect_equal(39431, nrow(t_results_snps))
  t_results_snps <- dplyr::filter(t_results_snps, ncbi::are_snps(variation))
  n_snps <- nrow(t_results_snps)
  testthat::expect_equal(38233, n_snps)
  # A SNP can work on multiple isoforms
  n_unique_snps <- length(unique(t_results_snps$snp_id))
  testthat::expect_equal(9621, n_unique_snps)

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  n_snps_in_tmp <- nrow(t_results_tmps)
  testthat::expect_equal(21576, n_snps_in_tmp)
  # A SNP can work on multiple isoforms
  n_unique_snps_in_tmp <- length(unique(t_results_tmps$snp_id))
  testthat::expect_equal(6026, n_unique_snps_in_tmp)

  # Statistics
  n <- n_snps_in_tmp
  n_success <- sum(t_results_tmps$is_in_tmh)
  n_success_expected <- sum(t_results_tmps$p_in_tmh)
  testthat::expect_equal(
    0.5,
    poisbinom::ppoisbinom(
      q = n_success_expected,
      pp = t_results_tmps$p_in_tmh
    ),
    tolerance = 0.003
  )
  p <- poisbinom::ppoisbinom(
    q = n_success,
    pp = t_results_tmps$p_in_tmh
  )

  xs <- seq(n_success - 100, ceiling(n_success_expected) + 100)
  ys <- poisbinom::ppoisbinom(
    q = xs,
    pp = t_results_tmps$p_in_tmh
  )
  points <- tibble::tibble(x = xs, y = ys)
  ggplot2::ggplot(points, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 1.0, lty = "dotted") +
    ggplot2::geom_vline(xintercept = n_success, lty = "dashed", col = "blue") +
    ggplot2::geom_vline(xintercept = n_success_expected, lty = "dashed", col = "red") +
    ggplot2::scale_y_log10("Chance the have this or fewer sucesses",
      limits = c(10^-13, 1.0),
      n.breaks = 13
    ) +
    ggplot2::scale_x_continuous(
      "Number of successes"
    ) + ggplot2::labs(
      caption = paste0(
        "p(n_success or less): ", format(p, nsmall = 2), "\n",
        "n: ", n, "\n",
        "n_success: ", n_success, "\n",
        "E(n_success): ", format(n_success_expected, nsmall = 2), "\n"
      )
    ) + ggplot2::ggsave(ppoisbinom_plot_filename, width = 7, height = 7)

  t_stats <- tibble::tribble(
    ~parameter, ~value,
    "p", format(p),
    "n", format(round(n)),
    "n_success", format(round(n_success)),
    "E(n_success)", format(n_success_expected)
  )
  t_stats
}
