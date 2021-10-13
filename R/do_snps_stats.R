#' Do the SNPs stats
#' @inheritParams default_params_doc
#' @export
do_snps_stats <- function(
  folder_name,
  ppoisbinom_plot_filename = "~/ppoisbinom.png"
) {
  p_in_tmh <- NULL; rm(p_in_tmh) # nolint, fixes warning: no visible binding for global variable
  x <- NULL; rm(x) # nolint, fixes warning: no visible binding for global variable
  y <- NULL; rm(y) # nolint, fixes warning: no visible binding for global variable

  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    dplyr::filter(t_results, !is.na(p_in_tmh)),
    ncbi::are_snps(variation)
  )
  testthat::expect_equal(
    nrow(t_results_snps),
    nrow(dplyr::distinct(t_results_snps))
  )
  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  testthat::expect_true(ncbiresults::are_all_rows_distinct(t_results_tmps))

  testthat::expect_equal(nrow(t_results_tmps), ncbiresults::get_n_variations_tmp())
  # A SNP can work on multiple isoforms
  testthat::expect_equal(
    ncbiresults::get_n_unique_snp_ids_tmp(),
    length(unique(t_results_tmps$snp_id))
  )

  # Statistics
  n <- nrow(t_results_tmps)
  testthat::expect_equal(n, ncbiresults::get_n_variations_tmp())
  n_success <- sum(t_results_tmps$is_in_tmh)
  testthat::expect_equal(n_success, ncbiresults::get_n_variations_tmp_in_tmh())
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
  ggplot2::ggplot(points, ggplot2::aes(x, y))
  ggplot2::geom_point()
  ggplot2::geom_hline(yintercept = 1.0, lty = "dotted")
  ggplot2::geom_vline(xintercept = n_success, lty = "dashed", col = "blue")
  ggplot2::geom_vline(
    xintercept = n_success_expected, lty = "dashed", col = "red"
  )
  ggplot2::scale_y_log10("Chance the have this or fewer sucesses",
      limits = c(10^-13, 1.0),
      n.breaks = 13
    ); ggplot2::scale_x_continuous(
      "Number of successes"
    ) ; ggplot2::ggsave(
      ppoisbinom_plot_filename,
      width = 180, units = "mm",
      height = 180
    )

  t_stats <- tibble::tribble(
    ~parameter, ~value,
    "p", format(p),
    "n", format(round(n)),
    "n_success", format(round(n_success)),
    "E(n_success)", format(n_success_expected)
  )
  t_stats
}
