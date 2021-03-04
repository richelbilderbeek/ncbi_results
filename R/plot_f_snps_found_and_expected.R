#' Plot
#' @inheritParams default_params_doc
#' @export
plot_f_snps_found_and_expected <- function(
  folder_name
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
  t_results_snps$name <- stringr::str_match(
    string = t_results_snps$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]
  n_proteins <- length(unique(t_results_snps$name))
  testthat::expect_equal(4780, n_proteins)

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  n_snps_in_tmp <- nrow(t_results_tmps)
  testthat::expect_equal(21576, n_snps_in_tmp)

  n_snps_in_tmh <- sum(t_results_tmps$is_in_tmh)
  n_snps_in_soluble <- sum(!t_results_tmps$is_in_tmh)
  testthat::expect_equal(3831, n_snps_in_tmh)
  testthat::expect_equal(17745, n_snps_in_soluble)
  testthat::expect_equal(n_snps_in_tmp, n_snps_in_tmh + n_snps_in_soluble)
  t <- dplyr::summarise(
    dplyr::group_by(
      t_results_tmps,
      name
    ),
    f_chance = mean(p_in_tmh),
    f_measured = sum(is_in_tmh) / dplyr::n(),
    .groups = "keep"
  )

  n_tmp <- nrow(t)
  testthat::expect_equal(2553, n_tmp)
  testthat::expect_true(all(t$f_chance >= 0.0 & t$f_chance <= 1.0))
  testthat::expect_true(all(t$f_measured >= 0.0 & t$f_measured <= 1.0))
  # Do not use n_tmp as a factor
  ggplot2::ggplot(
    t, ggplot2::aes(x = f_chance, y = f_measured)
  ) + ggplot2::geom_point(alpha = 0.25) +
    ggplot2::geom_smooth(method = "lm", fullrange = TRUE, color = "blue") +
    ggplot2::geom_abline(slope = 1.0, lty = "dashed") +
    ggplot2::scale_x_continuous(
      "% TMH", limits = c(0.0, 1.0), labels = scales::percent
    ) +
    ggplot2::scale_y_continuous(
      "% SNPs in TMH", limits = c(0.0, 1.0), labels = scales::percent
    ) +
    ggplot2::labs(
      caption = paste0(
        n_snps, " SNPs in ", n_proteins, " proteins,\n",
        "of which ", n_snps_in_tmp, " SNPs are in ", n_tmp, " TMPs,\n",
        "of which ", n_snps_in_tmh, " SNPs are in TMHs\n",
        "(thus ", n_snps_in_soluble, " SNPs are found in soluble domains).\n",
        "Solid blue line = linear fit.\n",
        "Dashed diagonal line = as expected by chance"
      )
    )
}
