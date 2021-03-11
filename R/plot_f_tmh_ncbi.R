#' Plot the distribution of the percentage TMH that the NCBI proteins are
#' @export
plot_f_tmh_ncbi <- function(
  folder_name
) {
  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)
  n_variations <- nrow(t_results)
  testthat::expect_equal(get_n_variations_raw(), n_variations)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    dplyr::filter(t_results, !is.na(p_in_tmh)),
    ncbi::are_snps(variation)
  )
  n_snps <- nrow(t_results_snps)
  testthat::expect_equal(ncbiresults::get_n_variations(), n_snps)
  t_results_snps$name <- stringr::str_match(
    string = t_results_snps$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]
  n_proteins <- length(unique(t_results_snps$name))
  testthat::expect_equal(4780, n_proteins)

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  n_snps_in_tmp <- nrow(t_results_tmps)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp(), n_snps_in_tmp)
  n_tmp <- length(unique(t_results_tmps$name))
  testthat::expect_equal(2553, n_tmp)
  n_map <- n_proteins - n_tmp
  testthat::expect_equal(2227, n_map)


  n_snps_in_tmh <- sum(t_results_tmps$is_in_tmh)
  n_snps_in_soluble <- sum(!t_results_tmps$is_in_tmh)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_in_tmh(), n_snps_in_tmh)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_in_sol(), n_snps_in_soluble)
  testthat::expect_equal(n_snps_in_tmp, n_snps_in_tmh + n_snps_in_soluble)

  t_tmh_per_protein <- dplyr::group_by(t_results_snps, name) %>%
    dplyr::summarise(f_tmh = mean(p_in_tmh))
  mean_f_tmh <- mean(t_tmh_per_protein$f_tmh)
  mean_f_tmh_tmp <- mean(t_tmh_per_protein$f_tmh[t_tmh_per_protein$f_tmh != 0.0])
  testthat::expect_equal(0.09978325, mean_f_tmh)
  testthat::expect_equal(0.1868249, mean_f_tmh_tmp, tol = 0.0001)

  ggplot2::ggplot(
    dplyr::filter(t_tmh_per_protein, f_tmh > 0.0),
    ggplot2::aes(x = f_tmh)
  ) + ggplot2::geom_histogram(col = "black", fill = "white", binwidth = 0.01) +
    ggplot2::geom_vline(xintercept = mean_f_tmh_tmp, lty = "dotted") +
    ggplot2::scale_x_continuous(
      "% TMH", labels = scales::percent
    ) +
    ggplot2::labs(
      title = "Percentage TMH in TMPs",
      caption = paste0(
        "Invisible vertical line: mean % TMH in all ", n_proteins, " proteins: ", format(100.0 * mean_f_tmh, digits = 2, nsmall = 2), "\n",
        "Dotted vertical line: mean % TMH in ", n_tmp, " TMPs: ", format(100.0 * mean_f_tmh_tmp, digits = 2, nsmall = 2), "\n",
        n_map, " MAPs (i.e. % TMH equals zero) not shown"
      )
    )
}
