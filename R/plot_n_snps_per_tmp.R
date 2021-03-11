#' Plot the number of SNPs per protein
#' @export
plot_n_snps_per_tmp <- function(
  folder_name
) {
  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)
  n_variations <- nrow(t_results)
  testthat::expect_equal(get_n_variations_raw(), n_variations)

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

  t_results_tmps$name <- stringr::str_match(
    string = t_results_tmps$variation,
    pattern = "^(.*):p\\..*$"
  )[, 2]

  t_snps_per_tmp <- dplyr::summarise(
    dplyr::group_by(t_results_tmps, name),
    n = dplyr::n()
  )
  n_tmp_with_single_snp <- sum(t_snps_per_tmp$n == 1)
  n_tmps <- nrow(t_snps_per_tmp)
  testthat::expect_equal(2553, n_tmps)
  testthat::expect_equal(n_snps_in_tmp, sum(t_snps_per_tmp$n))
  mean_n_snps_per_tmp <- mean(t_snps_per_tmp$n)

  ggplot2::ggplot(t_snps_per_tmp, ggplot2::aes(x = n)) +
    ggplot2::geom_histogram(col = "black", fill = "white", binwidth = 1) +
    ggplot2::scale_x_continuous("Number of SNPs in TMP") +
    ggplot2::scale_y_continuous("Occurance") +
    ggplot2::geom_vline(xintercept = mean_n_snps_per_tmp, lty = "dashed") +
    ggplot2::labs(
      title = "Number of SNPs per TMP",
      caption = paste0(
        "n_tmps: ", n_tmps, "\n",
        "n_snps: ", n_snps, "\n",
        "n_tmp_with_single_snp: ", n_tmp_with_single_snp, "\n",
        "mean_n_snps_per_tmp: ", mean_n_snps_per_tmp, "\n",
        "n_unique_snps: ", n_unique_snps, "\n",
        "n_snps_in_tmp: ", n_snps_in_tmp, "\n",
        "n_unique_snps_in_tmp: ", n_unique_snps_in_tmp, "\n"
      )
    )
}
