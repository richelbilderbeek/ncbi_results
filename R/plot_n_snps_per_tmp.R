#' Plot the number of SNPs per protein
#' @export
plot_n_snps_per_tmp <- function(
  folder_name
) {
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
  # A SNP can work on multiple isoforms
  testthat::expect_equal(
    ncbiresults::get_n_unique_snp_ids_tmp(),
    length(unique(t_results_tmps$snp_id))
  )

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
