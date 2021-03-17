#' Do the expected and observed number of SNPs
#' as a bar chart
#' @param ppoisbinom_plot_filename file to save the poisbinom
#'   plot to
#' @export
plot_conservation <- function(
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

  # Statistics

  n <- nrow(t_results_tmps) # get_n_variations_tmp()
  n_success <- sum(t_results_tmps$is_in_tmh)
  n_success_expected <- sum(t_results_tmps$p_in_tmh)

  t <- tibble::tribble(
    ~conservation, ~n, ~percentage,
    "chance", n_success_expected, 1,
    "observed", n_success, n_success / n_success_expected
  )
  t$conservation <- as.factor(t$conservation)
  p <- ggplot2::ggplot(t, ggplot2::aes(x = conservation, y = n, fill = conservation)) +
    ggplot2::geom_col(col = "black") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(percentage)), vjust = -0.5) +
    ggplot2::scale_y_continuous("Number of SNPs in TMHs") +
    ggplot2::scale_x_discrete("") +
    ggplot2::labs(
      title = "Evolutionary conservation of SNPs in TMHs",
      caption = paste0(
        "n_snps: ", ncbiresults::get_n_variations(), "\n",
        "n_snps_in_tmp: ", ncbiresults::get_n_variations_tmp(), "\n",
        "n_snps_in_tmh: ", n_success, "\n",
        "E(n_snps_in_tmh): ", format(n_success_expected), "\n"
      )
    )
  p
}