#' Do the expected and observed number of SNPs
#' as a bar chart
#' @inheritParams default_params_doc
#' @export
plot_conservation <- function(
  folder_name
) {
  p_in_tmh <- NULL; rm(p_in_tmh) # nolint, fixes warning: no visible binding for global variable
  variation <- NULL; rm(variation) # nolint, fixes warning: no visible binding for global variable
  conservation <- NULL; rm(conservation) # nolint, fixes warning: no visible binding for global variable
  percentage <- NULL; rm(percentage) # nolint, fixes warning: no visible binding for global variable

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
  testthat::expect_equal(
    nrow(t_results_tmps),
    ncbiresults::get_n_variations_tmp()
  )
  # A SNP can work on multiple isoforms
  testthat::expect_equal(
    ncbiresults::get_n_unique_snp_ids_tmp(),
    length(unique(t_results_tmps$snp_id))
  )

  # Statistics

  n <- nrow(t_results_tmps)
  n_success <- sum(t_results_tmps$is_in_tmh)
  n_success_expected <- sum(t_results_tmps$p_in_tmh)

  t <- tibble::tribble(
    ~conservation, ~n, ~percentage,
    "Chance", n_success_expected, 1,
    "Observed", n_success, n_success / n_success_expected
  )
  t$conservation <- as.factor(t$conservation)
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
    bbbq::get_bbbq_theme()
  p
}
