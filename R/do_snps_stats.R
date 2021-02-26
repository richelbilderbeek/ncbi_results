#' Do the SNPs stats
#' @export
do_snps_stats <- function(
  folder_name
) {
  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)
  t_results_snps <- dplyr::filter(t_results, !is.na(p_in_tmh))
  t_results_tmps <- dplyr::filter(t_results, p_in_tmh > 0.0)
  HIERO
  sum(is.na(t_results_tmps$is_in_tmh))
  nrow(t_results_tmps) > 8369 + 13369

  testthat::expect_true(length(topo_filenames) > 0)
  is_in_tmh_filenames <- stringr::str_replace(
    string = topo_filenames,
    pattern = "\\.topo$",
    replacement = "_is_in_tmh.csv"
  )
  testthat::expect_true(length(is_in_tmh_filenames) > 0)
  testthat::expect_equal(length(is_in_tmh_filenames), length(topo_filenames))
  t_is_in_tmh_all <- ncbiperegrine::read_is_in_tmh_files(is_in_tmh_filenames)
  n_snps <- sum(!is.na(t_is_in_tmh_all$p_in_tmh))
  testthat::expect_equal(38882, n_snps)
  n_proteins <- length(
    unique(
      stringr::str_match(
        (t_is_in_tmh_all %>%
            dplyr::filter(!is.na(is_in_tmh)) %>%
            dplyr::select(variation))$variation,
        "^(.*):p.*"
      )[, 2]
    )
  )
  testthat::expect_equal(4811, n_proteins)
  t_is_in_tmh <- dplyr::filter(t_is_in_tmh_all, !is.na(is_in_tmh))
  n_tmp <- length(
    unique(
      stringr::str_match(
        (t_is_in_tmh %>%
            dplyr::filter(p_in_tmh > 0.0) %>%
            dplyr::select(variation))$variation,
        "^(.*):p.*"
      )[, 2]
    )
  )
  testthat::expect_equal(2568, n_tmp)
  n_snps_in_tmp <- sum(t_is_in_tmh$p_in_tmh > 0.0)
  testthat::expect_equal(21738, n_snps_in_tmp)
  t_is_in_tmh$name <- stringr::str_match(
    string = t_is_in_tmh$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]

  # Check that p_in_tmh is the same for each protein name
  random_protein_name <- sample(t_is_in_tmh$name, size = 1)
  testthat::expect_equal(
    1,
    length(unique((t_is_in_tmh %>% dplyr::filter(name == random_protein_name))$p_in_tmh))
  )

  t_p_in_tmh <- t_is_in_tmh %>% dplyr::select(name, p_in_tmh) %>% dplyr::distinct()
  testthat::expect_equal(nrow(t_p_in_tmh), 4811)

  mean_f_tmh <- mean(t_p_in_tmh$p_in_tmh)
  mean_f_tmh_tmp <- mean(
    (
      t_p_in_tmh %>%
      dplyr::filter(p_in_tmh > 0.0) %>%
      dplyr::select(p_in_tmh)
    )$p_in_tmh
  )

  n_cytosolic_proteins <- n_proteins - n_tmp

  ggplot2::ggplot(
    t_p_in_tmh %>% dplyr::filter(p_in_tmh > 0.0),
    ggplot2::aes(x = p_in_tmh)
  ) + ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::geom_vline(xintercept = mean_f_tmh, lty = "dashed") +
    ggplot2::geom_vline(xintercept = mean_f_tmh_tmp, lty = "dotted") +
    ggplot2::scale_x_continuous(
      "% TMH", labels = scales::percent
    ) +
    ggplot2::labs(
      title = "Percentage TMH in TMPs",
      caption = paste0(
        "Dashed vertical line: mean % TMH in all ", n_proteins, " proteins: ", format(100.0 * mean_f_tmh, digits = 2, nsmall = 2), "\n",
        "Dotted vertical line: mean % TMH in ", n_tmp, " TMPs: ", format(100.0 * mean_f_tmh_tmp, digits = 2, nsmall = 2), "\n",
        n_cytosolic_proteins, " cytosolic proteins (i.e. % TMH equals zero) not shown"
      )
    )
}
