#' Plot
#' @inheritParams default_params_doc
#' @export
plot_f_snps_found_and_expected <- function(
  folder_name
) {
  is_in_tmh_filenames <- list.files(
    path = folder_name,
    pattern = ".*_is_in_tmh\\.csv$",
    full.names = TRUE
  )
  testthat::expect_true(length(is_in_tmh_filenames) > 0)
  t_is_in_tmh_all <- ncbiperegrine::read_is_in_tmh_files(is_in_tmh_filenames)
  n_snps <- sum(!is.na(t_is_in_tmh_all$p_in_tmh))
  t_is_in_tmh <- dplyr::filter(t_is_in_tmh_all, !is.na(is_in_tmh))
  testthat::expect_equal(
    0,
    sum(t_is_in_tmh$is_in_tmh == TRUE & t_is_in_tmh$p_in_tmh == 0.0)
  )
  n_snps_in_tmp <- sum(t_is_in_tmh$p_in_tmh > 0.0)
  t_is_in_tmh$name <- stringr::str_match(
    string = t_is_in_tmh$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]
  n_snps_in_tmh <- sum(t_is_in_tmh$is_in_tmh == TRUE)
  n_snps_in_soluble <- sum(t_is_in_tmh$is_in_tmh == FALSE)
  testthat::expect_equal(3903, n_snps_in_tmh)
  testthat::expect_equal(34979, n_snps_in_soluble)
  testthat::expect_equal(n_snps, n_snps_in_tmh + n_snps_in_soluble)
  t <- dplyr::summarise(
    dplyr::group_by(
      t_is_in_tmh,
      name
    ),
    f_chance = mean(p_in_tmh),
    f_measured = sum(is_in_tmh) / dplyr::n(),
    .groups = "keep"
  )
  n_proteins <- nrow(t)
  n_tmp <- nrow(dplyr::filter(t, f_chance > 0.0))

  testthat::expect_equal(4811, n_proteins)
  testthat::expect_equal(2568, n_tmp)
  testthat::expect_true(all(t$f_chance >= 0.0 & t$f_chance <= 1.0))
  testthat::expect_true(all(t$f_measured >= 0.0 & t$f_measured <= 1.0))
  # Do not use n_tmp as a factor
  ggplot2::ggplot(
    t, ggplot2::aes(x = f_chance, y = f_measured)
  ) + ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", fullrange = TRUE, color = "red") +
    ggplot2::geom_smooth(data = dplyr::filter(t, f_chance > 0.0), method = "lm", fullrange = TRUE, color = "blue") +
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
        "Solid red line = linear fit on all proteins\n",
        "Solid blue line = linear fit on transmembrane proteins.\n",
        "Dashed diagonal line = as expected by chance"
      )
    )
}
