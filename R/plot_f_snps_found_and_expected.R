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
  n_snps_in_tmp <- sum(t_is_in_tmh$p_in_tmh > 0.0)
  t_is_in_tmh$name <- stringr::str_match(
    string = t_is_in_tmh$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]

  t <- dplyr::summarise(
    dplyr::group_by(
      t_is_in_tmh,
      name
    ),
    f_chance = mean(p_in_tmh),
    f_measured = sum(is_in_tmh) / dplyr::n(),
    .groups = "keep"
  )
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
        n_snps, " SNPs in ", nrow(t), " proteins,\n",
        "of which ", n_snps_in_tmp, " in ",
        nrow(dplyr::filter(t, f_chance > 0.0)),
        " transmembrane proteins.\n",
        "Solid red line = linear fit on all proteins\n",
        "Solid blue line = linear fit on transmembrane proteins.\n",
        "Dashed diagonal line = as expected by chance"
      )
    )
}
