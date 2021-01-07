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
  t_is_in_tmh <- dplyr::filter(t_is_in_tmh_all, !is.na(is_in_tmh))
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
        "Number of SNPs: ", nrow(t), "\n",
        "Number of SNPs in proteins with at least 1 TMH: ",
          nrow(dplyr::filter(t, f_chance > 0.0)), "\n",
        "Solid red line = linear fit on all proteins\n",
        "Solid blue line = linear fit on all proteins with at least 1 TMH\n",
        "Dashed diagonal line = as expected by chance"
      )
    )
}
