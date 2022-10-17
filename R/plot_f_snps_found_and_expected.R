#' Plot
#' @inheritParams default_params_doc
#' @export
plot_f_snps_found_and_expected <- function(
  folder_name,
  use_color = TRUE,
  use_transparency = TRUE
) {
  p_in_tmh <- NULL; rm(p_in_tmh) # nolint, fixes warning: no visible binding for global variable
  variation <- NULL; rm(variation) # nolint, fixes warning: no visible binding for global variable
  name <- NULL; rm(name) # nolint, fixes warning: no visible binding for global variable
  is_in_tmh <- NULL; rm(is_in_tmh) # nolint, fixes warning: no visible binding for global variable
  f_chance <- NULL; rm(f_chance) # nolint, fixes warning: no visible binding for global variable
  f_measured <- NULL; rm(f_measured) # nolint, fixes warning: no visible binding for global variable

  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    dplyr::filter(t_results, !is.na(p_in_tmh)),
    ncbi::are_snps(variation)
  )
  testthat::expect_equal(ncbiresults::get_n_variations(), nrow(t_results_snps))
  t_results_snps$name <- stringr::str_match(
    string = t_results_snps$variation,
    pattern = "^(.*):p\\..*$"
  )[, 2]

  testthat::expect_equal(
    ncbiresults::get_n_unique_protein_names(),
    length(unique(t_results_snps$name))
  )

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  testthat::expect_equal(
    nrow(t_results_tmps),
    ncbiresults::get_n_variations_tmp()
  )
  testthat::expect_equal(
    ncbiresults::get_n_variations_tmp_in_tmh(),
    sum(t_results_tmps$is_in_tmh)
  )
  testthat::expect_equal(
    ncbiresults::get_n_variations_tmp_in_sol(),
    sum(!t_results_tmps$is_in_tmh)
  )
  t <- dplyr::summarise(
    dplyr::group_by(
      t_results_tmps,
      name
    ),
    f_chance = mean(p_in_tmh),
    f_measured = sum(is_in_tmh) / dplyr::n(),
    .groups = "keep"
  )

  testthat::expect_equal(
    ncbiresults::get_n_unique_protein_names_tmp(),
    nrow(t)
  )
  testthat::expect_true(all(t$f_chance >= 0.0 & t$f_chance <= 1.0))
  testthat::expect_true(all(t$f_measured >= 0.0 & t$f_measured <= 1.0))

  trendline_color <- NA
  if (use_color) {
    trendline_color <- "red"
    trendline_lty <- "solid"
  } else {
    trendline_color <- "black"
    trendline_lty <- "solid"
  }

  # Do not use n_tmp as a factor
  p <- ggplot2::ggplot(
    t, ggplot2::aes(x = f_chance, y = f_measured)
  )
  if (use_transparency) {
    p <- p + ggplot2::geom_point(alpha = 0.25)
  } else {
    p <- p + ggplot2::geom_point()
  }
  p + ggplot2::geom_smooth(
    method = "lm",
    fullrange = TRUE,
    color = trendline_color,
    lty = trendline_lty
  ) +
  ggplot2::geom_abline(slope = 1.0, lty = "dashed") +
  ggplot2::scale_x_continuous(
    "% TMH", limits = c(0.0, 1.0), labels = scales::percent
  ) +
  ggplot2::scale_y_continuous(
    "% SNPs in TMH", limits = c(0.0, 1.0), labels = scales::percent
  ) + bbbq::get_bbbq_theme()
}
