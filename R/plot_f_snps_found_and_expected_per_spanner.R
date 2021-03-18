#' Plot
#' @export
plot_f_snps_found_and_expected_per_spanner <- function(
  folder_name
) {
  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    t_results,
    ncbi::are_snps(variation)
  )
  testthat::expect_equal(ncbiresults::get_n_variations(), nrow(t_results_snps))
  testthat::expect_equal(
    ncbiresults::get_n_unique_protein_names(),
    length(unique(t_results_snps$name))
  )
  testthat::expect_equal(
    ncbiresults::get_n_unique_protein_names_map(),
    length(unique(t_results_snps$name[t_results_snps$p_in_tmh == 0.0]))
  )
  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  testthat::expect_equal(
    nrow(t_results_tmps),
    ncbiresults::get_n_variations_tmp()
  )
  testthat::expect_equal(
    ncbiresults::get_n_unique_protein_names_tmp(),
    length(unique(t_results_tmps$name))
  )
  testthat::expect_equal(
    ncbiresults::get_n_variations_tmp_in_tmh(),
    sum(t_results_tmps$is_in_tmh)
  )
  testthat::expect_equal(
    ncbiresults::get_n_variations_tmp_in_sol(),
    sum(!t_results_tmps$is_in_tmh)
  )
  # Merge
  t_single <- dplyr::filter(t_results_tmps, n_tmh == 1)
  t_multi <- dplyr::filter(t_results_tmps, n_tmh >= 2)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_single(), nrow(t_single))
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_multi(), nrow(t_multi))

  t <- dplyr::summarise(
    dplyr::group_by(
      dplyr::bind_rows(t_single, t_multi),
      name
    ),
    f_chance = mean(p_in_tmh),
    f_measured = sum(is_in_tmh) / dplyr::n(),
    n_tmh = mean(n_tmh),
    .groups = "keep"
  )
  t
  testthat::expect_true(all(t$f_chance >= 0.0 & t$f_chance <= 1.0))
  testthat::expect_true(all(t$f_measured >= 0.0 & t$f_measured <= 1.0))
  sub_t <- dplyr::filter(t, n_tmh > 0)
  sub_t$spanner <- ""
  sub_t$spanner[ sub_t$n_tmh == 1 ] <- "single"
  sub_t$spanner[ sub_t$n_tmh > 1 ] <- "multi"
  testthat::expect_true(all(nchar(sub_t$spanner) > 3))
  sub_t$spanner <- factor(sub_t$spanner, levels = c("single", "multi"))

  # Facet labels
  n_spanner_levels <- levels(sub_t$spanner)
  testthat::expect_equal(2, length(n_spanner_levels))

  facet_labels <- paste0(
    n_spanner_levels, "-spanner\n",
    c(
      nrow(t_single),
      nrow(t_multi)
    ), " variations\n"
  )
  names(facet_labels) <- levels(sub_t$spanner)

  testthat::expect_equal(0, sum(is.na(sub_t$f_chance)))
  testthat::expect_equal(0, sum(is.na(sub_t$f_measured)))
  testthat::expect_equal(0, sum(is.infinite(sub_t$f_chance)))
  testthat::expect_equal(0, sum(is.infinite(sub_t$f_measured)))
  testthat::expect_equal(0, sum(sub_t$f_chance < 0.0))
  testthat::expect_equal(0, sum(sub_t$f_measured < 0.0))
  testthat::expect_equal(0, sum(sub_t$f_chance > 1.0))
  testthat::expect_equal(0, sum(sub_t$f_measured > 1.0))
  ggplot2::ggplot(
    sub_t,
    ggplot2::aes(x = f_chance, y = f_measured)
  ) + ggplot2::geom_point(alpha = 0.25) +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      fullrange = TRUE,
      color = "red",
      na.rm = TRUE # 1 value is missing
    ) +
    ggplot2::geom_abline(slope = 1.0, lty = "dashed") +
    ggplot2::scale_x_continuous(
      "% TMH", limits = c(0.0, 1.0), labels = scales::percent
    ) +
    ggplot2::scale_y_continuous(
      "% SNPs in TMH", limits = c(0.0, 1.0), labels = scales::percent
    ) +
    ggplot2::labs(
      title = "SNPs expected and found per number of membrane spans"
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(spanner),
      labeller = ggplot2::as_labeller(facet_labels)
    )
}
