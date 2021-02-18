#' Plot
#' @export
plot_f_snps_found_and_expected_per_spanner <- function(
  folder_name
) {
  topo_filenames <- list.files(
    path = folder_name,
    pattern = ".*\\.topo$",
    full.names = TRUE
  )
  testthat::expect_true(length(topo_filenames) > 0)
  is_in_tmh_filenames <- stringr::str_replace(
    string = topo_filenames,
    pattern = "\\.topo$",
    replacement = "_is_in_tmh.csv"
  )
  testthat::expect_true(length(is_in_tmh_filenames) > 0)
  testthat::expect_equal(length(is_in_tmh_filenames), length(topo_filenames))
  t_is_in_tmh_all <- ncbiperegrine::read_is_in_tmh_files(is_in_tmh_filenames)
  t_is_in_tmh <- dplyr::filter(t_is_in_tmh_all, !is.na(is_in_tmh))
  t_is_in_tmh$name <- stringr::str_match(
    string = t_is_in_tmh$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]

  tibbles <- list()
  for (i in seq_along(topo_filenames)) {
    topo_filename <- topo_filenames[i]
    t <- dplyr::distinct(
      pureseqtmr::load_fasta_file_as_tibble_cpp(topo_filename)
    )
    t$n_tmh <- pureseqtmr::count_n_tmhs(t$sequence)
    tibbles[[i]] <- t
  }
  t_topo <- dplyr::bind_rows(tibbles)
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
  t <- dplyr::inner_join(t, t_topo %>% dplyr::select(name, n_tmh), by = "name")

  sub_t <- dplyr::filter(t, n_tmh > 0)
  sub_t$spanner <- ""
  sub_t$spanner[ sub_t$n_tmh == 1 ] <- "single"
  sub_t$spanner[ sub_t$n_tmh > 1 ] <- "multi"
  testthat::expect_true(all(nchar(sub_t$spanner) > 3))
  sub_t$spanner <- factor(sub_t$spanner, levels = c("single", "multi"))

  # Facet labels
  n_spanner_levels <- levels(sub_t$spanner)
  testthat::expect_equal(2, length(n_spanner_levels))

  n_proteins_per_level <- dplyr::summarise(
    dplyr::group_by(sub_t, spanner),
    n_proteins = dplyr::n_distinct(name),
    .groups = "keep"
  )$n_proteins
  testthat::expect_equal(length(n_spanner_levels), length(n_proteins_per_level))
  n_snps_per_level <- dplyr::summarise(
    dplyr::group_by(sub_t, spanner),
    n_snps = dplyr::n(),
    .groups = "keep"
  )$n_snps
  testthat::expect_equal(length(n_spanner_levels), length(n_snps_per_level))

  facet_labels <- paste0(
    n_spanner_levels, "-spanner\n",
    n_proteins_per_level, " proteins\n",
    n_snps_per_level, " SNPs\n"
  )
  names(facet_labels) <- levels(sub_t$spanner)

  ggplot2::ggplot(
    sub_t,
    ggplot2::aes(x = f_chance, y = f_measured)
  ) + ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", fullrange = TRUE, color = "red") +
    ggplot2::geom_abline(slope = 1.0, lty = "dashed") +
    ggplot2::scale_x_continuous(
      "% TMH", limits = c(0.0, 1.0), labels = scales::percent
    ) +
    ggplot2::scale_y_continuous(
      "% SNPs in TMH", limits = c(0.0, 1.0), labels = scales::percent
    ) +
    ggplot2::labs(
      title = "SNPs expected and found per number of membrane spans",
      caption = paste0(
        "Number of SNPs: ", nrow(sub_t), "\n",
        "Solid red line = linear fit\n",
        "Dashed diagonal line = as expected by chance"
      )
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(spanner),
      labeller = ggplot2::as_labeller(facet_labels)
    )
}
