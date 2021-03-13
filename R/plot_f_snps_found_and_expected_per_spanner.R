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
    dplyr::filter(t_results, !is.na(p_in_tmh)),
    ncbi::are_snps(variation)
  )
  testthat::expect_equal(ncbiresults::get_n_variations(), nrow(t_results_snps))
  t_results_snps$name <- stringr::str_match(
    string = t_results_snps$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]
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

  # Read the topology for the number of TMHs
  topo_filenames <- list.files(
    path = folder_name,
    pattern = ".*\\.topo$",
    full.names = TRUE
  )
  tibbles <- list()
  for (i in seq_along(topo_filenames)) {
    topo_filename <- topo_filenames[i]
    t <- pureseqtmr::load_fasta_file_as_tibble(topo_filename)
    t$n_tmh <- pureseqtmr::count_n_tmhs(t$sequence)
    tibbles[[i]] <- dplyr::select(t, name, n_tmh)
  }
  t_topo <- dplyr::bind_rows(tibbles)

  # Merge
  t_variation_per_spanner <- dplyr::left_join(
    dplyr::select(t_results_tmps, "variation", "name", "is_in_tmh", "p_in_tmh"),
    dplyr::distinct(dplyr::select(t_topo, "name", "n_tmh")),
    by = "name"
  )
  testthat::expect_equal(
    nrow(t_variation_per_spanner),
    ncbiresults::get_n_variations_tmp()
  )
  n_single_spanner_proteins <- nrow(
    t_variation_per_spanner %>%
    dplyr::filter(n_tmh == 1) %>%
    dplyr::select(name) %>%
    dplyr::distinct()
  )
  n_multi_spanner_proteins <- nrow(
    t_variation_per_spanner %>%
    dplyr::filter(n_tmh > 1) %>%
    dplyr::select(name) %>%
    dplyr::distinct()
  )
  testthat::expect_equal(1047, n_single_spanner_proteins)
  testthat::expect_equal(1506, n_multi_spanner_proteins)
  testthat::expect_equal(
    n_proteins,
    n_map + n_single_spanner_proteins + n_multi_spanner_proteins
  )
  testthat::expect_equal(
    n_tmp,
    n_single_spanner_proteins + n_multi_spanner_proteins
  )
  n_snps_single_spanners <- sum(t_variation_per_spanner$n_tmh == 1)
  n_snps_multi_spanners <- sum(t_variation_per_spanner$n_tmh > 1)
  n_snps_cytosolic <- n_snps - n_snps_single_spanners - n_snps_multi_spanners
  testthat::expect_equal(16623, n_snps_cytosolic)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_single(), n_snps_single_spanners)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_multi(), n_snps_multi_spanners)
  testthat::expect_equal(
    n_snps_cytosolic + n_snps_single_spanners + n_snps_multi_spanners,
    n_snps
  )
  testthat::expect_equal(
    n_snps_single_spanners + n_snps_multi_spanners,
    n_snps_in_tmp
  )
  n_snps_in_tmhs <- sum(t_variation_per_spanner$is_in_tmh == TRUE)
  n_snps_in_tmhs_single <- sum(t_variation_per_spanner$is_in_tmh == TRUE & t_variation_per_spanner$n_tmh == 1)
  n_snps_in_tmhs_multi <- sum(t_variation_per_spanner$is_in_tmh == TRUE & t_variation_per_spanner$n_tmh > 1)
  n_snps_in_soluble <- n_snps_in_tmp - n_snps_in_tmhs

  testthat::expect_equal(17405, n_snps_in_soluble)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_in_tmh(), n_snps_in_tmhs)
  testthat::expect_equal(452, n_snps_in_tmhs_single)
  testthat::expect_equal(3351, n_snps_in_tmhs_multi)
  testthat::expect_equal(
    n_snps_in_tmhs,
    n_snps_in_tmhs_single + n_snps_in_tmhs_multi
  )


  t <- dplyr::summarise(
    dplyr::group_by(
      t_variation_per_spanner,
      name
    ),
    f_chance = mean(p_in_tmh),
    f_measured = sum(is_in_tmh) / dplyr::n(),
    n_tmh = mean(n_tmh),
    .groups = "keep"
  )
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
    c(n_single_spanner_proteins, n_multi_spanner_proteins), " proteins\n",
    c(n_snps_single_spanners, n_snps_multi_spanners), " SNPs\n",
    c(n_snps_in_tmhs_single, n_snps_in_tmhs_multi), " SNPs in TMHs"
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
      title = "SNPs expected and found per number of membrane spans",
      caption = paste0(
        n_snps, " SNPs in ", n_proteins, " proteins\n",
        n_snps_in_tmp, " SNPs in ", n_tmp, " TMPs\n",
        n_snps_cytosolic, " SNPs in ", n_map, " soluble proteins.\n",
        n_snps_in_soluble, " SNPs in soluble domains of TMPs\n",
        "Solid red line = linear fit\n",
        "Dashed diagonal line = as expected by chance"
      )
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(spanner),
      labeller = ggplot2::as_labeller(facet_labels)
    )
}
