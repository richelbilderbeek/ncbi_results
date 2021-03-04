#' Plot
#' @export
plot_f_snps_found_and_expected_per_spanner <- function(
  folder_name
) {
  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)
  n_variations <- nrow(t_results)
  testthat::expect_equal(61705, n_variations)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(t_results, !is.na(p_in_tmh))
  testthat::expect_equal(39431, nrow(t_results_snps))
  t_results_snps <- dplyr::filter(t_results_snps, ncbi::are_snps(variation))
  n_snps <- nrow(t_results_snps)
  testthat::expect_equal(38233, n_snps)
  t_results_snps$name <- stringr::str_match(
    string = t_results_snps$variation,
    pattern = "^(.*):p\\..*$"
  )[,2]
  n_proteins <- length(unique(t_results_snps$name))
  testthat::expect_equal(4780, n_proteins)

  n_cytosolic_proteins <- length(unique(t_results_snps$name[t_results_snps$p_in_tmh == 0.0]))
  testthat::expect_equal(2227, n_cytosolic_proteins)

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  n_snps_in_tmp <- nrow(t_results_tmps)
  testthat::expect_equal(21576, n_snps_in_tmp)
  n_tmp <- length(unique(t_results_tmps$name))
  testthat::expect_equal(2553, n_tmp)

  n_snps_in_tmh <- sum(t_results_tmps$is_in_tmh)
  n_snps_in_soluble <- sum(!t_results_tmps$is_in_tmh)
  testthat::expect_equal(3831, n_snps_in_tmh)
  testthat::expect_equal(17745, n_snps_in_soluble)
  testthat::expect_equal(n_snps_in_tmp, n_snps_in_tmh + n_snps_in_soluble)

  # Read the topology for the number of TMHs
  topo_filenames <- list.files(
    path = folder_name,
    pattern = ".*\\.topo$",
    full.names = TRUE
  )
  testthat::expect_true(length(topo_filenames) == 1131)
  tibbles <- list()
  for (i in seq_along(topo_filenames)) {
    topo_filename <- topo_filenames[i]
    t <- pureseqtmr::load_fasta_file_as_tibble(topo_filename)
    t$n_tmh <- pureseqtmr::count_n_tmhs(t$sequence)
    tibbles[[i]] <- dplyr::select(t, name, n_tmh)
  }
  t_topo <- dplyr::bind_rows(tibbles)

  # Merge
  library(dplyr)
  t_variation_per_spanner <- dplyr::left_join(
    t_results_tmps %>% dplyr::select("variation", "name", "is_in_tmh", "p_in_tmh"),
    t_topo %>%  dplyr::select("name", "n_tmh") %>% dplyr::distinct(),
    by = "name"
  )
  testthat::expect_equal(nrow(t_variation_per_spanner), n_snps_in_tmp)
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
  testthat::expect_equal(2227, n_cytosolic_proteins)
  testthat::expect_equal(1047, n_single_spanner_proteins)
  testthat::expect_equal(1506, n_multi_spanner_proteins)
  testthat::expect_equal(
    n_proteins,
    n_cytosolic_proteins + n_single_spanner_proteins + n_multi_spanner_proteins
  )
  testthat::expect_equal(
    n_tmp,
    n_single_spanner_proteins + n_multi_spanner_proteins
  )
  n_snps_single_spanners <- sum(t_variation_per_spanner$n_tmh == 1)
  n_snps_multi_spanners <- sum(t_variation_per_spanner$n_tmh > 1)
  n_snps_cytosolic <- n_snps - n_snps_single_spanners - n_snps_multi_spanners
  testthat::expect_equal(16657, n_snps_cytosolic)
  testthat::expect_equal(8190, n_snps_single_spanners)
  testthat::expect_equal(13386, n_snps_multi_spanners)
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

  testthat::expect_equal(17745, n_snps_in_soluble)
  testthat::expect_equal(3831, n_snps_in_tmhs)
  testthat::expect_equal(454, n_snps_in_tmhs_single)
  testthat::expect_equal(3377, n_snps_in_tmhs_multi)
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

  ggplot2::ggplot(
    sub_t,
    ggplot2::aes(x = f_chance, y = f_measured)
  ) + ggplot2::geom_point(alpha = 0.25) +
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
        n_snps, " SNPs in ", n_proteins, " proteins\n",
        n_snps_in_tmp, " SNPs in ", n_tmp, " TMPs\n",
        n_snps_cytosolic, " SNPs in ", n_cytosolic_proteins, " soluble proteins.\n",
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
