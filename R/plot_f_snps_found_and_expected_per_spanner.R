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
  )[, 2]
  n_snps_in_tmh <- sum(t_is_in_tmh$is_in_tmh == TRUE)
  n_snps_in_soluble <- sum(t_is_in_tmh$is_in_tmh == FALSE)
  testthat::expect_equal(3903, n_snps_in_tmh)
  testthat::expect_equal(34979, n_snps_in_soluble)

  tibbles <- list()
  for (i in seq_along(topo_filenames)) {
    topo_filename <- topo_filenames[i]
    t <- pureseqtmr::load_fasta_file_as_tibble(topo_filename)
    t$n_tmh <- pureseqtmr::count_n_tmhs(t$sequence)
    tibbles[[i]] <- dplyr::select(t, name, n_tmh)
  }
  t_topo <- dplyr::bind_rows(tibbles)

  testthat::expect_equal(nrow(t_is_in_tmh), n_snps)
  testthat::expect_true( # More, due to proteins that had only frame shifts?
    length(unique(t_topo$name)) > length(unique(t_is_in_tmh$name))
  )
  testthat::expect_equal(5156, length(unique(t_topo$name))) # More, due to proteins that had only frame shifts?
  testthat::expect_true(all(t_is_in_tmh$name %in% t_topo$name)) #

  t_variation_per_spanner <- dplyr::left_join(
    t_is_in_tmh %>% dplyr::select("variation", "name", "is_in_tmh"),
    t_topo %>%  dplyr::select("name", "n_tmh") %>% dplyr::distinct(),
    by = "name"
  )
  testthat::expect_equal(nrow(t_is_in_tmh), nrow(t_variation_per_spanner))
  testthat::expect_equal(nrow(t_variation_per_spanner), n_snps)
  n_cytosolic_proteins <- nrow(
    t_variation_per_spanner %>%
    dplyr::filter(n_tmh == 0) %>%
    dplyr::select(name) %>%
    dplyr::distinct()
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
  testthat::expect_equal(2243, n_cytosolic_proteins)
  testthat::expect_equal(1044, n_single_spanner_proteins)
  testthat::expect_equal(1524, n_multi_spanner_proteins)
  testthat::expect_equal(
    n_proteins,
    n_cytosolic_proteins + n_single_spanner_proteins + n_multi_spanner_proteins
  )
  testthat::expect_equal(
    n_tmp,
    n_single_spanner_proteins + n_multi_spanner_proteins
  )
  n_snps_cytosolic <- sum(t_variation_per_spanner$n_tmh == 0)
  n_snps_single_spanners <- sum(t_variation_per_spanner$n_tmh == 1)
  n_snps_multi_spanners <- sum(t_variation_per_spanner$n_tmh > 1)
  testthat::expect_equal(17144, n_snps_cytosolic)
  testthat::expect_equal(8369, n_snps_single_spanners)
  testthat::expect_equal(13369, n_snps_multi_spanners)
  testthat::expect_equal(
    n_snps_cytosolic + n_snps_single_spanners + n_snps_multi_spanners,
    n_snps
  )
  testthat::expect_equal(
    n_snps_single_spanners + n_snps_multi_spanners,
    n_snps_in_tmp
  )
  n_snps_in_soluble <- sum(t_variation_per_spanner$is_in_tmh == FALSE)
  n_snps_in_tmhs <- sum(t_variation_per_spanner$is_in_tmh == TRUE)
  n_snps_in_tmhs_single <- sum(t_variation_per_spanner$is_in_tmh == TRUE & t_variation_per_spanner$n_tmh == 1)
  n_snps_in_tmhs_multi <- sum(t_variation_per_spanner$is_in_tmh == TRUE & t_variation_per_spanner$n_tmh > 1)
  testthat::expect_equal(34979, n_snps_in_soluble)
  testthat::expect_equal(3903, n_snps_in_tmhs)
  testthat::expect_equal(467, n_snps_in_tmhs_single)
  testthat::expect_equal(3436, n_snps_in_tmhs_multi)
  testthat::expect_equal(
    n_snps_in_tmhs,
    n_snps_in_tmhs_single + n_snps_in_tmhs_multi
  )


  t <- dplyr::summarise(
    dplyr::group_by(
      t_is_in_tmh,
      name
    ),
    f_chance = mean(p_in_tmh),
    f_measured = sum(is_in_tmh) / dplyr::n(),
    .groups = "keep"
  )
  t
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
        n_snps, " SNPs in ", n_proteins, " proteins\n",
        n_snps_in_tmp, " SNPs in ", n_tmp, " TMPs\n",
        n_snps_cytosolic, " SNPs in ", n_cytosolic_proteins, " soluble proteins.\n",
        n_snps_in_tmhs, "/", n_snps, " SNPs in TMHs\n",
        n_snps_in_soluble, "/", n_snps, " SNPs in soluble domains\n",
        "Solid red line = linear fit\n",
        "Dashed diagonal line = as expected by chance"
      )
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(spanner),
      labeller = ggplot2::as_labeller(facet_labels)
    )
}
