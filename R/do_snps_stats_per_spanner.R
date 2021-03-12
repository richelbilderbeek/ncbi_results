#' Do the SNPs stats per spanner
#' @param ppoisbinom_single_plot_filename file to save the poisbinom
#'   for the single-spanners to plot to
#' @param ppoisbinom_multi_plot_filename file to save the poisbinom
#'   for the single-spanners to plot to
#' @export
do_snps_stats_per_spanner <- function(
  folder_name,
  ppoisbinom_single_plot_filename = "~/ppoisbinom_single.png",
  ppoisbinom_multi_plot_filename = "~/ppoisbinom_multi.png"
) {
  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)
  n_variations <- nrow(t_results)
  testthat::expect_equal(ncbiresults::get_n_variations_raw(), n_variations)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    dplyr::filter(t_results, !is.na(p_in_tmh)),
    ncbi::are_snps(variation)
  )
  n_snps <- nrow(t_results_snps)
  testthat::expect_equal(ncbiresults::get_n_variations(), n_snps)
  # A SNP can work on multiple isoforms
  n_unique_snps <- length(unique(t_results_snps$snp_id))
  testthat::expect_equal(ncbiresults::get_n_unique_snp_ids(), n_unique_snps)

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  n_snps_in_tmp <- nrow(t_results_tmps)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp(), n_snps_in_tmp)
  n_snps_in_tmh <- sum(t_results_tmps$is_in_tmh)
  n_snps_in_soluble_of_tmp <- sum(!t_results_tmps$is_in_tmh)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_in_tmh(), n_snps_in_tmh)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_in_sol(), n_snps_in_soluble_of_tmp)

  # A SNP can work on multiple isoforms
  n_unique_snps_in_tmp <- length(unique(t_results_tmps$snp_id))
  testthat::expect_equal(ncbiresults::get_n_unique_snp_ids_tmp(), n_unique_snps_in_tmp)


  # Get the number of TMHs
  topo_filenames <- list.files(
    path = folder_name,
    pattern = ".*\\.topo$",
    full.names = TRUE
  )
  testthat::expect_true(length(topo_filenames) > 0)
  tibbles <- list()
  for (i in seq_along(topo_filenames)) {
    topo_filename <- topo_filenames[i]
    t <- pureseqtmr::load_fasta_file_as_tibble(topo_filename)
    t$n_tmh <- pureseqtmr::count_n_tmhs(t$sequence)
    tibbles[[i]] <- dplyr::select(t, name, n_tmh)
  }
  t_topo_all <- dplyr::bind_rows(tibbles)
  testthat::expect_equal(get_n_unique_protein_names_raw(), length(unique(t_topo_all$name)))
  t_topo <- dplyr::distinct(t_topo_all)
  testthat::expect_equal(get_n_unique_protein_names_raw(), nrow(t_topo))

  # Add name to results
  t_results_tmps$name <- stringr::str_match(
    string = t_results_tmps$variation,
    pattern = "^(.*):p\\..*$"
  )[, 2]

  # Merge
  testthat::expect_true(all(t_results_tmps$name %in% t_topo$name))
  t <- dplyr::left_join(t_results_tmps, t_topo, by = "name")
  testthat::expect_equal(0, sum(is.na(t$n_tmh)))
  testthat::expect_equal(0, sum(t$n_tmh == 0))
  testthat::expect_equal(get_n_variations_tmp_single(), sum(t$n_tmh == 1))
  testthat::expect_equal(get_n_variations_tmp_multi(), sum(t$n_tmh >= 2))

  t_single <- dplyr::filter(t, n_tmh == 1)
  t_multi <- dplyr::filter(t, n_tmh >= 2)
  n_snps_in_single_spanners <- nrow(t_single)
  n_snps_in_multi_spanners <- nrow(t_multi)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_single(), n_snps_in_single_spanners)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_multi(), n_snps_in_multi_spanners)
  n_unique_variations_in_single_spanners <- length(unique(t_single$variation))
  n_unique_variations_in_multi_spanners <- length(unique(t_multi$variation))
  testthat::expect_equal(ncbiresults::get_n_unique_variations_tmp_single(), n_unique_variations_in_single_spanners)
  testthat::expect_equal(ncbiresults::get_n_unique_variations_tmp_multi(), n_unique_variations_in_multi_spanners)
  n_unique_snps_in_single_spanners <- length(unique(t_single$snp_id))
  n_unique_snps_in_multi_spanners <- length(unique(t_multi$snp_id))
  testthat::expect_equal(get_n_unique_snps_in_single_spanners(), n_unique_snps_in_single_spanners)
  testthat::expect_equal(get_n_unique_snps_in_multi_spanners(), n_unique_snps_in_multi_spanners)

  # Some SNPs act on both single- and multi-spanners
  n_unique_snps_in_both_spanners <- length(
    unique(
      t_single$snp_id[which(t_single$snp_id %in% t_multi$snp_id)]
    )
  )
  testthat::expect_equal(get_n_unique_snps_in_both_spanners(), n_unique_snps_in_both_spanners)
  testthat::expect_equal(
    n_unique_snps_in_tmp + n_unique_snps_in_both_spanners,
    n_unique_snps_in_single_spanners + n_unique_snps_in_multi_spanners
  )
  n_unique_snps_in_single_spanners_in_tmh <- length(
    unique(
      dplyr::filter(t_single, is_in_tmh == TRUE)$snp_id
    )
  )
  n_unique_snps_in_single_spanners_in_sol <- length(
    unique(
      dplyr::filter(t_single, is_in_tmh == FALSE)$snp_id
    )
  )
  testthat::expect_equal(ncbiresults::get_n_unique_snps_in_single_spanners_in_tmh(), n_unique_snps_in_single_spanners_in_tmh)
  testthat::expect_equal(ncbiresults::get_n_unique_snps_in_single_spanners_in_sol(), n_unique_snps_in_single_spanners_in_sol)
  # Eight SNP IDs are present in both groups
  testthat::expect_equal(
    n_unique_snps_in_single_spanners + 8,
    n_unique_snps_in_single_spanners_in_tmh + n_unique_snps_in_single_spanners_in_sol
  )

  n_unique_snps_in_multi_spanners_in_tmh <- length(
    unique(
      dplyr::filter(t_multi, is_in_tmh == TRUE)$snp_id
    )
  )
  n_unique_snps_in_multi_spanners_in_sol <- length(
    unique(
      dplyr::filter(t_multi, is_in_tmh == FALSE)$snp_id
    )
  )

  testthat::expect_equal(ncbiresults::get_n_unique_snps_in_multi_spanners_in_tmh(), n_unique_snps_in_multi_spanners_in_tmh)
  testthat::expect_equal(ncbiresults::get_n_unique_snps_in_multi_spanners_in_sol(), n_unique_snps_in_multi_spanners_in_sol)
  # There are 40 SNPs in both TMH and soluble regions
  testthat::expect_equal(
    n_unique_snps_in_multi_spanners + 40,
    n_unique_snps_in_multi_spanners_in_tmh + n_unique_snps_in_multi_spanners_in_sol
  )

  n_snps_in_single_spanners_expected <- sum(t_single$p_in_tmh)
  testthat::expect_equal(462.6681, n_snps_in_single_spanners_expected, tol = 0.01)
  n_snps_in_multi_spanners_expected <- sum(t_multi$p_in_tmh)
  testthat::expect_equal(3678.406, n_snps_in_multi_spanners_expected, tol = 0.01)

  # Statistics, single
  statses <- list()
  for (i in seq(1, 2)) {
    if (i == 1) {
      t <- t_single
      ppoisbinom_plot_filename <- ppoisbinom_single_plot_filename
    } else {
      testthat::expect_equal(2, i)
      t <- t_multi
      ppoisbinom_plot_filename <- ppoisbinom_multi_plot_filename
    }

    n <- nrow(t)
    n_success <- sum(t$is_in_tmh)
    n_success_expected <- sum(t$p_in_tmh)
    testthat::expect_equal(
      0.5,
      poisbinom::ppoisbinom(
        q = n_success_expected,
        pp = t$p_in_tmh
      ),
      tolerance = 0.00944
    )
    p <- poisbinom::ppoisbinom(
      q = n_success,
      pp = t$p_in_tmh
    )

    xs <- seq(n_success - 1, ceiling(n_success_expected) + 1)
    ys <- poisbinom::ppoisbinom(
      q = xs,
      pp = t$p_in_tmh
    )
    points <- tibble::tibble(x = xs, y = ys)
    ggplot2::ggplot(points, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 1.0, lty = "dotted") +
      ggplot2::geom_vline(xintercept = n_success, lty = "dashed", col = "blue") +
      ggplot2::geom_vline(xintercept = n_success_expected, lty = "dashed", col = "red") +
      ggplot2::scale_y_log10("Chance the have this or fewer sucesses") +
      ggplot2::scale_x_continuous(
        "Number of successes"
      ) + ggplot2::labs(
        caption = paste0(
          "p(n_success or less): ", format(p, nsmall = 2), "\n",
          "n: ", n, "\n",
          "n_success: ", n_success, "\n",
          "E(n_success): ", format(n_success_expected, nsmall = 2), "\n"
        )
      ) +
      ggplot2::ggsave(ppoisbinom_plot_filename, width = 7, height = 7)


    t_stats <- tibble::tribble(
      ~parameter, ~value,
      "p", format(p),
      "n", format(round(n)),
      "n_success", format(round(n_success)),
      "E(n_success)", format(n_success_expected)
    )
    t_stats
    statses[[i]] <- t_stats
  }
  statses
}
