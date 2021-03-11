#' Do the SNPs stats per spanner
#' @param ppoisbinom_single_plot_filename file to save the poisbinom
#'   for the single-spanners to plot to
#' @param ppoisbinom_multi_plot_filename file to save the poisbinom
#'   for the single-spanners to plot to
#' @export
plot_conservation_per_spanner <- function(
  folder_name
) {
  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)
  n_variations <- nrow(t_results)
  testthat::expect_equal(get_n_variations_raw(), n_variations)

  # Get rid of the non-SNPs
  t_results_snps <- dplyr::filter(
    dplyr::filter(t_results, !is.na(p_in_tmh)),
    ncbi::are_snps(variation)
  )
  n_snps <- nrow(t_results_snps)
  testthat::expect_equal(ncbiresults::get_n_variations(), n_snps)
  # A SNP can work on multiple isoforms
  n_unique_snps <- length(unique(t_results_snps$snp_id))
  testthat::expect_equal(9621, n_unique_snps)

  t_results_tmps <- dplyr::filter(t_results_snps, p_in_tmh > 0.0)
  n_snps_in_tmp <- nrow(t_results_tmps)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp(), n_snps_in_tmp)
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
  testthat::expect_equal(ncbiresults::get_n_unique_protein_names_raw(), length(unique(t_topo_all$name)))
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
  n_snps_in_single_spanners <- sum(t$n_tmh == 1)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_single(), n_snps_in_single_spanners)
  n_snps_in_multi_spanners <- sum(t$n_tmh >= 2)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_multi(), n_snps_in_multi_spanners)

  t_single <- dplyr::filter(t, n_tmh == 1)
  t_multi <- dplyr::filter(t, n_tmh >= 2)
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_single(), nrow(t_single))
  testthat::expect_equal(ncbiresults::get_n_variations_tmp_multi(), nrow(t_multi))

  n_success_single <- sum(t_single$is_in_tmh)
  testthat::expect_equal(454, n_success_single)
  n_success_expected_single <- sum(t_single$p_in_tmh)
  testthat::expect_equal(462.6681, n_success_expected_single, tol = 0.00001)
  n_success_multi <- sum(t_multi$is_in_tmh)
  testthat::expect_equal(3377, n_success_multi)
  n_success_expected_multi <- sum(t_multi$p_in_tmh)
  testthat::expect_equal(3767.26, n_success_expected_multi)
  f_single <- n_success_single / n_success_expected_single
  testthat::expect_equal(0.9812649, f_single)
  f_multi <- n_success_multi / n_success_expected_multi
  testthat::expect_equal(0.8964075, f_multi, tol = 0.00001)

  t <- tibble::tribble(
    ~spanner, ~conservation, ~n, ~percentage,
    "single", "chance", n_success_expected_single, 1.0,
    "single", "observed", n_success_single, f_single,
    "multi", "chance", n_success_expected_multi, 1.0,
    "multi", "observed", n_success_multi, f_multi
  )
  t$spanner <- factor(t$spanner, levels = c("single", "multi"))

  t$conservation <- as.factor(t$conservation)

  facet_labels <- paste0(
    levels(t$spanner), "-spanners: ",
    c(n_snps_in_single_spanners, n_snps_in_multi_spanners), " SNPs"
  )
  names(facet_labels) <- levels(t$spanner)

  p <- ggplot2::ggplot(t, ggplot2::aes(x = conservation, y = n, fill = conservation)) +
    ggplot2::geom_col(col = "black") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(percentage)), vjust = -0.5) +
    ggplot2::scale_y_continuous("Number of SNPs in TMHs") +
    ggplot2::scale_x_discrete("") +
    ggplot2::facet_grid(
      cols = ggplot2::vars(spanner),
      labeller = ggplot2::as_labeller(facet_labels)
    ) +
    ggplot2::labs(
      title = "Evolutionary conservation of SNPs in TMHs",
      caption = paste0(
        "n_variations: ", n_variations, "\n",
        "n_snps: ", n_snps, "\n",
        "n_snps_in_single_spanner_tmh: ", n_success_single, "\n",
        "n_snps_in_multi_spanner_tmh: ", n_success_multi, "\n",
        "E(n_snps_in_single_spanner_tmh): ", format(n_success_expected_single), "\n",
        "E(n_snps_in_multi_spanner_tmh): ", format(n_success_expected_multi), "\n"
      )
    )
  p
}
