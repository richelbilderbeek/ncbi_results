#' The number of SNPs processed
#' @inheritParams default_params_doc
#' @export
plot_snps_per_gene_name_processed <- function(folder_name = folder_name) {

  results_filename <- file.path(folder_name, "results.csv")
  testthat::expect_true(file.exists(results_filename))
  t_results <- ncbiperegrine::read_results_file(results_filename)

  if (1 == 2) {
    variations_csv_filenames <- list.files(
      path = folder_name,
      pattern = ".*_variations\\.csv$",
      full.names = TRUE
    )

    testthat::expect_equal(1131, length(variations_csv_filenames))
    gene_names <- stringr::str_replace(
      string = basename(variations_csv_filenames),
      pattern = "_variations.csv",
      replacement = ""
    )
    testthat::expect_equal(1131, length(gene_names))
    n_gene_names <- length(gene_names)
    testthat::expect_equal(1131, n_gene_names)

    t_variations <- ncbiperegrine::read_variations_csv_files(variations_csv_filenames)
    testthat::expect_equal(60683, nrow(t_variations))
    unique_gene_ids <- unique(t_variations$gene_id)
    testthat::expect_equal(951, length(unique_gene_ids))
    n_gene_names_with_protein_variations <- length(unique_gene_ids)
    testthat::expect_equal(951, n_gene_names_with_protein_variations)

    # The missing genes have no SNPs that change the protein
    missing_genes <- gene_names[!gene_names %in% unique_gene_ids]
    testthat::expect_equal(178, length(missing_genes))
  }

  tally_protein_variations <- dplyr::group_by(t_results, gene_id) %>%
    dplyr::summarize(n = dplyr::n())
  n_protein_variations <- sum(tally_protein_variations$n)
  testthat::expect_equal(
    n_protein_variations,
    ncbiresults::get_n_variations_raw()
  )
  tally_protein_variations$type <- "protein variation"

  t_snps <- dplyr::filter(t_results, ncbi::are_snps(variation))
  testthat::expect_equal(
    nrow(t_snps),
    ncbiresults::get_n_variations()
  )
  tally_snps <- dplyr::summarize(
    dplyr::group_by(t_snps, gene_id),
    n = dplyr::n()
  )
  n_snps <- sum(tally_snps$n)
  testthat::expect_equal(
    ncbiresults::get_n_variations(),
    n_snps
  )
  n_proteins_with_snps <- nrow(tally_snps)
  testthat::expect_equal(
    ncbiresults::get_n_unique_gene_ids(),
    n_proteins_with_snps
  )
  tally_snps$type <- "SNP"


  tally <- dplyr::bind_rows(tally_protein_variations, tally_snps)
  tally$type <- as.factor(tally$type)

  ggplot2::ggplot(
    tally,
    ggplot2::aes(x = n)
  ) +
  ggplot2::geom_histogram(fill = "#BBBBBB", bins = 50) +
  ggplot2::scale_x_log10(name = "Number of variations/SNPs") +
  ggplot2::scale_y_continuous(name = "Number of gene names") +
  ggplot2::facet_grid(. ~ type) + bbbq::get_bbbq_theme()
}
