#' The number of SNPs processed
#' @inheritParams default_params_doc
#' @export
plot_snps_per_gene_name_processed <- function(folder_name = folder_name) {

  variations_csv_filenames <- list.files(
    path = folder_name,
    pattern = ".*_variations\\.csv$",
    full.names = TRUE
  )
  testthat::expect_equal(1129, length(variations_csv_filenames))
  # variations_rds_filenames <- list.files(
  #   path = folder_name,
  #   pattern = ".*_variations\\.rds$",
  #   full.names = TRUE
  # )
  # testthat::expect_equal(1129, length(variations_rds_filenames))
  # testthat::expect_equal(
  #   length(variations_csv_filenames),
  #   length(variations_rds_filenames)
  # )

  gene_names <- stringr::str_replace(
    string = basename(variations_rds_filenames),
    pattern = "_variations.rds",
    replacement = ""
  )
  testthat::expect_equal(1129, length(gene_names))
  n_gene_names <- length(gene_names)
  testthat::expect_equal(1129, n_gene_names)

  t_variations <- ncbiperegrine::read_variations_csv_files(variations_csv_filenames)
  unique_gene_ids <- unique(t_variations$gene_id)
  testthat::expect_equal(951, length(unique_gene_ids))
  n_gene_names_with_protein_variations <- length(unique_gene_ids)
  testthat::expect_equal(951, n_gene_names_with_protein_variations)

  # The missing genes have no SNPs that change the protein
  missing_genes <- gene_names[!gene_names %in% unique_gene_ids]
  testthat::expect_equal(178, length(missing_genes))

  tally_protein_variations <- dplyr::group_by(t_variations, gene_id) %>%
    dplyr::summarize(n = dplyr::n())
  n_protein_variations <- sum(tally_protein_variations$n)
  testthat::expect_equal(60683, n_protein_variations)
  tally_protein_variations$type <- "protein variation"

  tally_snps <- dplyr::filter(t_variations, ncbi::are_snps(variation)) %>%
    dplyr::group_by(gene_id) %>%
    dplyr::summarize(n = dplyr::n())
  n_snps <- sum(tally_snps$n)
  testthat::expect_equal(38967, n_snps)
  n_proteins_with_snps <- nrow(tally_snps)
  testthat::expect_equal(912, n_proteins_with_snps)
  tally_snps$type <- "SNP"

  tally <- dplyr::bind_rows(tally_protein_variations, tally_snps)
  tally$type <- as.factor(tally$type)

  ggplot2::ggplot(
    tally,
    ggplot2::aes(x = n)
  ) +
  ggplot2::geom_histogram(bins = 50) +
  ggplot2::scale_x_log10(name = "Number of variations") +
  ggplot2::facet_grid(. ~ type) +
  ggplot2::labs(
    title = "Number of variations per gene name processed",
    caption = paste0(
      "Number of gene names: ", n_gene_names, "\n",
      "Number of gene names with protein variations: ", n_gene_names_with_protein_variations, "\n",
      "Number of gene names with SNPs: ", n_proteins_with_snps, "\n",
      "Total number of protein variations: ", n_protein_variations, "\n",
      "Total number of SNPs: ", n_snps, "\n",
      "Accession date: 2020-12-14"
    )
  )

  # ggthemes::theme_clean(base_size = 22)
  # ggthemes::theme_excel_new(base_size = 24) # Never forget Excel

}
