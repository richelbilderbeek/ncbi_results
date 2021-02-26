#' Create a plot that shows the number of SNPs per gene name
#' found in the NCBI database
#' @inheritParams default_params_doc
#' @export
plot_snps_per_gene_name_ncbi <- function(folder_name = folder_name) {

  gene_id <- NULL; rm(gene_id) # nolint, fixes warning: no visible binding for global variable
  snp_id <- NULL; rm(snp_id) # nolint, fixes warning: no visible binding for global variable

  snps_filenames <- list.files(
    path = folder_name,
    pattern = ".*_snps\\.csv$",
    full.names = TRUE
  )
  testthat::expect_true(length(snps_filenames) > 0)
  n_snps_filenames <- length(unique(snps_filenames))
  testthat::expect_equal(1129, n_snps_filenames)
  gene_ids <- as.numeric(
    basename(
      stringr::str_replace(
        string = snps_filenames,
        pattern = "_snps.csv",
        replacement = ""
      )
    )
  )
  testthat::expect_equal(1129, length(gene_ids))

  t_snps <- ncbiperegrine::read_snps_files(snps_filenames = snps_filenames)

  unique_gene_ids <- unique(t_snps$gene_id)
  n_unique_gene_ids <- length(unique_gene_ids)
  testthat::expect_equal(1075, n_unique_gene_ids)

  missing_gene_ids <- gene_ids[which(!gene_ids %in% unique_gene_ids)]
  n_missing_gene_ids <- length(missing_gene_ids)
  testthat::expect_equal(54, n_missing_gene_ids)
  testthat::expect_equal(
    n_snps_filenames,
    n_unique_gene_ids + n_missing_gene_ids
  )

  n_snps_in_ncbi <- nrow(t_snps)
  testthat::expect_equal(20787968, n_snps_in_ncbi)

  t_snps


  t <- dplyr::summarise(
    dplyr::group_by(t_snps, gene_id),
    n_snps = dplyr::n()
  )
  testthat::expect_equal(20787968, sum(t$n_snps))
  nrow(t)

  t <- dplyr::summarise(
    dplyr::group_by(t_snps, gene_id),
    n_snps = dplyr::n_distinct(snp_id),
    .groups = "keep"
  )


  n_unique_gene_ids <- length(unique(t_snps$gene_id))
  testthat::expect_equal(1075, n_unique_gene_ids)
  testthat::expect_equal(n_snps_in_ncbi, sum(t$n_snps))
  testthat::expect_equal(20787968, sum(t$n_snps))
  n_unique_gene_ids <- length(t$gene_id)
  testthat::expect_equal(1075, n_unique_gene_ids)

  # for (random_gene_id in unique(t_snps$gene_id)) {
  #   message(random_gene_id)
  #   is_interesting <- !is_mis(which(t_snps$gene_id == random_gene_id))
  #   if (is_interesting) stop()
  # }


  testthat::expect_true(all(t$gene_id %in% t_snps$gene_id))

  n_snps <- NULL; rm(n_snps) # nolint, fixes warning: no visible binding for global variable

  ggplot2::ggplot(
    t,
    ggplot2::aes(x = n_snps)
  ) +
  ggplot2::geom_histogram(bins = 50) +
  ggplot2::scale_x_log10(name = "Number of SNPs") +
  ggplot2::labs(
    title = "Number of SNPs per gene name at NCBI",
    caption = paste0(
      "Number of gene names: ", n_snps_filenames, "\n",
      "Number of gene names with SNPs: ", n_unique_gene_ids, "\n",
      "Number of gene names without SNPs: ", n_missing_gene_ids, "\n",
      "Total number of SNPs in dbSNP: ", n_snps_in_ncbi, "\n",
      "Accession date: 2020-12-14"
    )
  )

  # ggthemes::theme_clean(base_size = 22)
  # ggthemes::theme_excel_new(base_size = 24) # Never forget Excel

}
