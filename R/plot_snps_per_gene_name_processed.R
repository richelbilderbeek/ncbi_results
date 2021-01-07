#' The number of SNPs processed
#' @inheritParams default_params_doc
#' @export
plot_snps_per_gene_name_processed <- function(folder_name = folder_name) {

  variations_rds_filenames <- list.files(
    path = folder_name,
    pattern = ".*_variations\\.rds$",
    full.names = TRUE
  )
  testthat::expect_true(length(variations_rds_filenames) > 0)

  gene_names <- stringr::str_replace(
    string = basename(variations_rds_filenames),
    pattern = "_variations.rds",
    replacement = ""
  )


  tibbleses <- ncbiperegrine::read_variations_rds_files(variations_rds_filenames)
  n_snps <- purrr::map_int(tibbleses, function(t) length(t))

  t <- tibble::tibble(
    gene_name = gene_names,
    n_snps = n_snps
  )

  # n_snps <- NULL; rm(n_snps) # nolint, fixes warning: no visible binding for global variable

  ggplot2::ggplot(
    t,
    ggplot2::aes(x = n_snps)
  ) +
  ggplot2::geom_histogram(bins = 50) +
  ggplot2::scale_x_log10(name = "Number of SNPs") +
  ggplot2::labs(
    title = "Number of SNPs per gene name processed",
    caption = paste0(
      "Number of gene names: ", nrow(t), "\n",
      "Lowest number of SNPs per gene: ", min(t$n_snps), "\n",
      "Mean number of SNPs per gene: ", mean(t$n_snps), "\n",
      "Highest number of SNPs per gene: ", max(t$n_snps), "\n",
      "Total number of SNPs: ", sum(t$n_snps), "\n",
      "Accession date: 2020-12-14"
    )
  ) + ggthemes::theme_clean(base_size = 22)

  # ggthemes::theme_excel_new(base_size = 24) # Never forget Excel

}
