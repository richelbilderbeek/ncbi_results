#' Create a plot that shows the number of SNPs per gene name
#' @inheritParams default_params_doc
#' @export
plot_snps_per_gene_name <- function(folder_name = folder_name) {

  snps_filenames <- list.files(
    path = folder_name,
    pattern = ".*_snps\\.csv$",
    full.names = TRUE
  )
  testthat::expect_true(length(snps_filenames) > 0)
  t_snps <- ncbiperegrine::read_snps_files(snps_filenames = snps_filenames)
  t_snps

  # library(dplyr)
  # t <- t_snps %>%
  #   dplyr::group_by(gene_id) %>%
  #   dplyr::summarise(n_snps = dplyr::n_distinct(snp_id), .groups = "keep")

  gene_id <- NULL; rm(gene_id) # nolint, fixes warning: no visible binding for global variable
  snp_id <- NULL; rm(snp_id) # nolint, fixes warning: no visible binding for global variable

  t <- dplyr::summarise(
    dplyr::group_by(t_snps, gene_id),
    n_snps = dplyr::n_distinct(snp_id),
    .groups = "keep"
  )

  testthat::expect_true(all(t$gene_id %in% t_snps$gene_id))

  n_snps <- NULL; rm(n_snps) # nolint, fixes warning: no visible binding for global variable

  ggplot2::ggplot(
    t,
    ggplot2::aes(x = n_snps)
  ) +
  ggplot2::geom_histogram(bins = 50) +
  ggplot2::scale_x_log10(name = "Number of SNPs") +
  ggplot2::labs(
    title = "Number of SNPs per gene name",
    caption = paste0(
      "Number of gene names: ", nrow(t), "\n",
      "Lowest number of SNPs per gene: ", min(t$n_snps), "\n",
      "Mean number of SNPs per gene: ", mean(t$n_snps), "\n",
      "Highest number of SNPs per gene: ", max(t$n_snps), "\n",
      "Total number of SNPs: ", sum(t$n_snps), "\n"
    )
  ) + ggthemes::theme_clean(base_size = 22)

  # ggthemes::theme_excel_new(base_size = 24) # Never forget Excel

}
