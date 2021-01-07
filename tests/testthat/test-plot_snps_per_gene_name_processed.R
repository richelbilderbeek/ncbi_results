test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  plot_snps_per_gene_name_processed(folder_name = folder_name) +
    ggplot2::ggsave(
      file.path(folder_name, "fig_snps_per_gene_name_processed.png"),
      width = 7,
      height = 7
    )
})
