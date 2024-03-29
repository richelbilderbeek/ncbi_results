test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_snps_per_gene_name_processed(folder_name = folder_name)
  p
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_snps_per_gene_name_processed.png"),
    width = 180, units = "mm",
    height = 180
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_snps_per_gene_name_processed.tiff"),
    width = 180, units = "mm",
    height = 180
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_snps_per_gene_name_processed.png"),
    width = 180, units = "mm",
    height = 180
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_snps_per_gene_name_processed.tiff"),
    width = 180, units = "mm",
    height = 180
  )
})
