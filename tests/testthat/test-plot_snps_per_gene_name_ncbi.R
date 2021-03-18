test_that("use", {
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_snps_per_gene_name_ncbi(folder_name = folder_name)
  p
  p + ggplot2::ggsave(
    file.path(folder_name, "fig_snps_per_gene_name_ncbi.png"),
    width = 7,
    height = 7
  )
  p + ggplot2::ggsave(
    file.path("~/fig_snps_per_gene_name_ncbi.png"),
    width = 7,
    height = 7
  )
})
