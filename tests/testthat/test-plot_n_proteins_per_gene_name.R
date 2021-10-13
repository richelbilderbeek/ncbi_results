test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_n_proteins_per_gene_name(folder_name = folder_name)
  p
  p ; ggplot2::ggsave(
    file.path("~/fig_n_proteins_per_gene_name.png"),
    width = 180, units = "mm",
    height = 7
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_n_proteins_per_gene_name.tiff"),
    width = 180, units = "mm",
    height = 7
  )
})
