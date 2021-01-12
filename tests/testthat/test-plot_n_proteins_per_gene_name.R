test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  plot_n_proteins_per_gene_name(folder_name = folder_name) +
    ggplot2::ggsave(
      file.path(folder_name, "fig_n_proteins_per_gene_name.png"),
      width = 7,
      height = 7
    )
})
