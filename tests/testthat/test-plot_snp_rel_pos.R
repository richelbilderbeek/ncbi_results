test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  plot_snp_rel_pos(folder_name = folder_name) +
    ggplot2::ggsave(
      file.path(folder_name, "fig_snp_rel_pos.png"),
      width = 7,
      height = 7
    )
})
