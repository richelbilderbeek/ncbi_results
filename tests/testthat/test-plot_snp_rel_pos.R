test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_snp_rel_pos(folder_name = folder_name)
  p
  p + ggplot2::ggsave(
    file.path(folder_name, "fig_snp_rel_pos.png"),
    width = 7,
    height = 7
  )
  p + ggplot2::ggsave(
    file.path("~/fig_snp_rel_pos.png"),
    width = 7,
    height = 7
  )
})
