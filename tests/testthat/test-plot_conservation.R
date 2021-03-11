test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_conservation(folder_name = folder_name)
  p
  p + ggplot2::ggsave(
    file.path(folder_name, "fig_conservation.png"),
    width = 7,
    height = 7
  )
  p + ggplot2::theme_gray(base_size = 24) +
    ggplot2::ggsave(
      file.path("~/fig_conservation.png"),
      width = 7,
      height = 7
    )
})
