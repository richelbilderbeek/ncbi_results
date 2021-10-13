test_that("use", {
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_conservation(folder_name = folder_name)
  p
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_conservation.png"),
    width = 180, units = "mm",
    height = 7
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_conservation.tiff"),
    width = 180, units = "mm",
    height = 7
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_conservation.eps"),
    width = 180, units = "mm",
    height = 7
  )
  p ; ggplot2::ggsave(
      file.path("~/fig_conservation.png"),
      width = 6,
      height = 7
    )
  p ; ggplot2::ggsave(
      file.path("~/fig_conservation.tiff"),
      width = 6,
      height = 7
    )
  p ; ggplot2::ggsave(
      file.path("~/fig_conservation.eps"),
      width = 6,
      height = 7
    )
})
