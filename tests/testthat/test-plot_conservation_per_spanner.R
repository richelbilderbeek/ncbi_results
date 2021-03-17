test_that("use", {
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_conservation_per_spanner(folder_name = folder_name)
  p
  p + ggplot2::ggsave(
    file.path(folder_name, "fig_conservation_per_spanner.png"),
    width = 7,
    height = 7
  )
  p + ggplot2::ggsave(
    file.path("~/fig_conservation_per_spanner.png"),
    width = 7,
    height = 7
  )
  p + ggplot2::theme_gray(base_size = 16) +
     ggplot2::labs(title = "",
      caption = ""
    ) +
    ggplot2::ggsave(
      file.path("~/fig_conservation_per_spanner_presentation.png"),
      width = 7,
      height = 7
    )
})
