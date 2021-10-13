test_that("use", {
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_f_tmh_ncbi(folder_name = folder_name)
  p
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_f_tmh_ncbi.png"),
    width = 180, units = "mm",
    height = 7
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_f_tmh_ncbi.tiff"),
    width = 180, units = "mm",
    height = 7
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_f_tmh_ncbi.eps"),
    width = 180, units = "mm",
    height = 7
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_f_tmh_ncbi.png"),
    width = 180, units = "mm",
    height = 7
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_f_tmh_ncbi.tiff"),
    width = 180, units = "mm",
    height = 7
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_f_tmh_ncbi.eps"),
    width = 180, units = "mm",
    height = 7
  )
})
