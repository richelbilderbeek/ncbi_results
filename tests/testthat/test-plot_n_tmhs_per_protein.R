test_that("use", {
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_n_tmhs_per_protein(folder_name = folder_name)
  p
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_n_tmhs_per_protein.png"),
    width = 180, units = "mm",
    height = 180
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_n_tmhs_per_protein.tiff"),
    width = 180, units = "mm",
    height = 180
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_n_tmhs_per_protein.png"),
    width = 180, units = "mm",
    height = 180
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_n_tmhs_per_protein.tiff"),
    width = 180, units = "mm",
    height = 180
  )
})
