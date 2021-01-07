test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  skip("Needs profiling")
  profvis::profvis({

    plot_n_tmhs_per_protein(folder_name = folder_name) +
      ggplot2::ggsave(
        file.path(folder_name, "fig_n_tmhs_per_protein.png"),
        width = 7,
        height = 7
      )
  })
})
