test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  # Color, transparency
  p <- plot_f_snps_found_and_expected_per_spanner(folder_name = folder_name)
  p
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_per_spanner.png"),
    width = 180, unit = "mm",
    height = 90
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_per_spanner.tiff"),
    width = 180, unit = "mm",
    height = 90
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_per_spanner.png"),
    width = 180, unit = "mm",
    height = 90
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_per_spanner.tiff"),
    width = 180, unit = "mm",
    height = 90
  )
  # Color, no transparency
  p <- plot_f_snps_found_and_expected_per_spanner(
    folder_name = folder_name,
    use_transparency = FALSE
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_per_spanner.eps"),
    width = 180, unit = "mm",
    height = 90
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_per_spanner.eps"),
    width = 180, unit = "mm",
    height = 90
  )


  # Black and white, transparency
  p <- plot_f_snps_found_and_expected_per_spanner(
    folder_name = folder_name,
    use_color = FALSE
  )
  p
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_per_spanner_bw.png"),
    width = 180, unit = "mm",
    height = 90
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_per_spanner_bw.tiff"),
    width = 180, unit = "mm",
    height = 90
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_per_spanner_bw.png"),
    width = 180, unit = "mm",
    height = 90
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_per_spanner_bw.tiff"),
    width = 180, unit = "mm",
    height = 90
  )

  # Black and white, no transparency
  p <- plot_f_snps_found_and_expected_per_spanner(
    folder_name = folder_name,
    use_color = FALSE,
    use_transparency = FALSE
  )
  p ; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_per_spanner_bw.eps"),
    width = 180, unit = "mm",
    height = 90
  )
  p ; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_per_spanner_bw.eps"),
    width = 180, unit = "mm",
    height = 90
  )

})
