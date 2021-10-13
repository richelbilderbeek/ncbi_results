test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()

  # Transparency and color
  p <- plot_f_snps_found_and_expected(folder_name = folder_name)
  p
  p; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected.png"),
    width = 180, units = "mm",
    height = 180
  )
  p; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected.tiff"),
    width = 180, units = "mm",
    height = 180
  )
  p; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected.png"),
    width = 180, units = "mm",
    height = 180
  )
  p; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected.tiff"),
    width = 180, units = "mm",
    height = 180
  )

    # Transparency and color
  p <- plot_f_snps_found_and_expected(
    folder_name = folder_name,
    use_transparency = FALSE
  )
  p; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected.eps"),
    width = 180, units = "mm",
    height = 180
  )
  p; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected.eps"),
    width = 180, units = "mm",
    height = 180
  )


  # No color, transparency
  p <- plot_f_snps_found_and_expected(
    folder_name = folder_name,
    use_color = FALSE
  )
  p
  p;  ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_bw.png"),
    width = 180, units = "mm",
    height = 180
  )
  p; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_bw.tiff"),
    width = 180, units = "mm",
    height = 180
  )
  p; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_bw.png"),
    width = 180, units = "mm",
    height = 180
  )
  p; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_bw.tiff"),
    width = 180, units = "mm",
    height = 180
  )
  # No color, no transparency
  p <- plot_f_snps_found_and_expected(
    folder_name = folder_name,
    use_color = FALSE,
    use_transparency = FALSE
  )
  p; ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_bw.eps"),
    width = 180, units = "mm",
    height = 180
  )
  p; ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_bw.eps"),
    width = 180, units = "mm",
    height = 180
  )

})
