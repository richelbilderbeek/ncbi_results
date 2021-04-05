test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  p <- plot_f_snps_found_and_expected_per_spanner(folder_name = folder_name)
  p
  p + ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_per_spanner.png"),
    width = 14,
    height = 7
  )
  p + ggplot2::ggsave(
    file.path(folder_name, "fig_f_snps_found_and_expected_per_spanner.tiff"),
    width = 14,
    height = 7
  )
  p + ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_per_spanner.png"),
    width = 14,
    height = 7
  )
  p + ggplot2::ggsave(
    file.path("~/fig_f_snps_found_and_expected_per_spanner.tiff"),
    width = 14,
    height = 7
  )
})
