test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  plot_f_snps_found_and_expected(folder_name = folder_name) +
    ggplot2::ggsave(
      file.path(folder_name, "fig_f_snps_found_and_expected.png"),
      width = 7,
      height = 7
    )
})
