test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  # 17 secs with load_fasta_file_as_tibble
  # 2 secs with load_fasta_file_as_tibble_cpp
  p <- plot_f_tmh_ncbi(folder_name = folder_name)
  p
  p + ggplot2::ggsave(
    file.path("~/fig_f_tmh_ncbi.png"),
    width = 7,
    height = 7
  )
})
