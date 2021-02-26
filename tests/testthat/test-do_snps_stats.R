test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  # 17 secs with load_fasta_file_as_tibble
  # 2 secs with load_fasta_file_as_tibble_cpp
  t <- do_snps_stats(folder_name = folder_name)
  t
  # save to LaTex
  readr::write_csv(t, "~/fig_f_tmh_ncbi.png")
})
