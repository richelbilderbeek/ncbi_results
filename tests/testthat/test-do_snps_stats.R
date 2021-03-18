test_that("use", {
  # Local only
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  t <- do_snps_stats(
    folder_name = folder_name,
    ppoisbinom_plot_filename = "~/ppoisbinom.png"
  )
  t
  # save
  readr::write_csv(t, "~/snp_stats.csv")
  knitr::kable(
    t, "latex",
    caption = paste0(
      "Statistics for all TMPs. ",
      "p = p value. ",
      "n = number of SNPs. ",
      "n\\_success = number of SNPs found in TMHs ",
      "(dashed blue line). ",
      "E(n\\_success) = ",
      "expected number of SNPs to be found in TMHs ",
      "(dashed red line). "
    ),
    label = "snp_stats"
  ) %>% cat(., file = "~/snp_stats.tex")
})
