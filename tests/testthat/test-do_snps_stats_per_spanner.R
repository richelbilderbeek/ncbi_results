test_that("use", {
  folder_name <- "~/GitHubs/ncbi_peregrine/scripts"
  if (!dir.exists(folder_name)) return()
  statses <- do_snps_stats_per_spanner(
    folder_name = folder_name,
    ppoisbinom_single_plot_filename = "~/ppoisbinom_single.png",
    ppoisbinom_multi_plot_filename = "~/ppoisbinom_multi.png"
  )
  statses
  # Save
  readr::write_csv(statses[[1]], "~/snp_stats_per_spanner_single.csv")
  readr::write_csv(statses[[2]], "~/snp_stats_per_spanner_multi.csv")

  knitr::kable(
    statses[[1]], "latex",
    caption = paste0(
      "Statistics for the single-spanners. ",
      "p = p value. ",
      "n = number of SNPs in single-spanners. ",
      "n\\_success = number of SNPs found in TMHs of single-spanners ",
      "(dashed blue line). ",
      "E(n\\_success) = ",
      "expected number of SNPs to be found in TMHs of single-spanners  ",
      "(dashed red line). "
    ),
    label = "snp_stats_per_spanner_single"
  ) %>% cat(., file = "~/snp_stats_per_spanner_single.tex")

  knitr::kable(
    statses[[2]], "latex",
    caption = paste0(
      "Statistics for the multi-spanners. ",
      "p = p value. ",
      "n = number of SNPs in multi-spanners. ",
      "n\\_success = number of SNPs found in TMHs of multi-spanners ",
      "(dashed blue line). ",
      "E(n\\_success) = ",
      "expected number of SNPs to be found in TMHs of multi-spanners  ",
      "(dashed red line). "
    ),
    label = "snp_stats_per_spanner_multi"
  ) %>% cat(., file = "~/snp_stats_per_spanner_multi.tex")

})
