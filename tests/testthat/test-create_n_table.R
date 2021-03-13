test_that("use", {
  t <- create_n_table()
  t
  # save
  readr::write_csv(t, "~/ncbi_counts.csv")
  knitr::kable(
    t,
    format = "latex",
    digits = 0,
    caption = paste0(
      "Amounts. ",
      "raw = all variations, including DNA variations. ",
      "all\\_proteins = all proteins. ",
      "map = MAP = membrane associated protein. ",
      "tmp = TMP = transmembrane protein. ",
      "in\\_tmh = in transmembrane helix (TMH) of TMP. ",
      "in\\_sol = in soluble region of TMP. ",
      "perc\\_tmh = percentage transmembrane helix (TMH). ",
      "The number of unique SNPs and unique gene names ",
      "does not add up for MAP and TMP, ",
      "as one SNP may work on multiple isoforms, some of which can be MAP ",
      "where others can be TMP. ",
      "The number of unique SNPs and unique gene names ",
      "does not add up for TMPs in TMH and TMP in soluble regions, ",
      "as one SNP may work on multiple isoforms, some of which can be MAP ",
      "where others can be TMP, and some SNPs fall in a TMH, where others ",
      "are found in soluble regions"
    ),
    label = "ncbi_counts"
  ) %>% cat(., file = "~/ncbi_counts.tex")
  testthat::expect_true(tibble::is_tibble(t))
})
