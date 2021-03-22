test_that("use", {
  t <- create_n_table()
  expect_true(tibble::is_tibble(t))
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
      "in\\_sol = in soluble region of TMP. "
    ),
    label = "ncbi_counts"
  ) %>% cat(., file = "~/ncbi_counts.tex")

  knitr::kable(
    dplyr::select(t, what, raw, all_proteins, map, tmp, in_tmh, in_sol),
    format = "latex",
    digits = 0,
    caption = paste0(
      "Amounts. ",
      "raw = all variations, including DNA variations. ",
      "all\\_proteins = all proteins. ",
      "map = membrane associated protein. ",
      "tmp = transmembrane protein. ",
      "in\\_tmh = in transmembrane helix of TMP. ",
      "in\\_sol = in soluble region of TMP. "
    ),
    label = "ncbi_counts_1"
  ) %>% cat(., file = "~/ncbi_counts_1.tex")

  knitr::kable(
    dplyr::select(
      t,
      what, single_in_tmh, single_in_sol, multi_in_tmh, multi_in_sol
    ),
    format = "latex",
    digits = 0,
    caption = paste0(
      "Amounts. ",
      "single\\_in\\_tmh = in transmembrane helix of single-spanner. ",
      "single\\_in\\_sol = in soluble region of single-spanner. ",
      "multi\\_in\\_tmh = in transmembrane helix of multi-spanner. ",
      "multi\\_in\\_sol = in soluble region of multi-spanner. "
    ),
    label = "ncbi_counts_2"
  ) %>% cat(., file = "~/ncbi_counts_2.tex")

})
