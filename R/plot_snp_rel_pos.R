#' The relative positions of the SNPs
#' @inheritParams default_params_doc
#' @export
plot_snp_rel_pos <- function(folder_name = folder_name) {
  name <- NULL; rm(name) # nolint, fixes warning: no visible binding for global variable
  variation <- NULL; rm(variation) # nolint, fixes warning: no visible binding for global variable
  pos <- NULL; rm(pos) # nolint, fixes warning: no visible binding for global variable

  variations_csv_filenames <- list.files(
    path = folder_name,
    pattern = ".*_variations\\.csv$",
    full.names = TRUE
  )
  testthat::expect_true(length(variations_csv_filenames) > 0)
  topo_filenames <- stringr::str_replace(
    string = variations_csv_filenames,
    pattern = "_variations\\.csv$",
    replacement = ".topo"
  )
  testthat::expect_true(length(topo_filenames) > 0)
  testthat::expect_equal(
    length(variations_csv_filenames),
    length(topo_filenames)
  )

  # Protein sizes
  tibbles <- list()
  for (i in seq_along(topo_filenames)) {
    t <- dplyr::distinct(
      pureseqtmr::load_fasta_file_as_tibble_cpp(topo_filenames[i])
    )
    t$length <- nchar(t$sequence)
    tibbles[[i]] <- dplyr::select(t, name, length)
  }
  protein_sizes <- dplyr::bind_rows(tibbles)

  # Variations
  t_variations <- ncbiperegrine::read_variations_csv_files(
    variations_csv_filenames
  )
  t_variations$name <- stringr::str_match(
    string = t_variations$variation,
    pattern = "^(.*):p\\..*$"
  )[, 2]

  t_pos <- dplyr::select(t_variations, variation, name)
  t_pos$pos <- as.numeric(
    stringr::str_match(
      string = t_pos$variation,
      pattern = "^.*:p\\.[[:upper:]][[:lower:]]{2}([[:digit:]]{1,4})[[:upper:]][[:lower:]]{2}$" # nolint indeed a long pattern
    )[, 2]
  )
  t_pos <- dplyr::filter(dplyr::select(t_pos, name, pos), !is.na(pos))

  t_rel_pos <- dplyr::inner_join(t_pos, protein_sizes, by = "name")
  t_rel_pos$rel_pos <- t_rel_pos$pos / t_rel_pos$length

  n_bins <- 100

  ggplot2::ggplot() +
    ggplot2::aes(t_rel_pos$rel_pos) +
    ggplot2::geom_histogram(fill = "#BBBBBB", bins = n_bins) +
    ggplot2::geom_hline(yintercept = nrow(t_rel_pos) / n_bins, lty = "dashed") +
    ggplot2::scale_x_continuous("Relative position of SNPs") +
    ggplot2::scale_y_continuous("Number of SNPs") +
    bbbq::get_bbbq_theme()
}
