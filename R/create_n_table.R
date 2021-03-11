#' Create a table with all numbers
#' @export
create_n_table <- function() {
  t <- tibble::tibble(
    what = c(
      "Number of variations",
      "Number of unique variations",
      "Number of unique SNPs",
      "Number of unique gene names",
      "Number of unique protein names",
      "Percentage TMH"
    ),
    raw = c(
      ncbiresults::get_n_variations_raw(),
      ncbiresults::get_n_unique_variations_raw(),
      NA,
      ncbiresults::get_n_unique_gene_names_raw(),
      ncbiresults::get_n_unique_protein_names_raw(),
      NA
    ),
    all_proteins = c(
      ncbiresults::get_n_variations(),
      ncbiresults::get_n_unique_variations(),
      ncbiresults::get_n_unique_snp_ids(),
      ncbiresults::get_n_unique_gene_names(),
      ncbiresults::get_n_unique_protein_names(),
      100.0 * ncbiresults::get_f_tmh()
    ),
    map = c(
      ncbiresults::get_n_variations_map(),
      ncbiresults::get_n_unique_variations_map(),
      ncbiresults::get_n_unique_snp_ids_map(),
      ncbiresults::get_n_unique_gene_names_map(),
      ncbiresults::get_n_unique_protein_names_map(),
      100.0 * ncbiresults::get_f_tmh_map()
    ),
    tmp = c(
      ncbiresults::get_n_variations_tmp(),
      ncbiresults::get_n_unique_variations_tmp(),
      ncbiresults::get_n_unique_snp_ids_tmp(),
      ncbiresults::get_n_unique_gene_names_tmp(),
      ncbiresults::get_n_unique_protein_names_tmp(),
      100.0 * ncbiresults::get_f_tmh_tmp()
    ),
    in_tmh = c(
      ncbiresults::get_n_variations_tmp_in_tmh(),
      ncbiresults::get_n_unique_variations_tmp_in_tmh(),
      ncbiresults::get_n_unique_snp_ids_tmp_in_tmh(),
      ncbiresults::get_n_unique_gene_names_tmp_in_tmh(),
      ncbiresults::get_n_unique_protein_names_tmp_in_tmh(),
      100.0 * ncbiresults::get_f_tmh_tmp_in_tmh()
    ),
    in_sol = c(
      ncbiresults::get_n_variations_tmp_in_sol(),
      ncbiresults::get_n_unique_variations_tmp_in_sol(),
      ncbiresults::get_n_unique_snp_ids_tmp_in_sol(),
      ncbiresults::get_n_unique_gene_names_tmp_in_sol(),
      ncbiresults::get_n_unique_protein_names_tmp_in_sol(),
      100.0 * ncbiresults::get_f_tmh_tmp_in_sol()
    )
  )
  t
}
