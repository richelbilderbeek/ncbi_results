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
    ),
    single_in_tmh = c(
      ncbiresults::get_n_variations_single_in_tmh(),
      ncbiresults::get_n_unique_variations_single_in_tmh(),
      ncbiresults::get_n_unique_snp_ids_single_in_tmh(),
      ncbiresults::get_n_unique_gene_names_single_in_tmh(),
      ncbiresults::get_n_unique_protein_names_single_in_tmh(),
      100.0 * ncbiresults::get_f_tmh_single_in_tmh()
    ),
    single_in_sol = c(
      ncbiresults::get_n_variations_single_in_sol(),
      ncbiresults::get_n_unique_variations_single_in_sol(),
      ncbiresults::get_n_unique_snp_ids_single_in_sol(),
      ncbiresults::get_n_unique_gene_names_single_in_sol(),
      ncbiresults::get_n_unique_protein_names_single_in_sol(),
      100.0 * ncbiresults::get_f_tmh_single_in_sol()
    ),
    multi_in_tmh = c(
      ncbiresults::get_n_variations_multi_in_tmh(),
      ncbiresults::get_n_unique_variations_multi_in_tmh(),
      ncbiresults::get_n_unique_snp_ids_multi_in_tmh(),
      ncbiresults::get_n_unique_gene_names_multi_in_tmh(),
      ncbiresults::get_n_unique_protein_names_multi_in_tmh(),
      100.0 * ncbiresults::get_f_tmh_multi_in_tmh()
    ),
    multi_in_sol = c(
      ncbiresults::get_n_variations_multi_in_sol(),
      ncbiresults::get_n_unique_variations_multi_in_sol(),
      ncbiresults::get_n_unique_snp_ids_multi_in_sol(),
      ncbiresults::get_n_unique_gene_names_multi_in_sol(),
      ncbiresults::get_n_unique_protein_names_multi_in_sol(),
      100.0 * ncbiresults::get_f_tmh_multi_in_sol()
    )
  )
  t
}



# Single-spanners in soluble regions
get_n_variations_single_in_sol <- function() 7952
get_n_unique_variations_single_in_sol <- function() 7951
get_n_unique_snp_ids_single_in_sol <- function() 2393
get_n_unique_gene_ids_single_in_sol <- function() 282
get_n_unique_gene_names_single_in_sol <- function() 1032
get_n_unique_protein_names_single_in_sol <- function() 1032
get_f_tmh_single_in_sol <- function() 0.05359196

# Multi-spanners in TMHs
get_n_variations_multi_in_tmh <- function() 3437
get_n_unique_variations_multi_in_tmh <- function() 3424
get_n_unique_snp_ids_multi_in_tmh <- function() 994
get_n_unique_gene_ids_multi_in_tmh <- function() 243
get_n_unique_gene_names_multi_in_tmh <- function() 243
get_n_unique_protein_names_multi_in_tmh <- function() 976
get_f_tmh_multi_in_tmh <- function() 0.3533563

# Multi-spanners in soluble regions
get_n_variations_multi_in_sol <- function() 9939
get_n_unique_variations_multi_in_sol <- function() 9757
get_n_unique_snp_ids_multi_in_sol <- function() 2762
get_n_unique_gene_ids_multi_in_sol <- function() 344
get_n_unique_gene_names_multi_in_sol <- function() 344
get_n_unique_protein_names_multi_in_sol <- function() 1435
get_f_tmh_multi_in_sol <- function() 0.2579164
