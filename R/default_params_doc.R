#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param fasta_filename name of a FASTA file with a protein sequence
#' @param fasta_filenames name of one or more FASTA files
#'   with protein sequences
#' @param folder_name name of the folder
#' @param gene_ids_filename the filename to save
#'   the gene IDs to.
#'   For the experiment, use \code{gene_ids.csv}
#' @param gene_names_filename the filename to save
#'   the gene IDs and gene names to,
#'   as can be read by \link[ncbiperegrine]{read_gene_names_file}.
#'   For the experiment, use \code{gene_names.csv}
#' @param is_in_tmh_filename the filename to save
#'   the \code{is_in_tmh} table to (as can be read by
#'   \link[ncbiperegrine]{read_is_in_tmh_file}).
#'   For the experiment, use \code{[gene_name]_is_in_tmh.csv}
#' @param is_in_tmh_filenames the filename to save
#'   the \code{is_in_tmh} tables to (each of which can be read by
#'   \link[ncbiperegrine]{read_is_in_tmh_file}).
#'   For the experiment, use \code{[gene_name]_is_in_tmh.csv}
#' @param n_gene_ids the number of gene IDs.
#'   Use \link{Inf} to use all gene IDs
#' @param n_snps the number of SNPs.
#'   Use \link{Inf} to use all SNS IDs
#' @param results_filename name of the results file.
#'   For the experiment, use \code{results.csv}
#' @param snps_filename one filenames of a file
#'   containing the SNP IDs,
#'   named \code{[gene_name]_snps.csv}.
#'   These files can be read by \link[ncbiperegrine]{read_snps_file}
#' @param snps_filenames one or more filenames of files
#'   containing the SNP IDs,
#'   named \code{[gene_name]_snps.csv}.
#'   These files can be read by \link[ncbiperegrine]{read_snps_file}
#' @param snps_id_filename one filenames of a file
#'   containing the SNP IDs,
#'   named \code{[gene_name]_snps.csv}.
#'   These files can be read by \link[ncbiperegrine]{read_snps_file}
#' @param snps_id_filenames one or more filenames of files
#'   containing the SNP IDs,
#'   named \code{[gene_name]_snps.csv}.
#'   These files can be read by \link[ncbiperegrine]{read_snps_file}
#' @param topo_filename name of a \code{.topo} file with a protein topology
#' @param topo_filenames name of one or more \code{.topo} files
#'   with protein topologies
#' @param variations_csv_filename name of a \code{[gene_name]_variations.csv}
#'   file
#' @param variations_csv_filenames names of one or more
#'   \code{[gene_name]_variations.csv}
#'   files
#' @param variations_rds_filename name of a \code{[gene_name]_variations.rds}
#'   file
#' @param variations_rds_filenames names of one or more
#'   \code{[gene_name]_variations.rds}
#'   files
#' @param verbose set to TRUE for more output
#' @author Richèl J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
default_params_doc <- function(
  fasta_filename,
  fasta_filenames,
  folder_name,
  gene_ids_filename,
  gene_names_filename,
  is_in_tmh_filename,
  is_in_tmh_filenames,
  n_gene_ids,
  n_snps,
  results_filename,
  snps_filename,
  snps_filenames,
  snps_id_filename,
  snps_id_filenames,
  topo_filename,
  topo_filenames,
  variations_csv_filename,
  variations_csv_filenames,
  variations_rds_filename,
  variations_rds_filenames,
  verbose
) {
  # Nothing
}
