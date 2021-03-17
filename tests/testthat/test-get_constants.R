test_that("Results of research", {
  results_filename <- "~/GitHubs/ncbi_peregrine/scripts/results.csv"

  # Raw
  t_results <- ncbiperegrine::read_results_file(results_filename)
  expect_equal(nrow(t_results), nrow(dplyr::distinct(t_results)))
  n_variations_raw <- length(t_results$variation)
  expect_equal(60931, n_variations_raw)
  expect_equal(60931, get_n_variations_raw())
  n_unique_variations_raw <- length(unique(t_results$variation))
  expect_equal(60544, n_unique_variations_raw)
  expect_equal(60544, get_n_unique_variations_raw())
  n_unique_gene_ids_raw <- length(unique(t_results$gene_id))
  expect_equal(953, n_unique_gene_ids_raw)
  expect_equal(953, get_n_unique_gene_ids_raw())
  n_unique_gene_names_raw <- length(unique(t_results$gene_name))
  expect_equal(953, n_unique_gene_names_raw)
  expect_equal(953, get_n_unique_gene_names_raw())
  n_unique_protein_names_raw <- length(unique(t_results$name))
  expect_equal(5163, n_unique_protein_names_raw)
  expect_equal(5163, get_n_unique_protein_names_raw())

  # Proteins
  t_snps <- dplyr::filter(t_results, ncbi::are_snps(variation))
  expect_equal(nrow(t_snps), nrow(dplyr::distinct(t_snps)))
  n_variations <- length(t_snps$variation)
  expect_equal(37831, n_variations)
  expect_equal(37831, get_n_variations())
  n_unique_variations <- length(unique(t_snps$variation))
  expect_equal(37630, n_unique_variations)
  expect_equal(37630, get_n_unique_variations())
  n_unique_snp_ids <- length(unique(t_snps$snp_id))
  expect_equal(9621, n_unique_snp_ids)
  expect_equal(9621, get_n_unique_snp_ids())
  n_unique_gene_ids <- length(unique(t_snps$gene_id))
  expect_equal(911, n_unique_gene_ids)
  expect_equal(911, get_n_unique_gene_ids())
  n_unique_gene_names <- length(unique(t_snps$gene_name))
  expect_equal(911, n_unique_gene_names)
  expect_equal(911, get_n_unique_gene_names())
  n_unique_protein_names <- length(unique(t_snps$name))
  expect_equal(4780, n_unique_protein_names)
  expect_equal(4780, get_n_unique_protein_names())
  f_tmh <- mean(
    dplyr::summarise(
      dplyr::group_by(
        dplyr::select(t_snps, name, p_in_tmh),
        name
      ),
      f_in_tmh = mean(p_in_tmh)
    )$f_in_tmh
  )
  expect_equal(0.09978325, f_tmh)
  expect_equal(0.09978325, get_f_tmh())

  # Membrane Associated Proteins
  t_snps_map <- dplyr::filter(t_snps, p_in_tmh == 0.0)
  expect_equal(nrow(t_snps_map), nrow(dplyr::distinct(t_snps_map)))
  n_variations_map <- length(t_snps_map$variation)
  expect_equal(16623, n_variations_map)
  expect_equal(16623, get_n_variations_map())
  n_unique_variations_map <- length(unique(t_snps_map$variation))
  expect_equal(16606, n_unique_variations_map)
  expect_equal(16606, get_n_unique_variations_map())
  n_unique_snp_ids_map <- length(unique(t_snps_map$snp_id))
  expect_equal(4219, n_unique_snp_ids_map)
  expect_equal(4219, get_n_unique_snp_ids_map())
  n_unique_gene_ids_map <- length(unique(t_snps_map$gene_id))
  expect_equal(457, n_unique_gene_ids_map)
  expect_equal(457, get_n_unique_gene_ids_map())
  n_unique_gene_names_map <- length(unique(t_snps_map$gene_name))
  expect_equal(457, n_unique_gene_names_map)
  expect_equal(457, get_n_unique_gene_names_map())
  n_unique_protein_names_map <- length(unique(t_snps_map$name))
  expect_equal(2227, n_unique_protein_names_map)
  expect_equal(2227, get_n_unique_protein_names_map())
  f_tmh_map <- mean(
    dplyr::summarise(
      dplyr::group_by(
        dplyr::select(t_snps_map, name, p_in_tmh),
        name
      ),
      f_in_tmh = mean(p_in_tmh)
    )$f_in_tmh
  )
  expect_equal(0.00000, f_tmh_map)
  expect_equal(0.00000, get_f_tmh_map())

  # Trans-Membrane Proteins
  t_snps_tmp <- dplyr::filter(t_snps, p_in_tmh > 0.0)
  expect_equal(nrow(t_snps_tmp), nrow(dplyr::distinct(t_snps_tmp)))
  n_variations_tmp <- length(t_snps_tmp$variation)
  expect_equal(21208, n_variations_tmp)
  expect_equal(21208, get_n_variations_tmp())
  n_unique_variations_tmp <- length(unique(t_snps_tmp$variation))
  expect_equal(21024, n_unique_variations_tmp)
  expect_equal(21024, get_n_unique_variations_tmp())
  n_unique_snp_ids_tmp <- length(unique(t_snps_tmp$snp_id))
  expect_equal(ncbiresults::get_n_unique_snp_ids_tmp(), n_unique_snp_ids_tmp)
  n_unique_gene_ids_tmp <- length(unique(t_snps_tmp$gene_id))
  expect_equal(605, n_unique_gene_ids_tmp)
  expect_equal(605, get_n_unique_gene_ids_tmp())
  n_unique_gene_names_tmp <- length(unique(t_snps_tmp$gene_name))
  expect_equal(605, n_unique_gene_names_tmp)
  expect_equal(605, get_n_unique_gene_names_tmp())
  n_unique_protein_names_tmp <- length(unique(t_snps_tmp$name))
  expect_equal(2553, n_unique_protein_names_tmp)
  expect_equal(2553, get_n_unique_protein_names_tmp())
  f_tmh_tmp <- mean(
    dplyr::summarise(
      dplyr::group_by(
        dplyr::select(t_snps_tmp, name, p_in_tmh),
        name
      ),
      f_in_tmh = mean(p_in_tmh)
    )$f_in_tmh
  )
  expect_equal(0.1868249, f_tmh_tmp, tol = 0.0001)
  expect_equal(0.1868249, get_f_tmh_tmp(), tol = 0.0001)

  # Sums of MAPs and TMPs must match with all proteins
  expect_equal(
    n_variations,
    n_variations_map + n_variations_tmp
  )
  expect_equal(
    n_unique_variations,
    n_unique_variations_map + n_unique_variations_tmp
  )
  # Count the SNPs that are associated with both MAPs and TMPs
  duplicate_snp_ids_map_tmp <- unique(
    t_snps_map$snp_id[t_snps_map$snp_id %in% t_snps_tmp$snp_id]
  )
  n_duplicate_snp_ids_map_tmp <- length(duplicate_snp_ids_map_tmp)
  expect_equal(624, n_duplicate_snp_ids_map_tmp)
  expect_equal(624, get_n_duplicate_snp_ids_map_tmp())
  expect_equal(
    n_unique_snp_ids,
    n_unique_snp_ids_map + n_unique_snp_ids_tmp -
      n_duplicate_snp_ids_map_tmp
  )
  # Count the gene names that are associated with both MAPs and TMPs
  duplicate_gene_names_map_tmp <- unique(
    t_snps_map$gene_name[t_snps_map$gene_name %in% t_snps_tmp$gene_name]
  )
  n_duplicate_gene_names_map_tmp <- length(duplicate_gene_names_map_tmp)
  expect_equal(151, n_duplicate_gene_names_map_tmp)
  expect_equal(151, get_n_duplicate_gene_names_map_tmp())
  expect_equal(
    n_unique_gene_ids,
    n_unique_gene_ids_map + n_unique_gene_ids_tmp -
      n_duplicate_gene_names_map_tmp
  )
  expect_equal(
    n_unique_protein_names,
    n_unique_protein_names_map + n_unique_protein_names_tmp
  )

  # TMPs in TMH
  t_snps_tmp_in_tmh <- dplyr::filter(t_snps_tmp, is_in_tmh)
  expect_equal(nrow(t_snps_tmp_in_tmh), nrow(dplyr::distinct(t_snps_tmp_in_tmh)))
  n_variations_tmp_in_tmh <- length(t_snps_tmp_in_tmh$variation)
  expect_equal(3803, n_variations_tmp_in_tmh)
  expect_equal(3803, get_n_variations_tmp_in_tmh())
  n_unique_variations_tmp_in_tmh <- length(unique(t_snps_tmp_in_tmh$variation))
  expect_equal(3789, n_unique_variations_tmp_in_tmh)
  expect_equal(3789, get_n_unique_variations_tmp_in_tmh())
  n_unique_snp_ids_tmp_in_tmh <- length(unique(t_snps_tmp_in_tmh$snp_id))
  expect_equal(1140, n_unique_snp_ids_tmp_in_tmh)
  expect_equal(1140, get_n_unique_snp_ids_tmp_in_tmh())
  n_unique_gene_ids_tmp_in_tmh <- length(unique(t_snps_tmp_in_tmh$gene_id))
  expect_equal(325, n_unique_gene_ids_tmp_in_tmh)
  expect_equal(325, get_n_unique_gene_ids_tmp_in_tmh())
  n_unique_gene_names_tmp_in_tmh <- length(unique(t_snps_tmp_in_tmh$gene_name))
  expect_equal(325, n_unique_gene_names_tmp_in_tmh)
  expect_equal(325, get_n_unique_gene_names_tmp_in_tmh())
  n_unique_protein_names_tmp_in_tmh <- length(unique(t_snps_tmp_in_tmh$name))
  expect_equal(1280, n_unique_protein_names_tmp_in_tmh)
  expect_equal(1280, get_n_unique_protein_names_tmp_in_tmh())
  f_tmh_tmp_in_tmh <- mean(
    dplyr::summarise(
      dplyr::group_by(
        dplyr::select(t_snps_tmp_in_tmh, name, p_in_tmh),
        name
      ),
      f_in_tmh = mean(p_in_tmh)
    )$f_in_tmh
  )
  expect_equal(0.2568375, f_tmh_tmp_in_tmh, tol = 0.0001)
  expect_equal(0.2568375, get_f_tmh_tmp_in_tmh(), tol = 0.0001)

  # TMPs in soluble regions
  t_snps_tmp_in_sol <- dplyr::filter(t_snps_tmp, !is_in_tmh)
  expect_equal(nrow(t_snps_tmp_in_sol), nrow(dplyr::distinct(t_snps_tmp_in_sol)))
  n_variations_tmp_in_sol <- length(t_snps_tmp_in_sol$variation)
  expect_equal(17405, n_variations_tmp_in_sol)
  expect_equal(17405, get_n_variations_tmp_in_sol())
  n_unique_variations_tmp_in_sol <- length(unique(t_snps_tmp_in_sol$variation))
  expect_equal(17235, n_unique_variations_tmp_in_sol)
  expect_equal(17235, get_n_unique_variations_tmp_in_sol())
  n_unique_snp_ids_tmp_in_sol <- length(unique(t_snps_tmp_in_sol$snp_id))
  expect_equal(4936, n_unique_snp_ids_tmp_in_sol)
  expect_equal(4936, get_n_unique_snp_ids_tmp_in_sol())
  n_unique_gene_ids_tmp_in_sol <- length(unique(t_snps_tmp_in_sol$gene_id))
  expect_equal(590, n_unique_gene_ids_tmp_in_sol)
  expect_equal(590, get_n_unique_gene_ids_tmp_in_sol())
  n_unique_gene_names_tmp_in_sol <- length(unique(t_snps_tmp_in_sol$gene_name))
  expect_equal(590, n_unique_gene_names_tmp_in_sol)
  expect_equal(590, get_n_unique_gene_names_tmp_in_sol())
  n_unique_protein_names_tmp_in_sol <- length(unique(t_snps_tmp_in_sol$name))
  expect_equal(2467, n_unique_protein_names_tmp_in_sol)
  expect_equal(2467, get_n_unique_protein_names_tmp_in_sol())
  f_tmh_tmp_in_sol <- mean(
    dplyr::summarise(
      dplyr::group_by(
        dplyr::select(t_snps_tmp_in_sol, name, p_in_tmh),
        name
      ),
      f_in_sol = mean(p_in_tmh)
    )$f_in_sol
  )
  expect_equal(0.1818121, f_tmh_tmp_in_sol, tol = 0.0001)
  expect_equal(0.1818121, get_f_tmh_tmp_in_sol(), tol = 0.0001)

  # Sums of in TMH and in soluble regions must match with TMPs
  expect_equal(
    n_variations_tmp,
    n_variations_tmp_in_tmh + n_variations_tmp_in_sol
  )
  expect_equal(
    n_unique_variations_tmp,
    n_unique_variations_tmp_in_tmh + n_unique_variations_tmp_in_sol
  )
  n_duplicate_snp_ids_tmp_in_tmh_sol <- 50
  expect_equal(
    n_unique_snp_ids_tmp,
    n_unique_snp_ids_tmp_in_tmh + n_unique_snp_ids_tmp_in_sol - n_duplicate_snp_ids_tmp_in_tmh_sol
  )
  n_duplicate_gene_ids_tmp_in_tmh_sol <- 310
  expect_equal(
    n_unique_gene_ids_tmp,
    n_unique_gene_ids_tmp_in_tmh + n_unique_gene_ids_tmp_in_sol - n_duplicate_gene_ids_tmp_in_tmh_sol
  )
  n_duplicate_gene_names_tmp_in_tmh_sol <- 310
  expect_equal(
    n_unique_gene_names_tmp,
    n_unique_gene_names_tmp_in_tmh + n_unique_gene_names_tmp_in_sol - n_duplicate_gene_names_tmp_in_tmh_sol
  )
  n_duplicate_protein_names_tmp_in_tmh_sol <- 1194
  expect_equal(
    n_unique_protein_names_tmp,
    n_unique_protein_names_tmp_in_tmh + n_unique_protein_names_tmp_in_sol - n_duplicate_protein_names_tmp_in_tmh_sol
  )

  # Spanners
  ## Raw variations
  expect_equal(
    get_n_variations_raw(),
    nrow(dplyr::filter(t_results, n_tmh == 0)) +
    nrow(dplyr::filter(t_results, n_tmh == 1)) +
    nrow(dplyr::filter(t_results, n_tmh > 1))
  )
  ## Variations
  expect_equal(
    get_n_variations(),
    nrow(dplyr::filter(t_snps, n_tmh == 0)) +
    nrow(dplyr::filter(t_snps, n_tmh == 1)) +
    nrow(dplyr::filter(t_snps, n_tmh > 1))
  )
  expect_equal(
    get_n_variations(),
    nrow(dplyr::filter(t_snps, n_tmh == 0)) +
    nrow(dplyr::filter(t_snps, n_tmh == 1)) +
    nrow(dplyr::filter(t_snps, n_tmh > 1))
  )
  expect_equal(
    nrow(dplyr::filter(t_snps, n_tmh == 1)),
    get_n_variations_tmp_single()
  )
  expect_equal(
    nrow(dplyr::filter(t_snps, n_tmh > 1)),
    get_n_variations_tmp_multi()
  )
  t_snp_single <- dplyr::filter(t_snps, n_tmh == 1)
  t_snp_multi <- dplyr::filter(t_snps, n_tmh > 1)
  expect_equal(
    nrow(t_snp_single),
    get_n_variations_tmp_single()
  )
  expect_equal(
    nrow(t_snp_multi),
    get_n_variations_tmp_multi()
  )
  expect_equal(
    length(unique(t_snp_single$variation)),
    get_n_unique_variations_tmp_single()
  )
  expect_equal(
    length(unique(t_snp_multi$variation)),
    get_n_unique_variations_tmp_multi()
  )
  expect_equal(
    length(unique(t_snp_single$snp_id)),
    get_n_unique_snps_in_single_spanners()
  )
  expect_equal(
    length(unique(t_snp_multi$snp_id)),
    get_n_unique_snps_in_multi_spanners()
  )
  expect_equal(
    length(unique(t_snp_single$gene_name)),
    get_n_unique_gene_names_in_single_spanners()
  )
  expect_equal(
    length(unique(t_snp_multi$gene_name)),
    get_n_unique_gene_names_in_multi_spanners()
  )

  expect_equal(
    length(unique(t_snp_single$name)),
    get_n_unique_protein_names_in_single_spanners()
  )
  expect_equal(
    length(unique(t_snp_multi$name)),
    get_n_unique_protein_names_in_multi_spanners()
  )
  expect_equal(
    mean(t_snp_single$p_in_tmh),
    get_f_tmh_in_single_spanners(),
    tol = 0.00001
  )
  expect_equal(
    mean(t_snp_multi$p_in_tmh),
    get_f_tmh_in_multi_spanners(),
    tol = 0.00001
  )









  ###############################################################################
  # Single/multi-spanners in TMH/soluble region
  ###############################################################################
  # Single-spanners in TMHs
  # TMPs in TMH

  t_single_in_tmh_all <- dplyr::filter(
    dplyr::filter(t_snps, is_in_tmh),
    n_tmh == 1
  )
  expect_true(all(t_single_in_tmh_all$is_in_tmh == TRUE))
  expect_true(all(t_single_in_tmh_all$n_tmh == 1))
  expect_equal(
    nrow(t_single_in_tmh_all),
    get_n_variations_single_in_tmh()
  )
  expect_equal(
    length(unique(t_single_in_tmh_all$variation)),
    get_n_unique_variations_single_in_tmh()
  )
  t_single_in_tmh <- dplyr::filter(
    t_single_in_tmh_all, ncbi::are_snps(variation)
  )
  expect_equal(
    length(unique(t_single_in_tmh$snp_id)),
    get_n_unique_snp_ids_single_in_tmh()
  )
  expect_equal(
    length(unique(t_single_in_tmh$gene_id)),
    get_n_unique_gene_ids_single_in_tmh()
  )
  expect_equal(
    length(unique(t_single_in_tmh$name)),
    get_n_unique_protein_names_single_in_tmh()
  )
  expect_equal(
    mean(t_single_in_tmh$p_in_tmh),
    get_f_tmh_single_in_tmh(),
    tol = 0.000001
  )
  # Single-spanners in soluble regionss
  # TMPs in soluble regions
  t_single_in_sol <- dplyr::filter(
    dplyr::filter(t_snps, !is_in_tmh),
    n_tmh == 1
  )
  expect_equal(
    nrow(t_single_in_sol),
    get_n_variations_single_in_sol()
  )
  expect_equal(
    length(unique(t_single_in_sol$variation)),
    get_n_unique_variations_single_in_sol()
  )
  expect_equal(
    length(unique(t_single_in_sol$snp_id)),
    get_n_unique_snp_ids_single_in_sol()
  )
  expect_equal(
    length(unique(t_single_in_sol$gene_id)),
    get_n_unique_gene_ids_single_in_sol()
  )
  expect_equal(
    length(unique(t_single_in_sol$gene_name)),
    get_n_unique_gene_names_single_in_sol()
  )
  expect_equal(
    length(unique(t_single_in_sol$name)),
    get_n_unique_protein_names_single_in_sol()
  )
  expect_equal(
    mean(t_single_in_sol$p_in_tmh),
    get_f_tmh_single_in_sol(),
    tol = 0.000001
  )







  # Multi-spanners in TMHs
  # TMPs in TMH
  t_multi_in_tmh <- dplyr::filter(
    dplyr::filter(t_snps, is_in_tmh),
    n_tmh > 1
  )
  expect_equal(
    nrow(t_multi_in_tmh),
    get_n_variations_multi_in_tmh()
  )
  expect_equal(
    length(unique(t_multi_in_tmh$variation)),
    get_n_unique_variations_multi_in_tmh()
  )
  expect_equal(
    length(unique(t_multi_in_tmh$snp_id)),
    get_n_unique_snp_ids_multi_in_tmh()
  )
  expect_equal(
    length(unique(t_multi_in_tmh$gene_id)),
    get_n_unique_gene_ids_multi_in_tmh()
  )
  expect_equal(
    length(unique(t_multi_in_tmh$name)),
    get_n_unique_protein_names_multi_in_tmh()
  )
  expect_equal(
    mean(t_multi_in_tmh$p_in_tmh),
    get_f_tmh_multi_in_tmh(),
    tol = 0.000001
  )
  # Multi-spanners in soluble regionss
  # TMPs in soluble regions
  t_multi_in_sol <- dplyr::filter(
    dplyr::filter(t_snps, !is_in_tmh),
    n_tmh > 1
  )
  expect_equal(
    nrow(t_multi_in_sol),
    get_n_variations_multi_in_sol()
  )
  expect_equal(
    length(unique(t_multi_in_sol$variation)),
    get_n_unique_variations_multi_in_sol()
  )
  expect_equal(
    length(unique(t_multi_in_sol$snp_id)),
    get_n_unique_snp_ids_multi_in_sol()
  )
  expect_equal(
    length(unique(t_multi_in_sol$gene_id)),
    get_n_unique_gene_ids_multi_in_sol()
  )
  expect_equal(
    length(unique(t_multi_in_sol$gene_name)),
    get_n_unique_gene_names_multi_in_sol()
  )
  expect_equal(
    length(unique(t_multi_in_sol$name)),
    get_n_unique_protein_names_multi_in_sol()
  )
  expect_equal(
    mean(t_multi_in_sol$p_in_tmh),
    get_f_tmh_multi_in_sol(),
    tol = 0.000001
  )

  #############################################################################
  # Combine
  #############################################################################
  # Variations
  expect_equal(
    get_n_variations_tmp(),
    get_n_variations_single_in_tmh() +
    get_n_variations_single_in_sol() +
    get_n_variations_multi_in_tmh() +
    get_n_variations_multi_in_sol()
  )
  expect_equal(
    get_n_variations_tmp_in_tmh(),
    get_n_variations_single_in_tmh() +
    get_n_variations_multi_in_tmh()
  )
  expect_equal(
    get_n_variations_tmp_in_sol(),
    get_n_variations_single_in_sol() +
    get_n_variations_multi_in_sol()
  )
  # Unique variations
  expect_equal(
    get_n_unique_variations_tmp(),
    get_n_unique_variations_single_in_tmh() +
    get_n_unique_variations_single_in_sol() +
    get_n_unique_variations_multi_in_tmh() +
    get_n_unique_variations_multi_in_sol()
  )
  expect_equal(
    get_n_unique_variations_tmp_in_tmh(),
    get_n_unique_variations_single_in_tmh() +
    get_n_unique_variations_multi_in_tmh()
  )
  expect_equal(
    get_n_unique_variations_tmp_in_sol(),
    get_n_unique_variations_single_in_sol() +
    get_n_unique_variations_multi_in_sol()
  )
})
