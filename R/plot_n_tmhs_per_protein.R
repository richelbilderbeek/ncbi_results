#' The number of TMHs per protein
#' @inheritParams default_params_doc
#' @export
plot_n_tmhs_per_protein <- function(folder_name = folder_name) {

  topo_filenames <- list.files(
    path = folder_name,
    pattern = ".*\\.topo$",
    full.names = TRUE
  )
  testthat::expect_true(length(topo_filenames) > 0)

  tibbles <- list()
  for (i in seq_along(topo_filenames)) {
    t <- dplyr::distinct(
      pureseqtmr::load_fasta_file_as_tibble_cpp(topo_filenames[i])
    )
    tibbles[[i]] <- pureseqtmr::count_n_tmhs(t$sequence)
  }

  n_tmhs <- unlist(tibbles)
  n_tmhs

  n_nonzero_tmhs <- n_tmhs[n_tmhs > 0]
  # n_snps <- NULL; rm(n_snps) # nolint, fixes warning: no visible binding for global variable

  ggplot2::ggplot() +
    ggplot2::aes(n_nonzero_tmhs) +
  ggplot2::geom_histogram(bins = 50) +
  ggplot2::scale_x_continuous(name = "Number of TMHs") +
  ggplot2::labs(
    title = "Number of TMHs per proteins",
    caption = paste0(
      "Number of proteins without TMHs: ", length(n_tmhs), "\n",
      "Number of proteins with TMHs: ", length(n_nonzero_tmhs), "\n",
      "Highest number of TMHs: ", max(n_nonzero_tmhs), "\n",
      "Total number of TMHs: ", sum(n_nonzero_tmhs)
    )
  ) + ggthemes::theme_clean(base_size = 22) +
      ggplot2::ggsave(
      file.path(folder_name, "fig_n_tmhs_per_protein.png"),
      width = 7,
      height = 7
    )

  # ggthemes::theme_excel_new(base_size = 24) # Never forget Excel

}
