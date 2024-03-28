#' Ego Network Cleaning and Measure Calculation (\code{ego_netwrite})
#'
#' @description The \code{ego_netwrite} function reads in data pertaining to ego networks and processes them into a set of standardized outputs, including measures commonly calculated for ego networks.
#'
#' @param egos A data frame containing measures of ego attributes.
#' @param ego_id A vector of unique identifiers corresponding to each ego, or a single character value indicating the name of the column in \code{egos} containing ego identifiers.
#' @param alters A data frame containing measures of alter attributes.
#' @param alter_id A vector of identifiers indicating which alter is associated with a given row in \code{alters}, or a single character value indicating the name of the column in \code{alters} containing alter identifiers.
#' @param alter_ego A vector of identifiers indicating which ego is associated with a given alter, or a single character value indicating the name of the column in \code{alters} containing ego identifiers.
#' @param alter_types A character vector indicating the columns in \code{alters} that indicate whether a given alter has certain types of relations with ego. These columns should all contain binary measures indicating whether alter has a particular type of relation with ego.
#' @param max_alters A numeric value indicating the maximum number of alters an ego in the dataset could have nominated
#' @param alter_alter A data frame containing an edgelist indicating ties between alters in each ego's network. This edgelist is optional, but \code{ego_netwrite} will not provide certain measures without it.
#' @param aa_ego A vector of identifiers indicating which ego is associated with a given tie between alters, or a single character indicating the name of the column in \code{alter_alter} containing ego identifiers.
#' @param i_elements A vector of identifiers indicating which alter is on one end of an alter-alter tie, or a single character indicating the name of the column in \code{alter_alter} containing these identifiers.
#' @param j_elements A vector of identifiers indicating which alter is on the other end of an alter-alter tie, or a single character indicating the name of the column in \code{alter_alter} containing these identifiers.
#' @param directed A logical value indicating whether network ties are directed or undirected.
#' @param aa_type A numeric or character vector indicating the types of relationships represented in the alter edgelist, or a single character value indicating the name of the column in \code{alter_alter} containing relationship type. If \code{alter_type} is specified, \code{ego_netwrite} will treat the data as a set of multi-relational networks and produce additional outputs reflecting the different types of ties occurring in each ego network.
#' @param missing_code A numeric value indicating "missing" values in the alter-alter edgelist.
#' @param na.rm A logical value indicating whether \code{NA} values should be excluded when calculating continuous measures.
#' @param egor A logical value indicating whether output should include an \code{egor} object, which is often useful for visualizaton and for simulation larger networks from egocentric data.
#' @param egor_design If creating an \code{egor} object, a list of arguments to \code{\link[srvyr:as_survey_design]{srvyr::as_survey_design}} specifying the sampling design for egos. This argument corresponds to \code{ego_design} in \code{\link[egor:egor]{egor::egor}}.
#'
#' @return \code{ego_netwrite} returns a list containing several output objects. Users may find it easier to access and work with outputs by applying \link{list2env} to this list, which will separate outputs and store them in the R Global Environment. Outputs include a data frame containing measures of ego attributes, another data frame containing measures of alter attributes and network position, a third containing the alter-alter edgelist (when applicable), a fourth containing summary measures for each individual ego network, and a fifth providing summary measures for the overall dataset. Additionally, \code{ego_netwrite} returns a list of \code{igraph} objects constructed for each individual ego network, as well as an \code{egor} object for the overall dataset if desired.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' # Simple Processing, Ignoring Ego-Alter or Alter-Alter Relation Types
#' ngq_nw <- ego_netwrite(egos = ngq_egos,
#'                        ego_id = ngq_egos$ego_id,
#'
#'                        alters = ngq_alters,
#'                        alter_id = ngq_alters$alter_id,
#'                        alter_ego = ngq_alters$ego_id,
#'
#'                        max_alters = 10,
#'                        alter_alter = ngq_aa,
#'                        aa_ego = ngq_aa$ego_id,
#'                        i_elements = ngq_aa$alter1,
#'                        j_elements = ngq_aa$alter2,
#'                        directed = FALSE,
#'
#'                        egor = TRUE)
#'
#' list2env(ngq_nw, .GlobalEnv)
#'
#' # View summaries of individual ego networks
#' head(summaries)
#'
#' # View summary of overall dataset
#' head(overall_summary)
#'
#' # View sociogram of second ego network
#' plot(igraph_objects[[2]]$igraph_ego)
#'
#'
#' #  Processing with Different Ego-Alter Relation Types
#' ngq_altertype <- ego_netwrite(egos = ngq_egos,
#'                               ego_id = ngq_egos$ego_id,
#'
#'                               alters = ngq_alters,
#'                               alter_id = ngq_alters$alter_id,
#'                               alter_ego = ngq_alters$ego_id,
#'                               alter_types = c("family", "friend", "other_rel"),
#'                               max_alters = 10,
#'
#'                               alter_alter = ngq_aa,
#'                               aa_ego = ngq_aa$ego_id,
#'                               i_elements = ngq_aa$alter1,
#'                               j_elements = ngq_aa$alter2,
#'
#'                               directed = FALSE)
#'
#' list2env(ngq_altertype, .GlobalEnv)
#'
#'
#' #  Processing with Different Alter-Alter Relation Types
#' ngq_aatype <- ego_netwrite(egos = ngq_egos,
#'                            ego_id = ngq_egos$ego_id,
#'
#'                            alters = ngq_alters,
#'                            alter_id = ngq_alters$alter_id,
#'                            alter_ego = ngq_alters$ego_id,
#'                            max_alters = 10,
#'                            alter_alter = ngq_aa,
#'
#'                            aa_ego = ngq_aa$ego_id,
#'                            i_elements = ngq_aa$alter1,
#'                            j_elements = ngq_aa$alter2,
#'                            aa_type = ngq_aa$type,
#'
#'                            directed = FALSE)
#'
#' list2env(ngq_aatype, .GlobalEnv)


ego_netwrite <- function(egos,
                         ego_id,
                         alters = NULL,
                         alter_id = NULL,
                         alter_ego = NULL,
                         alter_types = NULL,
                         max_alters = Inf,
                         alter_alter = NULL,
                         aa_ego = NULL,
                         i_elements = NULL,
                         j_elements = NULL,
                         directed = FALSE,
                         aa_type = NULL,
                         missing_code = 99999,

                         # Do we remove NA values when calculating continuous measures?
                         na.rm = FALSE,

                         # Egor compatibility
                         egor = FALSE,
                         egor_design = NULL) {

  # browser()

  # Create output list
  output_list <- list()



  ################################################################################
  # Basic Formatting
  ################################################################################
  # In case tibbles are entered, convert to basic data frames
  egos <- as.data.frame(egos)
  alters <- as.data.frame(alters)

  if (!is.null(alter_alter)) {
    alter_alter <- as.data.frame(alter_alter)
  }


  ################################################################################
  # Handling type indicator variables
  ################################################################################






  # Indicators for renaming objects
  ego_id_fix <- FALSE
  alter_ego_fix <- FALSE
  alter_id_fix <- FALSE
  aa_ego_fix <- FALSE
  i_elements_fix <- FALSE
  j_elements_fix <- FALSE
  alter_types_fix <- FALSE
  aa_type_fix <- FALSE

  # If ID columns are specified by character values, extract those columns

  if (methods::is(ego_id, "character") & length(ego_id) == 1) {
    ego_id1 <- egos[,(which(colnames(egos) == ego_id))]
    ego_id_fix <- TRUE
  }

  if (is.null(alter_ego) == TRUE) {
    alter_ego1 <- alters[,(which(colnames(alters) == ego_id))]
    alter_ego_fix <- TRUE
  } else if (methods::is(alter_ego, "character") & length(alter_ego) == 1) {
    alter_ego1 <- alters[,(which(colnames(alters) == alter_ego))]
    alter_ego_fix <- TRUE
  }

  if (methods::is(alter_id, "character") & length(alter_id) == 1) {
    alter_id1 <- alters[,(which(colnames(alters) == alter_id))]
    alter_id_fix <- TRUE
  }

  # if (class(alter_types) == "character" & length(alter_types) == 1) {
  #   alter_types1 <- alters[,(which(colnames(alters) == alter_types))]
  #   alter_types_fix <- TRUE
  # }



  if (!is.null(alter_alter)) {

    if (is.null(aa_ego) == TRUE) {
      aa_ego1 <- alter_alter[,(which(colnames(alter_alter) == aa_ego))]
      aa_ego_fix <- TRUE
    } else if (methods::is(aa_ego, "character") & length(aa_ego) == 1) {
      aa_ego1 <- alter_alter[,(which(colnames(alter_alter) == aa_ego))]
      aa_ego_fix <- TRUE
    }

    if (methods::is(i_elements, "character") & length(i_elements) == 1) {
      i_elements1 <- alter_alter[,(which(colnames(alter_alter) == i_elements))]
      i_elements_fix <- TRUE
    }

    if (methods::is(j_elements, "character") & length(j_elements) == 1) {
      j_elements1 <- alter_alter[,(which(colnames(alter_alter) == j_elements))]
      j_elements_fix <- TRUE
    }

    if (methods::is(aa_type, "character") & length(aa_type) == 1) {
      aa_type1 <- alter_alter[,(which(colnames(alter_alter) == aa_type))]
      aa_type_fix <- TRUE
    }

  }
  # If characters were used to identify ID columns, assign them to the correct
  # objects

  if (ego_id_fix == TRUE) {
    ego_id <- ego_id1
  }

  if (alter_ego_fix == TRUE) {
    alter_ego <- alter_ego1
  }

  if (alter_id_fix == TRUE) {
    alter_id <- alter_id1
  }

  # if (alter_types_fix == TRUE) {
  #   alter_types <- alter_types1
  # }

  if (!is.null(alter_alter)) {

    if (aa_ego_fix == TRUE) {
      aa_ego <- aa_ego1
    }

    if (i_elements_fix == TRUE) {
      i_elements <- i_elements1
    }

    if (j_elements_fix == TRUE) {
      j_elements <- j_elements1
    }

    if (aa_type_fix == TRUE) {
      aa_type <- aa_type1
    }

  }


  # EGO NAME FORMATTING
  if ("id" %in% colnames(egos)) {
    colnames(egos) <- stringr::str_replace_all(colnames(egos), "^id$", "original_id")
  } else if ("ego_id" %in% colnames(egos)) {
    colnames(egos) <- stringr::str_replace_all(colnames(egos), "^ego_id$", "original_ego_id")
  }

  # Define `ego_id` as whatever column was specified by user
  egos$ego_id <- ego_id

  egos <- egos %>%
    dplyr::select(ego_id, dplyr::everything())


  # ALTER NAME FORMATTING
  if ("ego_id" %in% colnames(alters)) {
    colnames(alters) <- stringr::str_replace_all(colnames(alters), "^ego_id$", "original_ego_id")
  }
  alters$ego_id <- alter_ego

  if ("alter_id" %in% colnames(alters)) {
    colnames(alters) <- stringr::str_replace_all(colnames(alters), "^alter_id$", "original_alter_id")
  }
  alters$alter_id <- alter_id

  # Columns named `weight` can lead to downstream issues with igraph. If such a column exists in `alters`,
  # we'll need to preemptively rename it here
  if ("weight" %in% colnames(alters)) {
    colnames(alters) <- stringr::str_replace_all(colnames(alters), "^weight$", "original_weight_alter")
  }




  # Adjust this to clearly label ego-alter edge type variables
  if (!is.null(alter_types)) {

    alter_types_df <- alters[, c("ego_id", "alter_id", alter_types)]
    # Rename to indicate type variables
    colnames(alter_types_df) <- paste("type", colnames(alter_types_df), sep = "_")
    colnames(alter_types_df)[[1]] <- "ego_id"
    colnames(alter_types_df)[[2]] <- "alter_id"

    alters <- alters %>%
      dplyr::left_join(alter_types_df, by = c("ego_id", "alter_id")) %>%
      dplyr::select(ego_id, alter_id, tidyr::starts_with("type"), dplyr::everything())

  } else {
    alters <- alters %>%
      dplyr::select(ego_id, alter_id, dplyr::everything())
  }


  if (!is.null(alter_alter)) {

    # Columns named `weight` can lead to downstream issues with igraph. If such a column exists in `alter_alter`,
    # we'll need to preemptively rename it here
    if ("weight" %in% colnames(alter_alter)) {
      colnames(alter_alter) <- stringr::str_replace_all(colnames(alter_alter), "^weight$", "original_weight_aa")
    }

    alter_alter$i_elements <- i_elements
    alter_alter$j_elements <- j_elements
    alter_alter$ego_id <- aa_ego

    if (!is.null(aa_type)) {
      alter_alter$type <- aa_type

      alter_alter <- alter_alter %>%
        dplyr::select(.data$ego_id, .data$i_elements, .data$j_elements, .data$type, dplyr::everything())
    } else {
      alter_alter <- alter_alter %>%
        dplyr::select(.data$ego_id, .data$i_elements, .data$j_elements, dplyr::everything())
    }
  }


  ################################################################################
  # Creating numeric IDs for Data
  ################################################################################

  # Create new numeric `ego_id` values
  ego_id_relabel <- data.frame(ego_id = egos$ego_id,
                               new_ego_id = 1:length(egos$ego_id))

  egos <- egos %>%
    dplyr::left_join(ego_id_relabel, by = "ego_id") %>%
    dplyr::select(-.data$ego_id) %>%
    dplyr::rename(ego_id = .data$new_ego_id) %>%
    dplyr::select(.data$ego_id, dplyr::everything())
  alters <- alters %>%
    dplyr::left_join(ego_id_relabel, by = "ego_id") %>%
    dplyr::select(-.data$ego_id) %>%
    dplyr::rename(ego_id = .data$new_ego_id) %>%
    dplyr::select(.data$ego_id, dplyr::everything())
  if (!is.null(alter_alter)) {
    alter_alter <- alter_alter %>%
      dplyr::left_join(ego_id_relabel, by = "ego_id") %>%
      dplyr::select(-.data$ego_id) %>%
      dplyr::rename(ego_id = .data$new_ego_id) %>%
      dplyr::select(.data$ego_id, dplyr::everything())
  }

  # Create new numeric `alter_id` values
  alter_id_relabel <- alters %>%
    dplyr::group_by(.data$ego_id) %>%
    dplyr::mutate(new_alter_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$ego_id, .data$alter_id, .data$new_alter_id)

  alters <- alters %>%
    dplyr::left_join(alter_id_relabel, by = c("ego_id", "alter_id")) %>%
    dplyr::select(-.data$alter_id) %>%
    dplyr::rename(alter_id = .data$new_alter_id) %>%
    dplyr::select(.data$ego_id, .data$alter_id, dplyr::everything())

  if (!is.null(alter_alter)) {

    # These are used to identify i and j in the alter-alter edgelist
    i_elements_relabel <- alter_id_relabel %>%
      dplyr::rename(i_elements = .data$alter_id,
                    new_i_elements = .data$new_alter_id)
    j_elements_relabel <- alter_id_relabel %>%
      dplyr::rename(j_elements = .data$alter_id,
                    new_j_elements = .data$new_alter_id)

    alter_alter <- alter_alter %>%
      dplyr::left_join(i_elements_relabel, by = c("ego_id", "i_elements")) %>%
      dplyr::left_join(j_elements_relabel, by = c("ego_id", "j_elements")) %>%
      dplyr::select(-.data$i_elements, -.data$j_elements) %>%
      dplyr::rename(i_elements = .data$new_i_elements,
                    j_elements = .data$new_j_elements) %>%
      dplyr::select(.data$ego_id, .data$i_elements, .data$j_elements, dplyr::everything())

  }

  ################################################################################
  # Creating list of igraph objects for each ego network
  ################################################################################

  # Get full list of edge types in alter-alter edgelist
  if (!is.null(alter_alter)) {

    # AGGREGATE GRAPHS
    # Get unique values of `ego_id`
    ego_ids <- unique(egos$ego_id)

    # Make list to store igraph objects
    igraph_list <- list()
    # Make object for storing alter information
    alters_output <- "to_populate"
    # Make object for storing alter edgelist
    alter_alter_output <- "to_populate"

    for (i in 1:length(ego_ids)) {

      # a. Within each node ID, get the unique values for alter IDs in the alter DF
      #    and in the alter-alter edgelist. Then zero-index.
      # b. Make sure
      this_ego <- ego_ids[[i]]
      this_ego_info <- egos[egos$ego_id == this_ego,]

      # this_alters <- alters[alters$ego_id == this_ego, ]
      # this_alter_id <- alter_id[alter_ego == this_ego]
      # this_alters$alter_id <- this_alter_id

      this_alters <- alters[alters$ego_id == this_ego, ]
      # Create an isolate indicator for downstream
      this_iso <- ifelse(nrow(this_alters) == 0, TRUE, FALSE)

      # Need a bit of finagling if ego is an isolate (makes zero nominations)
      if (this_iso == TRUE) {
        this_alters <- alters[1,]
        this_alters$ego_id <- this_ego
        this_alters[1, 2:ncol(this_alters)] <- NA
      }

      this_alter_alter <- alter_alter[alter_alter$ego_id == this_ego, ]


      # If an ego nominates alters with no connections to each other,
      # we need to create the igraph object a bit differently
      if (nrow(this_alter_alter) == 0) {

        # If ego is an isolate, though, we don't need to create an igraph object
        if (this_iso == TRUE) {
          # Store in list of igraph objects
          igraph_list[[i]] <- list(ego = this_ego,
                                   ego_info = this_ego_info,
                                   igraph = NA)

          # Need to add column `id` to `alters` to make compatible for merging in
          # final output
          this_alters$id <- NA

          # If ego isn't an isolate, though, just that alters aren't connected,
          # proceed this way
        } else {

          unique_alters <- unique(this_alters$alter_id)
          alter_id_merge <- data.frame(alter_id = unique_alters,
                                       id = (1:length(unique_alters)) - 1)

          this_alters <- this_alters %>%
            dplyr::left_join(alter_id_merge, by = "alter_id") %>%
            dplyr::select(.data$id, .data$alter_id, .data$ego_id, dplyr::everything())

          # If there's a variable in `this_alters` called `name`, it'll mess up igraph
          # processing. Because of this, we have to rename

          if ("name" %in% colnames(this_alters)) {
            colnames(this_alters) <- stringr::str_replace_all(colnames(this_alters), "^name$", "alter_name")
          }

          this_alter_alter <- this_alter_alter %>%
            dplyr::select(.data$i_elements, .data$j_elements, .data$ego_id, dplyr::everything())

          this_igraph <- igraph::graph_from_data_frame(this_alter_alter, vertices = this_alters,
                                                       directed = directed)

          # Add in ego to the graph (without attributes)
          this_igraph_ego <- igraph::add_vertices(this_igraph, 1)
          this_igraph_ego <- igraph::add_edges(this_igraph_ego, c(rbind(seq(igraph::gorder(this_igraph_ego) - 1),
                                                                        igraph::gorder(this_igraph_ego))))
          igraph::V(this_igraph_ego)$name[[igraph::gorder(this_igraph_ego)]] <- "ego"

          # Store in list of igraph objects
          igraph_list[[i]] <- list(ego = this_ego,
                                   ego_info = this_ego_info,
                                   igraph = this_igraph,
                                   igraph_ego = this_igraph_ego)

        }

        # Alters have edges with each other, proceed normally
      } else {

        # this_aa_i <- aa_i[aa_ego == this_ego]
        # this_aa_j <- aa_j[aa_ego == this_ego]
        # this_alter_alter$aa_i <- this_aa_i
        # this_alter_alter$aa_j <- this_aa_j

        # Get unique alter identifiers from both dataframes
        # unique_alters <- unique(c(this_alter_id, this_aa_i, this_aa_j))
        unique_alters <- unique(c(this_alters$alter_id,
                                  this_alter_alter$i_elements,
                                  this_alter_alter$j_elements))
        alter_id_merge <- data.frame(alter_id = unique_alters,
                                     id = (1:length(unique_alters)) - 1)
        aa_i_merge <- data.frame(i_elements = unique_alters,
                                 i_id = (1:length(unique_alters)) - 1)
        aa_j_merge <- data.frame(j_elements = unique_alters,
                                 j_id = (1:length(unique_alters)) - 1)

        this_alters <- this_alters %>%
          dplyr::left_join(alter_id_merge, by = "alter_id") %>%
          dplyr::select(.data$id, .data$alter_id, .data$ego_id, dplyr::everything())

        # If there's a variable in `this_alters` called `name`, it'll mess up igraph
        # processing. Because of this, we have to rename

        if ("name" %in% colnames(this_alters)) {
          colnames(this_alters) <- stringr::str_replace_all(colnames(this_alters), "^name$", "alter_name")
        }

        this_alter_alter <- this_alter_alter %>%
          dplyr::left_join(aa_i_merge, by = "i_elements") %>%
          dplyr::left_join(aa_j_merge, by = "j_elements") %>%
          dplyr::select(.data$i_id, .data$j_id, .data$i_elements, .data$j_elements, .data$ego_id, dplyr::everything())

        # # Make simplified versions of data frames for the purposes of creating these
        # # igraph objects
        # alters_simp <- this_alters %>% dplyr::select(id)
        # aa_simp <- this_alter_alter %>% dplyr::select(i_elements, j_elements)
        #
        # this_igraph <- igraph::graph_from_data_frame(aa_simp, vertices = alters_simp,
        #                                              directed = FALSE)

        # # To make `graph_from_data_frame` work, we need to reshape `this_alters`
        # # so that only one row corresponds to a particular alter
        # this_alters <- this_alters %>%
        #   dplyr::mutate(pivot_hold = 1) %>%
        #   tidyr::pivot_wider(names_from = type,
        #                      values_from = pivot_hold,
        #                      values_fill = 0,
        #                      names_prefix = "type_") %>%
        #   dplyr::group_by(ego_id, id) %>%
        #   dplyr::summarise_all(max, na.rm = TRUE) %>%
        #   dplyr::ungroup() %>%
        #   dplyr::select(id, alter_id, ego_id, dplyr::everything())

        this_igraph <- igraph::graph_from_data_frame(this_alter_alter, vertices = this_alters,
                                                     directed = directed)

        # Add in ego to the graph (without attributes)
        this_igraph_ego <- igraph::add_vertices(this_igraph, 1)
        this_igraph_ego <- igraph::add_edges(this_igraph_ego, c(rbind(seq(igraph::gorder(this_igraph_ego) - 1),
                                                                      igraph::gorder(this_igraph_ego))))
        igraph::V(this_igraph_ego)$name[[igraph::gorder(this_igraph_ego)]] <- "ego"

        # Store in list of igraph objects
        igraph_list[[i]] <- list(ego = this_ego,
                                 ego_info = this_ego_info,
                                 igraph = this_igraph,
                                 igraph_ego = this_igraph_ego)

        # Reorder columns of alter edgelist for final output
        this_alter_alter <- this_alter_alter %>%
          dplyr::select(.data$ego_id, .data$i_elements, .data$i_id, .data$j_elements, .data$j_id, dplyr::everything())

        # Storing updated alter edgelist
        if (methods::is(alter_alter_output, "character")) {
          if (alter_alter_output == "to_populate") {
            alter_alter_output <- this_alter_alter
          }
        } else {
          alter_alter_output <- dplyr::bind_rows(alter_alter_output, this_alter_alter)
        }

      }

      # Reorder columns of alters list for final output
      this_alters <- this_alters %>%
        dplyr::select(.data$ego_id, .data$id, .data$alter_id, dplyr::everything())

      # Storing updated alters list
      if (methods::is(alters_output, "character")) {
        if (alters_output == "to_populate") {
          alters_output <- this_alters
        }
      } else {
        alters_output <- dplyr::bind_rows(alters_output, this_alters)
      }
    }

    # Get centrality measures for alters in each ego network and add to
    # `alters_output`
    alter_cent <- dplyr::bind_rows(lapply(igraph_list, alter_centrality, directed = directed))

    alters_output <- dplyr::left_join(alters_output, alter_cent, by = c("ego_id", "id"))

    # Add Obs_ID column to alter edgelist object
    alter_alter_output$Obs_ID <- 1:nrow(alter_alter_output)
    alter_alter_output <- alter_alter_output %>%
      dplyr::select(.data$Obs_ID, dplyr::everything())


    # WITHIN-RELATION TYPE
    if (!is.null(aa_type)) {

      # Get unique values of `ego_id`
      # ego_ids <- unique(ego_id)
      # Get full list of edge types in alter-alter edgelist
      edge_types <- unique(aa_type)
      # Record all possible combinations of egos and alter-alter edge types
      type_index <- data.frame(ego = rep(ego_ids, each = length(edge_types)),
                               type = rep(edge_types, length(ego_ids)))
      type_index$name <- paste(type_index$ego, type_index$type, sep = "_")

      # Make list to store igraph objects
      igraph_list2 <- list()
      # Make object for storing alter information
      # alters_output <- "to_populate"
      # Make object for storing alter edgelist
      # alter_alter_output <- "to_populate"

      for (i in 1:length(type_index$name)) {
        # a. Within each node ID, get the unique values for alter IDs in the alter DF
        #    and in the alter-alter edgelist. Then zero-index.
        # b. Make sure
        this_ego <- type_index$ego[[i]]
        this_type <- type_index$type[[i]]
        this_ego_info <- egos[ego_id == this_ego,]

        this_alters <- alters[alters$ego_id == this_ego, ]
        # Create an isolate indicator for downstream
        this_iso <- ifelse(nrow(this_alters) == 0, TRUE, FALSE)

        # Need a bit of finagling if ego is an isolate (makes zero nominations)
        if (nrow(this_alters) == 0) {
          this_alters <- alters[1,]
          this_alters$ego_id <- this_ego
          this_alters[1, 2:ncol(this_alters)] <- NA
        }

        this_alter_alter <- alter_alter[alter_alter$ego_id == this_ego & alter_alter$type == this_type, ]



        # If an ego nominates alters with no connections to each other,
        # no need to create an igraph object
        if (nrow(this_alter_alter) == 0) {

          # If ego is an isolate, though, we don't need to create an igraph object
          if (this_iso == TRUE) {
            # Store in list of igraph objects
            igraph_list2[[i]] <- list(ego = this_ego,
                                      type = this_type,
                                      ego_info = this_ego_info,
                                      igraph = NA)

            # Need to add column `id` to `alters` to make compatible for merging in
            # final output
            this_alters$id <- NA

            # Also store in main `igraph_list`. This is a bit involved,
            # so I'll do my best to walk through the steps in the comments here
            #### First, we need to use `lappy` to figure out which item in `igraph_list`
            #### corresponds to the ego we're interested in
            list_id <- which(unlist(lapply(igraph_list, function(x, num) {x$ego == num}, num = this_ego)))
            #### Now that we have this, we're going to store `igraph` and `igraph_ego`,
            #### as constructed in this step of the loop, into the appropriate item
            #### in igraph_list (which is the corresponding ego). Placeholder names
            #### are used in this step
            igraph_list[[list_id]]$this_igraph <- NA
            igraph_list[[list_id]]$this_igraph_ego <- NA
            #### Now we rename the new additions to this_ego's item in `igraph_list`
            names(igraph_list[[list_id]])[(length(names(igraph_list[[list_id]]))-1)] <- paste("igraph", this_type, sep = "_")
            names(igraph_list[[list_id]])[(length(names(igraph_list[[list_id]])))] <-   paste("igraph_ego", this_type, sep = "_")

            # If ego isn't an isolate, though, just that alters aren't connected,
            # proceed this way
          } else {

            unique_alters <- unique(this_alters$alter_id)
            alter_id_merge <- data.frame(alter_id = unique_alters,
                                         id = (1:length(unique_alters)) - 1)
            this_alters <- this_alters %>%
              dplyr::left_join(alter_id_merge, by = "alter_id") %>%
              dplyr::select(.data$id, .data$alter_id, .data$ego_id, dplyr::everything())

            # If there's a variable in `this_alters` called `name`, it'll mess up igraph
            # processing. Because of this, we have to rename

            if ("name" %in% colnames(this_alters)) {
              colnames(this_alters) <- stringr::str_replace_all(colnames(this_alters), "^name$", "alter_name")
            }

            this_alter_alter <- this_alter_alter %>%
              dplyr::select(.data$i_elements, .data$j_elements, .data$ego_id, dplyr::everything())

            this_igraph <- igraph::graph_from_data_frame(this_alter_alter, vertices = this_alters,
                                                         directed = directed)

            # Add in ego to the graph (without attributes)
            this_igraph_ego <- igraph::add_vertices(this_igraph, 1)
            this_igraph_ego <- igraph::add_edges(this_igraph_ego, c(rbind(seq(igraph::gorder(this_igraph_ego) - 1),
                                                                          igraph::gorder(this_igraph_ego))))
            igraph::V(this_igraph_ego)$name[[igraph::gorder(this_igraph_ego)]] <- "ego"

            # Store in list of igraph objects
            igraph_list2[[i]] <- list(ego = this_ego,
                                      type = this_type,
                                      ego_info = this_ego_info,
                                      igraph = this_igraph,
                                      igraph_ego = this_igraph_ego)

            # Also store in main `igraph_list`. This is a bit involved,
            # so I'll do my best to walk through the steps in the comments here
            #### First, we need to use `lappy` to figure out which item in `igraph_list`
            #### corresponds to the ego we're interested in
            list_id <- which(unlist(lapply(igraph_list, function(x, num) {x$ego == num}, num = this_ego)))
            #### Now that we have this, we're going to store `igraph` and `igraph_ego`,
            #### as constructed in this step of the loop, into the appropriate item
            #### in igraph_list (which is the corresponding ego). Placeholder names
            #### are used in this step
            igraph_list[[list_id]]$this_igraph <- this_igraph
            igraph_list[[list_id]]$this_igraph_ego <- this_igraph_ego
            #### Now we rename the new additions to this_ego's item in `igraph_list`
            names(igraph_list[[list_id]])[(length(names(igraph_list[[list_id]]))-1)] <- paste("igraph", this_type, sep = "_")
            names(igraph_list[[list_id]])[(length(names(igraph_list[[list_id]])))] <-   paste("igraph_ego", this_type, sep = "_")

            # Reorder columns of alter edgelist for final output
            this_alter_alter <- this_alter_alter %>%
              dplyr::select(.data$ego_id, .data$i_elements,
                            #.data$i_id,
                            .data$j_elements,
                            #.data$j_id,
                            dplyr::everything())
          }


          # Not an isolate, at least one dyad has an edge. Proceed normally
        } else {

          # this_aa_i <- aa_i[aa_ego == this_ego]
          # this_aa_j <- aa_j[aa_ego == this_ego]
          # this_alter_alter$aa_i <- this_aa_i
          # this_alter_alter$aa_j <- this_aa_j

          # Get unique alter identifiers from both dataframes
          # unique_alters <- unique(c(this_alter_id, this_aa_i, this_aa_j))
          unique_alters <- unique(c(this_alters$alter_id,
                                    this_alter_alter$i_elements,
                                    this_alter_alter$j_elements))
          alter_id_merge <- data.frame(alter_id = unique_alters,
                                       id = (1:length(unique_alters)) - 1)
          aa_i_merge <- data.frame(i_elements = unique_alters,
                                   i_id = (1:length(unique_alters)) - 1)
          aa_j_merge <- data.frame(j_elements = unique_alters,
                                   j_id = (1:length(unique_alters)) - 1)

          this_alters <- this_alters %>%
            dplyr::left_join(alter_id_merge, by = "alter_id") %>%
            dplyr::select(.data$id, .data$alter_id, .data$ego_id, dplyr::everything())

          # If there's a variable in `this_alters` called `name`, it'll mess up igraph
          # processing. Because of this, we have to rename

          if ("name" %in% colnames(this_alters)) {
            colnames(this_alters) <- stringr::str_replace_all(colnames(this_alters), "^name$", "alter_name")
          }

          this_alter_alter <- this_alter_alter %>%
            dplyr::left_join(aa_i_merge, by = "i_elements") %>%
            dplyr::left_join(aa_j_merge, by = "j_elements") %>%
            dplyr::select(.data$i_id, .data$j_id, .data$i_elements, .data$j_elements, .data$ego_id, dplyr::everything())

          # # Make simplified versions of data frames for the purposes of creating these
          # # igraph objects
          # alters_simp <- this_alters %>% dplyr::select(id)
          # aa_simp <- this_alter_alter %>% dplyr::select(i_elements, j_elements)
          #
          # this_igraph <- igraph::graph_from_data_frame(aa_simp, vertices = alters_simp,
          #                                              directed = FALSE)


          this_igraph <- igraph::graph_from_data_frame(this_alter_alter, vertices = this_alters,
                                                       directed = directed)

          # Add in ego to the graph (without attributes)
          this_igraph_ego <- igraph::add_vertices(this_igraph, 1)
          this_igraph_ego <- igraph::add_edges(this_igraph_ego, c(rbind(seq(igraph::gorder(this_igraph_ego) - 1),
                                                                        igraph::gorder(this_igraph_ego))))
          igraph::V(this_igraph_ego)$name[[igraph::gorder(this_igraph_ego)]] <- "ego"

          # Store in list of igraph objects
          igraph_list2[[i]] <- list(ego = this_ego,
                                    type = this_type,
                                    ego_info = this_ego_info,
                                    igraph = this_igraph,
                                    igraph_ego = this_igraph_ego)

          # Also store in main `igraph_list`. This is a bit involved,
          # so I'll do my best to walk through the steps in the comments here
          #### First, we need to use `lappy` to figure out which item in `igraph_list`
          #### corresponds to the ego we're interested in
          list_id <- which(unlist(lapply(igraph_list, function(x, num) {x$ego == num}, num = this_ego)))
          #### Now that we have this, we're going to store `igraph` and `igraph_ego`,
          #### as constructed in this step of the loop, into the appropriate item
          #### in igraph_list (which is the corresponding ego). Placeholder names
          #### are used in this step
          igraph_list[[list_id]]$this_igraph <- this_igraph
          igraph_list[[list_id]]$this_igraph_ego <- this_igraph_ego
          #### Now we rename the new additions to this_ego's item in `igraph_list`
          names(igraph_list[[list_id]])[(length(names(igraph_list[[list_id]]))-1)] <- paste("igraph", this_type, sep = "_")
          names(igraph_list[[list_id]])[(length(names(igraph_list[[list_id]])))] <-   paste("igraph_ego", this_type, sep = "_")

          # Reorder columns of alter edgelist for final output
          this_alter_alter <- this_alter_alter %>%
            dplyr::select(.data$ego_id, .data$i_elements, .data$i_id, .data$j_elements, .data$j_id, dplyr::everything())

          # Storing updated alter edgelist
          # if (class(alter_alter_output) == "character") {
          #   if (alter_alter_output == "to_populate") {
          #     alter_alter_output <- this_alter_alter
          #   }
          # } else {
          #   alter_alter_output <- dplyr::bind_rows(alter_alter_output, this_alter_alter)
          # }

        }

        # Reorder columns of alters list for final output
        # this_alters <- this_alters %>%
        #   dplyr::select(ego_id, id, alter_id, dplyr::everything())

        # Storing updated alters list
        ### For multirelational nets, we only need to do this once per egoid
        # if (i %% length(edge_types) == 1) {
        # if (class(alters_output) == "character") {
        #   if (alters_output == "to_populate") {
        #     alters_output <- this_alters
        #   }
        # } else {
        #   alters_output <- dplyr::bind_rows(alters_output, this_alters)
        # }
        # }
      }

      # Get centrality measures for alters in each ego network and add to
      # `alters_output`
      alter_cent2 <- lapply(igraph_list2, alter_centrality, directed = directed)
      # Add type indicator
      for (i in 1:length(alter_cent2)) {
        alter_cent2[[i]]$type <- type_index$type[[i]]
      }
      # Bind rows
      alter_cent2 <- dplyr::bind_rows(alter_cent2)
      # Reshape to wide format
      alter_cent2 <- alter_cent2 %>%
        tidyr::pivot_wider(id_cols = c("ego_id", "id"),
                           names_from = .data$type,
                           values_from = .data$total_degree:.data$reachability) %>%
        dplyr::filter(!is.na(.data$id))


      alters_output <- dplyr::left_join(alters_output, alter_cent2, by = c("ego_id", "id"))


    }


  } else {

    alters_output <- alters
    alter_alter_output <- NULL

  }


  ################################################################################
  # NETWORK-LEVEL, ATTRIBUTE-AGNOSTIC MEASURES
  ################################################################################
  #### For this, we basically develop a function to get summary measures from
  #### an igraph object, then apply that to each igraph object in the igraph list.
  #### I've put together the barebones version of this, you (Gabe) just need
  #### to flesh out the set of measures being calculated

  if (!is.null(alter_alter)) {

    egonet_summaries <- dplyr::bind_rows(lapply(igraph_list, igraph_apply, directed = directed))

    # Within-relation type, if applicable
    if (!is.null(aa_type)) {
      egonet_summaries2 <- lapply(igraph_list2, igraph_apply, directed = directed)
      # Rename columns
      for (i in 1:length(egonet_summaries2)) {
        egonet_summaries2[[i]]$type <- type_index$type[[i]]
      }
      # Bind rows
      egonet_summaries2 <- dplyr::bind_rows(egonet_summaries2)
      # Reshape to wide format
      egonet_summaries2 <- egonet_summaries2 %>%
        tidyr::pivot_wider(names_from = .data$type,
                           values_from = -.data$ego_id)
      # Remove `type_` artifact columns
      egonet_summaries2 <- egonet_summaries2[,1:(ncol(egonet_summaries2)-length(unique(aa_type)))]
      # Merge into `egonet_summaries`
      egonet_summaries <- dplyr::left_join(egonet_summaries, egonet_summaries2, by = "ego_id")
    }

    # If we don't have data on alter-alter ties, we may still need `egonet_summaries`
    # for multiplex edge correlation on the ego-alter edgelist. But at this stage
    # the only thing that really needs to be calculated is network size
  } else {
    ### Create `egonet_summaries`
    egonet_summaries <- data.frame(ego_id = unique(egos$ego_id))

    ### Use `dplyr` to get ego network sizes
    alter_hold <- alters %>% dplyr::select(.data$ego_id, .data$alter_id, dplyr::starts_with("type_"))
    net_sizes <- alter_hold %>% dplyr::group_by(.data$ego_id) %>%
      dplyr::summarize(network_size = length(unique(.data$alter_id))) %>%
      dplyr::ungroup()

    ### Merge into `egonet_summaries`
    egonet_summaries <- egonet_summaries %>%
      dplyr::left_join(net_sizes, by = "ego_id")

    ##### If we have multiple relation types, we'll want degree counts for those as well
    if (!is.null(alter_types)) {
      type_sizes <- alter_hold %>%
        dplyr::select(-.data$alter_id) %>%
        dplyr::group_by(.data$ego_id) %>%
        dplyr::summarize_all(~sum(.x == 1, na.rm = T)) %>%
        dplyr::ungroup()

      colnames(type_sizes) <- gsub("^type_", "network_size_", colnames(type_sizes))

      egonet_summaries <- egonet_summaries %>%
        dplyr::left_join(type_sizes, by = "ego_id")

    }

  }

  ################################################################################
  # MULTIPLEX EDGE CORRELATIONS
  ################################################################################

  # On the alter-alter edgelist
  if (!is.null(aa_type)) {

    # Get alter edgelist in format that Jon's function will like
    aa_multi <- alter_alter_output
    aa_multi$weight <- 1
    aa_multi <- aa_multi[,c("Obs_ID", "i_elements", "i_id", "j_elements", "j_id", "weight", "type", "ego_id")]
    # Store unique values for alter edge types
    edge_types <- unique(aa_type)

    cors_list <- list()

    for (i in 1:nrow(egos)) {
      # print(i)
      this_ego <- egos[i, ]
      this_ego_id <- this_ego[, "ego_id"]
      # Get edgelist for only this ego
      this_el <- aa_multi[aa_multi$ego_id == this_ego_id, ]

      # Handling if ego is an isolate
      if (nrow(this_el) > 0) {
        this_cors <- suppressWarnings(as.data.frame(t(multiplex_ego(edgelist = this_el,
                                                                    directed = directed,
                                                                    type = aa_multi$type))))
        this_cors$ego_id <- this_ego_id

        cors_list[[i]] <- this_cors
      }

    }

    cors_list <- dplyr::bind_rows(cors_list)

    # Identify that these are correlations for alter-alter ties
    colnames(cors_list) <- paste("aa", colnames(cors_list), sep = "_")
    colnames(cors_list)[ncol(cors_list)] <- "ego_id"

    egonet_summaries <- egonet_summaries %>%
      dplyr::left_join(cors_list, by = "ego_id")

  }


  # On the ego-alter edgelist
  if (!is.null(alter_types)) {

    # Get just the columns we need
    alter_cor_df <- alters[, c("ego_id", "alter_id", alter_types)]
    ### Add a prefix for `tidyr` call later
    colnames(alter_cor_df) <- paste("type", colnames(alter_cor_df), sep = "_")
    colnames(alter_cor_df)[[1]] <- "ego_id"
    colnames(alter_cor_df)[[2]] <- "alter_id"
    # Now we need to convert to a long dataframe
    alter_cor_df <- alter_cor_df %>%
      tidyr::pivot_longer(cols = tidyr::starts_with("type"),
                          names_to = "type",
                          values_to = "type_bin") %>%
      dplyr::filter(.data$type_bin == 1) %>%
      # Some reformatting to work with `multiplex_ego`
      dplyr::mutate(Obs_ID = dplyr::row_number(),
                    i_elements = .data$ego_id,
                    i_id = 0,
                    j_elements = .data$alter_id,
                    j_id = .data$j_elements,
                    weight = 1,
                    type = stringr::str_replace_all(.data$type, "^type_", "")) %>%
      dplyr::select(.data$Obs_ID, .data$i_elements, .data$i_id, .data$j_elements, .data$j_id, .data$weight, .data$type, .data$ego_id)

    # Store unique values for edge types
    alter_edge_types <- unique(alter_cor_df$type)

    alter_cors_list <- list()

    for (i in 1:nrow(egos)) {
      this_ego <- egos[i, ]
      this_ego_id <- this_ego[, "ego_id"]
      # Get edgelist for only this ego
      this_el <- alter_cor_df[alter_cor_df$ego_id == this_ego_id, ]

      # Handling if ego is an isolate
      if (nrow(this_el) > 0) {
        this_cors <- suppressWarnings(as.data.frame(t(multiplex_ego(edgelist = this_el,
                                                                    directed = directed,
                                                                    type = alter_edge_types))))
        this_cors$ego_id <- this_ego_id

        alter_cors_list[[i]] <- this_cors
      }
    }

    alter_cors_list <- dplyr::bind_rows(alter_cors_list)
    # Identify that these are correlations for ego-alter ties
    colnames(alter_cors_list) <- paste("alter", colnames(alter_cors_list), sep = "_")
    colnames(alter_cors_list)[ncol(alter_cors_list)] <- "ego_id"

    # Merge into `egonet_summaries`
    egonet_summaries <- egonet_summaries %>%
      dplyr::left_join(alter_cors_list, by = "ego_id")

    # End ego-alter edge correlations
  }




  ################################################################################
  # Dataset-level summary
  ################################################################################

  # Define variable descriptions
  summary_names <- c("num_egos",
                     "num_alters",
                     "num_isolates",
                     "num_one_alter",
                     "min_net_size",
                     "max_net_size",
                     "avg_net_size",
                     "avg_density",
                     "avg_fragmentation")
  summary_titles <- c("Number of egos/ego networks",
                      "Number of alters",
                      "Number of isolates",
                      "Number of one-node networks",
                      "Smallest non-isolate network size",
                      "Largest network size",
                      "Average network size",
                      "Average network density",
                      "Average fragmentation")
  summary_descriptions <- c("Total number of egos providing ego networks in dataset",
                            "Total number of alters nominated by egos across entire dataset",
                            "Number of egos who did not report any alters in their personal network",
                            "Number of egos who reported only one alter in their personal network",
                            "Smallest number of alters provided by a single ego",
                            "Largest number of alters provided by a single ego",
                            "Average number of alters provided by a single ego",
                            "The average density of personal networks provided by egos (networks with 0-1 alters excluded from calculation)",
                            "The mean fragmentation index score of personal networks provided by egos (networks with 0-1 alters excluded from calculation)")

  # Combine into single dataframe
  summary_labels <- data.frame(var_name = summary_names,
                               measure_labels = summary_titles,
                               measure_descriptions = summary_descriptions)

  # If we have an alter-alter edgelist, this gets us what we need

  if (!is.null(alter_alter)) {

    # 1. Get unique list of egos
    summary_df <- data.frame(ego_id = egos$ego_id)
    # 2. Merge in ego-level network summaries, selecting only the variables we need
    summary_df <- summary_df %>%
      dplyr::left_join(egonet_summaries, by = "ego_id") %>%
      dplyr::select(.data$ego_id, .data$network_size, .data$mean_degree, .data$density, .data$fragmentation_index) %>%
      # Handle egos who nominate zero alters
      dplyr::mutate(network_size = ifelse(is.na(.data$network_size), 0, .data$network_size)) %>%
      # 3. Create summary measures
      dplyr::summarize(num_egos =          as.character(dplyr::n()),
                       num_alters =        as.character(sum(.data$network_size, na.rm = TRUE)),
                       num_isolates =      as.character(sum(.data$network_size == 0)),
                       num_one_alter =     as.character(sum(.data$network_size == 1)),
                       min_net_size =      as.character(min(.data$network_size[.data$network_size != 0])),
                       max_net_size =      as.character(max(.data$network_size, na.rm = TRUE)),
                       avg_net_size =      as.character(mean(.data$network_size, na.rm = TRUE)),
                       avg_density =       as.character(mean(.data$density, na.rm = TRUE)),
                       avg_fragmentation = as.character(mean(.data$fragmentation_index, na.rm = TRUE)))
    # 4. Transpose
    summary_t <- as.data.frame(t(summary_df))
    summary_t$var_name <- rownames(summary_t)
    colnames(summary_t) <- c("measures", "var_name")
    # 5. Merge into `summary_merge`
    summary_merge <- summary_labels %>%
      dplyr::left_join(summary_t, by = "var_name") %>%
      dplyr::select(-.data$var_name)

    # If there are multiple alter-alter edge types, we'll want to do the same process
    # within each edge type

    if (!is.null(aa_type)) {

      for (i in 1:length(unique(aa_type))) {
        summary_df <- data.frame(ego_id = egos$ego_id)

        # 2. Merge in ego-level network summaries, selecting only the variables we need
        summary_df <- summary_df %>%
          dplyr::left_join(egonet_summaries, by = "ego_id")
        # Which variables are we working with here?
        summary_df <- summary_df[,c("ego_id", paste(c("network_size", "mean_degree", "density", "fragmentation_index"), unique(aa_type)[[i]], sep = "_"))]
        # Rename columns
        colnames(summary_df) <- c("ego_id", "network_size", "mean_degree", "density", "fragmentation_index")

        summary_df <- summary_df %>%
          dplyr::select(.data$ego_id, .data$network_size, .data$mean_degree, .data$density, .data$fragmentation_index) %>%
          # Handle egos who nominate zero alters
          dplyr::mutate(network_size = ifelse(is.na(.data$network_size), 0, .data$network_size)) %>%
          # 3. Create summary measures
          dplyr::summarize(num_egos =          as.character(dplyr::n()),
                           num_alters =        as.character(sum(.data$network_size, na.rm = TRUE)),
                           num_isolates =      as.character(sum(.data$network_size == 0)),
                           num_one_alter =     as.character(sum(.data$network_size == 1)),
                           min_net_size =      as.character(min(.data$network_size[.data$network_size != 0])),
                           max_net_size =      as.character(max(.data$network_size, na.rm = TRUE)),
                           avg_net_size =      as.character(mean(.data$network_size, na.rm = TRUE)),
                           avg_density =       as.character(mean(.data$density, na.rm = TRUE)),
                           avg_fragmentation = as.character(mean(.data$fragmentation_index, na.rm = TRUE)))

        # 4. Transpose
        summary_t <- as.data.frame(t(summary_df))
        summary_t$var_name <- rownames(summary_t)
        colnames(summary_t) <- c("measures", "var_name")
        # 5. Merge into `summary_merge`
        this_merge <- summary_labels %>%
          dplyr::left_join(summary_t, by = "var_name") %>%
          dplyr::select(-.data$var_name)

        this_merge$measure_labels <- paste("(Alter-Alter ", unique(aa_type)[[i]], ") ", this_merge$measure_labels, sep = "")
        # We don't need the top row
        this_merge <- this_merge[2:nrow(this_merge),]

        if (i == 1) {
          multi_summary <- this_merge
        } else {
          multi_summary <- dplyr::bind_rows(multi_summary, this_merge)
        }
        # End for loop
      }
      # Merge into `summary_merge`
      summary_merge <- dplyr::bind_rows(summary_merge, multi_summary)

    }

  } else {

    # 1. Get unique list of egos
    summary_df <- data.frame(ego_id = egos$ego_id)
    # 2. Get network sizes from alters dataframe
    net_sizes <- alters_output %>%
      dplyr::group_by(.data$ego_id) %>%
      dplyr::summarize(network_size = sum(!is.na(.data$alter_id))) %>%
      dplyr::ungroup()
    summary_df <- summary_df %>%
      dplyr::left_join(net_sizes, by = "ego_id") %>%
      dplyr::mutate(network_size = ifelse(is.na(.data$network_size), 0, .data$network_size)) %>%
      dplyr::summarize(num_egos =          as.character(dplyr::n()),
                       num_alters =        as.character(sum(.data$network_size, na.rm = TRUE)),
                       num_isolates =      as.character(sum(.data$network_size == 0)),
                       num_one_alter =     as.character(sum(.data$network_size == 1)),
                       min_net_size =      as.character(min(.data$network_size)),
                       max_net_size =      as.character(max(.data$network_size)),
                       avg_net_size =      as.character(mean(.data$network_size, na.rm = TRUE)))
    # 3. Transpose
    summary_t <- as.data.frame(t(summary_df))
    summary_t$var_name <- rownames(summary_t)
    colnames(summary_t) <- c("measures", "var_name")
    # 4. Merge into `summary_merge`
    summary_merge <- summary_t %>%
      dplyr::left_join(summary_labels, by = "var_name") %>%
      dplyr::select(-.data$var_name) %>%
      dplyr::select(.data$measure_labels, .data$measure_descriptions, .data$measures)

  }

  ######### I think we need unique summary measures for ego->alter edge types
  if (!is.null(alter_types)) {
    for (i in 1:length(alter_types)) {
      # Create placeholder variable for this edge type
      this_sizes <- alters_output
      this_sizes$this_var <- this_sizes[, paste("type", alter_types[[i]], sep = "_")]
      # Get network sizes from alters dataframe
      this_sizes <- this_sizes %>%
        dplyr::group_by(.data$ego_id) %>%
        dplyr::summarize(network_size = sum(.data$this_var)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(network_size2 = .data$network_size,
                      network_size = ifelse(is.na(.data$network_size), 0, .data$network_size),
                      network_size2 = ifelse(.data$network_size2 == 0, NA, .data$network_size2)) %>%
        dplyr::summarize(num_alters =        as.character(sum(.data$network_size, na.rm = TRUE)),
                         num_isolates =      as.character(sum(.data$network_size == 0)),
                         num_one_alter =     as.character(sum(.data$network_size == 1)),
                         min_net_size =      as.character(min(.data$network_size2, na.rm = TRUE)),
                         max_net_size =      as.character(max(.data$network_size2, na.rm = TRUE)),
                         avg_net_size =      as.character(mean(.data$network_size2, na.rm = TRUE)))
      # Transpose
      this_t <- as.data.frame(t(this_sizes))
      this_t$var_name <- rownames(this_t)
      colnames(this_t) <- c("measures", "var_name")
      # Merge into `summary_merge`
      this_t <- this_t %>%
        dplyr::left_join(summary_labels, by = "var_name") %>%
        dplyr::mutate(measure_labels = paste("(Ego-Alter ", alter_types[[i]], ") ", .data$measure_labels, sep = "")) %>%
        dplyr::select(-.data$var_name) %>%
        dplyr::select(.data$measure_labels, .data$measure_descriptions, .data$measures)
      summary_merge <- dplyr::bind_rows(summary_merge, this_t)
    }
  }


  ################################################################################
  # Assigning output to the global environment
  ################################################################################

  if (directed == FALSE) {
    egonet_summaries <- egonet_summaries %>%
      dplyr::select(-tidyr::starts_with("pairwise_weak_dir"),
                    -tidyr::starts_with("pairwise_strong_dir"),
                    -tidyr::starts_with("dyad_asym"),
                    -tidyr::starts_with("triad_012"), -tidyr::starts_with("triad_021"),
                    -tidyr::starts_with("triad_111"), -tidyr::starts_with("triad_030"),
                    -tidyr::starts_with("triad_120"), -tidyr::starts_with("triad_210"))
  }


  output_list$egos <- egos
  # assign(x = paste(output_name, "_egos", sep = ""), value = egos, .GlobalEnv)

  # Remove placeholder rows in `alter_output` given for isolate egos
  # only needed when alter-alter edgelist is present
  if (!is.null(alter_alter)) {
    alters_output <- alters_output[!is.na(alters_output$id),]
  }

  output_list$alters <- alters_output
  # assign(x = paste(output_name, "_alters", sep = ""), value = alters_output, .GlobalEnv)

  output_list$summaries <- egonet_summaries
  # assign(x = paste(output_name, "_summaries", sep = ""), value = egonet_summaries, .GlobalEnv)

  output_list$overall_summary <- summary_merge
  # assign(x = paste(output_name, "_overall_summary", sep = ""), value = summary_merge, .GlobalEnv)

  if (!is.null(alter_alter)) {

    output_list$alter_edgelist <- alter_alter_output
    # assign(x = paste(output_name, "_alter_edgelist", sep = ""), value = alter_alter_output, .GlobalEnv)

    output_list$igraph_objects <- igraph_list
    # assign(x = paste(output_name, "_igraph", sep = ""), value = igraph_list, .GlobalEnv)
  }

  ################################################################################
  # Egor object creation
  ################################################################################


  if (egor == TRUE) {

    # 1. Rename columns to reflect egor formatting. `egor` is flexible with column
    # names in theory, but in practice using other names can result in bugs when
    # trying to incorporate survey weights.
    egos_egor <- egos %>%
      dplyr::rename(.egoID = .data$ego_id)
    alters_egor <- alters %>%
      dplyr::rename(.altID = .data$alter_id,
                    .egoID = .data$ego_id)

    if (!is.null(alter_alter)) {
      alter_alter_egor <- alter_alter %>%
        dplyr::rename(.egoID = .data$ego_id,
                      .srcID = .data$i_elements,
                      .tgtID = .data$j_elements)
    } else {
      alter_alter_egor <- NULL
    }

    egor_object <- egor::egor(alters = alters_egor,
                              egos = egos_egor,
                              aaties = alter_alter_egor,
                              ID.vars = list(ego = ".egoID",
                                             alter = ".altID",
                                             source = ".srcID",
                                             target = ".tgtID"),
                              ego_design = egor_design,
                              alter_design = list(max = max_alters))

    output_list$egor <- egor_object
    # assign(x = paste(output_name, "_egor", sep = ""), value = egor_object, .GlobalEnv)

  }

  return(output_list)
  # End function

}

################################################################################
# Support Functions (Will relocate to their own R script later)
################################################################################

detect_integer <- function (x) {
  if (is.numeric(x) & (length(unique(x[!is.na(x)])) > 2)) {
    if (sum(x %% 1, na.rm = TRUE) == 0) {
      output = TRUE
    } else {
      output = FALSE
    }
  } else {
    output = FALSE
  }
  return(output)
}

detect_integer_binary <- function (x) {
  if (is.numeric(x) & length(unique(x[!is.na(x)])) == 2) {
    if (sum(x %% 1, na.rm = TRUE) == 0) {
      output = TRUE
    }
  } else {
    output = FALSE
  }
  return(output)
}

detect_integer_cat <- function (x) {
  if (is.numeric(x) & (length(unique(x[!is.na(x)])) > 2) & ((length(unique(x[!is.na(x)]))) < 8)) {
    if (sum(x %% 1, na.rm = TRUE) == 0) {
      output = TRUE
    }
  } else {
    output = FALSE
  }
  return(output)
}

detect_char_cat <- function (x) {
  if (is.character(x) & (length(unique(x[!is.na(x)])) > 2) & (length(unique(x[!is.na(x)])) < 8)) {
    output = TRUE
  } else {
    output = FALSE
  }
  return(output)
}

detect_char_binary <- function (x) {
  if (is.character(x) & (length(unique(x[!is.na(x)])) == 2)) {
    output = TRUE
  } else {
    output = FALSE
  }
  return(output)
}

detect_numeric <- function(x) {
  if (is.numeric(x)) {
    if (sum(x %% 1, na.rm = TRUE) != 0) {
      output = TRUE
    } else {
      output = FALSE
    }
  } else {
    output = FALSE
  }
  return(output)
}


get_prop_df <- function(x, ego_id) {

  denom <- length(x)

  prop_df <- data.frame(val = as.character(x)) %>%
    dplyr::group_by(.data$val) %>%
    dplyr::summarize(prop = dplyr::n()/denom) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ego_id = ego_id) %>%
    dplyr::select(.data$ego_id, dplyr::everything())

  return(prop_df)

}


get_count_df <- function(x, ego_id) {

  denom <- length(x)

  count_df <- data.frame(cat = as.character(x)) %>%
    dplyr::group_by(.data$cat) %>%
    dplyr::summarize(val = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ego_id = ego_id,
                  type = "count") %>%
    dplyr::select(.data$ego_id, .data$cat, .data$type, dplyr::everything())

  prop_df <- count_df %>%
    dplyr::mutate(val = .data$val/denom,
                  type = "prop")

  comb_df <- dplyr::bind_rows(count_df, prop_df) %>%
    dplyr::mutate(name = paste(.data$type, .data$cat, sep = "_")) %>%
    dplyr::select(-.data$cat, -.data$type) %>%
    dplyr::select(.data$ego_id, .data$name, .data$val)

  return(comb_df)

}


##### NOTES
# When `id` in the outputted alter list is `NA`, it's because alters were in a network
# in which ego nominated them, but no alters were tied to one another in the alter-alter edgelist.
# We may need to revisit if we even want this column in the output

# In igraph list, ego->alter edge attributes are stored as a node property of the alter.
# I think this is defensible.




