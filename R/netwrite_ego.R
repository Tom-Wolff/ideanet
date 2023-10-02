#' Ego Network Cleaning and Measure Calculation (`ego_netwrite`)
#'
#' @description The `ego_netwrite` function reads in data pertaining to ego networks and processes them into a set of standardized outputs, including measures commonly calculated for ego networks. `ego_netwrite` is also designed to process data produced by Network Canvas, a popular tool for egocentric data capture.
#'
#' @param egos A data frame containing measures of ego attributes.
#' @param ego_id A vector of unique identifiers corresponding to each ego, or a single character value indicating the name of the column in `egos` containing ego identifiers.
#' @param alters A data frame containing measures of alter attributes.
#' @param alter_id A vector of identifiers indicating which alter is associated with a given row in `alters`, or a single character value indicating the name of the column in `alters` containing alter identifiers.
#' @param alter_ego A vector of identifiers indicating which ego is associated with a given alter, or a single character value indicating the name of the column in `alters` containing ego identifiers.
#' @param alter_alter A data frame containing an edgelist indicataing ties between alters in each ego's network. This edgelist is optional, but `ego_netwrite` wil not provide certain measures without it.
#' @param aa_ego A vector of identifiers indicating which ego is associated with a given tie between alters, or a single character indicating the name of the column in `alter_alter` containing ego identifiers.
#' @param i_elements A vector of identifiers indicating which alter is on one end of an alter-alter tie, or a single character indicating the name of the column in `alter_alter` containing these identifiers.
#' @param j_elements A vector of identifiers indicating which alter is on the other end of an alter-alter tie, or a single character indicating the name of the column in `alter_alter` containing these identifiers.
#' @param missing_code A numeric value indicating "missing" values in the alter-alter edgelist.
#' @param na.rm A logical value indicating whether `NA` values should be excluded when calculating continuous measures.
#' @param output_name A character value indicating the name or prefix that should be given to output objects.
#' @param network_canvas_path If using data from Network Canvas, a character value indicating the directory in which Network Canvas CSVs are located. `ego_netwrite` will automatically read in all CSV files located in this directory and process them.
#' @param cat.to.factor When using Network Canvas data, a logical vaue indicating whether categorical variables, originally stored as a series of TRUE/FALSE columns, should be converted into a single factor column.
#' @param egor A logical value indicating whether output should include an `egor` object, which is often useful for visualizaton and for simulation larger networks from egocentric data.
#' @param egor_design If creating an `egor` object, a list of arguments to `srvyr::as_survey_design()` specifying the sampling design for egos. This argument corresponds to `ego_design` in `egor::egor`.
#' @param egor_alter_design If creating an `egor` object, the maximum number of alters than an ego can nominate. This argument corresponds to `alter_design` in `egor::egor`.
#'
#' @return `ego_netwrite` returns several data frames, one containing measures of ego attributes, another containing measures of alter attributes and network position, a third containing the alter-alter edgelist (when applicable), a fourth containing summary measures for each individual ego network, and a fifth providing summary measures for the overall dataset. Additionally, `ego_netwrite`  returns a list of igraph objects constructed for each individual ego network, as well as an `egor` object for the overall dataset if desired.
#'
#' @export


ego_netwrite <- function(egos,
                         ego_id,
                         alters,
                         alter_id = NULL,
                         alter_ego = NULL,
                         alter_alter = NULL,
                         aa_ego = NULL,
                         i_elements = NULL,
                         j_elements = NULL,
                         missing_code = 99999,

                         # Do we remove NA values when calculating continuous measures?
                         na.rm = FALSE,

                         # Name for output objects
                         output_name = "egonet",

                         # Network canvas reading
                         network_canvas_path = NULL,
                         cat.to.factor = TRUE,

                         # Egor compatibility
                         egor = FALSE,
                         egor_design = NULL,
                         egor_alter_design = list(max = Inf)) {

  if (!is.null(network_canvas_path)) {

    nc_list <- nc_read(path = network_canvas_path,
                       cat.to.factor = cat.to.factor,
                       ideanet = TRUE)

    egos <- nc_list$egos
    alters <- nc_list$alters
    alter_alter <- nc_list$alter_edgelists

    # In case tibbles are created, convert to basic data frames
    egos <- as.data.frame(egos)
    alters <- as.data.frame(alters)
    alter_alter <- as.data.frame(alter_alter) %>%
      dplyr::rename(i_elements = from,
                    j_elements = to)

    ego_id <- egos$ego_id


    # If users are loading Network Canvas data, the `nc_read` function will already
    # process these data in a way that's compatible with the rest of `ego_netwrite`.
    # As such, the next section can be skipped if working with NC data
  } else {

    ################################################################################
    # Basic Formatting
    ################################################################################
    # In case tibbles are entered, convert to basic data frames
    egos <- as.data.frame(egos)
    alters <- as.data.frame(alters)

    if (!is.null(alter_alter)) {
      alter_alter <- as.data.frame(alter_alter)
    }


    # Indictors for renaming objects
    ego_id_fix <- FALSE
    alter_ego_fix <- FALSE
    alter_id_fix <- FALSE
    aa_ego_fix <- FALSE
    i_elements_fix <- FALSE
    j_elements_fix <- FALSE

    # If ID columns are specified by character values, extract those columns

    if (class(ego_id) == "character" & length(ego_id) == 1) {
      ego_id1 <- egos[,(which(colnames(egos) == ego_id))]
      ego_id_fix <- TRUE
    }

    if (is.null(alter_ego) == TRUE) {
      alter_ego1 <- alters[,(which(colnames(alters) == ego_id))]
      alter_ego_fix <- TRUE
    } else if (class(alter_ego) == "character" & length(alter_ego) == 1) {
      alter_ego1 <- alters[,(which(colnames(alters) == alter_ego))]
      alter_ego_fix <- TRUE
    }

    if (class(alter_id) == "character" & length(alter_id) == 1) {
      alter_id1 <- alters[,(which(colnames(alters) == alter_id))]
      alter_id_fix <- TRUE
    }

    if (!is.null(alter_alter)) {

      if (is.null(aa_ego) == TRUE) {
        aa_ego1 <- alter_alter[,(which(colnames(alter_alter) == aa_ego))]
        aa_ego_fix <- TRUE
      } else if (class(aa_ego) == "character" & length(aa_ego) == 1) {
        aa_ego1 <- alter_alter[,(which(colnames(alters) == aa_ego))]
        aa_ego_fix <- TRUE
      }

      if (class(i_elements) == "character" & length(i_elements) == 1) {
        i_elements1 <- alter_alter[,(which(colnames(alter_alter) == i_elements))]
        i_elements_fix <- TRUE
      }

      if (class(j_elements) == "character" & length(j_elements) == 1) {
        j_elements1 <- alter_alter[,(which(colnames(alter_alter) == j_elements))]
        j_elements_fix <- TRUE
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

    alters <- alters %>%
      dplyr::select(ego_id, alter_id, dplyr::everything())

    if (!is.null(alter_alter)) {
      alter_alter$i_elements <- i_elements
      alter_alter$j_elements <- j_elements
      alter_alter$ego_id <- aa_ego
      alter_alter <- alter_alter %>%
        dplyr::select(ego_id, i_elements, j_elements, dplyr::everything())
    }

  }

  ################################################################################
  # Creating list of igraph objects for each ego network
  ################################################################################

  if (!is.null(alter_alter)) {

    # Get unique values of `ego_id`
    ego_ids <- unique(ego_id)

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
      this_ego_info <- egos[ego_id == this_ego,]

      # this_alters <- alters[alters$ego_id == this_ego, ]
      # this_alter_id <- alter_id[alter_ego == this_ego]
      # this_alters$alter_id <- this_alter_id

      this_alters <- alters[alters$ego_id == this_ego, ]
      # Need a bit of finagling if ego is an isolation (makes zero nominations)
      if (nrow(this_alters) == 0) {
        this_alters <- alters[1,]
        this_alters$ego_id <- this_ego
        this_alters[1, 2:ncol(this_alters)] <- NA
      }

      this_alter_alter <- alter_alter[alter_alter$ego_id == this_ego, ]


      # If an ego nominate alters with no connections to each other,
      # no need to create an igraph object
      if (nrow(this_alter_alter) == 0) {
        # Store in list of igraph objects
        igraph_list[[i]] <- list(ego = this_ego,
                                 ego_info = this_ego_info,
                                 igraph = NA)

        # Need to add column `id` to `alters` to make compatible for merging in
        # final output
        this_alters$id <- NA

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
          dplyr::select(id, alter_id, ego_id, dplyr::everything())

        # If there's a variable in `this_alters` called `name`, it'll mess up igraph
        # processing. Because of this, we have to rename

        if ("name" %in% colnames(this_alters)) {
          colnames(this_alters) <- stringr::str_replace_all(colnames(this_alters), "^name$", "alter_name")
        }

        this_alter_alter <- this_alter_alter %>%
          dplyr::left_join(aa_i_merge, by = "i_elements") %>%
          dplyr::left_join(aa_j_merge, by = "j_elements") %>%
          dplyr::select(i_id, j_id, i_elements, j_elements, ego_id, dplyr::everything())

        # # Make simplified versions of data frames for the purposes of creating these
        # # igraph objects
        # alters_simp <- this_alters %>% dplyr::select(id)
        # aa_simp <- this_alter_alter %>% dplyr::select(i_elements, j_elements)
        #
        # this_igraph <- igraph::graph_from_data_frame(aa_simp, vertices = alters_simp,
        #                                              directed = FALSE)


        this_igraph <- igraph::graph_from_data_frame(this_alter_alter, vertices = this_alters,
                                                     directed = FALSE)

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
          dplyr::select(ego_id, i_elements, i_id, j_elements, j_id, dplyr::everything())

        # Storing updated alter edgelist
        if (class(alter_alter_output) == "character") {
          if (alter_alter_output == "to_populate") {
            alter_alter_output <- this_alter_alter
          }
        } else {
          alter_alter_output <- dplyr::bind_rows(alter_alter_output, this_alter_alter)
        }

      }

      # Reorder columns of alters list for final output
      this_alters <- this_alters %>%
        dplyr::select(ego_id, id, alter_id, dplyr::everything())

      # Storing updated alters list
      if (class(alters_output) == "character") {
        if (alters_output == "to_populate") {
          alters_output <- this_alters
        }
      } else {
        alters_output <- dplyr::bind_rows(alters_output, this_alters)
      }
    }

    # Get centrality measures for alters in each ego network and add to
    # `alters_output`
    alter_cent <- dplyr::bind_rows(lapply(igraph_list, alter_centrality))

    alters_output <- dplyr::left_join(alters_output, alter_cent, by = c("ego_id", "id"))

    # Add Obs_ID column to alter edgelist object
    alter_alter_output$Obs_ID <- 1:nrow(alter_alter_output)
    alter_alter_output <- alter_alter_output %>%
      dplyr::select(Obs_ID, dplyr::everything())

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

    attr_agnostic <- dplyr::bind_rows(lapply(igraph_list, igraph_apply))

  }

  ################################################################################
  # NETWORK-LEVEL, ATTRIBUTE-SPECIFIC MEASURES
  ################################################################################

  #### For this, I imagine the plan would be to take the alters data frame,
  #### develop a set of functions that's applied to columns of a specific class,
  #### then use dplyr::group_by(ego_id) %>% dplyr::summarize_all() on these
  #### columns. I've developed an example with the H index measure of diversity
  #### to give you (Gabe) a template to work with.

  # # Debugging reference; ignore for now; delete later
  # df <- read.csv("~/Desktop/netwrite_debug/test_alter_df.csv")
  # df[df==""] <- NA
  # df$factor <- as.factor(df$factor)
  # df$factor_na <- as.factor(df$factor_na)
  # df$ordered <- as.ordered(df$factor)
  # df$ordered_na <- as.ordered(df$factor_na)

  alter_classes <- sapply(alters, class)

  #### Criteria for certain functions:
  ###### CATEGORICAL MEASURES are assumed IF:
  ############# a. Variables are factors (but not ordered)
  ############# b. Variables are characters with some finite set of unique values
  #############    I'm thinking more than 2 but less than 8. I'm willing to negotiate
  #############    on this point.
  ############# c. Integers with some finite set of unique values matching the
  #############    criteria specified in b.

  # Compile which columns should be treated as categorical measures
  alter_integers_cat <- unlist(lapply(alters, FUN = detect_integer_cat))
  alter_character_cat <- unlist(lapply(alters, FUN = detect_char_cat))
  alter_factors <- unlist(lapply(alter_classes, FUN = function(x) ("factor" %in% x) & !("ordered" %in% x)))
  cat_vars <- as.logical(alter_integers_cat + alter_character_cat + alter_factors)
  if (sum(cat_vars) > 0) {
    ### First value of `cat_measures`, corresponding to `ego_id`, also needs to be TRUE
    cat_vars[[1]] <- TRUE
    ### But `alter_id`, the second value, needs to be FALSE
    cat_vars[[2]] <- FALSE
    # Now extract from alters (NOTE: REPLACE `df` with `alters` ONCE FINISHED)
    cat_df <- alters[,cat_vars]


    # Gabe suggested counts and proportions for categorical variable values
    for (i in 1:length(unique(cat_df$ego_id))) {
      this_ego <- unique(cat_df$ego_id)[[i]]
      # Remove `ego_id` before using `lapply`
      this_cat <- cat_df %>%
        dplyr::filter(ego_id == this_ego) %>%
        dplyr::select(-ego_id)

      counts_applied <- lapply(this_cat, get_count_df, ego_id = this_ego)

      # Extract variable names to paste
      rep_nums <- unlist(lapply(counts_applied, nrow))
      cat_names <- colnames(this_cat)

      counts_applied <- dplyr::bind_rows(counts_applied)

      counts_applied$name <- paste(rep(cat_names, times = rep_nums), counts_applied$name, sep = "_")

      if (i == 1) {
        counts_df <- counts_applied
      } else {
        counts_df <- dplyr::bind_rows(counts_df, counts_applied)
      }

    }

    counts_df <- counts_df %>%
      tidyr::pivot_wider(names_from = "name",
                         values_from = "val")

    counts_df[is.na(counts_df)] <- 0




    cat_measures <- cat_df %>%
      dplyr::group_by(ego_id) %>%
      dplyr::summarise_all(.funs = list(
        h_index = ~ h_index(.),
        iqv = ~ iqv_index(.)) # add function assigned to categorical variables.
      ) %>%
      dplyr::ungroup() %>%

      dplyr::left_join(counts_df, by = "ego_id")
  } else {
    cat_measures <- NULL
  }

  ###### BINARY MEASURES are assumed IF:
  ############# a. Variables are logicals (duh)
  ############# b. Variables are characters with two unique values
  ############# c. Integers with two unique values

  alter_logicals <- unlist(lapply(alter_classes, FUN = function(x) "logical" %in% x))
  alter_integer_binary <- unlist(lapply(alters, FUN = detect_integer_binary))
  alter_char_binary <- unlist(lapply(alters, FUN = detect_char_binary))
  bin_vars <- as.logical(alter_logicals + alter_integer_binary + alter_char_binary)
  if (sum(bin_vars) > 0) {

    ### First value of `cat_measures`, corresponding to `ego_id`, also needs to be TRUE
    bin_vars[[1]] <- TRUE
    ### But `alter_id`, the second value, needs to be FALSE
    bin_vars[[2]] <- FALSE
    # Now extract from alters (NOTE: REPLACE `df` with `alters` ONCE FINISHED)
    bin_df <- alters[,bin_vars]

    for (i in 1:length(unique(bin_df$ego_id))) {
      this_ego <- unique(bin_df$ego_id)[[i]]
      # Remove `ego_id` before using `lapply`
      this_bin <- bin_df %>%
        dplyr::filter(ego_id == this_ego) %>%
        dplyr::select(-ego_id)

      props_applied <- lapply(this_bin, get_prop_df, ego_id = this_ego)

      # Extract variable names to paste
      rep_nums <- unlist(lapply(props_applied, nrow))
      bin_names <- colnames(this_bin)

      props_applied <- dplyr::bind_rows(props_applied)

      props_applied$val <- paste(rep(bin_names, times = rep_nums), props_applied$val, sep = "_")

      if (i == 1) {
        props_df <- props_applied
      } else {
        props_df <- dplyr::bind_rows(props_df, props_applied)
      }

    }

    props_df <- props_df %>%
      tidyr::pivot_wider(names_from = "val",
                         values_from = "prop",
                         names_prefix = "prop_")

    props_df[is.na(props_df)] <- 0

    # If we don't add anything beyond just proportions, we can just leave it at this
    bin_measures <- props_df



    #
    # bin_measures <- bin_df %>%
    #   dplyr::group_by(ego_id) %>%
    #   dplyr::summarize_all(.funs = list(prop1 = ~ get_prop(., out = 1),
    #                                     prop2 = ~ get_prop(., out = 2),
    #                                     propNA = ~ get_prop(., out = 3))
    #   )
  } else {
    bin_measures <- NULL
  }

  ###### CONTINUOUS MEASURES are assumed IF:
  ############# a. Variables are numerics that include non-integer values
  ############# b. Variables are numerics with 3+ unique values

  alter_numeric <- unlist(lapply(alters, FUN = detect_numeric))
  alter_integers <- unlist(lapply(alters, FUN = detect_integer))
  cont_vars <- as.logical(alter_numeric + alter_integers)
  if (sum(cont_vars) > 0) {
    ### First value of `cat_measures`, corresponding to `ego_id`, also needs to be TRUE
    cont_vars[[1]] <- TRUE
    ### But `alter_id`, the second value, needs to be FALSE
    cont_vars[[2]] <- FALSE
    # Now extract from alters (NOTE: REPLACE `df` with `alters` ONCE FINISHED)
    cont_df <- alters[,cont_vars]

    cont_measures <- cont_df %>%
      dplyr::group_by(ego_id) %>%
      dplyr::summarize_all(.funs = list(mean = ~ mean(., na.rm = na.rm),
                                        sd = ~ sd(., na.rm = na.rm)))

  } else {
    cont_measures <- NULL
  }

  # Merge together
  attr_specific <- data.frame(ego_id = egos$ego_id)

  if (!is.null(cat_measures)) {
    attr_specific <- dplyr::left_join(attr_specific, cat_measures, by = "ego_id")
  }

  if (!is.null(cont_measures)) {
    attr_specific <- dplyr::left_join(attr_specific, cont_measures, by = "ego_id")
  }

  if (!is.null(bin_measures)) {
    attr_specific <- dplyr::left_join(attr_specific, bin_measures, by = "ego_id")
  }

  ### Merge in `attr_agnostic` if applicable
  if (!is.null(alter_alter)) {
    egonet_summaries <- attr_agnostic %>%
      dplyr::left_join(attr_specific, by = "ego_id")
  } else {
    egonet_summaries <- attr_specific
  }

  ################################################################################
  # Dataset-level summary
  ################################################################################

  # Define variable descriptions
  summary_names <- c("num_egos",
                     "num_alters",
                     "num_isolates",
                     "min_net_size",
                     "max_net_size",
                     "avg_net_size",
                     "avg_density",
                     "avg_fragmentation")
  summary_titles <- c("Number of egos/ego networks",
                      "Number of alters",
                      "Number of isolates",
                      "Smallest network size",
                      "Largest network size",
                      "Average network size",
                      "Average network density",
                      "Average fragmentation")
  summary_descriptions <- c("Total number of egos providing ego networks in dataset",
                            "Total number of alters nominated by egos across entire dataset",
                            "Number of egos who did not report any alters in their personal network",
                            "Smallest number of alters provided by a single ego",
                            "Largest number of alters provided by a single ego",
                            "Average number of alters provded by a single ego",
                            "The average density of personal networks provided by egos (networks with zero alters excluded from calculation)",
                            "The mean fragmentation index score of personal networks provided by egos (networks with zero alters excluded from calculation)")

  # Combine into single dataframe
  summary_merge <- data.frame(var_name = summary_names,
                              measure_labels = summary_titles,
                              measure_descriptions = summary_descriptions)

  # If we have an alter-alter edgelist, this gets us what we need

  if (!is.null(alter_alter)) {

    # 1. Get unique list of egos
    summary_df <- data.frame(ego_id = egos$ego_id)
    # 2. Merge in ego-level network summaries, selecting only the variables we need
    summary_df <- summary_df %>%
      dplyr::left_join(egonet_summaries, by = "ego_id") %>%
      dplyr::select(ego_id, network_size, mean_degree, density, fragmentation_index) %>%
      # Handle egos who nominate zero alters
      dplyr::mutate(network_size = ifelse(is.na(network_size), 0, network_size)) %>%
      # 3. Create summary measures
      dplyr::summarize(num_egos =          as.character(dplyr::n()),
                       num_alters =        as.character(sum(network_size, na.rm = TRUE)),
                       num_isolates =      as.character(sum(network_size == 0)),
                       min_net_size =      as.character(min(network_size)),
                       max_net_size =      as.character(max(network_size, na.rm = TRUE)),
                       avg_net_size =      as.character(mean(network_size, na.rm = TRUE)),
                       avg_density =       as.character(mean(density, na.rm = TRUE)),
                       avg_fragmentation = as.character(mean(fragmentation_index, na.rm = TRUE)))
    # 4. Transpose
    summary_t <- as.data.frame(t(summary_df))
    summary_t$var_name <- rownames(summary_t)
    colnames(summary_t) <- c("measures", "var_name")
    # 5. Merge into `summary_merge`
    summary_merge <- summary_merge %>%
      dplyr::left_join(summary_t, by = "var_name") %>%
      dplyr::select(-var_name)

  } else {

    # 1. Get unique list of egos
    summary_df <- data.frame(ego_id = egos$ego_id)
    summary_df <- data.frame(ego_id = 0:4)
    # 2. Get network sizes from alters dataframe
    net_sizes <- alters_output %>%
      dplyr::group_by(ego_id) %>%
      dplyr::summarize(network_size = dplyr::n()) %>%
      dplyr::ungroup()
    summary_df <- summary_df %>%
      dplyr::left_join(net_sizes, by = "ego_id") %>%
      dplyr::mutate(network_size = ifelse(is.na(network_size), 0, network_size)) %>%
      dplyr::summarize(num_egos =          as.character(dplyr::n()),
                       num_alters =        as.character(sum(network_size, na.rm = TRUE)),
                       num_isolates =      as.character(sum(network_size == 0)),
                       min_net_size =      as.character(min(network_size)),
                       max_net_size =      as.character(max(network_size)),
                       avg_net_size =      as.character(mean(network_size, na.rm = TRUE)))
    # 3. Transpose
    summary_t <- as.data.frame(t(summary_df))
    summary_t$var_name <- rownames(summary_t)
    colnames(summary_t) <- c("measures", "var_name")
    # 4. Merge into `summary_merge`
    summary_merge <- summary_t %>%
      dplyr::left_join(summary_merge, by = "var_name") %>%
      dplyr::select(-var_name) %>%
      dplyr::select(measure_labels, measure_descriptions, measures)
  }




  ################################################################################
  # Assigning output to the global environment
  ################################################################################


  assign(x = paste(output_name, "_egos", sep = ""), value = egos, .GlobalEnv)
  assign(x = paste(output_name, "_alters", sep = ""), value = alters_output, .GlobalEnv)

  assign(x = paste(output_name, "_summaries", sep = ""), value = egonet_summaries, .GlobalEnv)
  assign(x = paste(output_name, "_overall_summary", sep = ""), value = summary_merge, .GlobalEnv)

  if (!is.null(alter_alter)) {

    assign(x = paste(output_name, "_alter_edgelist", sep = ""), value = alter_alter_output, .GlobalEnv)

    assign(x = paste(output_name, "_igraph", sep = ""), value = igraph_list, .GlobalEnv)
  }

  ################################################################################
  # Egor object creation
  ################################################################################


  if (egor == TRUE) {

    # 1. Rename columns to reflect egor formatting. `egor` is flexible with column
    # names in theory, but in practice using other names can result in bugs when
    # trying to incorporate survey weights.
    egos_egor <- egos %>%
      dplyr::rename(.egoID = ego_id)
    alters_egor <- alters %>%
      dplyr::rename(.altID = alter_id,
                    .egoID = ego_id)

    if (!is.null(alter_alter)) {
      alter_alter_egor <- alter_alter %>%
        dplyr::rename(.egoID = ego_id,
                      .srcID = i_elements,
                      .tgtID = j_elements)
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
                              alter_design = egor_alter_design)

    assign(x = paste(output_name, "_egor", sep = ""), value = egor_object, .GlobalEnv)

  }

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


# H-index for ego network diversity of categorical measures
h_index <- function(x) {
  # Get total number of values in `x`
  num_vals <- length(x)

  # To deal with treating `NAs` as their own category, let's rely on `dplyr`
  h_df <- data.frame(value = x) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(prop_sq = (dplyr::n()/num_vals)^2) %>%
    dplyr::ungroup()

  if (NA %in% h_df$value) {
    base::warning("NA values detected. NA will be treated as its own category when calculating H-index.")
  }

  h_val <- 1-sum(h_df$prop_sq)
  # Return output
  return(h_val)
}

# Normalized H_index
iqv_index <- function(x) {

  num_vals <- length(x)
  num_distinct_vals <- length(unique(x))

  # To deal with treating `NAs` as their own category, let's rely on `dplyr`
  h_df <- data.frame(value = x) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(prop_sq = (dplyr::n()/num_vals)^2) %>%
    dplyr::ungroup()

  if (NA %in% h_df$value) {
    base::warning("NA values detected. NA will be treated as its own category when calculating IQV")
  }

  h_val <- 1-sum(h_df$prop_sq)
  iqv <- h_val / (1 - (1/num_distinct_vals))
  return(iqv)
}

# In theory you could use `mean` to get proportions for categorical variables,
# but there are cases where you have `NA` values that you want included in the
# denominator, thus making `mean(x, na.rm = TRUE)` unreliable. This function
# gets you proportions for binary measures while avoiding that issue.
# get_prop <- function(x, out = 1) {
#   # prop <- sum(x, na.rm = TRUE)/length(x)
#   # return(prop)
#
#   denom <- length(x)
#
#   prop_df <- data.frame(val = x) %>%
#     dplyr::group_by(val) %>%
#     dplyr::summarize(prop = dplyr::n()/denom) %>%
#     dplyr::ungroup()
#
#   # If there's an NA value in the mix, extract it and remove from `prop_df`
#   if (NA %in% prop_df$val) {
#     na_there <- TRUE
#     prop_na <- prop_df %>%
#       dplyr::filter(is.na(val))
#     prop_df <- prop_df %>%
#       dplyr::filter(!is.na(val))
#   } else {
#     na_there <- FALSE
#   }
#
#   if (out == 1) {
#     output <- unname(unlist(prop_df[1, 2]))
#   } else if (out == 2) {
#     output <- unname(unlist(prop_df[2, 2]))
#   } else {
#     if (na_there == TRUE) {
#       output <- unname(unlist(prop_na$prop))
#     } else {
#       output <- 0
#     }
#   }
#
#   # Sometimes you'll get an NA value, go ahead and replace with 0
#   if (is.na(output) == TRUE) {
#     output <- 0
#   }
#
#   return(output)
#
# }

get_prop_df <- function(x, ego_id) {

  denom <- length(x)

  prop_df <- data.frame(val = as.character(x)) %>%
    dplyr::group_by(val) %>%
    dplyr::summarize(prop = dplyr::n()/denom) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ego_id = ego_id) %>%
    dplyr::select(ego_id, dplyr::everything())

  return(prop_df)

}


get_count_df <- function(x, ego_id) {

  denom <- length(x)

  count_df <- data.frame(cat = as.character(x)) %>%
    dplyr::group_by(cat) %>%
    dplyr::summarize(val = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ego_id = ego_id,
                  type = "count") %>%
    dplyr::select(ego_id, cat, type, dplyr::everything())

  prop_df <- count_df %>%
    dplyr::mutate(val = val/denom,
                  type = "prop")

  comb_df <- dplyr::bind_rows(count_df, prop_df) %>%
    dplyr::mutate(name = paste(type, cat, sep = "_")) %>%
    dplyr::select(-cat, -type) %>%
    dplyr::select(ego_id, name, val)

  return(comb_df)

}



fragmentation_index <- function(x) {

  # Note: igraph::distances gives a matrix of paths, is this what we need?
  # TURN TO 1/O for REACHABILITY AND MAKE IT WORK
  # Get distance matrix
  t <- igraph::distances(x)
  # Remove `inf` values
  t[is.infinite(t)] <- 0
  # Binarize
  t <- t > 0
  # Make diagonal NA
  diag(t) <- NA
  # Take the mean
  out <- mean(t, na.rm = TRUE)

  # t <- intergraph::asNetwork(x)
  # t <- sna::reachability(t)
  # diag(t) <- 0
  # out <- mean(t)

  return(out)

}

igraph_apply <- function(x) {

  if ("igraph" %in% class(x$igraph)) {

  frag_index <- fragmentation_index(x$igraph)

  graph_summary <- data.frame(ego_id = x$ego,
                              network_size = igraph::gorder(x$igraph),
                              mean_degree = mean(igraph::degree(x$igraph)),
                              density = igraph::edge_density(x$igraph),
                              num_weakcomponent = igraph::count_components(x$igraph, mode = "weak"),
                              num_strongcomponent = igraph::count_components(x$igraph, mode = "strong"),
                              component_ratio = (igraph::count_components(x$igraph, mode = "weak") - 1) / (igraph::gorder(x$igraph) - 1),
                              fragmentation_index = frag_index,
                              effective_size = igraph::gorder(x$igraph) - mean(igraph::degree(x$igraph)),
                              efficiency = (igraph::gorder(x$igraph) - mean(igraph::degree(x$igraph))) / igraph::gorder(x$igraph),
                              constraint = igraph::constraint(x$igraph_ego)[["ego"]],
                              betweenness = igraph::betweenness(x$igraph_ego)[["ego"]],
                              norm_betweenness = igraph::betweenness(x$igraph_ego, normalized = TRUE)[["ego"]])
  } else {
    graph_summary <- data.frame(ego_id = x$ego,
                                network_size = 0,
                                mean_degree = NA,
                                density = NA,
                                num_weakcomponent = NA,
                                num_strongcomponent = NA,
                                component_ratio = NA,
                                fragmentation_index = NA,
                                effective_size = NA,
                                efficiency = NA,
                                constraint = NA,
                                betweenness = NA,
                                norm_betweenness = NA)
  }
}


alter_centrality <- function(x) {

  # If ego is an isolate (no nominated ties)
  if (!("igraph" %in% class(x$igraph))) {
    # Make a dataframe that's merge-compatible but just contains NAs
    out <- data.frame(total_degree = NA,
                      closeness = NA,
                      betweenness_scores = NA,
                      bonpow = NA,
                      bonpow_negative = NA,
                      eigen_centrality = NA,
                      burt_constraint = NA,
                      effective_size = NA,
                      reachability = NA)
    out$ego_id <- x$ego
    out$id <- NA
  } else {

  total_degree <- igraph::degree(x$igraph, mode = "all", loops = FALSE)
  # WEIGHTED DEGREE TBD
  comp_membership <- ideanet:::component_memberships(x$igraph)
  closeness <- ideanet:::closeness_igraph(x$igraph)
  # DO WE NEED EGO IN THIS CALCULATION? CHECK WITH GABE
  betweenness_scores <- ideanet:::betweenness(x$igraph_ego, weights = NULL, directed = FALSE)
  # Remove the final value here, as that's ego's score
  betweenness_scores <- betweenness_scores[-length(betweenness_scores)]
  bonpow <- ideanet:::bonacich_igraph(x$igraph, directed = FALSE, message = TRUE)
  bonpow_negative <- ideanet:::bonacich_igraph(x$igraph, directed = FALSE, bpct = -.75, message = TRUE)
  colnames(bonpow_negative) <- c("bonacich_negative", "bon_centralization_negative")
  eigen_cen <- ideanet:::eigen_igraph(x$igraph, directed = FALSE, message = TRUE)
  constraint <- ideanet:::burt_ch(x$igraph)
  effective_size <- ideanet:::ens(x$igraph)
  reachability <- ideanet:::reachable_igraph(x$igraph, directed = FALSE)

  bon_cent <- bonpow[[2]]
  bonpow <- bonpow[[1]]
  bon_cent_neg <- bonpow_negative[[2]]
  bonpow_negative <- bonpow_negative[[1]]

  # Get ego and alter IDs for merging
  ego_ids <- igraph::V(x$igraph)$ego_id
  alter_ids <- as.numeric(igraph::V(x$igraph)$name)

  # Compile into data frame
  out <- cbind(total_degree, closeness, betweenness_scores,
               bonpow, bonpow_negative, eigen_cen, constraint, effective_size,
               reachability)
  out$ego_id <- ego_ids
  out$id <- alter_ids
}
  return(out)

}


