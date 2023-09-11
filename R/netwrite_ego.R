#' Ego Network Cleaning and Variable Calculation (`ego_netwrite`)
#'
#' @description The `ego_netwrite` function reads in relational data of several formats and processes them into a set of standardized outputs. These outputs include sets of commonly calculated measures at the individual node and network-wide levels.
#'
#' @param ego_data A data frame containing data pertaining to respondents (egos).
#' @param alter_data A data frame containing data pertaining to alters nominated by respondents (egos).
#' @param ego_id A character value indicating the name of the column containing unique identifiers for egos. `ego_id` is used to link `ego_data` and `alter_data`.
#' @param alter_id A character value indicating the name of the column containing unique identifiers for alters nominated by egos.
#' @param format A character value indicating whether `alter_id` is currently in `"long"` or `"wide"` format. If `"wide"`, `netwrite_ego` will reshape the data into long format. Data processed by `netwrite_ego` will always be stored in long format.
#' @param prefix A character value indicating which columns correspond to which individual alter nominated by ego when `alter_id` is in a `"wide"` format. `prefix` should be used when the beginning of column names is used to indicate corresponding alter.
#' @param suffix A character value indicating which columns correspond to which individual alter nominated by ego when `alter_id` is in a `"wide"` format. `suffix` should be used when the end of column names is used to indicate corresponding alter.
#'
#' @return `netwrite` returns a variety of outputs and assigns them to the R global environment. Depending on the values assigned to the `output` argument, `netwrite` will produce any or all of the following:
#'
#' If `output` contains `graph`, `netwrite` will return an igraph object of the network represented in the original data.
#' If a vector is entered into the `type` argument, `netwrite` also produces a list containing igraph objects for each unique relation type as well as the overall network. These output objects are named according to the value specified in the `net_name` argument.
#'
#' If `output` contains `nodelist`, `netwrite` will return a dataframe containing individual-level information for each node in the network. This dataframe contains a set of frequently used node-level measures for each node in the network. If a vector is entered into the `type` argument, `netwrite` will produce these node-level measures for each unique relattion type.
#'
#' If `output` contains `edgelist`, `netwrite` will return a formatted edgelist for the network represented in the original data. If a vector is entered into the `type` argument, `netwrite` also produces a list containing edgelists for each unique relation type as well as the overall network.
#'
#' If `output` contains `system_level_measures`, `netwrite` will return a data frame providing network-level summary information.
#'
#' If `output` contains `node_measure_plot`, `netwrite` will return a plot summarizing the distribution of frequently used node-level measures across all nodes in the network. If a vector is entered into the `type` argument, `netwrite` also produces a list containing node-level summary plots for each unique relation type as well as the overall network.
#'
#' If `output` contains `system_measure_plot`, `netwrite` will return a plot summarizing the distribution of frequently used network-level measures. If a vector is entered into the `type` argument, `netwrite` also produces a list containing network-level summary plots for each unique relation type as well as the overall network.
#'
#' If `output` contains `largest_bi_component`, `netwrite` will return an igraph object of the largest bicomponent in the network represented in the original data. If a vector is entered into the `type` argument, `netwrite` also produces a list containing the largest bicomponent for each unique relation type as well as the overall network.
#'
#' If `output` contains `largest_bi_component`, `netwrite` will return an igraph object of the largest main component in the network represented in the original data. If a vector is entered into the `type` argument, `netwrite` also produces a list containing the largest main component for each unique relation type as well as the overall network.
#'
#' @export
#'
#' @examples
#' # Use netwrite on an edgelist
#' netwrite(nodelist = fauxmesa_nodes,
#'         node_id = "id",
#'         i_elements = fauxmesa_edges$from,
#'         j_elements = fauxmesa_edges$to,
#'         directed = TRUE,
#'         net_name = "faux_mesa")
#'
#' ### Inspect updated edgelist
#' head(edgelist)
#'
#' ### Inspect data frame of node-level measures
#' head(node_measures)
#'
#' ### Inspect system-level summary
#' system_level_measures
#'
#' ### Plot sociogram of network
#' plot(faux_mesa)
#'
#' ### View node-level summary visualization
#' node_measure_plot
#'
#' ### View system-level summary visualization
#' system_measure_plot
#'
#'
#'
#' # Run netwrite on an adjacency matrix
#' fauxmesa_adjmat <- as.matrix(igraph::as_adjacency_matrix(faux_mesa))
#'
#' netwrite(data_type = "adjacency_matrix",
#'         adjacency_matrix = fauxmesa_adjmat,
#'         directed = TRUE,
#'         net_name = "faux_mesa")
#'
#'
#' # Run netwrite on a multirelational network
#' netwrite(i_elements = florentine$node,
#'         j_elements = florentine$target,
#'         type = florentine$layer,
#'         directed = TRUE,
#'         net_name = "florentine")
#'
#' # View system level summary for aggregate network
#' system_level_measures_list$summary_graph
#'
#' # View system level summary for network of `type 1` relations
#' system_level_measures_list$`1`
#'


# NOTES:
# DO WE NEED TO HANDLE A CASE WHERE EGO AND ALTER INFO ARE COMBINED IN A
# SINGLE WIDE DATASET? PROBABLY. FUCK.



ego_netwrite <- function(ego_id,
                         alter_id,
                         ego_data,
                         alter_data,
                         alter_alter,
                         format = "long",

                         # For wide datasets
                         prefix = NULL,
                            # prefix should either be a vector of prefixes
                            # (to handle something like "first_age,"
                            # "second_age", etc.)

                            # Or a regular expression
                            # Or a phrase that's assumed to have a numeric
                            # after it so that we can append a regular expression
                            # to search for numbers after it
                         suffix = NULL) {


# 1. Data Processing/Formatting
  # If `format == "wide"`, need to reshape into long format

  if (format == "long") {

    ### If variables are arranged according to a prefix
    if (!is.null(prefix) == TRUE) {

    }

  }

  # 1. a. Do we need an alter-alter edgelist argument?

  # If we assume

  # 2. Basic Summary Measures
    # This might need to be a separate function so people can
    # specify what they want summaries of

  # 3. Corresponding Summary Visualizations
  #### Should this be a separate function?
  #### Other thing is to have a default set of measures
  #### ubiquitous to ego nets (degree, density if applicable)
      # `egor` has "ego grams", "clustered graphs", regular sociograms


  # Final step, optional export to `egor`


}


# External function for dyadic variables


#
# artnet_wide <- ARTnetData::ARTnet.wide
#
# library(tidyverse)
#
# for_test <- artnet_wide %>%
#   select(AMIS_ID, starts_with("PART")) %>%
#   select(-PART2013)
#
#
#
# # Rename original ID variable
# original_id <- "AMIS_ID"
#
# colnames(for_test)[which(colnames(for_test) == original_id)] <- "original_id"
#
#
#
# # 1. Get unique prefixes
#
# for_test <- for_test %>%
#   group_by(original_id) %>%
#   mutate(ego_id = cur_group_id()) %>%
#   select(original_id, ego_id, everything()) %>%
#   ungroup()
#
# prefixes <- unique(unlist(stringr::str_extract_all(colnames(for_test), prefix)))
#
# for (i in 1:length(prefixes)) {
#
#   this_alter <- for_test %>%
#     select(original_id, ego_id, starts_with(prefixes[[i]])) %>%
#     # Add column indicating this alter
#     mutate(alter = prefixes[[i]]) %>%
#     select(original_id, ego_id, alter, everything())
#
#   # Remove prefix from beginning of column names
#   colnames(this_alter) <- str_remove_all(colnames(this_alter), prefixes[[i]])
#
#   # Some rows won't contain any values due to there not actually being an alter
#   # named. We'll want to remove those rows from `this_alter` to keep things cleaner
#
#   na_vals <- is.na(this_alter)
#   blank_chars <- this_alter == ""
#   blank_chars[is.na(blank_chars)] <- FALSE
#
#   to_remove <- rowSums(na_vals+blank_chars) != ncol(this_alter) - 3
#
#   this_alter <- this_alter[to_remove,]
#
#
#   if (i == 1) {
#     data_long <- this_alter
#   } else {
#     data_long <- rbind(data_long, this_alter)
#   }
#
#
# }
#
# data_long <- data_long %>%
#   arrange(ego_id, alter)
#
#
# artnet_counts <- artnet_long %>% group_by(AMIS_ID) %>%
#   summarize(artnet_count = n()) %>% ungroup()
#
# ideanet_counts <- data_long %>% group_by(original_id) %>%
#   summarize(count = n()) %>%
#   ungroup() %>%
#   rename(AMIS_ID = original_id) %>%
#   full_join(artnet_counts, by = "AMIS_ID")
#
# ideanet_counts$bad <- ideanet_counts$count != ideanet_counts$artnet_count
# sum(ideanet_counts$bad)
#
# just_bad <- ideanet_counts$AMIS_ID[ideanet_counts$bad == TRUE]
#
# bad_long <- data_long %>% filter(original_id %in% just_bad)
#
# for_test$PART2013
#
# View(for_test %>% select(PART2013))

