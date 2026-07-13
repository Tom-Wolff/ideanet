bi_freeman <- function(x, mode, var) {

  # Create data frame storing output
  return_df <- data.frame()

  if (mode == 1) {
    this_network <- x$mode1$network
  } else {
    this_network <- x$mode2$network
  }

    # Get unique weight values
    unique_weights <- sort(unique(igraph::E(this_network)$weight))

    for (i in unique_weights) {
      filter_graph <- igraph::delete_edges(this_network, edges = which(igraph::E(this_network)$weight < i))

      return_df <- dplyr::bind_rows(return_df,
                                    data.frame(weight = i,
                                               density = igraph::edge_density(filter_graph),
                                               freeman = netseg::freeman(filter_graph, vattr = var)))
    }

    return(return_df)

  }


# What if we just did the average h-index of nodes in one mode?

bi_hindex <- function(x, var) {
  edgelist <- x$full_graph$edgelist
  nodes <- x$full_graph$node_measures
  ##### Limit nodelist to just the nodes that have the variable for which
  ##### we want to calculate h-indices
  nodes <- nodes[!is.na(nodes[,var]),]
  ##### Which mode do these nodes belong to?
  which_mode <- unique(nodes$mode)
  ##### Determine which mode is needed for merging
  if (which_mode == 1) {
    mode_merge <- data.frame(mode1 = nodes$id,
                             var = nodes[,var])
    ##### Merge into edgelist and calculate h-index values
    h_indices <- edgelist %>%
      dplyr::left_join(mode_merge, by = "mode1") %>%
      dplyr::group_by(.data$mode2) %>%
      dplyr::summarize(h_index = single_h_index(var)) %>%
      dplyr::ungroup()

  } else {
    mode_merge <- data.frame(mode2 = nodes$id,
                             var = nodes[,var])
    ##### Merge into edgelist
    h_indices <- edgelist %>%
      dplyr::left_join(mode_merge, by = "mode2") %>%
      dplyr::group_by(.data$mode1) %>%
      dplyr::summarize(h_index = single_h_index(var)) %>%
      dplyr::ungroup()
  }

  ##### Get mean h-index value
  mean_h <- mean(h_indices$h_index)

  # Return list of output
  return(list(mean = mean_h,
              each_node = h_indices))
}

# What if for weighted networks you multiplied each case by the number of weights
# so that ties with heavier weights matter more in the calculation? I think
# this is defensible for the venue attendance example (assuming representative sample,
# shows how what the composition would be like on a given day).

# Do we want a minimum number of nodes per venue? Worth considering, since venues
# with only one node are going to be completely segregated by default.
##### Alternatively, what if we weighted each node's h-index score by the proportion
##### of ties in the network is makes up. Is that defensible?
