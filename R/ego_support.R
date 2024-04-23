################################################################################
# Function for getting node-level centrality measures for alters
################################################################################

alter_centrality <- function(x, directed) {

  # Two necessary conditions:
  ### 1. Is ego an isolate? If that's the case, no igraph objects would have
  ### been stored for it in `igraph_list`
  ego_isolate <- !("igraph" %in% class(x$igraph))
  ### 2. Does ego have ties but no ties exist between alters?
  if (ego_isolate == FALSE) {
    alters_noties <- length(igraph::E(x$igraph)) == 0
  } else {
    alters_noties <- FALSE
  }

  # If ego is an isolate (no nominated ties)
  if (ego_isolate == TRUE) {

    if (directed == FALSE) {
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
      # Make a dataframe that's merge-compatible but just contains NAs
      out <- data.frame(indegree = NA,
                        outdegree = NA,
                        total_degree = NA,
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
    }

    # Handling if ego is not an isolate but alters have no ties to one another.
    # May need to revisit how we decide on measure values here.
  } else if (alters_noties == TRUE) {

    if (directed == FALSE) {

      # total_degree <- igraph::degree(x$igraph, mode = "all", loops = FALSE)
      # total_degree <- total_degree(x$igraph, directed = FALSE)$total_degree_all
      total_degree <- rep(0, length(igraph::V(x$igraph)))
      # WEIGHTED DEGREE TBD
      comp_membership <- component_memberships(x$igraph)
      closeness <- closeness_igraph(x$igraph, directed = FALSE)
      # DO WE NEED EGO IN THIS CALCULATION? CHECK WITH GABE
      betweenness_scores <- betweenness(x$igraph_ego, weights = NULL, directed = FALSE)
      # Remove the final value here, as that's ego's score
      betweenness_scores <- betweenness_scores[-length(betweenness_scores)]
      eigen_cen <- rep(NA, length(total_degree))
      constraint <- burt_ch(x$igraph)
      # effective_size <- ef2(x$igraph)
      effective_size <- rep(0, length(igraph::V(x$igraph)))
      reachability <- reachable_igraph(x$igraph, directed = FALSE)


      bonpow <- rep(NA, length(total_degree))
      bonpow_negative <- rep(NA, length(total_degree))

      # Get ego and alter IDs for merging
      ego_ids <- igraph::V(x$igraph)$ego_id
      alter_ids <- as.numeric(igraph::V(x$igraph)$name)

      # Compile into data frame
      out <- cbind(total_degree, closeness, betweenness_scores,
                   bonpow, bonpow_negative, eigen_cen, constraint, effective_size,
                   reachability)
      out$ego_id <- ego_ids
      out$id <- alter_ids

    } else {

      # custom_degree <- total_degree(x$igraph, directed = TRUE)

      indegree <- rep(0, length(igraph::V(x$igraph)))
      outdegree <- rep(0, length(igraph::V(x$igraph)))
      total_degree <- rep(0, length(igraph::V(x$igraph)))
      # WEIGHTED DEGREE TBD
      comp_membership <- component_memberships(x$igraph)
      closeness_scores <- closeness_igraph(x$igraph, directed = TRUE)
      closeness_in <- closeness_scores$closeness_in
      closeness_out <- closeness_scores$closeness_out
      closeness_un <- closeness_scores$closeness_un
      # DO WE NEED EGO IN THIS CALCULATION? CHECK WITH GABE
      betweenness_scores <- betweenness(x$igraph_ego, weights = NULL, directed = TRUE)
      # Remove the final value here, as that's ego's score
      betweenness_scores <- betweenness_scores[-length(betweenness_scores)]
      eigen_cen <- rep(NA, length(total_degree))
      constraint <- burt_ch(x$igraph)
      effective_size <- ef2(x$igraph)
      reachability <- reachable_igraph(x$igraph, directed = TRUE)


      bonpow <- rep(NA, length(total_degree))
      bonpow_negative <- rep(NA, length(total_degree))

      # Get ego and alter IDs for merging
      ego_ids <- igraph::V(x$igraph)$ego_id
      alter_ids <- as.numeric(igraph::V(x$igraph)$name)

      # Compile into data frame
      out <- cbind(indegree, outdegree, total_degree,
                   closeness_in, closeness_out, closeness_un,
                   betweenness_scores,
                   bonpow, bonpow_negative, eigen_cen, constraint, effective_size,
                   reachability)
      out$ego_id <- ego_ids
      out$id <- alter_ids
    }

    # Handling if ego is not an isolate and at least one tie exists between two alters
  } else {

    if (directed == FALSE) {

      total_degree <- total_degree(x$igraph, directed = FALSE)$total_degree_all
      # WEIGHTED DEGREE TBD
      comp_membership <- component_memberships(x$igraph)
      closeness <- closeness_igraph(x$igraph, directed = FALSE)
      # DO WE NEED EGO IN THIS CALCULATION? CHECK WITH GABE
      betweenness_scores <- betweenness(x$igraph_ego, weights = NULL, directed = FALSE)
      # Remove the final value here, as that's ego's score
      betweenness_scores <- betweenness_scores[-length(betweenness_scores)]
      bonpow <- bonacich_igraph(x$igraph, directed = FALSE, message = TRUE)
      bonpow_negative <- bonacich_igraph(x$igraph, directed = FALSE, bpct = -.75, message = TRUE)
      colnames(bonpow_negative) <- c("bonacich_negative", "bon_centralization_negative")
      eigen_cen <- eigen_igraph(x$igraph, directed = FALSE, message = TRUE)
      constraint <- burt_ch(x$igraph)
      effective_size <- ef2(x$igraph)
      reachability <- reachable_igraph(x$igraph, directed = FALSE)

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

    } else {

      indegree <- igraph::degree(x$igraph, mode = "in", loops = FALSE)
      outdegree <- igraph::degree(x$igraph, mode = "out", loops = FALSE)
      total_degree <- igraph::degree(x$igraph, mode = "all", loops = FALSE)
      # WEIGHTED DEGREE TBD
      comp_membership <- component_memberships(x$igraph)
      closeness_scores <- closeness_igraph(x$igraph, directed = TRUE)
      closeness_in <- closeness_scores$closeness_in
      closeness_out <- closeness_scores$closeness_out
      closeness_un <- closeness_scores$closeness_un
      # DO WE NEED EGO IN THIS CALCULATION? CHECK WITH GABE
      betweenness_scores <- betweenness(x$igraph_ego, weights = NULL, directed = TRUE)
      # Remove the final value here, as that's ego's score
      betweenness_scores <- betweenness_scores[-length(betweenness_scores)]
      bonpow <- bonacich_igraph(x$igraph, directed = TRUE, message = TRUE)
      bonpow_negative <- bonacich_igraph(x$igraph, directed = TRUE, bpct = -.75, message = TRUE)
      colnames(bonpow_negative) <- c("bonacich_negative", "bon_centralization_negative")
      eigen_cen <- eigen_igraph(x$igraph, directed = TRUE, message = TRUE)
      constraint <- burt_ch(x$igraph)
      effective_size <- ef2(x$igraph)
      reachability <- reachable_igraph(x$igraph, directed = TRUE)

      bon_cent <- bonpow[[2]]
      bonpow <- bonpow[[1]]
      bon_cent_neg <- bonpow_negative[[2]]
      bonpow_negative <- bonpow_negative[[1]]

      # Get ego and alter IDs for merging
      ego_ids <- igraph::V(x$igraph)$ego_id
      alter_ids <- as.numeric(igraph::V(x$igraph)$name)

      # Compile into data frame
      out <- cbind(indegree, outdegree, total_degree,
                   closeness_in, closeness_out, closeness_un,
                   betweenness_scores,
                   bonpow, bonpow_negative, eigen_cen, constraint, effective_size,
                   reachability)
      out$ego_id <- ego_ids
      out$id <- alter_ids
    }


  }
  # Remove extraneous `eigen_cen` column if it appears
  out$eigen_cen <- NULL
  return(out)

}


################################################################################
# Fragmentation Index
################################################################################

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


################################################################################
# Function for calculating network-level summaries
################################################################################

igraph_apply <- function(x, directed) {


  if ("igraph" %in% class(x$igraph)) {

    # FRAGMENTATION INDEX
    frag_index <- fragmentation_index(x$igraph)

    # PAIRWISE REACHABILITY
    if (directed == TRUE) {

      g_undir <- igraph::as.undirected(x$igraph)

      # Pairwise reachability (weak, undirected)
      weak_clusters_un <- igraph::clusters(g_undir, mode = "weak")
      pairwise_weak_un <- sum(weak_clusters_un$csize * (weak_clusters_un$csize-1)) / (length(igraph::V(x$igraph))*(length(igraph::V(x$igraph)) - 1))

      # Pairwise reachability (strong, undirected)
      strong_clusters_un <- igraph::clusters(g_undir, mode = "strong")
      pairwise_strong_un <- sum(strong_clusters_un$csize * (strong_clusters_un$csize-1)) / (length(igraph::V(x$igraph))*(length(igraph::V(x$igraph)) - 1))


      # Pairwise reachability (weak, directed)
      weak_clusters_dir <- igraph::clusters(x$igraph, mode = "weak")
      pairwise_weak_dir <- sum(weak_clusters_dir$csize * (weak_clusters_dir$csize-1)) / (length(igraph::V(x$igraph))*(length(igraph::V(x$igraph)) - 1))

      # Pairwise reachability (strong, directed)
      strong_clusters_dir <- igraph::clusters(x$igraph, mode = "strong")
      pairwise_strong_dir <- sum(strong_clusters_dir$csize * (strong_clusters_dir$csize-1)) / (length(igraph::V(x$igraph))*(length(igraph::V(x$igraph)) - 1))

    } else {

      # Pairwise reachability (weak, undirected)
      weak_clusters_un <- igraph::clusters(x$igraph, mode = "weak")
      pairwise_weak_un <- sum(weak_clusters_un$csize * (weak_clusters_un$csize-1)) / (length(igraph::V(x$igraph))*(length(igraph::V(x$igraph)) - 1))

      # Pairwise reachability (strong, undirected)
      strong_clusters_un <- igraph::clusters(x$igraph, mode = "strong")
      pairwise_strong_un <- sum(strong_clusters_un$csize * (strong_clusters_un$csize-1)) / (length(igraph::V(x$igraph))*(length(igraph::V(x$igraph)) - 1))


      # Pairwise reachability (weak, directed)
      pairwise_weak_dir <- NA

      # Pairwise reachability (strong, directed)
      pairwise_strong_dir <- NA


    }


    graph_summary <- suppressWarnings(data.frame(ego_id = x$ego,
                                                 network_size = igraph::gorder(x$igraph),
                                                 mean_degree = mean(igraph::degree(x$igraph)),
                                                 density = igraph::edge_density(x$igraph),
                                                 num_isolates = sum(igraph::degree(x$igraph) == 0),
                                                 prop_isolates = sum(igraph::degree(x$igraph) == 0)/igraph::gorder(x$igraph),
                                                 num_weakcomponent = igraph::count_components(x$igraph, mode = "weak"),
                                                 size_largest_weakcomponent = max(igraph::components(x$igraph, mode = "weak")$csize, na.rm = T),
                                                 prop_largest_weakcomponent = max(igraph::components(x$igraph, mode = "weak")$csize, na.rm = T)/igraph::gorder(x$igraph),
                                                 num_strongcomponent = igraph::count_components(x$igraph, mode = "strong"),
                                                 size_largest_strongcomponent = max(igraph::components(x$igraph, mode = "strong")$csize, na.rm = T),
                                                 prop_largest_strongcomponent = max(igraph::components(x$igraph, mode = "strong")$csize, na.rm = T)/igraph::gorder(x$igraph),
                                                 component_ratio = (igraph::count_components(x$igraph, mode = "weak") - 1) / (igraph::gorder(x$igraph) - 1),
                                                 pairwise_strong_un = pairwise_strong_un,
                                                 pairwise_weak_un = pairwise_weak_un,
                                                 pairwise_strong_dir = pairwise_strong_dir,
                                                 pairwise_weak_dir = pairwise_weak_dir,
                                                 fragmentation_index = frag_index,
                                                 effective_size = igraph::gorder(x$igraph) - mean(igraph::degree(x$igraph)),
                                                 efficiency = (igraph::gorder(x$igraph) - mean(igraph::degree(x$igraph))) / igraph::gorder(x$igraph),
                                                 constraint = igraph::constraint(x$igraph_ego)[["ego"]],
                                                 betweenness = igraph::betweenness(x$igraph_ego)[["ego"]],
                                                 norm_betweenness = igraph::betweenness(x$igraph_ego, normalized = TRUE)[["ego"]],
                                                 dyad_mut = igraph::dyad_census(x$igraph)$mut,
                                                 dyad_asym = igraph::dyad_census(x$igraph)$asym,
                                                 dyad_null = igraph::dyad_census(x$igraph)$null,
                                                 triad_003 = igraph::triad_census(x$igraph)[[1]],
                                                 triad_012 = igraph::triad_census(x$igraph)[[2]],
                                                 triad_102 = igraph::triad_census(x$igraph)[[3]],
                                                 triad_021D = igraph::triad_census(x$igraph)[[4]],
                                                 triad_021U = igraph::triad_census(x$igraph)[[5]],
                                                 triad_021C = igraph::triad_census(x$igraph)[[6]],
                                                 triad_111D = igraph::triad_census(x$igraph)[[7]],
                                                 triad_111U = igraph::triad_census(x$igraph)[[8]],
                                                 triad_030T = igraph::triad_census(x$igraph)[[9]],
                                                 triad_030C = igraph::triad_census(x$igraph)[[10]],
                                                 triad_201 = igraph::triad_census(x$igraph)[[11]],
                                                 triad_120D = igraph::triad_census(x$igraph)[[12]],
                                                 triad_120U = igraph::triad_census(x$igraph)[[13]],
                                                 triad_120C = igraph::triad_census(x$igraph)[[14]],
                                                 triad_210 = igraph::triad_census(x$igraph)[[15]],
                                                 triad_300 = igraph::triad_census(x$igraph)[[16]]
    ))
  } else {
    graph_summary <- data.frame(ego_id = x$ego,
                                network_size = 0,
                                mean_degree = NA,
                                density = NA,
                                num_isolates = NA,
                                prop_isolates = NA,
                                num_weakcomponent = NA,
                                size_largest_weakcomponent = NA,
                                prop_largest_weakcomponent = NA,
                                num_strongcomponent = NA,
                                size_largest_strongcomponent = NA,
                                prop_largest_strongcomponent = NA,
                                component_ratio = NA,
                                pairwise_strong_un = NA,
                                pairwise_weak_un = NA,
                                pairwise_strong_dir = NA,
                                pairwise_weak_dir = NA,
                                fragmentation_index = NA,
                                effective_size = NA,
                                efficiency = NA,
                                constraint = NA,
                                betweenness = NA,
                                norm_betweenness = NA,
                                dyad_mut =   NA,
                                dyad_asym =  NA,
                                dyad_null =  NA,
                                triad_003 =  NA,
                                triad_012 =  NA,
                                triad_102 =  NA,
                                triad_021D = NA,
                                triad_021U = NA,
                                triad_021C = NA,
                                triad_111D = NA,
                                triad_111U = NA,
                                triad_030T = NA,
                                triad_030C = NA,
                                triad_201 =  NA,
                                triad_120D = NA,
                                triad_120U = NA,
                                triad_120C = NA,
                                triad_210 =  NA,
                                triad_300 =  NA)
  }

}


################################################################################
# Adaptation of Jon's multiplex edge correlation function for use in `ego_netwrite`
################################################################################

multiplex_ego <- function(edgelist, directed, type, weight_type = "frequency") {


  # Creating edgelist to manipulate internally
  edges <- as.data.frame(edgelist[,])

  # Moving back to One-Index for Comparison Purposes
  edges[,3] <- edges[,3] + 1
  edges[,5] <- edges[,5] + 1

  # Recovering original weight for the purposes of comparison
  if(weight_type == 'frequency') {
    edges[,6] <- as.numeric(1/edges[,6])
  }else{
    edges[,6] <- edges[,6]
  }

  # Generating Correlations Either as Directed or Undirected
  if(as.logical(directed) == TRUE) {
    # Generating Sub-Networks Based on Type
    types <- sort(unique(type))
    subnets <- vector('list', length(types))
    names(subnets) <- types
    for(i in seq_along(types)){
      subnets[[i]] <- as.data.frame(edges[(type == types[[i]]), ])
      subnets[[i]] <- subnets[[i]][,c('i_id', 'j_id', 'type', 'weight')]
      colnames(subnets[[i]])[[3]] <- names(subnets)[[i]]
      colnames(subnets[[i]])[[4]] <- paste0(colnames(subnets[[i]])[[3]],'_',colnames(subnets[[i]])[[4]])
    }

    # Creating a Wide Data-Set to Generate Correlations
    ties <- unique(as.data.frame(edges[ ,c("i_id", "j_id")]))
    for(i in seq_along(types)){
      ties <- dplyr::left_join(ties, subnets[[i]], by=c('i_id', 'j_id'))
      ties[is.na(ties)] <- 0
    }

    # Calculating the Correlation for Unique Combination of Types
    pairs <- t(utils::combn(paste0(types,'_','weight'), 2))
    pair_cors <- c()
    for(i in 1:nrow(pairs)) {
      column_set <- pairs[i, ]
      tie_set <- ties[,column_set]
      pair_cors <- suppressWarnings(c(pair_cors, stats::cor(tie_set)[1,2]))
      rm(column_set, tie_set)
    }

    names(pair_cors) <- paste("cor",
                              stringr::str_replace_all(paste(pairs[,1], pairs[,2], sep = "_"),
                                                       pattern = "_weight",
                                                       replacement = ""),
                              sep = "_")

    rm(pairs, types, subnets, ties)
    # End Directed Ties Condition

    # Start Undirected Ties Condition
  }else{
    # Creating a separate edgelist (Symmetric Edges) to Perform Operations
    s_edges <- edges[,c('i_id', 'j_id', 'type', 'weight')]

    # Eliminating Duplicate Pairs
    s_edges <- s_edges[!duplicated(t(apply(s_edges[,c(1:2)], 1, sort))),]

    # Creating Edge Groups & Glossary
    edges_1 <- cbind(s_edges[,c(1,2)], seq(1, dim(s_edges)[[1]], 1))
    colnames(edges_1)[[3]] <- c('edge_group')

    edges_2 <- cbind(s_edges[,c(2,1)], seq(1, dim(s_edges)[[1]], 1))
    colnames(edges_2) <- c('i_id','j_id','edge_group')

    edges_glossary <- rbind(edges_1, edges_2)
    edges_glossary <- edges_glossary[order(edges_glossary$edge_group), ]
    rm(edges_1, edges_2, s_edges)

    # Joining edge_groups to edges
    if('Obs_ID' %in% colnames(edgelist)){
      edges <- edges
    }else{
      edges <- cbind(seq(1, dim(edges)[[1]], 1), edges)
      names(edges)[[1]] <- c('Obs_ID')
    }
    edges <- dplyr::left_join(as.data.frame(edges), edges_glossary, by=c('i_id', 'j_id'))

    # Eliminating Duplicates Caused by Self-Loops
    edges <- edges[!(duplicated(edges$Obs_ID)), ]
    rm(edges_glossary)

    # Collapsing Ties and Summing Weights
    edge_groups <- unique(edges$edge_group)
    ties <- vector('list', length(edge_groups))
    names(ties) <- edge_groups
    for(i in seq_along(edge_groups)) {
      e_group <- edges[(edges$edge_group == edge_groups[[i]]), ]
      row.names(e_group) <- seq(1, nrow(e_group), 1)
      e_types <- unique(e_group$type)
      ties[[i]] <- as.data.frame(e_group$type)
      ties[[i]]$weight <- sum(e_group$weight)
      ties[[i]]$i_id <- e_group[1,3]
      ties[[i]]$j_id <- e_group[1,5]
      colnames(ties[[i]])[[1]] <- c('type')
      ties[[i]] <- ties[[i]][,c(3,4,1,2)]
      rm(e_group, e_types)
    }

    ties <- do.call("rbind", ties)

    # Generating Sub-Networks Based on Type
    types <- sort(unique(type))
    subnets <- vector('list', length(types))
    names(subnets) <- types
    for(i in seq_along(types)){
      subnets[[i]] <- ties[(ties$type == types[[i]]), ]
      colnames(subnets[[i]])[[3]] <- names(subnets)[[i]]
      colnames(subnets[[i]])[[4]] <- paste0(colnames(subnets[[i]])[[3]],'_',colnames(subnets[[i]])[[4]])
    }

    # Creating a Wide Data-Set to Generate Correlations
    ties <- unique(ties[ ,c("i_id", "j_id")])
    for(i in seq_along(types)){
      ties <- dplyr::left_join(ties, subnets[[i]], by=c('i_id', 'j_id'))
      ties[is.na(ties)] <- 0
    }

    # Calculating the Correlation for Unique Combination of Types
    pairs <- t(utils::combn(paste0(types,'_','weight'), 2))
    pair_cors <- c()
    for(i in 1:nrow(pairs)) {
      column_set <- pairs[i, ]
      tie_set <- ties[,column_set]
      pair_cors <- suppressWarnings(c(pair_cors, stats::cor(tie_set)[1,2]))
      rm(column_set, tie_set)
    }

    names(pair_cors) <- paste("cor",
                              stringr::str_replace_all(paste(pairs[,1], pairs[,2], sep = "_"),
                                                       pattern = "_weight",
                                                       replacement = ""),
                              sep = "_")

    rm(pairs, types, subnets, ties)
  }

  # Assigning final scores to global environment
  return(pair_cors)
}
