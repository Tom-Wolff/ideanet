###############################################
#    N O D E - L E V E L   M E A S U R E S    #
###############################################

node_level_igraph <- function(nodes, g, directed, message, weights,
                              weight_type) {

  # browser()

  # node_start <- Sys.time()

  # Degree Per Jim's specification
  custom_degree <- total_degree(g, directed = directed)

  total_degree <- custom_degree$total_degree_all
  weighted_degree <- igraph::strength(g, mode='all', loops=FALSE)
  comp_membership <- component_memberships(g)

  # degree_time <- Sys.time()


  if(directed == TRUE){

    in_degree <- custom_degree$total_degree_in
    out_degree <- custom_degree$total_degree_out
    weighted_indegree <- igraph::strength(g, mode='in', loops=FALSE)
    weighted_outdegree <- igraph::strength(g, mode='out', loops=FALSE)
    # Closeness has three elements now
    closeness_in <- closeness_igraph(g, directed = TRUE, weight_type = weight_type)$closeness_in
    closeness_out <- closeness_igraph(g, directed = TRUE, weight_type = weight_type)$closeness_out
    closeness_undirected <- closeness_igraph(g, directed = TRUE, weight_type = weight_type)$closeness_un
    # closeness_time <- Sys.time()
    betweenness_scores <- betweenness(g, weights, directed, weight_type = weight_type)
    # betweenness_time <- Sys.time()
    # bonpow <- igraph::bonpow(g, loops=FALSE, exponent = 0.75)
    bonpow <- bonacich_igraph(g, directed=as.logical(directed),
                              message = message)
    bonpow_negative <- bonacich_igraph(g, directed = as.logical(directed), bpct = -.75,
                                       message = message)
    colnames(bonpow_negative) <- c("bonacich_negative", "bon_centralization_negative")
    # bon_time <- Sys.time()
    #eigen_cen <- igraph::eigen_centrality(g, directed=as.logical(directed), scale=FALSE)$vector
    eigen_cen <- eigen_igraph(g, directed = as.logical(directed),
                              message = message)
    # eigen_time <- Sys.time()
    constraint <- burt_ch(g) #, weights = weights)
    effective_size <- ef2(g)
    # burt_time <- Sys.time()
    reachability <- reachable_igraph(g, directed = as.logical(directed))
    # reach_time <- Sys.time()

    if (ncol(bonpow) == 6) {

      bonpow_in <- bonpow[[1]]
      bonpow_out <- bonpow[[3]]
      bonpow_sym <- bonpow[[5]]

      bon_cent_in <- max(bonpow[[2]], na.rm = T)
      bon_cent_out <- max(bonpow[[4]], na.rm = T)
      bon_cent_sym <- max(bonpow[[6]], na.rm = T)

      bonpow_in_negative <- bonpow_negative[[1]]
      bonpow_out_negative <- bonpow_negative[[3]]
      bonpow_sym_negative <- bonpow_negative[[5]]

      bon_cent_in_negative <- max(bonpow_negative[[2]], na.rm = T)
      bon_cent_out_negative <- max(bonpow_negative[[4]], na.rm = T)
      bon_cent_sym_negative <- max(bonpow_negative[[6]], na.rm = T)

      nodes <- as.data.frame(cbind(nodes, comp_membership, total_degree, weighted_degree, in_degree, out_degree,
                                   weighted_indegree, weighted_outdegree,
                                   closeness_in, closeness_out, closeness_undirected,
                                   betweenness_scores,
                                   bonpow_in, bonpow_out, bonpow_sym,
                                   bonpow_in_negative, bonpow_out_negative, bonpow_sym_negative,
                                   eigen_cen, constraint, effective_size, reachability))

      # assign(x = "bon_cent_in", bon_cent_in)
      # assign(x = "bon_cent_out", bon_cent_out)
      # assign(x = "bon_cent_sym", bon_cent_sym)

      # assign(x = "bon_cent_in_negative", bon_cent_in_negative)
      # assign(x = "bon_cent_out_negative", bon_cent_out_negative)
      # assign(x = "bon_cent_sym_negative", bon_cent_sym_negative)


    } else {

      bon_cent <- max(bonpow[[2]], na.rm = T)
      bonpow <- bonpow[[1]]
      bon_cent_neg <- max(bonpow_negative[[2]], na.rm = T)
      bonpow_negative <- bonpow_negative[[1]]

      nodes <- as.data.frame(cbind(nodes, comp_membership, total_degree, weighted_degree, in_degree, out_degree,
                                   weighted_indegree, weighted_outdegree,
                                   closeness_in, closeness_out, closeness_undirected,
                                   betweenness_scores, bonpow, bonpow_negative,
                                   eigen_cen, constraint, effective_size, reachability))

      # assign(x = "bon_cent", bon_cent)
      # assign(x = "bon_cent_neg", bon_cent_neg)

    }


  }else{

    closeness <- closeness_igraph(g, directed = FALSE, weight_type = weight_type)
    # closeness_time <- Sys.time()
    betweenness_scores <- betweenness(g, weights, directed, weight_type = weight_type)
    # betweenness_time <- Sys.time()
    # bonpow <- igraph::bonpow(g, loops=FALSE, exponent = 0.75)
    bonpow <- bonacich_igraph(g, directed=as.logical(directed),
                              message = message)
    bonpow_negative <- bonacich_igraph(g, directed = as.logical(directed), bpct = -.75,
                                       message = message)
    # bon_time <- Sys.time()
    colnames(bonpow_negative) <- c("bonacich_negative", "bon_centralization_negative")
    #eigen_cen <- igraph::eigen_centrality(g, directed=as.logical(directed), scale=FALSE)$vector
    eigen_cen <- eigen_igraph(g, directed = as.logical(directed),
                              message = message)
    # eigen_time <- Sys.time()
    constraint <- burt_ch(g) #, weights = weights)
    effective_size <- ef2(g)
    # burt_time <- Sys.time()
    reachability <- reachable_igraph(g, directed = as.logical(directed))
    # reach_time <- Sys.time()

    bon_cent <- bonpow[[2]]
    bonpow <- bonpow[[1]]
    bon_cent_neg <- bonpow_negative[[2]]
    bonpow_negative <- bonpow_negative[[1]]

    nodes <- as.data.frame(cbind(nodes, comp_membership, total_degree, weighted_degree,
                                 closeness, betweenness_scores, bonpow,
                                 bonpow_negative,
                                 eigen_cen, constraint, effective_size,
                                 reachability))

    # assign(x = "bon_cent", bon_cent)
    # assign(x = "bon_cent_neg", bon_cent_neg)

  }

  # time_vec <- c(degree_time-node_start, closeness_time-degree_time,
  #               betweenness_time-closeness_time, bon_time-betweenness_time,
  #               eigen_time-bon_time, burt_time-eigen_time,
  #               reach_time-burt_time,
  #               reach_time-node_start)
  #
  # time_df <- data.frame(stage = c("Degree", "Closeness",
  #                                 "Betweeness", "Bonacich",
  #                                 "Eigenvector", "Burt Measures", "Reachability", "Total"),
  #                       duration = time_vec)
  #
  # assign("node_measure_times", time_df, .GlobalEnv)

  # Depending on if the network given is weighted or not, the name of the betweenness
  # may appear as either `betweenness` or `betweenness_scores`. We don't want the latter
  # to occur, so we're renaming it here to be safe:
  colnames(nodes)[colnames(nodes) == "betweenness_scores"] <- "betweenness"

  return(nodes)

}




#################################
#    T O T A L   D E G R E E    #
#################################

# Jim wants total degree to be measured as the number of (unique) nodes ego is
# adjacent to, rather than the number of arcs ego is associated with.


total_degree <- function(g,
                         directed = directed) {


  # Extract edgelist from igraph object.
  el1 <- as.data.frame(igraph::get.edgelist(g, names = TRUE))
  colnames(el1) <- c("ego", "alter")
  el1$mode <- "out"
  # Flip edgelist to count inbound ties
  el2 <- data.frame(ego = el1$alter,
                    alter = el1$ego,
                    mode = "in")

  # Combine into a single edgelist
  full_el <- dplyr::bind_rows(el1, el2)
  # Need unique values here to avoid counting duplicate arcs
  full_el <- unique(full_el)

  # Filter out self-loops
  full_el <- full_el %>%
    dplyr::filter(.data$ego != .data$alter)

  # Handling Directed Networks
  if (directed == TRUE) {

    # Group by `ego` and `mode` to get number of nodes ego is tied to for both
    # in and outbound ties
    directed_degree <- full_el %>%
      dplyr::group_by(.data$ego, .data$mode) %>%
      dplyr::summarize(total_degree = dplyr::n()) %>%
      tidyr::pivot_wider(names_from = "mode",
                         names_prefix = "total_degree_",
                         values_from = "total_degree",
                         values_fill = 0) %>%
      dplyr::ungroup()

    # For total degree for both in and outbound ties, we just group by `ego` so as
    # not to double-count alters who both send ties to ego and receive ties from ego
    undirected_degree <- full_el %>%
      dplyr::select(-.data$mode) %>%
      unique() %>%
      dplyr::group_by(.data$ego) %>%
      dplyr::summarize(total_degree_all = dplyr::n()) %>%
      dplyr::ungroup()

    # Merge `directed_degree` and `undirected_degree` together
    tot_degree <- dplyr::full_join(directed_degree, undirected_degree, by = "ego") %>%
      dplyr::rename(id = .data$ego)

  } else {

    tot_degree <- full_el %>%
      dplyr::select(-.data$mode) %>%
      unique() %>%
      dplyr::group_by(.data$ego) %>%
      dplyr::summarize(total_degree_all = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(total_degree_in = NA,
                    total_degree_out = NA) %>%
      dplyr::rename(id = .data$ego)

  }

  # Get full list of node IDs (In case we had isolates)
  final_df <- data.frame(id = igraph::V(g)$name)
  # Merge in total degree counts
  final_df <- dplyr::left_join(final_df, tot_degree, by = "id")
  # Replace `NA` values for zeros to properly assign values to isolates
  final_df[is.na(final_df)] <- 0

  # If undirected network, resent `total_degree_in` and `total_degree_out` as
  # `NA` values
  if (directed == FALSE) {
    final_df$total_degree_in <- NA
    final_df$total_degree_out <- NA
  }


  # Return `final_df` as function output
  return(final_df)

}



###########################
#    C L O S E N E S S    #
###########################

# Create an alternate closeness function
closeness_igraph <- function(g, directed, weight_type){

  # browser()

  # If weights are frequency measures, convert to distances
  if (weight_type == "frequency") {
    geo <- 1/igraph::distances(g, mode='out', weights = 1/igraph::edge_attr(g, "weight"))
  } else {
    geo <- 1/igraph::distances(g, mode='out')
  }

  diag(geo) <- 0 # Define self-ties as 0

  # Jim wants scores normalized (divided by n-1), so let's collect that
  denom <- nrow(geo) - 1


  if (directed == TRUE) {

    # Need to create an undirected version of the network and do the same process above
    g_un <- igraph::as.undirected(g)

    # Handing weights as distances again
    if (weight_type == "frequency") {
      geo2 <- 1/igraph::distances(g_un, mode = "out")
    } else {
      geo2 <- igraph::distances(g_un, mode = "out")
    }


    diag(geo2) <- 0

    closeness_list <- list(closeness_out = rowSums(geo)/denom,
                           closeness_in = colSums(geo)/denom,
                           closeness_un = rowSums(geo2)/denom)
    return(closeness_list)
  } else {
    closeness <- rowSums(geo)/denom
    return(closeness)
  }
}



#############################
#    B E T W E E N E S S    #
#############################

betweenness <- function(g, weights, directed, weight_type){

  # browser()

  # Binarizing Network if Weights Used
  if (!is.null(weights) == TRUE){
    # Binarizing Graph: Getting Nodes
    nodes <- as.data.frame(1:length(igraph::V(g)))
    colnames(nodes) <- c('id')

    # Binarizing Graph: Getting edges
    edges <- as.data.frame(igraph::as_edgelist(g, names=FALSE))

    # Creating Binarized Graph
    b_g <- igraph::graph_from_data_frame(d = edges[c(1,2)], directed = as.logical(directed), vertices = nodes$id)

    # Calculating Betweenness on Binarized Graph
    b_betweenness <- igraph::betweenness(b_g, directed=as.logical(directed))
  }else{
    g <- g
  }

  # Calculating Betweenness
  if(is.null(weights) == TRUE){
    betweenness <- igraph::betweenness(g, directed=as.logical(directed), weights=NULL)
  }else{

    # Making sure edge weights are being treated as distances
    if (weight_type == "frequency") {
      betweenness <- igraph::betweenness(g, directed=as.logical(directed),
                                         weights = 1/igraph::edge_attr(g, "weight"))
    } else {
      betweenness <- igraph::betweenness(g, directed=as.logical(directed),
                                         weights = igraph::edge_attr(g, "weight"))
    }


  }

  # Creating Output Matrix
  if(!is.null(weights) == TRUE){
    betweenness <- as.data.frame(cbind(b_betweenness, betweenness))
    colnames(betweenness) <- c('binarized_betweenness', 'betweenness')
  }else{
    betweenness <- betweenness
  }

  # Returning Betweenness Scores
  return(betweenness)
}



#######################################
#    W E I G H T E D   D E G R E E    #
#######################################

total_weighted_degree <- function(nodes, edges){
  # Isolating node_ids
  node_ids <- sort(unique(nodes$id))
  node_weights <- vector('numeric', length(node_ids))

  # Isolating node acting as ego and as an alter
  for(i in seq_along(node_weights)){
    ego <- edges[(edges[,3] == node_ids[[i]]), ]
    alter <- edges[(edges[,5] == node_ids[[i]]), ]
    node_edges <- rbind(ego, alter)
    node_weights[[i]] <- sum(node_edges[,6])
    rm(ego, alter, node_edges)
  }

  # Return node_weights
  return(node_weights)
}



#################################
#    R E A C H A B I L I T Y    #
#################################

reachable_igraph <- function(g, directed){
  # Isolating the node's ego-network, the number of reachable nodes, and calculating
  # the proportion of the total

  # Get number of nodes
  num_nodes <- length(igraph::V(g))

  # Get edgelist
  edgelist <- igraph::get.edgelist(g, names = F)

  # Remove self-loops
  edgelist <- edgelist[edgelist[,1] != edgelist[,2], ]

  if(directed == TRUE){
    proportion_reachable_in <- vector('numeric', num_nodes)
    proportion_reachable_out <- vector('numeric', num_nodes)
    proportion_reachable_all <- vector('numeric', num_nodes)

    for(i in seq_along(proportion_reachable_in)){


      # We have to subtract 1 from the numerator because `igraph` counts ego itself
      # in the set of reacable nodes. For similar reasons, we also need to subtract 1 from the denominator

      proportion_reachable_in[[i]] <- (length(igraph::subcomponent(g, v = i, mode = "in")) - 1)/(num_nodes-1)
      proportion_reachable_out[[i]] <- (length(igraph::subcomponent(g, v = i, mode = "out")) - 1)/(num_nodes-1)
      proportion_reachable_all[[i]] <- (length(igraph::subcomponent(g, v = i, mode = "all")) - 1)/(num_nodes-1)


    }

    proportion_reachable <- cbind(proportion_reachable_in,
                                  proportion_reachable_out,
                                  proportion_reachable_all)


  }else{
    proportion_reachable <- vector('numeric', num_nodes)
    for(i in seq_along(proportion_reachable)){
      # Isolating connected vertices

      # Calculating the proportion reachable
      proportion_reachable[[i]]  <- (length(igraph::subcomponent(g, v = i, mode = "all")) - 1)/(num_nodes-1)
      #  rm(ego_net)
    }


  }

  # Writing to global environment
  # assign(x = 'reachability', value = proportion_reachable,.GlobalEnv)
  return(proportion_reachable)

}


#########################################################
#    H I E R A R C H Y   A N D   C O N S T R A I N T    #
#########################################################

# burt_ch_o <- function(g) {
#
#   #, weights = FALSE) {
#
#   # if (weights[[1]] != FALSE) {
#   #   adj <- as.matrix(igraph::get.adjacency(g, attr = "weight"))
#   # } else {
#   #   adj <- as.matrix(igraph::get.adjacency(g))
#   # }
#
#   adj <- as.matrix(igraph::get.adjacency(g))
#
#   # See if this is a weighted matrix
#   weighted <- max(adj, na.rm = TRUE) > 1
#
#   # Symmetrize the matrix
#   adj <- adj + t(adj)
#
#   # If not weighted, binarize the symmetrized matrix
#   if (weighted == FALSE) {
#     adj <- adj >= 1
#   }
#
#
#   # Calculate ego's degree
#   degree <- rowSums(adj)
#   # Give people with degree of zero a temporary degree of 1
#   degree0 <- ifelse(degree == 0, 1, degree)
#
#   p <- adj/degree0
#   pp <- p%*%p * (adj>0)
#   c <- (p+pp)^2
#   constv <- rowSums(c)
#
#   cn <-  constv/degree0
#   cn <- ifelse(is.nan(cn), 0, cn)
#   cn <- ifelse(is.infinite(cn), 0, cn)
#
#
#   cij_cn <- c/cn
#   cij_cn <- ifelse(is.nan(cij_cn), NA, cij_cn)
#   cij_cn <- ifelse(cij_cn == 0, NA, cij_cn)
#
#   clnc <- ifelse(cij_cn > 0, cij_cn * log(cij_cn), 0)
#   h_num <- rowSums(clnc, na.rm = TRUE)
#   h_den <- degree0*log(degree0)
#
#   hier <- h_num/h_den
#
#   # Fix scores for isolates and pendants
#   hier[degree == 1] <- 1 # Match UCINet, if degree = 1, hier = 1,
#   hier[degree == 0] <- NA # Undo fake degree change, make isolates NA
#   constv[degree == 1] <- 1 # Match UCINet, if degree = 1, fully constrained
#   constv[degree == 0] <- 1 # Match UCINet, if degree = 0, fully constrained
#
#
#
#   output <- data.frame(burt_constraint = constv,
#                        burt_hierarchy = hier)
#
#   return(output)
#
# }

burt_ch <- function(g) {

  # If network lacks weight attribute, set weights to 1
  if (is.null(igraph::edge_attr(g, "weight"))) {
    igraph::E(g)$weight <- 1
  }

  adj <- igraph::as_adjacency_matrix(g, attr = "weight")

  # See if this is a weighted matrix
  weighted <- max(adj, na.rm = TRUE) > 1

  # Symmetrize the matrix
  adj <- adj + Matrix::t(adj)

  # If not weighted, binarize the symmetrized matrix
  if (weighted == FALSE) {
    adj <- adj >= 1
  }


  # Calculate ego's degree
  degree <- Matrix::rowSums(adj)
  # Give people with degree of zero a temporary degree of 1
  degree0 <- ifelse(degree == 0, 1, degree)

  p <- adj/degree0
  pp <- p%*%p * (adj>0)
  c <- (p+pp)^2
  constv <- Matrix::rowSums(c)

  cn <-  constv/degree0
  cn <- ifelse(is.nan(cn), 0, cn)
  cn <- ifelse(is.infinite(cn), 0, cn)

  cij_cn <- c/cn
  cij_cn[is.nan(cij_cn)] <- NA
  cij_cn[cij_cn == 0] <- NA

  clnc <- cij_cn * log(cij_cn)
  clnc[cij_cn <= 0] <- 0

  h_num <- Matrix::rowSums(clnc, na.rm = TRUE)
  h_den <- degree0*log(degree0)

  hier <- h_num/h_den

  # Fix scores for isolates and pendants
  hier[degree == 1] <- 1 # Match UCINet, if degree = 1, hier = 1,
  hier[degree == 0] <- NA # Undo fake degree change, make isolates NA
  constv[degree == 1] <- 1 # Match UCINet, if degree = 1, fully constrained
  constv[degree == 0] <- 1 # Match UCINet, if degree = 0, fully constrained



  output <- data.frame(burt_constraint = constv,
                       burt_hierarchy = hier)

  return(output)

}



###################################################
#    B U R T ' S   E F F E C T I V E   S I Z E    #
###################################################

# Using the Borgatti simplification formula here
# ef2_o <- function(g) {
#
#   # Convert igraph object to adjmat
#   mat <- Matrix::as.matrix(igraph::as_adjacency_matrix(igraph::as.undirected(g)))
#
#   deg <- rowSums(mat)
#   redun <- rep(0, nrow(mat))
#   mat <- mat - diag(diag(mat))
#   for (i in 1:nrow(mat)) {
#     if (deg[i] > 0) {
#       egoi <- which(mat[i, ] > 0)
#       d <- length(egoi)
#       submat <- mat[egoi, egoi]
#       t <- sum(submat)
#       redun[i] <- t / d
#     }
#   }
#   efsize <- deg - redun
#   return(efsize)
# }

ef2 <- function(g) {

  # browser()

  # Convert igraph object to adjmat
  mat <- igraph::as_adjacency_matrix(igraph::as.undirected(g))

  deg <- Matrix::rowSums(mat)
  redun <- rep(0, nrow(mat))
  # mat <- mat - diag(diag(mat))
  dumb_diagonal <- which(row(mat) == col(mat))
  dumb_diagonal2 <- mat[dumb_diagonal]

  mat <- mat - Matrix::Diagonal(x = dumb_diagonal2)

  for (i in 1:nrow(mat)) {
    if (deg[i] > 0) {
      egoi <- which(mat[i, ] > 0)
      d <- length(egoi)
      submat <- mat[egoi, egoi]
      t <- sum(submat)
      redun[i] <- t / d
    }
  }
  efsize <- deg - redun
  return(efsize)
}

#########################
#    B O N A C I C H    #
#########################

#matrix <- igraph::as_adjacency_matrix(bn$network)

# Custom Bonacich Function
# bonacich_o <- function(matrix, bpct = .75) {
#   # Calculate eigenvalues
#   evs <- eigen(matrix)
#
#   # The ones we want are in the first column
#   # Something's weird here -- it's combining the two columns that SAS outputs into a single value
#   # This value is a complex sum. For now, it seems like coercing the complex sum using `as.numeric`
#   # gets us the values we want.
#
#   # Get maximum eigenvalue
#   maxev <- max(as.numeric(evs$value))
#
#   # Get values that go into computation
#   b <- bpct*(1/maxev) # Diameter of power weight
#   n <- nrow(matrix) # Size
#   i <- diag(n) # Identity matrix
#   w <- matrix(rep(1, n), ncol = 1) # Column of 1s
#
#   # For some reason, the output of the `solve` function is the transpose
#   # of the `INV` function in SAS. I'm going to transpose the output of `solve` here
#   # so that results are consistent with what we get in SAS, but this is something
#   # we need to confirm and possibly be wary of.
#
#   # Key equation, this is the centrality score
#   C <- (t(solve(i-b*matrix)))%*%t(matrix)%*%w
#
#   # This is Bonacich normalizing value alpha
#   A <- sqrt(n/(t(C) %*% C))
#
#   # This is the power centrality score
#   cent <- c(A) * c(C)
#
#   # This is the centralization score
#   NBCNT <- sum(max(C) - C)/((n-1)*(n-2))
#
#   # Vector of centralization scores
#   NBCD <- c(matrix(NBCNT, ncol = n))
#
#   # Collect power centrality scores and centralization scores into single dataframe
#   bonacich_output <- data.frame(bonacich = cent, bon_centralization = NBCD)
#
#   return(bonacich_output)
# }

bonacich <- function(matrix, bpct = .75) {

  # browser()

  # If the network consists of only two nodes, the rest of this function will break.
  # `igraph`'s function for Bonacich centrality returns `NaN` values under given such a network.
  # Safe to say if we have two nodes, we can just assign `NA` values.

  if (nrow(matrix) <= 2) {

    bonacich_output <- data.frame(bonacich = rep(NA, nrow(matrix)),
                                  bon_centralization = rep(NA, nrow(matrix)))
  } else {

    # Calculate eigenvalues
    evs <- RSpectra::eigs(matrix, k = 1, which = "LR")

    # The ones we want are in the first column
    # Something's weird here -- it's combining the two columns that SAS outputs into a single value
    # This value is a complex sum. For now, it seems like coercing the complex sum using `as.numeric`
    # gets us the values we want.

    # Get maximum eigenvalue
    maxev <- evs$values

    # Get values that go into computation
    b <- bpct*(1/maxev) # Diameter of power weight
    n <- nrow(matrix) # Size
    i <- Matrix::Diagonal(n) # Identity matrix
    w <- Matrix::Matrix(rep(1, n), ncol = 1) # Column of 1s

    # For some reason, the output of the `solve` function is the transpose
    # of the `INV` function in SAS. I'm going to transpose the output of `solve` here
    # so that results are consistent with what we get in SAS, but this is something
    # we need to confirm and possibly be wary of.

    # Key equation, this is the centrality score
    C <- (Matrix::t(Matrix::solve(i-b*matrix)))%*%Matrix::t(matrix)%*%w

    # This is Bonacich normalizing value alpha
    A <- sqrt(n/(Matrix::t(C) %*% C))

    # This is the power centrality score
    cent <- as.numeric(A) * as.numeric(C)

    # This is the centralization score
    NBCNT <- sum(max(C) - C)/((n-1)*(n-2))

    # Collect power centrality scores and centralization scores into single dataframe
    bonacich_output <- data.frame(bonacich = cent, bon_centralization = NBCNT)

  }

  return(bonacich_output)
}

# Bonacich igraph
bonacich_igraph <- function(g, directed, bpct = .75,
                            message = TRUE) {

  # browser()

  # Store complete nodelist for merging later on
  nodelist <- data.frame(id = igraph::V(g)$name)

  # Detect if isolates are present in the network
  if (0 %in% igraph::degree(g, mode = "all")) {
    # If isolates are present, indicate that isolates are going to be removed
    if (message == TRUE) {
      warning("(Bonacich power centrality) Isolates detected in network. Isolates will be removed from network when calculating power centrality measure, and will be assigned NA values in final output.")
    }
    # Remove isolates
    g <- igraph::delete.vertices(g, v = igraph::degree(g, mode = "all", loops = F) == 0)
  }

  # Convert igraph object into adjacency matrix
  #bon_adjmat <- as.matrix(igraph::get.adjacency(g, type = "both", names = TRUE))
  bon_adjmat <- igraph::get.adjacency(g, type = "both", names = TRUE)

  # We need to ensure that the adjacency matrix used in calculating eigenvectors/values
  # is not singular. The following checks for this. If the adjacency matrix is found
  # to be singular, network will be treated as undirected when calculating EVs
  if (directed == TRUE) {
    # Get generalized inverse of matrix
    # inv_adj <- MASS::ginv(bon_adjmat)

    # singular_check <- round(sum(diag(inv_adj %*% bon_adjmat)))

    ## new check

    is_singular <- abs(Matrix::det(bon_adjmat)) < 1e-8 ## tolerance


    #if (singular_check < nrow(nodelist)) {
    if (isTRUE(is_singular)) {
      if (message == TRUE){
        warning("(Bonacich power centrality) Adjacency matrix for network is singular. Network will be treated as undirected in order to calculate measures\n")
      }
      directed <- FALSE
      g <- igraph::as.undirected(g)
      # Make `bon_adjmat` undirected
      # bon_adjmat <- as.matrix(igraph::get.adjacency(g, type = "both", names = TRUE))
      bon_adjmat <- igraph::get.adjacency(g, type = "both", names = TRUE)
    }
  }

  # When we have a directed network, there are three power centrality scores we can get:
  # An "indegree" one based on the original adjacency matrix, and "outdegree" one based on the
  # transpose of the original adjacency matrix, and a third one based on a symmetrized version
  # of the adjacency matrix. The following conditional flow generates all three measures
  # if netwrite is working with a directed network:
  if (directed == T) {
    # Create symmetrized (undirected) version of network
    undir_net <- igraph::as.undirected(g)

    # Convert undirected network into adjacency matrix
    # bon_sym_mat <- as.matrix(igraph::get.adjacency(undir_net, type = "both", names = TRUE))
    bon_sym_mat <- igraph::get.adjacency(undir_net, type = "both", names = TRUE)

    # We now have everyting we need to get the three versions of Bonacich power centrality
    # First let's get the indegree version
    bonacich_in <- bonacich(matrix = bon_adjmat, bpct = bpct)

    # Update column names
    colnames(bonacich_in) <- paste(colnames(bonacich_in), "_in", sep = "")

    # Next we get the outdegree version (note that we're transposing `bon_adjmat` in the `matrix` argument)
    bonacich_out <- bonacich(matrix = t(bon_adjmat), bpct = bpct)

    # Update column names
    colnames(bonacich_out) <- paste(colnames(bonacich_out), "_out", sep = "")

    # Finally, we get the undirected version from the symmetrized adjacency matrix
    bonacich_sym <- bonacich(matrix = bon_sym_mat, bpct = bpct)

    # Update column names
    colnames(bonacich_sym) <- paste(colnames(bonacich_sym), "_sym", sep = "")

    # Combine into single data frame
    bon_scores <- cbind(bonacich_in, bonacich_out, bonacich_sym)
  }else{
    # If the network is undirected, proceed to get Bonacich power centrality scores on
    # just the base adjacency matrix
    bon_scores <- bonacich(bon_adjmat, bpct = bpct)
  }

  # Add ID variable for merging back into nodelist
  bon_scores$id <- igraph::V(g)$name

  # Merge scores back into nodelist
  nodelist <- dplyr::left_join(nodelist, bon_scores, by = "id")

  # Remove ID variable
  nodelist$id <- NULL

  return(nodelist)
}



#####################################################
#    E I G E N V E C T O R   C E N T R A L I T Y    #
#####################################################

# Custom Function for Calculating Eigenvector Centrality
# eigen_custom_o <- function(matrix) {
#   # To replicate output from SAS, we first need to transpose the adjacency matrix
#   eigen_calc <- eigen(t(matrix))
#
#   # Eigenvector centrality is the eigenvector associated with the largest
#   # eigenvalue. I think by convention this is always the first one, but going to find
#   # it explicitly to be sure:
#
#   # Remove complex sums from eigenvalues
#   evs <- as.numeric(eigen_calc$values)
#
#   # Indicate which is the maximum eigenvalue
#   max_eval <- which(evs == max(evs))
#
#   # If multiple eigenvectors have the maximum eigenvalue,
#   # take only the first one
#   if (length(max_eval) > 1) {
#     max_eval <- max_eval[1]
#   }
#
#   # Now get that column from the eigenvector matrix;
#   # these are the eigenvector centrality scores.
#   # You sometimes get negative values, but that's
#   # not an error. Just take the absoluate value of
#   # negative values
#   eigencent <- abs(as.numeric(eigen_calc$vectors[,max_eval]))
#
#   # Prepare data frame for output
#   eigen_df <- data.frame(eigen_centrality = eigencent)
#   return(eigen_df)
# }

eigen_custom <- function(matrix) {
  # To replicate output from SAS, we first need to transpose the adjacency matrix
  # eigen_calc <- eigen(t(matrix))

  if (nrow(matrix) == 2) {
    eigen_df <- data.frame(eigen_centrality = rep(NA, nrow(matrix)))
  } else {
    eigen <- RSpectra::eigs(Matrix::t(matrix), k = 1, which = "LR")

    # Now get that column from the eigenvector matrix;
    # these are the eigenvector centrality scores.
    # You sometimes get negative values, but that's
    # not an error. Just take the absoluate value of
    # negative values
    # eigencent <- abs(as.numeric(eigen_calc$vectors[,max_eval]))
    eigencent <- abs(as.numeric(eigen$vectors))

    # Prepare data frame for output
    eigen_df <- data.frame(eigen_centrality = eigencent)
  }
  return(eigen_df)
}

# IGRAPH
eigen_igraph <- function(g, directed,
                         message = TRUE){

  # browser()

  # Store complete nodelist for merging later on
  nodelist <- data.frame(id = igraph::V(g)$name)

  # Detect if isolates are present in the network
  if (0 %in% igraph::degree(g, mode = "all")) {
    # If isolates are present, indicate that isolates are going to be removed
    if (message == TRUE){
      warning("(Eigenvector centrality) Isolates detected in network. Isolates will be removed from network when calculating eigenvector centrality measure, and will be assigned NA values in final output.\n")
    }
    # Remove isolates
    g <- igraph::delete.vertices(g, v = igraph::degree(g, mode = "all", loops = F) == 0)
  }

  # Assign component membership as a vertex attribute
  igraph::V(g)$component <- igraph::components(g)$membership

  # Calculate Eigenvector centrality for each component
  # Unique component ids
  unique_components <- unique(igraph::V(g)$component)

  # We need to ensure that the adjacency matrix used in calculating eigenvectors/values
  # is not singular. The following checks for this. If the adjacency matrix is found
  # to be singular, network will be treated as undirected when calculating EVs
  if (directed == TRUE) {
    # Get adjacency matrix
    check_adj <- igraph::as_adjacency_matrix(g, type = "both")
    #
    # # Get generalized inverse of matrix
    # inv_adj <- MASS::ginv(check_adj)
    # singular_check <- round(sum(diag(inv_adj %*% check_adj)))
    is_singular <- abs(Matrix::det(check_adj)) < 1e-8 ## tolerance

    if (isTRUE(is_singular)) {
      if (message == TRUE){
        warning("(Eigenvector centrality) Adjacency matrix for network is singular. Network will be treated as undirected in order to calculate measures\n")
      }
      directed <- FALSE
      g <- igraph::as.undirected(g)
    }
  }

  # Detect if multiple components exist in the network
  if (length(unique_components) > 1) {
    # Outputting message to the user
    if (message == TRUE){
      warning("(Eigenvector centrality) Network consists of 2+ unconnected components. Eigenvector centrality scores will be calculated for nodes based on their position within their respective components. Nodes in components consisting of a single dyad will be assigned NA values in final output.\n")
    }
    # Initialize data frame for storing eigen centrality measures
    eigen_scores <- data.frame()

    # If the network is a directed network
    if (directed == TRUE) {
      # For each component...
      for (i in 1:length(unique_components)) {
        # Make subgraph of component
        subgraph <- igraph::delete.vertices(g, v = igraph::V(g)$component != i)

        # Convert subgraph of component into an adjacency matrix
        sub_adj <- igraph::as_adjacency_matrix(subgraph, type = "both")

        # Make transpose of subgraph adjmat
        sub_adj_t <- Matrix::t(sub_adj)

        # Make undirected version of component
        subgraph_undir <- igraph::as.undirected(subgraph)

        # Convert into adjacency matrix
        undir_mat <- igraph::as_adjacency_matrix(subgraph_undir, type = "both")

        # Get eigenvector centrality measures for indegree
        eigen_in <- eigen_custom(sub_adj)

        # Update names to indicate indegree
        colnames(eigen_in) <- paste(colnames(eigen_in), "_in", sep = "")

        # Get eigenvector centrality measures for outdegree
        eigen_out <- eigen_custom(sub_adj_t)

        # Update names to outdicate outdegree
        colnames(eigen_out) <- paste(colnames(eigen_out), "_out", sep = "")

        # On symmetric matrix
        eigen_sym <- eigen_custom(undir_mat)
        colnames(eigen_sym) <- paste(colnames(eigen_sym), "_sym", sep = "")

        # Combine into single dataframe
        subgraph_scores <- cbind(eigen_in, eigen_out, eigen_sym)

        # Add component indicator
        subgraph_scores$component <- i

        # Add ID variable
        subgraph_scores$id <- igraph::V(subgraph)$name

        # Bind to `eigen_scores` data frame
        eigen_scores <- rbind(eigen_scores, subgraph_scores)
      }

    # End directed network condition
    } else {
      # For each component...
      for (i in 1:length(unique_components)) {
        # Make subgraph of component
        subgraph <- igraph::delete.vertices(g, v = igraph::V(g)$component != i)

        # Convert subgraph of component into an adjacency matrix
        sub_adj <- igraph::as_adjacency_matrix(subgraph, type = "both")

        # If component consists of a single dyad, go ahead and skip
        if (nrow(sub_adj) <= 2) {
          next
        }

        # Get eigenvector centrality measures
        subgraph_scores <- eigen_custom(sub_adj)

        # Add component indicator
        subgraph_scores$component <- i

        # Add ID variable
        subgraph_scores$id <- igraph::V(subgraph)$name

        # Bind to `eigen_scores` data frame
        eigen_scores <- rbind(eigen_scores, subgraph_scores)
      }
    }

  # If network is only a single component
  }else{
    # If the network is a directed network
    if (directed == TRUE) {
      # Convert graph to adjacency matrix
      eigen_adj <- igraph::as_adjacency_matrix(g, type = "both")

      # Make transpose of subgraph adjmat
      eigen_adj_t <- Matrix::t(eigen_adj)

      # Make undirected version of component
      eigen_undir <- igraph::as.undirected(g)

      # Convert into adjacency matrix
      undir_mat <- igraph::as_adjacency_matrix(g, type = "both")

      # Get eigenvector centrality measures for indegree
      eigen_in <- eigen_custom(eigen_adj)

      # Update names to indicate indegree
      colnames(eigen_in) <- paste(colnames(eigen_in), "_in", sep = "")

      # Get eigenvector centrality measures for outdegree
      eigen_out <- eigen_custom(eigen_adj_t)

      # Update names to outdicate outdegree
      colnames(eigen_out) <- paste(colnames(eigen_out), "_out", sep = "")

      # On symmetric matrix
      eigen_sym <- eigen_custom(undir_mat)
      colnames(eigen_sym) <- paste(colnames(eigen_sym), "_sym", sep = "")

      # Combine into single dataframe
      eigen_scores <- cbind(eigen_in, eigen_out, eigen_sym)

      # Add ID variable
      eigen_scores$id <- igraph::V(g)$name
    } else {
      # If the network is undirected...
      # Convert graph to adjacency matrix
      eigen_adj <- igraph::as_adjacency_matrix(g, type = "both")

      # Get Eigenvector centrality measures
      eigen_scores <- eigen_custom(eigen_adj)

      # Add ID variable
      eigen_scores$id <- igraph::V(g)$name
    }
  }

  # There's a rare case in which a network might consist of multiple components,
  # each consisting of no more than a single dyad. `eigen_scores` will be an empty
  # data frame. The below code corrects this, giving `NA` values to all nodes (verify with Jim)
  if (nrow(eigen_scores) == 0) {
    eigen_scores <- data.frame(id = igraph::V(g)$name,
                               eigen_centrality = NA)
  }

  # Merge `eigen_scores` back into nodelist
  nodelist <- dplyr::left_join(nodelist, eigen_scores, by = "id")

  # Remove `id` variable
  nodelist$id <- NULL

  # Return Result
  return(nodelist)
}




#################################################
#    C O M P O N E N T   M E M B E R S H I P    #
#################################################

component_memberships <- function(g) {

  # Get weak component membership
  weak_clusters <- igraph::clusters(g, mode = "weak")

  # Get dataframe of weak component assignments
  weak_membership <- data.frame(id = names(weak_clusters$membership),
                                membership = weak_clusters$membership)

  # In case component IDs aren't automatically sorted by size,
  # we need to manually relabel them:

  weak_ids <- data.frame(membership = 1:length(weak_clusters$csize),
                         size = weak_clusters$csize)

  weak_ids <- weak_ids[order(weak_ids$size, decreasing = TRUE),]

  weak_ids$weak_membership <- 1:nrow(weak_ids)

  weak_membership <- dplyr::left_join(weak_membership, weak_ids, by = "membership")

  # Add indicator of membership in largest component
  weak_membership$in_largest_weak = weak_membership$weak_membership == 1


  # Remove old membership ID column
  weak_membership <- weak_membership[,c("weak_membership", "in_largest_weak")]




  # Get strong component membership
  strong_clusters <- igraph::clusters(g, mode = "strong")

  # Get dataframe of strong component assignments
  strong_membership <- data.frame(id = names(strong_clusters$membership),
                                  membership = strong_clusters$membership)

  # In case component IDs aren't automatically sorted by size,
  # we need to manually relabel them:

  strong_ids <- data.frame(membership = 1:length(strong_clusters$csize),
                           size = strong_clusters$csize)

  strong_ids <- strong_ids[order(strong_ids$size, decreasing = TRUE),]

  strong_ids$strong_membership <- 1:nrow(strong_ids)

  strong_membership <- dplyr::left_join(strong_membership, strong_ids, by = "membership")

  # Add indicator of membership in largest component
  strong_membership$in_largest_strong = strong_membership$strong_membership == 1


  # Remove old membership ID column
  strong_membership <- strong_membership[,c("strong_membership", "in_largest_strong")]


  # Bind Together
  memberships <- cbind(weak_membership, strong_membership)

  return(memberships)

}
