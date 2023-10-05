#####################################################
#    L A R G E S T   W E A K   C O M P O N E N T    #
#####################################################

largest_weak_component_igraph <- function(g){
  # Isolating the graph's components
  components <- igraph::clusters(g, mode="weak")
  biggest_cluster_id <- which.max(components$csize)

  # Extracting the ids of the largest component
  largest_component_ids <- igraph::V(g)[components$membership == biggest_cluster_id]

  # Extracting Subgraph
  largest_component <- igraph::induced_subgraph(g, largest_component_ids)

  # Assigning the ID list and Subgraph to the Global Environment
  # assign(x = 'weak_membership', value = weak_membership,.GlobalEnv)
  assign(x = 'largest_component_ids', value = largest_component_ids,.GlobalEnv)
  assign(x = 'largest_component', value = largest_component,.GlobalEnv)
}



###############################################
#    L A R G E S T   B I C O M P O N E N T    #
###############################################

largest_bicomponent_igraph <- function(g) {
  # Extracting bi-components
  bi_components <- igraph::biconnected_components(g)
  bi_component_list <- as.list(bi_components$components)
  bi_lengths <- unlist(lapply(bi_component_list, function(x) length(x)))
  bi_lengths <- cbind(as.data.frame(seq(1, length(bi_lengths), 1)), bi_lengths)
  colnames(bi_lengths) <- c('list_id', 'length')
  largest_id <- bi_lengths[(bi_lengths$length == max(bi_lengths$length)), 1]

  # Handling in the event that there's more than one maximally-size bicomponent
  if (length(largest_id) == 1) {

    largest_bicomp_ids <- sort(bi_component_list[[largest_id]])
    rm(bi_components, bi_component_list, bi_lengths, largest_id)

    # Extracting Subgraph
    largest_bi_component <- igraph::induced_subgraph(g, largest_bicomp_ids)

    bicomponent_df <- data.frame(id = largest_bicomp_ids$name,
                                 in_largest_bicomponent = TRUE)
    merge_df <- data.frame(id = igraph::V(g)$name)
    merge_df <- dplyr::left_join(merge_df, bicomponent_df, by = "id")
    merge_df$id <- as.numeric(merge_df$id)

    # Get Bicomponent summary info
    bicomponent_summary <- data.frame(num_bicomponents = 1,
                                      size_bicomponent = sum(merge_df$in_largest_bicomponent, na.rm = T),
                                      prop_bicomponent = sum(merge_df$in_largest_bicomponent, na.rm = T)/nrow(merge_df))

    assign(x = "bicomponent_summary", value = bicomponent_summary, .GlobalEnv)
    assign(x = "largest_bicomponent_memberships", value = merge_df, .GlobalEnv)
    rm(bicomponent_df, merge_df)


    # Assigning the ID list and Subgraph to the Global Environment
    assign(x = 'largest_bicomponent_ids', value = largest_bicomp_ids,.GlobalEnv)
    assign(x = 'largest_bi_component', value = largest_bi_component,.GlobalEnv)

  } else {

    bicomponent_df <- data.frame()

    bicomponent_summary <- data.frame()

    # Make a list for storing all largest bicomponents
    largest_bi_component <- list()

    for (i in 1:length(largest_id)) {

      # Get nodes in this bicomponent
      largest_bicomp_ids <- sort(bi_component_list[[largest_id[[i]]]])

      bicomp_id_merge <- data.frame(id = largest_bicomp_ids$name,
                                    in_largest_bicomponent = TRUE,
                                    bicomponent_id = largest_id[[i]])

      if (i == 1) {

        sum_df <- data.frame(num_bicomponents = length(largest_id),
                             size_bicomponent = sum(bicomp_id_merge$in_largest_bicomponent, na.rm = T),
                             prop_bicomponent = sum(bicomp_id_merge$in_largest_bicomponent, na.rm = T)/length(igraph::V(g)$name))

        bicomponent_summary <- rbind(bicomponent_summary, sum_df)

      }


      bicomponent_df <- rbind(bicomponent_df, bicomp_id_merge)

      largest_bi_component[[i]] <- igraph::induced_subgraph(g, largest_bicomp_ids)

      assign(x = paste('largest_bicomponent_ids', i, sep = "_"), value = largest_bicomp_ids,.GlobalEnv)
 #      assign(x = paste('largest_bi_component', i, sep = "_"), value = largest_bi_component,.GlobalEnv)


    }

    assign(x = 'largest_bi_component', value = largest_bi_component,.GlobalEnv)

    bicomponent_df2 <- data.frame()

    for (i in 1:length(unique(bicomponent_df$id))) {

      this_node <- bicomponent_df[bicomponent_df$id == unique(bicomponent_df$id)[[i]], ]
      this_node <- data.frame(id = unique(bicomponent_df$id)[[i]],
                              in_largest_bicomponent = TRUE,
                              bicomponent_id = paste(sort(this_node$bicomponent_id), collapse = ", "))

      bicomponent_df2 <- rbind(bicomponent_df2, this_node)



    }


    merge_df <- data.frame(id = igraph::V(g)$name)
    merge_df <- dplyr::left_join(merge_df, bicomponent_df2, by = "id")
    merge_df$id <- as.numeric(merge_df$id)

    assign(x = "bicomponent_summary", value = bicomponent_summary, .GlobalEnv)
    assign(x = "largest_bicomponent_memberships", value = merge_df, .GlobalEnv)

    rm(bi_components, bi_component_list, bi_lengths, largest_id, this_node, bicomponent_df, bicomponent_df2, merge_df, sum_df)


  }
}



###########################################
#    T R A N S I T I V I T Y   R A T E    #
###########################################

trans_rate_igraph <- function(g) {
  # Specifying matrix power operator
  pow = function(x, n) {
    if (n == 0) {
      I <- diag(length(diag(x)))
      return(I)
    }
    Reduce(`%*%`, replicate(n, x, simplify = FALSE))
  }

  # Extracting graph's adjacency matrix
  inmat <- as.matrix(igraph::as_adjacency_matrix(g))

  # Binarize
  inmat[inmat > 1] <- 1

  # Remove Self-Loops
  inmat[row(inmat) == col(inmat)] <- 0

  # ij cell = number of two paths from i to j
  i2 <- pow(inmat, 2)

  # Elementwise multiply by adjacency, will = 1 if two path is transitive
  t3 <- sum(i2*inmat)

  # Sum of two-paths, minus diagonal
  a3 <- sum(i2[row(i2) != col(i2)])

  # Calculate the proportion
  tv <- t3/a3

  # Assigning transitivity_rate to the global environment
  assign(x = 'transitivity_rate', value = tv,.GlobalEnv)
  rm(inmat, i2, t3, a3)
}



#################################################
#    D E G R E E   A S S O R T A T I V I T Y    #
#################################################

assortativity_degree <- function(edges, g) {
  # Extracting the graph's edgelist
  edges <- as.data.frame(edges)

  # Calculating the total degree for each node
  node_degree <- sna::degree(g, gmode=gmode, cmode='freeman', ignore.eval=TRUE)
  node_degree <- as.data.frame(cbind(seq(1, length(node_degree), 1), node_degree))

  # Joining i & j ids
  colnames(node_degree)[[1]] <- colnames(edges)[[3]]
  edges <- dplyr::left_join(edges, node_degree, by=colnames(edges)[[3]])
  colnames(edges)[[7]] <- c('i_degree')

  colnames(node_degree)[[1]] <- colnames(edges)[[5]]
  edges <- dplyr::left_join(edges, node_degree, by=colnames(edges)[[5]])
  colnames(edges)[[8]] <- c('j_degree')
  rm(node_degree)

  # Calculating the Pearson Correlation of i and j degree variables
  degree_assortatvity <- stats::cor(edges$i_degree, edges$j_degree, method='pearson')

  # Assigning correlation value to the global environment
  assign(x = 'degree_assortatvity', value = degree_assortatvity,.GlobalEnv)
}



#########################################
#    A V E R A G E   G E O D E S I C    #
#########################################

average_geodesic <- function(g) {
  # Generating the number and lengths of all geodesics between all nodes
  gd <- sna::geodist(g, count.paths = FALSE)

  # Extracting the distances
  geodesics <- gd$gdist
  geodesics <- geodesics[(lower.tri(geodesics))]

  # Replacing infinite values with 0 for the purposes of calculating the average
  geodesics <- geodesics[!is.infinite(geodesics)]

  # Calculating the average shortest path length
  average_path_length <- mean(geodesics)

  # Assgining to the global environment
  assign(x = 'average_path_length', value = average_path_length,.GlobalEnv)
  rm(gd, geodesics)
}



#############################################################
#    M U L T I P L E X   E D G E   C O R R E L A T I O N    #
#############################################################

multiplex_edge_corr_igraph <- function(edgelist, directed, weight_type, type) {
  if('type' %in% colnames(edgelist)){
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
      pairs <- t(combn(paste0(types,'_','weight'), 2))
      for(i in nrow(pairs)) {
        column_set <- pairs[i,]
        tie_set <- ties[,column_set]
        multiplex_edge_correlation <- paste0('Edge Correlation for ', paste(column_set, collapse= ' and '), ': ', round(stats::cor(tie_set)[1,2], digits=2))
        rm(column_set, tie_set)
      }
      rm(pairs, types, subnets, ties)
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
      pairs <- t(combn(paste0(types,'_','weight'), 2))
      for(i in nrow(pairs)) {
        column_set <- pairs[i,]
        tie_set <- ties[,column_set]
        multiplex_edge_correlation <- paste0('Edge Correlation for ', paste(column_set, collapse= ' and '), ': ', round(stats::cor(tie_set)[1,2], digits=2))
        rm(column_set, tie_set)
      }
      rm(pairs, types, subnets, ties)
    }

    # Assigning final scores to global environment
    assign(x = 'multiplex_edge_correlation', value = multiplex_edge_correlation,.GlobalEnv)
  }else{
    edgelist <- edgelist[,]
    multiplex_edge_correlation <- 'Simplex Network'
  }
}



#######################################
#    K - C O R E   C O H E S I O N    #
#######################################


k_cohesion <- function(graph) {

  # 1. Convert to undirected
  # Give warning that graph will be converted to undirected
  if (igraph::is_directed(graph) == TRUE) {
    base::warning("Graph will be treated as undirected for calculation of k-core cohesion measure.")
    graph <- igraph::as.undirected(graph)
  }

  # 2. Simplify graph (remove multiple edges)
  graph <- igraph::simplify(graph, remove.multiple = T, remove.loops = T)

  # 3. Get k-coreness value for all nodes
  v1_cores <- data.frame(V1 = names(igraph::coreness(graph)),
                         core1 = igraph::coreness(graph))
  v2_cores <- data.frame(V2 = names(igraph::coreness(graph)),
                         core2 = igraph::coreness(graph))

  # 4. Get edgelist
  k_edges <- as.data.frame(igraph::get.edgelist(graph, names = TRUE))
  k_edges$V1 <- as.character(k_edges$V1)
  k_edges$V2 <- as.character(k_edges$V2)

  # 5. Merge node-level coreness values into edgelist
  k_edges <- k_edges %>%
    dplyr::left_join(v1_cores, by = "V1") %>%
    dplyr::left_join(v2_cores, by = "V2") %>%
    # 6. Assign lower nodel-level coreness value as edge-level
    # coreness value
    dplyr::mutate(core3 = dplyr::case_when(core1 <= core2 ~ core1,
                                           TRUE ~ core2))

  # 7. Get number of zeroes (absent edges)
  num_nodes <- length(igraph::V(graph))
  #### Number of possible edges in graph
  num_possible <- (num_nodes*(num_nodes-1))/2
  #### Number of zeroes we'll need for calculation
  #### (`num_possible` - num_edges)
  num_zeros <- num_possible - length(k_edges)
  ### Now calculate cohesion measure
  k_cohesion <- sum(k_edges$core3, rep(0, num_zeros))/num_possible

  return(k_cohesion)

}
