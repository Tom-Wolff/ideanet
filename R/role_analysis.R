#' Positional (Role) Analysis in Networks (\code{role_analysis})
#'
#' @description The \code{role_analysis} function takes networks processed by the \code{\link{netwrite}} function and performs positional analysis on them. Positional analysis methods allows users to infer distinct "roles" in networks from patterns in network activity. \code{role_analysis} currently supports the identification of roles using two methods: hierarchical clustering (cite) and convergence of correlations (CONCOR, Breiger 1975).
#'
#' @param graph An igraph object or a list of igraph objects produced as output from \code{\link{netwrite}}.
#' @param nodes A data frame containing individual-level network measures for each node in the network. Ideally, the \code{node_measures} data frame produced by \code{\link{netwrite}} should be assigned to this argument.
#' @param directed A logical value indicating whether network edges should be treated as directed.
#' @param method A character value indicating the method used for positional analysis. Valid arguments are currently \code{"cluster"} for hierarchical clustering and \code{"concor"} for CONCOR.
#' @param min_partitions A numeric value indicating the number of minimum number of clusters or partitions to assign to nodes in the network. When using hierarchical clustering, this value reflects the minimum number of clusters produced by analysis. When using CONCOR, this value reflects the minimum number of partitions produced in analysis, such that a value of 1 results in a partitioning of two groups, a value of 2 results in four groups, and so on.
#' @param max_partitions A numeric value indicating the number of maximum number of clusters or partitions to assign to nodes in the network. When using hierarchical clustering, this value reflects the maximum number of clusters produced by analysis. When using CONCOR, this value reflects the maximum number of partitions produced in analysis, such that a value of 1 results in a partitioning of two groups, a value of 2 results in four groups, and so on.
#' @param min_partition_size A numeric value indicating the minimum number of nodes required for inclusion in a cluster. If an inferred cluster or partition contains fewer nodes than the number assigned to \code{min_partition_size}, nodes in this cluster/partition will be labeled as members of a parent cluster/partition.
#' @param backbone A numeric value ranging from 0-1 indicating which edges in the similarity/correlation matrix should be kept when calculating modularity of cluster/partition assignments. When calculating optimal modularity, it helps to backbone the similarity/correlation matrix according to the nth percentile. Larger networks benefit from higher \code{backbone} values, while lower values generally benefit smaller networks.
#' @param viz A logical value indicating whether to produce summary visualizations of the positional analysis.
#' @param fast_triad (Hierarchical clustering method only.) A logical value indicating whether to use a faster method for counting individual nodes' positions in different types of triads. This faster method may lead to memory issues and should be avoided when working with larger networks.
#' @param retain_variables (Hierarchical clustering method only.) A logical value indicating whether output should include a data frame of all node-level measures used in hierarchical clustering.
#' @param cluster_summaries (Hierarchical clustering method only.) A logical value indicating whether output should includde a data frame containing by-cluster mean values of variables used in hierarchical clustering.
#' @param dendro_names (Hierarchical clustering method only.) A logical value indicating whether the cluster dendrogram visualization should display node labels rather than ID numbers.
#' @param self_ties (CONCOR only.) A logical value indicting whether to include self-loops (ties directed toward oneself) in CONCOR calculation.
#' @param cutoff (CONCOR only.) A numeric value ranging from 0 to 1 that indicates the correlation cutoff for detecting convergence in CONCOR calculation.
#' @param max_iter (CONCOR only.) A numeric value indicating the maximum number of iteractions allowed for CONCOR calculattion.
#' @return The \code{role_analysis} returns a list of outputs that users can access to help interpret results. This contents of this list varies somewhat depending on the method being used for positional analysis.
#'
#' When hierarchical clustering is used, the list contains the following:
#' \code{cluster_assignments} is a data frame indicating each node's membership within inferred clusters at each level of partitioning.
#' \code{cluster_sociogram} contains a visualization of the network wherein nodes are colored by their membership within clusters at the optimal level of partitioning.
#' \code{cluster_dendrogram} is a visualization of the dendrogram produced from clustering nodes. Red boxes on the visualization indicate nodes' cluster memberships at the optimal level of partitioning.
#' \code{cluster_modularity} is a visualization of the modularity scores of the matrix of similarity scores between nodes for each level of partitioning. This visualization helps identify the optimal level of partitioning inferred by the \code{role_analysis} function.
#' \code{cluster_summaries_cent} contains one or more visualization representing how clusters inferred at the optimal level of partitioning differ from one another on several important node-level measures.
#' \code{cluster_summaries_triad} contains one or more visualization representing how clusters inferred at the optimal level of partitioning differ from one another on in terms of their positions within certain kinds of triads in the network.
#' \code{cluster_relations_heatmaps} is a list object containing several heatmap visualizations representing the extent to which nodes in one inferred cluster are connected to nodes in another cluster.
#' \code{cluster_relations_sociogram} contains a network visualization representing the extent to which nodes in clusters inferred at the optimal level of partitioning are tied to one another. Nodes in this visualization represent inferred clusters in the aggregate.
#'
#' When CONCOR is used, this list contains the following:
#' \code{concor_assignments} is a data frame indicating each node's membership within inferred blocks at each level of partitioning.
#' \code{concor_sociogram} contains a visualization of the network wherein nodes are colored by their membership within blocks at the optimal level of partitioning.
#' \code{concor_block_tree} is a visualization representing how smaller blocks are derived from larger blocks at each level of partitioning using CONCOR.
#' \code{concor_modularity} is a visualization of the modularity scores of the matrix of similarity scores between nodes for each level of partitioning. This visualization helps identify the optimal level of partitioning inferred by the \code{role_analysis} function.
#' \code{concor_relations_heatmaps} is a list object containing several heatmap visualizations representing the extent to which nodes in one inferred block are connected to nodes in another block.
#' \code{concor_relations_sociogram} contains a network visualization representing the extent to which nodes in blocks inferred at the optimal level of partitioning are tied to one another. Nodes in this visualization represent inferred blocks in the aggregate.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
# Run netwrite
#' flor <- netwrite(nodelist = florentine_nodes,
#'                  node_id = "id",
#'                  i_elements = florentine_edges$source,
#'                  j_elements = florentine_edges$target,
#'                  type = florentine_edges$type,
#'                  directed = FALSE,
#'                  net_name = "florentine")
#'
#' # Clustering method
#' flor_cluster <- role_analysis(graph = flor$igraph_list,
#'                               nodes = flor$node_measures,
#'                               directed = FALSE,
#'                               method = "cluster",
#'                               min_partitions = 2,
#'                               max_partitions = 8,
#'                               viz = TRUE)
#'
#' ### View cluster dendrogram
#' flor_cluster$cluster_dendrogram
#'
#' ### View modularity summary plot
#' flor_cluster$cluster_modularity
#'
#' ### View cluster assignments
#' head(flor_cluster$cluster_assignments)
#'
#' ### View centrality summary plot for aggregate network
#' flor_cluster$cluster_summaries_cent$summary_graph
#' ### View cenrality summary plot for network of relation `business`
#' flor_cluster$cluster_summaries_cent$business
#'
#' ### View triad position summary plot for network of relation `marriage`
#' flor_cluster$cluster_summaries_triad$marriage
#'
#'
#' # CONCOR method
#' flor_concor <- role_analysis(graph = flor$igraph_list,
#'                              nodes = flor$node_measures,
#'                              directed = FALSE,
#'                              method = "concor",
#'                              min_partitions = 1,
#'                              max_partitions = 4,
#'                              viz = TRUE)
#'
#' ### View CONCOR tree
#' flor_concor$concor_block_tree
#'
#' ### View modularity summary plot
#' flor_concor$concor_modularity
#'
#' ### View cluster assignments
#' head(flor_concor$concor_assignments)
#'
#' ### View chi-squared heatmaps of relations between blocks
#' flor_concor$concor_relations_heatmaps$chisq



role_analysis <- function(graph, # igraph object generated from netwrite
                          # Or list of igraph objects
                          nodes, # node-level measures generated from netwrite
                          # Or list of node information
                          directed = NA, # whether or not network is directed
                          method = "cluster", # method of role inference/assignment (more options later)
                          min_partitions = NA, # minimum number of clusters to test
                          max_partitions = NA, # maximum number of clusters to test
                          min_partition_size = NA, # minimum number of nodes required for a cluster to exist
                          # If a numeric value is specified, this program uses the `cluster_collapse` function
                          # to try and aggregate smaller clusters into larger ones based on their parent brances
                          # in the dendrogram
                          backbone = .9, # When calculating optimal modularity, it helps to backbone the similarity/correlation
                          # matrix according to the nth percentile. How much you backbone tends to vary depending on the size of
                          # the network, which larger nets requiring higher thresholds.
                          viz = FALSE, # Produce summary visualizations

                          # Arguments Specific to Clustering Method
                          fast_triad = TRUE, # Whether to use dplyr method for triad position counting
                          retain_variables = FALSE, # Export a dataframe of variables used in clustering
                          cluster_summaries = FALSE, # Export a dataframe containing mean values of clustering variables within each cluster
                          dendro_names = FALSE, # If TRUE, `cluster_dendogram` lists nodel labels rathher than ID numbers

                          # Arguments Specific to CONCOR
                          self_ties = FALSE, # Whether to include self-ties in CONCOR calculation
                          cutoff = .999, # Correlation cutoff for detecting convergence in CONCOR
                          max_iter = 50 # Maximum number of iteration for CONCOR algorithm


) {


  if (method == "cluster") {

    role_output <- cluster_method(graph = graph,
                                  nodes = nodes,
                                  directed = directed,
                                  min_partitions = min_partitions,
                                  max_partitions = max_partitions,
                                  min_partition_size = min_partition_size,
                                  backbone = backbone,
                                  viz = viz,
                                  fast_triad = fast_triad,
                                  retain_variables = retain_variables,
                                  cluster_summaries = cluster_summaries,
                                  dendro_names = dendro_names)

    return(role_output)

  } else if (method == "concor" | method == "CONCOR") {

    role_output <- concor_method(graph = graph,
                                 nodes = nodes,
                                 directed = directed,
                                 min_partitions = min_partitions,
                                 max_partitions = max_partitions,
                                 min_partition_size = min_partition_size,
                                 backbone = backbone,
                                 viz = viz,
                                 self_ties = self_ties,
                                 cutoff = cutoff,
                                 max_iter = max_iter)

    return(role_output)

  } else {

    base::message("Error: Incorrect analysis method entered")

  }

}


#####################################################################
# Function for hierarchical clustering method
#####################################################################

cluster_method <- function(graph, # igraph object generated from netwrite
                           # Or list of igraph objects
                           nodes, # node-level measures generated from netwrite
                           # Or list of node information
                           directed = NA, # whether or not network is directed
                           method = "cluster", # method of role inference/assignment (more options later)
                           fast_triad = TRUE, # Whether to use dplyr method for triad position counting
                           min_partitions = NA, # minimum number of clusters to test
                           max_partitions = NA, # maximum number of clusters to test
                           min_partition_size = NA, # minimum number of nodes required for a cluster to exist
                           # If a numeric value is specified, this program uses the `cluster_collapse` function
                           # to try and aggregate smaller clusters into larger ones based on their parent brances
                           # in the dendrogram
                           backbone = .9, # When calculating optimal modularity, it helps to backbone the similarity/correlation
                           # matrix according to the nth percentile. How much you backbone tends to vary depending on the size of
                           # the network, which larger nets requiring higher thresholds.
                           retain_variables = FALSE, # Export a dataframe of variables used in clustering
                           cluster_summaries = FALSE, # Export a dataframe containing mean values of clustering variables within each cluster
                           viz = FALSE, # Produce summary visualizations
                           num_vars = 10, # Plots top 10 variables with greatest SD across clusters in summary plot
                           plot_all = FALSE, # Summary plot has all variables,
                           dendro_names = FALSE
) {


  # Create output list
  output_list <- list()

  # 1. If `graph` is a single igraph object, go ahead and store it in a list
  if (("igraph" %in% class(graph)) == TRUE) {
    graph <- list(summary_graph = graph)
  }

  # Make a copy of `graph` for displaying sociograms later. This copy
  # is necessary in the event that we have to deal with isolates
  original_graph <- graph

  # 2. Select required variables from `nodes` for clustering
  role_centrality <- dplyr::select(nodes,
                                   .data$id,
                                   dplyr::contains("degree"),
                                   dplyr::contains("betweenness"),
                                   dplyr::contains("bonpow"),
                                   dplyr::contains("eigen"),
                                   dplyr::contains("closeness"))

  # 3. Make dummy variable(s) indicating isolate status in each graph/subgraph
  ### Get total degree variables
  isolate_check <- dplyr::select(role_centrality,
                                 .data$id, dplyr::contains("total_degree"))
  ### Check if any total degree values equal zero
  isolate_check[, 2:ncol(isolate_check)] <- isolate_check[, 2:ncol(isolate_check)] == 0
  ### Rename columns in `isolate_check`
  colnames(isolate_check) <- stringr::str_replace_all(colnames(isolate_check), "total_degree", "isolate")
  ### Merge back into `role_centrality`
  role_centrality <- dplyr::left_join(role_centrality, isolate_check, by = "id")


  # 4. Getting correlations across pairs of relation types
  ## Note: only needed if there's more than one relation type
  if (length(graph) > 1) {
    ### First, we'll need to convert our igraph objects into adjacency matrices
    adjmats <- to_adjmats(graph)

    ### Then we get correlations across relation type pairs
    ### (See `source_files/relation_cors.R` for debugging)
    rel_cors <- relation_cors(role_centrality = role_centrality,
                              adjmats = adjmats)
    ### Now bind into `role_centrality`
    role_centrality <- cbind(role_centrality, rel_cors)
  } else {
    adjmats <- list()
    adjmats[[1]] <- as.matrix(igraph::as_adjacency_matrix(graph$summary_graph, attr = "weight"))
  }


  # 5. Triad/position census (using dplyr, faster than other function)


  if (fast_triad == TRUE) {

    for (i in 1:length(graph)) {

      # Extract graph and remove multiple edges
      this_graph <- igraph::simplify(graph[[i]],
                                     remove.multiple = TRUE,
                                     remove.loops = TRUE)


      # Get edgelist and convert to numeric
      el <- as.data.frame(igraph::get.edgelist(this_graph))
      el[,1] <- as.numeric(el[,1])
      el[,2] <- as.numeric(el[,2])

      triad_census <- positions_dplyr(nodes = role_centrality$id,
                                      edges = el,
                                      directed = igraph::is.directed(this_graph))

      # Exclude 003 and 001(2?) triads
      # DOUBLE CHECK THIS WITH JIM
      remove_vec <- stringr::str_detect(colnames(triad_census), "003") +
        stringr::str_detect(colnames(triad_census), "102") +
        stringr::str_detect(colnames(triad_census), "012")
      triad_census <- triad_census[,!remove_vec]

      # Relabel for relation type
      colnames(triad_census) <- c("id",
                                  paste(names(graph)[[i]],
                                        colnames(triad_census)[2:ncol(triad_census)],
                                        sep = "_"))

      # Add to `role_centrality` dataframe
      role_centrality <- dplyr::left_join(role_centrality,
                                          triad_census,
                                          by = "id")

      role_centrality[is.na(role_centrality)] <- 0


    }

  } else {

    for (i in 1:length(adjmats)){

      # Binarize adjmat
      adjmat <- adjmats[[i]]
      adjmat[adjmat != 0] <- 1
      #Remove loops
      diag(adjmat) <- 0

      # Run Triad census
      triad_census <- f.rc(mat = adjmat)


    }

    # Label `triad_census` columns according to triad type
    # colnames(triad_census) <- c("003", "012_S", "021D_S", "012_E", "021U_E", "102_D", "201_B", "021C_B", "111U_B", "111D_B",
    #                             "102_I", "111D_S", "120D_S", "111U_E", "120U_E", "201_S", "300", "120C_B", "210_S", "210_B",
    #                             "012_I","021C_S", "030T_S", "012D_E", "030T_E","111U_S", "210_E", "030T_B", "120U_S", "120C_E",
    #                             "021U_S", "021C_E","111D_E", "030C","120C_S", "120D_E")
    colnames(triad_census) <- c("003", "012_S", "021D_S", "012_E", "021U_E", "102_D", "201_B", "021C_B", "111U_B", "111D_B",
                                "102_I", "111D_S", "120D_S", "111U_E", "120U_E", "201_S", "300", "120C_B", "210_S", "210_B",
                                "012_I","021C_S", "030T_S", "021D_E", "030T_E","111U_S", "210_E", "030T_B", "120U_S", "120C_E",
                                "021U_S", "021C_E","111D_E", "030C","120C_S", "120D_E")




    # Exclude 000 and 001 triads
    remove_vec <- stringr::str_detect(colnames(triad_census), "003") +
      stringr::str_detect(colnames(triad_census), "102") +
      stringr::str_detect(colnames(triad_census), "012")
    triad_census <- triad_census[,!remove_vec]

    # Add to `role_centrality` dataframe
    role_centrality <- cbind(role_centrality, triad_census)

  }



  # Total isolates in the network should be treated as their own "role"
  # and excluded from the clustering process.
  ### Check to see if there are any total isolates
  has_isolates <- FALSE

  if (sum(role_centrality$isolate) > 0) {

    has_isolates <- TRUE

    isolate_vector <- role_centrality$isolate

    # Store isolates in their own separate data frame
    isolate_df <- dplyr::filter(role_centrality, .data$isolate == TRUE)
    # Remove isolate nodes from `role_centrality`
    role_centrality <- dplyr::filter(role_centrality, .data$isolate == FALSE)

    # Now we need to remove isolate nodes from igraph objects in the
    # `graph` list
    for (i in 1:length(graph)) {

      graph[[i]] <- igraph::delete.vertices(graph[[i]],
                                            v = isolate_vector)
      # v = igraph::degree(graph[[1]]) == 0)


    }
  }


  #############################
  #    C L U S T E R I N G    #
  #############################

  # Make a data frame for standardized scores
  role_std <- data.frame(id = role_centrality$id)

  # Standardize all measures
  for (i in 2:ncol(role_centrality)) {

    role_std[,i] <- scale(role_centrality[,i])

    if (is.nan(sum(role_std[,i]))) {
      role_std[,i] <- 0
    }

  }

  colnames(role_std) <- colnames(role_centrality)

  euclid_mat <- as.matrix(role_std[,2:ncol(role_std)])
  rownames(euclid_mat) <- role_std$id
  #colnames(euclid_mat) <- role_centrality$id


  # Now calculate Euclidean distances
  euclid_dist <- cluster::daisy(euclid_mat, metric="euclidean")

  # Clustering using Ward algorithm
  hc=stats::hclust(euclid_dist, method = "ward.D2")

  # In order to find the solution that maximizes modularity, we first want to see what
  # the partitioning looks like at every point of divergence in the dendrogram
  cut_df <- as.data.frame(stats::cutree(hc, h = hc$height))
  # Reverse order of columns
  cut_df <- cut_df[,ncol(cut_df):1]
  # Rename columns
  colnames(cut_df) <- paste("cut_", 1:ncol(cut_df), sep = "")

  # For larger networks, it's going to be a pain in the ass if we go through the whole
  # range of heights. If a maximum number of clusters is set, limit `cut_df` to that
  if (!is.na(max_partitions)){

    cut_df <- cut_df[,1:max_partitions]


  }

  cut_df <- cbind(data.frame(id = role_centrality$id), cut_df)



  #######################################################
  #    M O D U L A R I T Y   M A X I M I Z A T I O N    #
  #######################################################

  # Calculating modularity
  ## 1. Make igraph object of distance matrix
  dist_mat <- 1/(as.matrix(euclid_dist) + 1) # I think adding 1 to the distance matrix
  # before taking the reciprocal is appropriate, but need to confirm.
  diag(dist_mat) <- NA
  # ##### WE HAVE AN ISSUE HERE where perfectly similar nodes have
  # ##### a similarity score of `Inf`. We need to figure out a good
  # ##### solution for replacing Infinite values before proceeding
  # ##### to calculating modularity
  # sim_vec <- dist_mat
  # sim_vec[is.infinite(sim_vec)] <- NA
  # max(sim_vec, na.rm = T) # This is getting kind of wacky,
  # # the range is kind of out of control
  #
  #

  # Take top nth percentile of similarity scores
  for (i in 1:nrow(dist_mat)) {

    dist_mat[i, (dist_mat[i,] < stats::quantile(dist_mat[i,], backbone, na.rm = T))] <- 0

  }


  dist_igraph <- igraph::graph_from_adjacency_matrix(dist_mat,
                                                     mode = "undirected",
                                                     weighted = TRUE)
  dist_igraph <- igraph::simplify(dist_igraph, remove.loops = TRUE)

  ## 2. Calculate modularity
  modularity_df <- data.frame()
  for (i in 2:ncol(cut_df)) {

    igraph::V(dist_igraph)$cluster <- cut_df[,i]

    this_mod <- data.frame(cut = colnames(cut_df)[i],
                           modularity = igraph::modularity(dist_igraph, membership = igraph::V(dist_igraph)$cluster))

    modularity_df <- rbind(modularity_df, this_mod)

  }

  ## 3. Create a numeric variable in `modularity_df` that specifies
  # number of clusters
  modularity_df$num_clusters <- as.numeric(gsub("[^0-9]", "", modularity_df$cut))

  max_mod <- modularity_df[which(modularity_df$modularity == max(modularity_df$modularity)), ]

  cut_df$max_mod <- cut_df[,max_mod$cut]


  # 4. If cluster sizes are smaller than preferred, use `cluster_collapse` to aggregate
  # into larger clusters
  if (!is.na(min_partition_size)) {

    cut_df$best_fit <- cluster_collapse(min_partition_size = min_partition_size,
                                        max_mod = max_mod,
                                        cut_df = cut_df)
  } else {
    cut_df$best_fit <- cut_df$max_mod
  }

  ### 4.a. At this point we need to designate isolates as their own cluster
  original_ids <- data.frame(id = nodes$id)
  cut_df2 <- dplyr::left_join(original_ids, cut_df, by = "id")
  ##### Replace NA values with a new cluster id
  for (i in 1:ncol(cut_df2)) {

    this_vec <- cut_df2[,i]
    this_vec[is.na(this_vec)] <- (max(this_vec, na.rm = T) + 1)
    cut_df2[,i] <- this_vec

  }

  # This part needs to be changed for handling list of igraph objects
  # if (is.list(graph)) {

  for (i in 1:length(graph)) {

    # Weird thing is happening with igraph objects stored in list and
    # assigning cluster ID values

    this_graph <- original_graph[[i]]
    this_graph <- igraph::set_vertex_attr(this_graph, "cluster", value = cut_df2$best_fit)
    # igraph::V(this_graph)$cluster <- unlist(igraph::V(this_graph)$cluster)
    #
    # igraph::V(graph[[i]])$cluster <- cut_df$best_fit
    original_graph[[i]] <- this_graph

  }



  #################################
  #    S U M M A R Y   D A T A    #
  #################################


  # Summary table
  ### Assign cluster assignments to `role_centrality` and `role_std`
  role_centrality$cluster <- cut_df$best_fit
  role_std$cluster <- cut_df$best_fit

  role_std_summary <- role_std %>%
    dplyr::group_by(.data$cluster) %>%
    dplyr::summarize_all(mean, na.rm = T) %>%
    dplyr::select(-.data$id) %>%
    dplyr::ungroup()


  role_std_sd <- as.data.frame(t(role_std_summary %>%
                                   dplyr::summarize_all(stats::sd, na.rm = T)))
  colnames(role_std_sd) <- "sd"
  role_std_sd$var <- rownames(role_std_sd)

  role_std_summary2 <- role_std_summary %>%
    tidyr::pivot_longer(-.data$cluster, names_to = "var") %>%
    dplyr::left_join(role_std_sd, by = "var") %>%
    dplyr::group_by(.data$var)


  # If we have total isolates in the graph, we need to add their
  # de facto cluster assignment into this summary table
  if (has_isolates == TRUE) {

    isolate_summary <- role_std_summary2 %>%
      dplyr::group_by(.data$var) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cluster = max(cut_df2$best_fit),
                    value = ifelse(stringr::str_detect(.data$var, "isolate"), 1, NA))

    role_std_summary2 <- dplyr::bind_rows(role_std_summary2, isolate_summary)

  }



  # Order ID (for plotting all variables)
  order_id <- data.frame(var = unique(role_std_summary2$var),
                         order_id = 1:length(unique(role_std_summary2$var)))

  role_std_summary2 <- role_std_summary2 %>%
    dplyr::left_join(order_id, by = "var") %>%
    dplyr::arrange(dplyr::desc(.data$sd), .data$var)

  role_std_summary2$var_id <- rep(1:length(unique(role_std_summary2$var)),
                                  each = length(unique(role_std_summary2$cluster)))



  #######################
  #    O U T P U T S    #
  #######################

  # Rename columns in `role_std`
  colnames(role_std) <- c("id", paste(names(role_std), "std", sep = "_")[2:length(names(role_std))])

  # Store cluster assignments to global environment
  output_list$cluster_assignments <- cut_df2
  # assign(x = "cluster_assignments", value = cut_df2, .GlobalEnv)

  # Option to export variables used for clustering
  if (retain_variables == TRUE) {

    clustering_vars <- dplyr::left_join(role_centrality, role_std, by = "id") %>%
      dplyr::select(-.data$cluster_std)

    output_list$clustering_variables <- clustering_vars
    # assign(x = 'clustering_variables', value = clustering_vars, .GlobalEnv)

  }

  # Option to produce summary dataframe
  if (cluster_summaries == TRUE) {

    # Put together data for summarizing
    cluster_sum_start <- cut_df2[, c("id", "max_mod")]
    cluster_sum_start <- dplyr::left_join(cluster_sum_start, role_centrality, by = "id")
    cluster_sum_start <- dplyr::left_join(cluster_sum_start, role_std, by = "id")

    # Get unique cluster values
    cluster_ids <- sort(unique(cluster_sum_start$max_mod))

    # Make dataframe for storing summaries
    cluster_sum <- data.frame()

    # Loop over each value in `cluster_ids`
    for (i in 1:length(cluster_ids)) {

      # Filter `cluster_sum_start` to contain only nodes in this cluster
      this_cluster <- cluster_sum_start[cluster_sum_start$max_mod == cluster_ids[[i]], 3:ncol(cluster_sum_start)]
      # Calculate means of all variables using `sapply`
      this_cluster_sum <- c(cluster_ids[[i]], nrow(this_cluster), sapply(this_cluster, mean))

      cluster_sum <- rbind(cluster_sum, this_cluster_sum)
    }

    # Add column names
    colnames(cluster_sum) <- c("cluster", "size",
                               paste("mean_",
                                     names(cluster_sum_start)[3:ncol(cluster_sum_start)],
                                     sep = ""))

    cluster_sum <- cluster_sum %>% dplyr::select(-dplyr::contains("mean_cluster"))

    # Assign to global environment
    output_list$cluster_summaries <- cluster_sum
    # assign(x = "cluster_summaries", value = cluster_sum, .GlobalEnv)

  }

  # Option to produce summary visualizations
  if (viz == TRUE) {

    # Cluster dendrogram
    if (dendro_names == TRUE) {
      attr(euclid_dist, "Labels") <- nodes[nodes$id %in% role_centrality$id, 2]
      hc=stats::hclust(euclid_dist, method = "ward.D2")
    }

    plot(hc)
    stats::rect.hclust(hc, k = max_mod$num_clusters)
    dendrogram <- grDevices::recordPlot()
    # assign(x = 'cluster_dendrogram', value = dendrogram, .GlobalEnv)
    grDevices::dev.off()
    output_list$cluster_dendrogram <- dendrogram

    # Sociogram with nodes labeled by optimal clusters
    ### Make a dataframe for color assignments to ensure consistency
    ### across different plot objects
    color_df <- data.frame(id = cut_df2$id,
                           cluster = cut_df2$best_fit)
    color_df$color <- color_assign(color_df$cluster)

    # Condensed color_df for legend plotting
    color_df2 <- color_df[,2:3]
    color_df2 <- unique(color_df2)
    color_df2 <- dplyr::arrange(color_df2, .data$cluster)

    # Assign colors to igraph object list
    for (i in 1:length(original_graph)){

      igraph::V(original_graph[[i]])$color <- color_df$color

    }


    output_list$cluster_sociogram <- cluster_sociogram(graph_list = original_graph,
                                                       version = "cluster",
                                                       color2 = color_df2)


    # Modularity plot
    graphics::plot.new()
    plot(x = modularity_df$num_clusters,
         y = modularity_df$modularity,
         main = "Cluster Modularity",
         xlab = "Number of Clusters",
         ylab = "Modularity")
    graphics::lines(modularity_df$num_clusters, modularity_df$modularity)
    mod_plot <- grDevices::recordPlot()
    # assign(x = "cluster_modularity", value = mod_plot, .GlobalEnv)
    grDevices::dev.off()
    output_list$cluster_modularity <- mod_plot

    # "Supernode" sociograms
    cluster_relations_sociogram <- role_sociogram(graph = original_graph,
                                                  version = "cluster",
                                                  color2 = color_df2)
    output_list$cluster_relations_sociogram <- cluster_relations_sociogram
    # assign(x = "cluster_relations_sociogram", value = cluster_relations_sociogram, .GlobalEnv)


    # Summary Visualization (Cluster Mean Values)
    # Things to add:
    ### 1. Argument about number of variables in viz
    ### 2. Plotting variables in panel (Not sorted)


    sum_plots <- cluster_summary_plots(graph_list = graph,
                                       summary_data = role_std_summary2)

    ### Store in `output_list`
    output_list$cluster_summaries_cent <- sum_plots$cluster_summaries_cent
    output_list$cluster_summaries_triad <- sum_plots$cluster_summaries_triad


    if (length(graph) > 1) {

      output_list$cluster_summaries_correlations <- cluster_summary_cor(summary_data = role_std_summary2)

    }


    # Summary Visualization (Cluster Relations)
    output_list$cluster_relations_heatmaps <- cluster_heatmaps(node_data = cut_df2,
                                                               graph_list = graph,
                                                               version = "cluster")

  }

  return(output_list)

}


#####################################################################
# Function for CONCOR
#####################################################################

concor_method <- function(graph,
                          nodes,
                          directed = NA,
                          min_partitions = NA,
                          max_partitions = NA,
                          min_partition_size = NA,
                          self_ties = FALSE,
                          cutoff = .999,
                          max_iter = 50,
                          backbone = .9,
                          retain_variables = FALSE,
                          cluster_summaries = FALSE,
                          viz = FALSE,
                          num_vars = 10,
                          plot_all = FALSE
) {

  # Create `output_list`
  output_list <- list()


  # 1. If `graph` is a single igraph object, go ahead and store it in a list
  if (("igraph" %in% class(graph)) == TRUE) {
    graph <- list(summary_graph = graph)
  }

  # Make a copy of `graph` for displaying sociograms later. This copy
  # is necessary in the event that we have to deal with isolates
  original_graph <- graph

  # 2. Concor doesn't work on adjmats with isolates (and I think)
  # with fully connected nodes, need to verify. As such, total isolates
  # need to be removed from the igraph object(s) contained in `graph`
  isolate_check <- nodes$total_degree == 0

  if (sum(isolate_check) > 0) {

    for (i in 1:length(graph)) {
      graph[[i]] <- igraph::delete.vertices(graph[[i]], v = isolate_check)
    }
  }

  # Need to remove fully connected nodes (if you nominate everyone)


  # 3. Convert `graph` to list of adjmats using `to_adjmats`
  adjmat_list <- to_adjmats(graph)


  # NEED TO CREATE TRANSPOSES OF MATRICES AND ADD TO STACKS
  if (directed == TRUE) {
    transpose_list <- adjmat_list
    # Replace all elements in `transpose_list` with their transpose
    for (i in 1:length(transpose_list)) {
      transpose_list[[i]] <- t(transpose_list[[i]])
    }
    # Rename transpose matrices to indicate their transpose status
    names(transpose_list) <- paste(names(transpose_list), "t", sep = "_")

    # Add `transpose_list` to `adjmat_list`
    adjmat_list <- c(adjmat_list, transpose_list)
  }


  ### 3a. Need to ask Jim, do we want `summary_graph`'s adjmat
  ### in the concor calculation? If not, we need to remove it
  ### from `adjmat_list`
  # adjmat_list$summary_graph <- NULL


  # 4. Make a dataframe for storing block assignments
  assignment_df <- data.frame(id = igraph::V(graph[[1]])$id)


  # 5. Run Concor

  for (i in min_partitions:max_partitions) {


    blks <- tryCatch(concorR::concor(m_list = adjmat_list, nsplit = i, self_ties = F),
                     error = function(e) e)

    if ('error' %in% class(blks)) {

      message(paste(
        "Error in computing concor blocks when number of desired partitions is ", i,
        ".\nInformation for this desired partition value will not be recorded.", sep = ""))

    } else {

      this_assignment <- blks
      colnames(this_assignment) <- c(paste("split", i, sep = "_"),
                                     "id")
      this_assignment$id <- as.numeric(this_assignment$id)
      colnames(this_assignment) <- c(paste("block_", i, sep = ""),
                                     "id")
      assignment_df <- dplyr::left_join(assignment_df, this_assignment, by = "id")


    }

  }


  #######################################################
  #    M O D U L A R I T Y   M A X I M I Z A T I O N    #
  #######################################################

  # Calculating modularity
  ## 1. Get correlation matrix
  ##### Stack adjacency matrices
  mat_stack <- do.call(rbind, adjmat_list)
  cor_mat <- stats::cor(mat_stack)
  diag(cor_mat) <- NA

  ## 2. Adjust correlation scores so that there are only non-negative
  ## scores in the matrix. igraph won't play with negative edge weights.
  min_val <- min(cor_mat, na.rm = T)
  if (min_val < 0) {
    cor_mat <- cor_mat + -1*min_val
  }
  ##### Add 1 to all values
  cor_mat <- cor_mat + 1

  ## 3. Backboning
  # Right now we're having an issue where the modularity calculation isn't giving
  # anything informative for larger networks. It seems that the similarity matrix
  # is too dense and requires some pruning to get proper mod scores.

  # Possible avenues include...
  # We COULD do the separate modularity calculations
  # as separate functions stored in the source code folder
  # and then in this script just run one function or the other
  # depending on what the user selects

  # a. Top 90th percentile of similarity scores
  # a2. NEED TO MAKE THE PERCENTILE THRESHOLD ADJUSTABLE
  for (i in 1:nrow(cor_mat)) {

    cor_mat[i, (cor_mat[i,] < stats::quantile(cor_mat[i,], backbone, na.rm = T))] <- 0

  }

  # # b. 2 SDs above mean similarity score (currently giving the same number of clusters)
  # for (i in 1:nrow(cor_mat)) {
  #
  #   cor_mat[i, (scale(cor_mat[i,])[,1] < 2)] <- 0
  #
  # }

  ## 4. Make igraph object
  cor_igraph <- igraph::graph_from_adjacency_matrix(cor_mat,
                                                    mode = "undirected",
                                                    weighted = TRUE)
  cor_igraph <- igraph::simplify(cor_igraph, remove.loops = TRUE)

  ## 5. Calculate modularity
  modularity_df <- data.frame()
  for (i in 2:ncol(assignment_df)) {

    # Sometimes the concor function doesn't assign nodes to a block, thereby giving it
    # an `NA` value. This will crash the modularity calculation. To remedy this, we need
    # to replace `NA` values and treat them as their own de facto block (unclassified)
    these_blocks <- assignment_df[,i]
    these_blocks[is.na(these_blocks)] <- max(these_blocks, na.rm = T) + 1
    igraph::V(cor_igraph)$block <- these_blocks

    this_mod <- data.frame(cut = colnames(assignment_df)[i],
                           modularity = igraph::modularity(cor_igraph, membership = igraph::V(cor_igraph)$block))

    modularity_df <- rbind(modularity_df, this_mod)

  }

  ## 6. Create a numeric variable in `modularity_df` that specifies
  # number of clusters
  modularity_df$num_partitions <- as.numeric(gsub("[^0-9]", "", modularity_df$cut))
  modularity_df$num_blocks <- 2^modularity_df$num_partitions

  max_mod <- modularity_df[which(modularity_df$modularity == max(modularity_df$modularity)), ]

  assignment_df$max_mod <- assignment_df[,max_mod$cut]


  ## 7. If cluster sizes are smaller than preferred, use `cluster_collapse` to aggregate
  # into larger clusters
  #### Need to test/debug
  if (!is.na(min_partition_size)) {

    assignment_df$best_fit <- cluster_collapse(min_partition_size = min_partition_size,
                                               max_mod = max_mod,
                                               cut_df = assignment_df)
  } else {
    assignment_df$best_fit <- assignment_df$max_mod
  }

  ## 8. Merge best fit assignments to full nodelist
  full_roster <- dplyr::left_join(nodes, assignment_df, by = "id")
  full_roster$best_fit <- ifelse(is.na(full_roster$best_fit),
                                 max(full_roster$best_fit, na.rm = T) + 1,
                                 full_roster$best_fit)

  ## 6. Add block assignments to list of igraph objects

  # This part needs to be changed for handling list of igraph objects
  # if (is.list(graph)) {

  for (i in 1:length(graph)) {

    # Weird thing is happening with igraph objects stored in list and
    # assigning cluster ID values

    this_graph <- original_graph[[i]]
    this_graph <- igraph::set_vertex_attr(this_graph, "block", value = full_roster$best_fit)
    # igraph::V(this_graph)$cluster <- unlist(igraph::V(this_graph)$cluster)
    #
    # igraph::V(graph[[i]])$cluster <- cut_df$best_fit
    original_graph[[i]] <- this_graph

  }



  #######################
  #    O U T P U T S    #
  #######################

  # Store cluster assignments to global environment
  # Make sure `id` is numeric for merge consistency
  full_roster$id <- as.numeric(full_roster$id)
  output_list$concor_assignments <- full_roster
  # assign(x = "concor_assignments", value = full_roster, .GlobalEnv)


  # Option to produce summary visualizations
  if (viz == TRUE) {


    # Sociogram with nodes labeled by optimal clusters
    ### Make a dataframe for color assignments to ensure consistency
    ### across different plot objects
    color_df <- data.frame(id = full_roster$id,
                           cluster = full_roster$best_fit)
    color_df$color <- color_assign(color_df$cluster)

    # Condensed color_df for legend plotting
    color_df2 <- color_df[,2:3]
    color_df2 <- unique(color_df2)
    color_df2 <- dplyr::arrange(color_df2, .data$cluster)

    # Assign colors to igraph object list
    for (i in 1:length(original_graph)){

      igraph::V(original_graph[[i]])$color <- color_df$color

    }


    output_list$concor_sociogram <- cluster_sociogram(graph_list = original_graph,
                                                      version = "block",
                                                      color2 = color_df2)
    # assign(x = "concor_sociogram", value = cluster_sociogram, .GlobalEnv)
    # rm(cluster_sociogram)

    # old.par <- par(mar = c(0, 0, 0, 0))
    # par(old.par)


    # Modularity plot
    graphics::plot.new()
    plot(x = modularity_df$num_blocks,
         y = modularity_df$modularity,
         main = "CONCOR Modularity",
         xlab = "Number of Blocks",
         ylab = "Modularity")
    graphics::lines(modularity_df$num_blocks, modularity_df$modularity)
    mod_plot <- grDevices::recordPlot()
    # assign(x = "concor_modularity", value = mod_plot, .GlobalEnv)
    grDevices::dev.off()
    output_list$concor_modularity <- mod_plot


    # CONCOR Tree
    output_list$concor_block_tree <- concor_tree(df = assignment_df)


    # "Supernode" sociograms
    concor_relations_sociogram <- role_sociogram(graph = original_graph,
                                                 version = "concor",
                                                 color2 = color_df2)
    output_list$concor_relations_sociogram <- concor_relations_sociogram
    # assign(x = "concor_relations_sociogram", value = concor_relations_sociogram, .GlobalEnv)





    # Summary Visualization (Cluster Relations)
    output_list$concor_relations_heatmaps <- cluster_heatmaps(node_data = assignment_df,
                                                              graph_list = graph, version = "block")



  }

  return(output_list)

}
