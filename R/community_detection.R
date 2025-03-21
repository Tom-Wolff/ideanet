#' Community Detection Across Multiple Routines (\code{comm_detect})
#'
#' @description The \code{comm_detect} function runs a set of several commonly-used community detection routines on a network and provides community assignments from these routines. Need to mention that only supports undirected nets and that for some routines the median community value is used.
#'
#' @param g An igraph object. If the igraph object contains a directed network, the function will treat the network as undirected before running community detection routines.
#' @param modres A modularity resolution parameter used when performing community detection using the Leiden method.
#' @param slow_routines A logical indicating whether time-intensive community detection routines should be performed on larger networks. Edge betweenness, leading eigenvector, link communities, and stochastic blockmodeling each take a very long time to identify communities in networks consisting of more than a few thousand nodes. By default, \code{comm_detect} will skip performing these routines on networks with more than 5,000 nodes and inform the user that it is doing so.
#' @param shiny An argument indicating whether the output from the \code{comm_detect} function will be fed into the IDEANet visualization app.
#'
#' @return \code{comm_detect} returns a list contianing three data frames. \code{comm_members} indicates each node's assigned community membership from each community detection routine. \code{comm_summaries} indicates the number of communities inferred from each routine as well as the modularity score arising from community assignments. \code{comp_scores} contains a matrix indicating the similarity of community assignments between each pair of community detection routines, measured using adjusted rand scores. A fourth element in the list, \code{plots}, contains a series of network visualizations in which nodes are colored by their assigned community memberships from each routine. If \code{shiny == FALSE}, this function will display these visualizations in the user's plot window.
#'
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Run netwrite
#' nw_fauxmesa <- netwrite(nodelist = fauxmesa_nodes,
#'                       node_id = "id",
#'                       i_elements = fauxmesa_edges$from,
#'                       j_elements = fauxmesa_edges$to,
#'                       directed = TRUE,
#'                       net_name = "faux_mesa",
#'                       output = "graph")
#'
#' # Run comm_detect
#' faux_communities <- comm_detect(g = nw_fauxmesa$faux_mesa)


###############################################
#    C O M M U N I T Y   D E T E C T I O N    #
###############################################

# Separate function for community detection
# Takes a single argument, `g`, which is an igraph object.
# `g` can be directed or undirected, but directed graphs are
# automatically converted into undirected graphs

#Orig Tom Wolff: 3.27.2022.

#JWM: 3.27.2022. Added export of community summary tables
#JWM: 3.27.2022. Added optional Leiden modularity resolution value
#JWM: 3.28.2022. Changed export datasets to have input graph name as <dataset>_<graphname>.
# TW: 5.25.2022. Updated for compatibility with graphs consisting of 2+ unconnected conmponents
# TW: 9.08.2022. Updated to better handle 2+ unconnected components and isolates with leaging eigen and spinglass
#PJM: 5.03.2024. Streamlined opening conversions and fixed modularity function calls to use weights

comm_detect <- function(g, modres=1,
                        slow_routines = FALSE,
                        shiny = FALSE) {

  # browser()

  # The `slow_routines` threshold of 5,000 nodes is usually too time-intensive
  # for reasonable use in the Shiny app, even when it's passable in RStudio.
  # We're going to set another, much smaller threshold for the Shiny app.
  # Initial value set 2.13.25, but subject to change.
  if (shiny == FALSE) {
    shiny_skip <- FALSE
  } else {
      n_nodes <- igraph::vcount(g)
      n_edges <- igraph::ecount(g)
      shiny_skip <- n_nodes*n_edges > 100
  }

  # For processing generic igraph input, not necessarily created by netwrite:
  if (NA %in% as.numeric(igraph::V(g)$name)) {
    cat('\n')
    warning("Non-numeric IDs detected in the igraph object's `name` attribute. The `name` attribute will be updated to contain zero-indexed ID numbers. The original `name` values will be reassigned to the `original_name` attribute.\n")
    igraph::V(g)$original_name <- igraph::V(g)$name
    igraph::V(g)$name <- 0:(igraph::vcount(g)-1)
  }

  # Change ARPACK defaults
  # ad <- igraph::arpack_defaults
  # ad$maxiter <- 8000


  # Create a list for storing output
  output_list <- list()

  # Maybe add an option to plot communities.
  # Create a similarity matrix. How similar outputs to each other
  # Search for adjusted Rand for calculating similarity. Takes partition info.

  # If igraph object doesn't feature edge weights, add them and set equal to 1
  if (is.null(igraph::E(g)$weight)) {

    igraph::E(g)$weight <- 1

  }

  # Many of `igraph`'s off-the-shelf functions require undirected networks.
  # To get going with these, we'll make an undirected version of input graph `g`
  # Add warning that network will be converted to undirected
  if (igraph::is_directed(g)) {
    message("The `comm_detect` function currently supports undirected graphs only. Directed networks will be collapsed to undirected before running community detection algorithms.")
  }

  # Note that, if g is a weighted directed graph here, mode = "collapse" 
  # actually sums the weights of directed edges, so a reciprocal tie collapses 
  # to a single undirected edge but with the weight summed. However, if g is
  # an undirected multigraph, as_undirected leaves it as such (hence the need
  # for the simplify call that follows)
  g_undir <- igraph::as_undirected(g, mode = "collapse") #`as.undirected()` was deprecated in igraph 2.1.0.

  # Simplify network to convert multiedges to weighted edges and remove self-loops
  # https://stackoverflow.com/questions/12998456/r-igraph-convert-parallel-edges-to-weight-attribute
  g_undir <- igraph::simplify(g_undir, edge.attr.comb=list(weight="sum"))

  # Get adjacency matrix, which is already symmetric, for some calculations
  g_sym_i <- igraph::as_adjacency_matrix(g_undir, attr = "weight")
  # Convert to matrix object
  g_sym <- as.matrix(g_sym_i)

  # Get weighted density for CPM
  n <- igraph::vcount(g_undir)
  w_dens <- sum(igraph::E(g_undir)$weight)*2/(n*(n-1))

  # If size of network exceeds 5,000, skip SBM, linkcomm, edge betweenness, and leading eigen
  if (n > 5000 & slow_routines == FALSE) {
    message("Detecting network consisting of more than 5000 nodes. Edge betweenness, leading eigenvector, link communities, and stochastic blockmodel routines will be skipped to ensure quick processing. If you would like to proceed with these methods included, set `slow_routines = TRUE`.")
  }

  # Get reciprocal of edge weight for algorithms that treat edge weights as distances
  igraph::E(g_undir)$r_weight <- 1/igraph::E(g_undir)$weight

  # Betweenness does edges as distances. Need reciprocal
  if (stats::var(igraph::E(g_undir)$weight) > 0) {
    warning("Calling cluster_edge_betweenness with reciprocal weights, which may affect selected membership vector incorrectly.\n")
  }

  if (n > 5000 & slow_routines == FALSE) {
    edge_betweenness <- data.frame(names = igraph::V(g_undir)$name,
                                   membership = NA,
                                   modularity = NA)
  } else if (shiny_skip == TRUE) {
    edge_betweenness <- data.frame(names = igraph::V(g_undir)$name,
                                   membership = NA,
                                   modularity = NA)
  } else {
    edge_betweenness <- igraph::cluster_edge_betweenness(g_undir,
                                                         weights = igraph::E(g_undir)$r_weight,
                                                         directed = FALSE,
                                                         membership = TRUE,
                                                         modularity = TRUE)
  }


  fast_greedy <- igraph::cluster_fast_greedy(g_undir,
                                             weights = igraph::E(g_undir)$weight) # Only undirected graphs


  infomap <- igraph::cluster_infomap(g_undir,
                                     e.weights = igraph::E(g_undir)$weight) # Modularity for undirected graphs only


  label_prop <- igraph::cluster_label_prop(g_undir,
                                           weights = igraph::E(g_undir)$weight) # Needs to be undirected

  leiden_mod <- igraph::cluster_leiden(g_undir,
                                       objective_function = "modularity",
                                       resolution = modres,
                                       weights = igraph::E(g_undir)$weight,
                                       n_iterations = 5) # Only undirected graphs # Leiden is better version of louvain

  leiden_cpm <- igraph::cluster_leiden(g_undir,
                                       objective_function = "CPM",
                                       resolution = w_dens,
                                       weights = igraph::E(g_undir)$weight,
                                       n_iterations = 5)



  # louvain <- igraph::cluster_louvain(g_undir) # Will take directed but gives error
  # optimal <- igraph::cluster_optimal(g) # Works fine with directed, but ignores directions

  walktrap <- igraph::cluster_walktrap(g_undir,
                                       weights = igraph::E(g_undir)$weight) # Works with directed


  # Get number of isolated components
  num_components <- igraph::components(g_undir)

  if (num_components$no == 1) {
    spinglass <- igraph::cluster_spinglass(g_undir, weights = igraph::E(g_undir)$weight) # Works with directed, but need to ask about arguments (there are many)



    if (n > 5000 & slow_routines == FALSE) {

      leading_eigen <- list(membership = rep(NA, length(igraph::V(g_undir))),
                            modularity = NA)
    } else if (shiny_skip == TRUE) {
      leading_eigen <- list(membership = rep(NA, length(igraph::V(g_undir))),
                            modularity = NA)
    } else {

      leading_eigen <- tryCatch(igraph::cluster_leading_eigen(g_undir, weights = igraph::E(g_undir)$weight), # Needs to be undirected
                                error = function(cond) {
                                  warning("Could not find clustering solution using leading eigenvector. Values associated with this method will be set to NA.")


                                  return(list(membership = rep(NA, length(igraph::V(g_undir))),
                                              modularity = NA))

                                }
      )
    }


  } else {

    igraph::V(g_undir)$component <- num_components$membership

    subgraph_memberships <- data.frame()

    # Isolate components
    for (i in 1:num_components$no) {

      this_component <- igraph::delete_vertices(g_undir, v = (igraph::V(g_undir)$component != i))

      if (length(igraph::V(this_component)) <= 5) {

        component_sums <- data.frame(id = as.numeric(names(igraph::V(this_component))),
                                     component = i,
                                     leading_eigen_membership = NA,
                                     spinglass_membership = NA)


        subgraph_memberships <- rbind(subgraph_memberships, component_sums)



      } else {



        if (n > 5000 & slow_routines == FALSE) {

          this_leading_eigen <- rep(NA, length(igraph::V(this_component)))

        } else if (shiny_skip == TRUE) {

          this_leading_eigen <- rep(NA, length(igraph::V(this_component)))

        } else {

          this_leading_eigen <- tryCatch(igraph::cluster_leading_eigen(this_component, weights = igraph::E(this_component)$weight)$membership, # Needs to be undirected
                                         error = function(cond) {
                                           message("Could not find clustering solution for this component using leading eigenvector. Values associated with this method will be set to NA.")


                                           return(membership = rep(NA, length(igraph::V(this_component))))


                                         }
          )

        }



        this_spinglass <- igraph::cluster_spinglass(this_component,
                                                    weights = igraph::E(this_component)$weight)$membership # Works with directed, but need to ask about arguments (there are many)

        component_sums <- data.frame(id = as.numeric(names(igraph::V(this_component))),
                                     component = i,
                                     leading_eigen_membership = this_leading_eigen,
                                     spinglass_membership = this_spinglass)

        subgraph_memberships <- rbind(subgraph_memberships, component_sums)


      }

    }

    # Assign new unique membership IDs. This is because, as it currently stands, each component
    # has membership ids 1-n, which are redundant labels across several components
    subgraph_memberships$leading_eigen_paste <- paste(subgraph_memberships$component, subgraph_memberships$leading_eigen_membership, sep = "_")
    subgraph_memberships$spinglass_paste <- paste(subgraph_memberships$component, subgraph_memberships$spinglass_membership, sep = "_")

    leading_eigen_ids <- data.frame(leading_eigen_paste = unique(subgraph_memberships$leading_eigen_paste),
                                    new_leading_eigen = 1:length(unique(subgraph_memberships$leading_eigen_paste)))

    spinglass_ids <- data.frame(spinglass_paste = unique(subgraph_memberships$spinglass_paste),
                                new_spinglass = 1:length(unique(subgraph_memberships$spinglass_paste)))

    subgraph_memberships <- dplyr::left_join(subgraph_memberships, leading_eigen_ids, by = "leading_eigen_paste")
    subgraph_memberships <- dplyr::left_join(subgraph_memberships, spinglass_ids, by = "spinglass_paste")

    # subgraph_memberships$new_leading_eigen <- ifelse(is.na(subgraph_memberships$leading_eigen_membership),
    #                                                  0,
    #                                                  subgraph_memberships$new_leading_eigen)
    #
    # subgraph_memberships$new_spinglass <- ifelse(is.na(subgraph_memberships$spinglass_membership),
    #                                                  0,
    #                                                  subgraph_memberships$new_spinglass)

    subgraph_memberships <- subgraph_memberships[, c("id", "component", "new_leading_eigen", "new_spinglass")]
    colnames(subgraph_memberships) <- c("id", "component", "leading_eigen_membership", "spinglass_membership")

  }


  # Ideally, we'd also wanna run the cliquefinder one
  # Ken Frank has a package for his cliquefinder tool (Either in Email or on CRAN)

  # Need to make node-level dataset indicating community membership

  if (num_components$no == 1) {

    memberships <- data.frame(id = as.numeric(edge_betweenness$names),
                              edge_betweenness_membership = edge_betweenness$membership,
                              fast_greedy_membership = fast_greedy$membership,
                              infomap_membership = infomap$membership,
                              label_prop_membership = label_prop$membership,
                              leading_eigen_membership = leading_eigen$membership,
                              leiden_mod_membership = leiden_mod$membership,
                              leiden_cpm_membership = leiden_cpm$membership,
                              # louvain_membership = louvain$membership,
                              # optimal_membership = optimal$membership,
                              spinglass_membership = spinglass$membership,
                              walktrap_membership = walktrap$membership)

    memberships$id <- as.character(memberships$id)


  } else {

    memberships <- data.frame(id = as.numeric(edge_betweenness$names),
                              edge_betweenness_membership = edge_betweenness$membership,
                              fast_greedy_membership = fast_greedy$membership,
                              infomap_membership = infomap$membership,
                              label_prop_membership = label_prop$membership,
                              # leading_eigen_membership = leading_eigen$membership,
                              leiden_mod_membership = leiden_mod$membership,
                              leiden_cpm_membership = leiden_cpm$membership,
                              # louvain_membership = louvain$membership,
                              # optimal_membership = optimal$membership,
                              #  spinglass_membership = spinglass$membership,
                              walktrap_membership = walktrap$membership)

    memberships <- dplyr::left_join(memberships, subgraph_memberships, by = "id")

    memberships$id <- as.character(memberships$id)

  }

  ##############################################
  # This is probably where we want to add the two additional methods Gabe programmed

  ## Clique Percolation ##
  cf1 <- CliquePercolation::cpAlgorithm(W = g_sym, k = 3, method = "unweighted") # Running as unweighted
  clust <- cf1$list.of.communities.labels # extract cluster assignments
  #### In some cases, such as when given a star graph, `CliquePercolation` won't detect any communities.
  #### if this is the case, just create the `cf1_membership` dataframe manually and assign all nodes
  #### to the same community (or `NA`s depending on our team's ultimate preference)
  if (length(clust) != 0) {
    clust <- lapply(clust, as.numeric)
    cf1_membership <- multigroup_assign(g_sym, clust)
    colnames(cf1_membership) <- c("cp_cluster", "id")
  } else {
    warning("Clique Percolation did not detect any distinct communities. All nodes will be assigned to the same single community (1).")
    cf1_membership <- data.frame(id = memberships$id,
                                 cp_cluster = 1)
  }

  ## Link comm ##
  if (n > 5000 & slow_routines == FALSE) {

    lc_membership <-  data.frame(id = memberships$id,
                                 lc_cluster = NA)
  } else if (shiny_skip == TRUE) {
    lc_membership <-  data.frame(id = memberships$id,
                                 lc_cluster = NA)
  } else {
    #gmat <- as.matrix((get.adjacency(network))) # LC does not require it
    linkcomm_el <- igraph::as_data_frame(g_undir, what = "edges") %>% dplyr::select(from, to)
    #### In some cases, such as when given a star graph, `linkcomm` won't detect any communities.
    #### if this is the case, just create the `lc_membership` dataframe manually and assign all nodes
    #### to the same community (or `NA`s depending on our team's ultimate preference)
    lc <- tryCatch(linkcomm::getLinkCommunities(linkcomm_el, hcmethod = "average", directed = FALSE, verbose = FALSE, plot = FALSE),
                   error = function(e) {return(NULL)})# Defaulting to false for now)
    if (!is.null(lc)) {
      # browser()
      # lc <- linkcomm::getLinkCommunities(linkcomm_el, hcmethod = "average", directed = FALSE, verbose = FALSE, plot = FALSE) # Defaulting to false for now
      clust <- split(as.numeric(lc$nodeclusters$node), lc$nodeclusters$cluster) # Turn into list of vectors
      clust <- clust[order(as.numeric(names(clust)))] # Make sure its ordered
      lc_membership <- multigroup_assign(g_sym, clust)
      colnames(lc_membership) <- c("lc_cluster", "id")
    } else {
      warning("linkcomm did not detect any distinct communities. All nodes will be assigned to the same single community (1).")
      lc_membership <- data.frame(id = memberships$id,
                                  lc_cluster = 1)
    }

  }

  lc_membership$id <- as.character(lc_membership$id)
  cf1_membership$id <- as.character(cf1_membership$id)

  # Add into overall memberships dataframe
  memberships <- dplyr::left_join(memberships, cf1_membership, by = "id")
  memberships <- dplyr::left_join(memberships, lc_membership, by = "id")

  # Need to make network-level dataset recording number of communities and modularity scores
  # We'll make it a long dataset for starters, assuming that we're only working with one network at a time
  # Depending on feedback from others, we can easily make this wide for comparison between multiple graphs

  # Merge network-level stuff into node-level dataframe
  # For missing modularity scores, we'll have to calculate it ourselves

  # Note that the maximum modularity identified by cluster_edge_betweenness for weighted networks,
  # which we have computed here with reciprocal weights, is not the same as the modularity for
  # those memberships with the original weights, so we recalculate it here.

  if (n > 5000 & slow_routines == FALSE) {

    edge_betweenness_stats <- data.frame(method = "edge_betweenness",
                                         num_communities = NA,
                                         #modularity = edge_betweenness$modularity)
                                         #modularity = max(edge_betweenness$modularity))
                                         modularity = NA)

    lc_stats <- data.frame(method = "lc",
                           num_communities = NA,
                           modularity = NA)


  } else if (shiny_skip == TRUE) {

    edge_betweenness_stats <- data.frame(method = "edge_betweenness",
                                         num_communities = NA,
                                         #modularity = edge_betweenness$modularity)
                                         #modularity = max(edge_betweenness$modularity))
                                         modularity = NA)

    lc_stats <- data.frame(method = "lc",
                           num_communities = NA,
                           modularity = NA)

  } else {

    edge_betweenness_stats <- data.frame(method = "edge_betweenness",
                                         num_communities = max(edge_betweenness$membership),
                                         #modularity = edge_betweenness$modularity)
                                         #modularity = max(edge_betweenness$modularity))
                                         modularity = igraph::modularity(g_undir, edge_betweenness$membership, weights = igraph::E(g_undir)$weight))

    lc_stats <- data.frame(method = "lc",
                           num_communities = length(unique(memberships$lc_cluster)),
                           modularity = igraph::modularity(g_undir, membership = (memberships$lc_cluster), weights = igraph::E(g_undir)$weight))

  }


  fast_greedy_stats <- data.frame(method = "fast_greedy",
                                  num_communities = max(fast_greedy$membership),
                                  modularity = fast_greedy$modularity)
  fast_greedy_stats <- fast_greedy_stats[which(fast_greedy_stats$modularity == max(fast_greedy_stats$modularity)),]

  infomap_stats <- data.frame(method = "infomap",
                              num_communities = max(infomap$membership),
                              modularity = infomap$modularity)

  label_prop_stats <- data.frame(method = "label_prop",
                                 num_communities = max(label_prop$membership),
                                 modularity = label_prop$modularity)

  if (num_components$no == 1) {

    if (n > 5000 & slow_routines == FALSE) {

      leading_eigen_stats <- data.frame(method = "leading_eigen",
                                        num_communities = NA,
                                        modularity = NA)
    } else if (shiny_skip == TRUE) {

      leading_eigen_stats <- data.frame(method = "leading_eigen",
                                        num_communities = NA,
                                        modularity = NA)
    } else {

      leading_eigen_stats <- data.frame(method = "leading_eigen",
                                        num_communities = max(leading_eigen$membership),
                                        modularity = leading_eigen$modularity)
    }


  } else {

    if (n > 5000 & slow_routines == FALSE) {

      leading_eigen_stats <- data.frame(method = "leading_eigen",
                                        num_communities = NA,
                                        modularity = NA)
    } else if (shiny_skip == TRUE) {

      leading_eigen_stats <- data.frame(method = "leading_eigen",
                                        num_communities = NA,
                                        modularity = NA)
    } else {

      igraph::V(g_undir)$leading_eigen <- memberships$leading_eigen_membership

      leading_eigen_stats <- data.frame(method = "leading_eigen",
                                        num_communities = max(memberships$leading_eigen_membership, na.rm = TRUE),
                                        modularity = igraph::modularity(g_undir, membership = igraph::V(g_undir)$leading_eigen, weights = igraph::E(g_undir)$weight))
    }
  }


  leiden_mod_stats <- data.frame(method = "leiden_mod",
                                 num_communities = max(leiden_mod$membership),
                                 modularity = igraph::modularity(g_undir, membership = leiden_mod$membership, weights = igraph::E(g_undir)$weight))

  leiden_cpm_stats <- data.frame(method = "leiden_cpm",
                                 num_communities = max(leiden_cpm$membership),
                                 modularity = igraph::modularity(g_undir, membership = leiden_cpm$membership, weights = igraph::E(g_undir)$weight))

  # louvain_stats <- data.frame(method = "louvain",
  #                                      num_communities = max(louvain$membership),
  #                                      modularity = louvain$modularity)
  # optimal_stats <- data.frame(method = "optimal",
  #                                      num_communities = max(optimal$membership),
  #                                      modularity = optimal$modularity)

  if (num_components$no == 1) {

    spinglass_stats <- data.frame(method = "spinglass",
                                  num_communities = max(spinglass$membership),
                                  modularity = spinglass$modularity)
  } else {

    igraph::V(g_undir)$spinglass <- memberships$spinglass_membership

    spinglass_stats <- data.frame(method = "spinglass",
                                  num_communities = max(memberships$spinglass_membership, na.rm = TRUE),
                                  modularity = igraph::modularity(g_undir, membership = igraph::V(g_undir)$spinglass, weights = igraph::E(g_undir)$weight))


  }

  walktrap_stats <- data.frame(method = "walktrap",
                               num_communities = max(walktrap$membership),
                               modularity = walktrap$modularity)

  walktrap_stats <- walktrap_stats[which(walktrap_stats$modularity == max(walktrap_stats$modularity)),]

  igraph::V(g)$cf1 <- memberships$cp_cluster
  igraph::V(g)$lc <- memberships$lc_cluster


  cf1_stats <- data.frame(method = "cp",
                          num_communities = length(unique(memberships$cp_cluster)),
                          modularity = igraph::modularity(g_undir, membership = (memberships$cp_cluster), weights = igraph::E(g_undir)$weight))


  community_summaries <- rbind(edge_betweenness_stats,
                               fast_greedy_stats,
                               infomap_stats,
                               label_prop_stats,
                               leading_eigen_stats,
                               leiden_mod_stats,
                               leiden_cpm_stats,
                               # louvain_stats,
                               # optimal_stats,
                               spinglass_stats,
                               walktrap_stats,
                               cf1_stats,
                               lc_stats)

  # Some functions give multiple modularity scores depending on different cutoff points. Need to ask Jim which one to keep.
  # Is it just the maximum?

  # Get highest modularity score

  # Some functions create modularity scores that vary depending on seed. Something to keep in mind for later.

  # Get median k and feed into stochastic block model
  median_k <- stats::median(community_summaries$num_communities, na.rm = TRUE)


  # Turn graph `g` into an adjacency matrix
  #adjmat <- as.matrix(igraph::as_adjacency_matrix(g_undir, type = "both", names = TRUE))



  # Run stochastic blockmodel
  if (n > 5000 & slow_routines == FALSE) {
    memberships$sbm_membership <- NA
    sbm_stats <- data.frame(method = "sbm", num_communities = NA,
                            modularity = NA)

  } else if (shiny_skip == TRUE) {
    memberships$sbm_membership <- NA
    sbm_stats <- data.frame(method = "sbm", num_communities = NA,
                            modularity = NA)

  } else {
    sbm <- spectral_sbm(Adj = g_sym,
                        k = median_k)

    # Add SBM information to node and network-level datasets
    memberships$sbm_membership <- sbm

    sbm_stats <- data.frame(method = "sbm", num_communities = max(sbm),
                            modularity = igraph::modularity(g_undir, membership = sbm, weights = igraph::E(g_undir)$weight))

  }


  community_summaries <- rbind(community_summaries, sbm_stats)
  rownames(community_summaries) <- NULL

  # I don't have the `reg.SSP` function anywhere, seems like this is
  # vestigial from Alex's source?
  # g_sbm = reg.SSP(as_adjacency_matrix(gsym, sparse=FALSE), K=7)
  # mems_sbm = g_sbm$cluster
  # mod_sbm<-modularity(gsym,mems_sbm)


  # TW: I added a column for unique component ID in case users want to see how
  # spinglass/leading eigen cluster assignments are given within components.
  # But it's messing up the below code. The easy fix is just to rearrange the order
  # of columns in `memberships`:

  if (num_components$no == 1) {

    memberships <- memberships[, c("id",
                                   "edge_betweenness_membership",
                                   "fast_greedy_membership","infomap_membership",
                                   "label_prop_membership", "leiden_mod_membership",
                                   "leiden_cpm_membership", "walktrap_membership",
                                   "leading_eigen_membership",
                                   "spinglass_membership", "sbm_membership",
                                   "cp_cluster", "lc_cluster")]

    start_col <- 2
    sub_val <- 1

  } else {

    memberships <- memberships[, c("id", "component",
                                   "edge_betweenness_membership",
                                   "fast_greedy_membership","infomap_membership",
                                   "label_prop_membership", "leiden_mod_membership",
                                   "leiden_cpm_membership", "walktrap_membership",
                                   "leading_eigen_membership",
                                   "spinglass_membership", "sbm_membership",
                                   "cp_cluster", "lc_cluster")]

    start_col <- 3
    sub_val <- 2

  }

  # Comparing cluster assignments
  compare_scores <- c()

  for(i in start_col:ncol(memberships)) {

    for (j in start_col:ncol(memberships)) {

      if (sum(is.na(memberships[,i])) == nrow(memberships) | sum(is.na(memberships[,j])) == nrow(memberships)) {
        compare_scores[length(compare_scores) + 1] <- NA
      } else {
        compare_scores[length(compare_scores) + 1] <- igraph::compare(memberships[,i], memberships[,j], "adjusted.rand")
      }
    }
  }

  compare_scores <- matrix(compare_scores, nrow = (ncol(memberships)-sub_val))

  # Kieran: Made unique here as was having dimension issues because of repetition
  rownames(compare_scores) <- c("edge_betweenness",
                                "fast_greedy","infomap",
                                "label_prop", "leiden_mod",
                                "leiden_cpm", "walktrap",
                                "leading_eigen",
                                "spinglass", "sbm",
                                "cp", "lc")
  colnames(compare_scores) <- c("edge_betweenness",
                                "fast_greedy","infomap",
                                "label_prop", "leiden_mod",
                                "leiden_cpm", "walktrap",
                                "leading_eigen",
                                "spinglass", "sbm",
                                "cp", "lc")
  diag(compare_scores) <- NA

  compare_scores[is.nan(compare_scores)] <- NA

  # Plot network with nodes colored by community membership
  if (!shiny) {
    # Don't create plots if too large
    if (n > 5000) {
      base::message("Network is too large to visualize all membership assignments simultaneously. Visualizations will not be generated.")
    } else {

      # browser()

      fr <- igraph::layout_with_fr(g) #`layout.fruchterman.reingold()` was deprecated in igraph 2.1.0.

      eb_plot <- function(){
        plot(g,
             vertex.size = 5,
             #edge.width = .1,
             edge.arrow.size = .01,
             vertex.label = NA,
             vertex.color = memberships[,'edge_betweenness_membership'],
             layout = fr)
      }
      eb_grob <- cowplot::as_grob(eb_plot)


      fg_plot <- function() {
        plot(g,
             vertex.size = 5,
             edge.arrow.size = .01,
             vertex.label = NA,
             vertex.color = memberships[,'fast_greedy_membership'],
             layout = fr)
      }
      fg_grob <- cowplot::as_grob(fg_plot)

      infomap_plot <- function(){
        plot(g,
            vertex.size = 5,
            edge.arrow.size = .01,
            vertex.label = NA,
            vertex.color = memberships[,'infomap_membership'],
            layout = fr)
      }
      infomap_grob <- cowplot::as_grob(infomap_plot)

      label_prop_plot <- function(){
        plot(g,
             vertex.size = 5,
             edge.arrow.size = .01,
             vertex.label = NA,
             vertex.color = memberships[,"label_prop_membership"],
             layout = fr)
      }
      label_prop_grob <- cowplot::as_grob(label_prop_plot)


      communityplot1 <- cowplot::plot_grid(eb_grob, fg_grob, infomap_grob, label_prop_grob, nrow = 2,
                                           scale = 1.3,
                                           labels = c("Edge Betweenness", "Fast-Greedy", "Infomap", "Label Prop."),
                                           hjust = c(-.5, -1, -1.8, -1.2)
                        )

      plot(communityplot1)





      leading_eig_plot <- ~plot(g,
                                vertex.size = 5,
                                edge.arrow.size = .01,
                                vertex.label = NA,
                                vertex.color = memberships[,"leading_eigen_membership"],
                                layout = fr)
      leading_eig_grob <- cowplot::as_grob(leading_eig_plot)


      leiden_mod_plot <- ~plot(g,
                           vertex.size = 5,
                           edge.arrow.size = .01,
                           vertex.label = NA,
                           vertex.color = memberships[,"leiden_mod_membership"],
                           layout = fr)
      leiden_mod_grob <- cowplot::as_grob(leiden_mod_plot)

      leiden_cpm_plot <- ~plot(g,
                               vertex.size = 5,
                               edge.arrow.size = .01,
                               vertex.label = NA,
                               vertex.color = memberships[,"leiden_cpm_membership"],
                               layout = fr)
      leiden_cpm_grob <- cowplot::as_grob(leiden_cpm_plot)

      spinglass_plot <- ~plot(g,
                              vertex.size = 5,
                              edge.arrow.size = .01,
                              vertex.label = NA,
                              vertex.color = memberships[,"spinglass_membership"],
                              layout = fr)
      spinglass_grob <- cowplot::as_grob(spinglass_plot)



      communityplot2 <- cowplot::plot_grid(leading_eig_grob, leiden_mod_grob, leiden_cpm_grob, spinglass_grob, nrow = 2,
                                           scale = 1.3,
                                           labels = c("Leading Eigen.", "Leiden (Modularity)", "Leiden (CPM)", "Spinglass"),
                                           hjust = c(-.7, -.4, -.9, -1.3)
      )

      plot(communityplot2)



      walktrap_plot <- ~plot(g,
                             vertex.size = 5,
                             edge.arrow.size = .01,
                             vertex.label = NA,
                             vertex.color = memberships[,"walktrap_membership"],
                             layout = fr)
      walktrap_grob <- cowplot::as_grob(walktrap_plot)

      sbm_plot <- ~plot(g,
                        vertex.size = 5,
                        edge.arrow.size = .01,
                        vertex.label = NA,
                        vertex.color = memberships[,"sbm_membership"],
                        layout = fr)
      sbm_grob <- cowplot::as_grob(sbm_plot)

     cp_plot <- ~plot(g,
                      vertex.size = 5,
                      edge.arrow.size = .01,
                      vertex.label = NA,
                      vertex.color = memberships[,"cp_cluster"],
                      layout = fr)
     cp_grob <- cowplot::as_grob(cp_plot)

     lc_plot <- ~plot(g,
                      vertex.size = 5,
                      edge.arrow.size = .01,
                      vertex.label = NA,
                      vertex.color = memberships[,"lc_cluster"],
                      layout = fr)
     lc_grob <- cowplot::as_grob(lc_plot)


     communityplot3 <- cowplot::plot_grid(walktrap_grob, sbm_grob, cp_grob, lc_grob, nrow = 2,
                                          scale = 1.3,
                                          labels = c("Walktrap", "Stochastic Blockmodel", "Clique Percolation", "Linkcomm"),
                                          hjust = c(-1.5, -.35, -.5, -1.3)
     )

     plot(communityplot3)

    }
  }


  # Assigns the data frame of each node's community membership
  # to the global environment

  # THIS METHOD OF GETTING THE NETWORK NAME DOESN'T WORK.
  # NEED TO CONFER WITH KIERAN ABOUT A BETTER ALTERNATIVE
  gname<-deparse(substitute(g))  #just do it once, fast, but cleaner
  cn<-paste0("comm_members_",gname)

  #assign(x = 'comm_members_net', value = memberships,.GlobalEnv)
  # Make `id` variable to ensure consistent merging with other ideanet dataframes
  memberships$id <- as.numeric(memberships$id)
  output_list$memberships <- memberships
  # assign(x = "comm_members_net", value = memberships, .GlobalEnv)

  # Assigns summaries of community detection output to global environment

  cn<-paste0("comm_summaries_",gname)

  #assign(x = "community_summaries", value = community_summaries,.GlobalEnv)
  output_list$summaries <- community_summaries
  # assign(x = cn, value = community_summaries,.GlobalEnv)


  # Assigns the matrix of adjusted rand scores to global environment
  # assign(x = "community_comparison", value = compare_scores,.GlobalEnv)

  #want to have everything as a dataframe
  cs<-as.data.frame(compare_scores)
  cn<-paste0("comp_scores_",gname)

  # Assigns the matrix of adjusted rand scores to global environment
  output_list$score_comparison <- cs
  # assign(x= cn, value = cs,.GlobalEnv)

  # Add plot to output
  if (shiny == FALSE) {
      output_list$plots <- list("membership_plot1" = communityplot1,
                                "membership_plot2" = communityplot2,
                                "membership_plot3" = communityplot3)
  }


  return(output_list)

}

###########################################################################
# Supporting functions



# Stochastic Blockmodel Function (from Alexander Volfovsky)

spectral_sbm <- function(Adj, ##adjacency matrix
                         k=2, ##how many clusters
                         type="clusters", ##what to output --- cluster labels or centers
                         nstart=10, ##how many restarts for kmeans
                         absol=TRUE, ##do you want to look for smallest and largest eigenvalues, default yes
                         elim_diag=TRUE){
  As <- as.matrix(Adj);
  if(elim_diag)diag(As) <- 0
  DD <- diag(rowSums(As)^(-1/2))
  DD[DD==Inf] <- 0
  LL <- DD%*%As%*%DD

  eigenLL <- eigen(LL)
  if(!absol)svdLL <- eigenLL$vec[,order((eigenLL$val),decreasing=TRUE)[1:k]]
  if(absol)svdLL <- eigenLL$vec[,order(abs(eigenLL$val),decreasing=TRUE)[1:k]]
  # svdLL <- eigen(LL)$vec[,1:k]
  if(type=="clusters")return(stats::kmeans(svdLL,k,nstart=nstart)$cluster)
  if(type=="centers")return(stats::kmeans(svdLL,k)$centers,nstart=nstart)
}

# Function that assigns nodes with multiple communities to a single community to which they are most connected to.
# If there is a tie, then the community is chosen at random (applies to CP and LC methods).

multigroup_assign <- function(gmat, clust){

  # browser()

  cpcomms <- matrix(nrow = nrow(gmat), ncol=length(clust), 0) # create node by community matrix
  rownames(cpcomms) <- rownames(gmat)
  colnames(cpcomms) <- 1:length(clust)


  for (i in 1:length(clust)){ # assign a 1 if node is part of community
    cpcomms[rownames(cpcomms) %in% clust[[i]], i] <- 1
  }

  ties_sent <- gmat %*% cpcomms # number of ties ego sends to group k
  ties_sent <- ties_sent * cpcomms # remove ties that are sent to a group ego is not a part of

  cp_maxcomm <- matrix(nrow = nrow(ties_sent), ncol = 1, 0)
  rownames(cp_maxcomm) <- rownames(gmat)

  for (i in 1:nrow(ties_sent)) {
    maxcols <- which(ties_sent[i,] == max(ties_sent[i,])); # Pick the community to which ego has the most ties
    if (length(maxcols) > 1){
      maxcols <- sample(maxcols,1) # If tied at largest, pick a random community
    }
    cp_maxcomm[i] <- maxcols
  }

  nodes <- as.numeric(rownames(gmat))
  isolates <- setdiff(nodes, unlist(clust))

  if (length(isolates) > 0) { # if isolates, assign them each to their own cluster
    new_cluster_start <- length(clust) + 1
    isolate_clusters <- seq(new_cluster_start, new_cluster_start + length(isolates) - 1)
    names(isolate_clusters) <- isolates
    cp_maxcomm[rownames(cp_maxcomm) %in% isolates, ] <- isolate_clusters[as.character(rownames(cp_maxcomm)[rownames(cp_maxcomm) %in% isolates])]
  }

  cp_maxcomm <- tibble::tibble(cluster = cp_maxcomm) %>% dplyr::mutate(id = as.numeric(rownames(gmat)))
  return(cp_maxcomm)
}




