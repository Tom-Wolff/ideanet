###############################################
#                                             #
#    B I P A R T I T E   F U N C T I O N S    #
#                                             #
###############################################

###################
#    S E T U P    #
###################

library(tidyverse)

# Read in edgelist of Davis women/events network
davis_el <- read.csv("~/Desktop/davis_edgelist.csv")
i_elements <- davis_el$person
j_elements <- davis_el$event
weights <- NULL
type <- NULL
nodelist <- FALSE

# Read in adjacency matrix of same network
davis_adjmat <- readRDS("~/Desktop/davis_mat.rds")
adjacency_matrix <- davis_adjmat
data_type <- "adjacency_matrix"

# Set `bipartite` to `FALSE` to check step 1. below
bipartite <- NULL


###########################################################
#    1 .   C H E C K   B I P A R T I T E   S T A T U S    #
###########################################################

### By default, `netwrite` will check the given data to see if it contains a
### regular network or a bipartite network. If data are found to contain a bipartite
### network, `netwrite` will inform the user that this is the case and treat the network
### as bipartite. Users can override this by setting the `bipartite` argument to `FALSE`

if (is.null(bipartite)) {

  if (data_type == "edgelist") {
    # a. Determine if edgelist is two-mode
    bi_check <- sum(i_elements %in% j_elements) == 0
  }

  if (data_type == "adjacency_matrix") {
    # b. Determine if adjmat is two-mode
    bi_check <- nrow(adjacency_matrix) != ncol(adjacency_matrix)
  }

  if (isTRUE(bi_check)) {
    base::warning("Data found to contain bipartite network. Network will be treated as bipartite unless `bipartite` is set to `FALSE`.")
  }

  bipartite <- bi_check

}

###########################################################
#    2 .   M A K E   N O D E L I S T / E D G E L I S T    #
###########################################################

make_bipartite_list <- function(data,
                                data_type,
                                i_elements = NULL,
                                j_elements = NULL,
                                weights = NULL,
                                type = NULL) {

 # browser()

### Object for storing nodelist and edgelist
bipartite_list <- list(nodelist = NULL,
                       edgelist = NULL,
                       adjmat = NULL)

if (data_type == "adjacency_matrix") {
  bipartite_list$adjmat <- adjacency_matrix

  adj_df <- as.data.frame(adjacency_matrix)
  adj_df$mode1 <- rownames(adj_df)

  bi_el <- adj_df %>%
    tidyr::pivot_longer(cols = -mode1,
                        names_to = "mode2",
                        values_to = "weight") %>%
    dplyr::filter(weight != 0) %>%
    dplyr::mutate(type = 1) %>%
    dplyr::select(mode1, mode2, type, weight)

  bi_nl <- data.frame(id = c(unique(bi_el$mode1),
                                unique(bi_el$mode2)),
                         mode = c(rep(1, length(unique(bi_el$mode1))),
                                  rep(2, length(unique(bi_el$mode2)))
                                  )
  )

  bipartite_list$edgelist <- bi_el
  bipartite_list$nodelist <- bi_nl
}

if (data_type == "edgelist") {
  bi_el <- data.frame(mode1 = i_elements,
                      mode2 = j_elements)

  if (!is.null(type)) {
    bi_el$type <- type
  } else {
    bi_el$type <- 1
  }

  if (!is.null(weights)) {
    bi_el$weight <- weights
  } else {
    bi_el$weight <- 1
  }



  bi_nl <- data.frame(id = c(unique(bi_el$mode1),
                             unique(bi_el$mode2)),
                      mode = c(rep(1, length(unique(bi_el$mode1))),
                               rep(2, length(unique(bi_el$mode2)))
                      ))


  bipartite_list$edgelist <- bi_el
  bipartite_list$nodelist <- bi_nl


}

return(bipartite_list)

}


###########################################
#    3 .   I G R A P H   O B J E C T S    #
###########################################

bi_igraph <- function(bipartite_list) {
  # Make standard igraph object from data frames
  regular_graph <- igraph::graph_from_data_frame(bipartite_list$edgelist,
                                                 directed = FALSE,
                                                 vertices = bipartite_list$nodelist)
  # Identify modes
  bi_map <- igraph::bipartite_mapping(regular_graph)

  igraph::V(regular_graph)$type <- bi_map$type
  igraph::V(regular_graph)$shape <- ifelse(bi_map$type, "square", "circle")
  igraph::V(regular_graph)$color <- ifelse(bi_map$type, "salmon", "lightblue")

  return(regular_graph)

}

regular_graph <- bi_igraph(bipartite_list)

#################################################################################
#    4 .   O N E - M O D E   P R O J E C T I O N S   F R O M   E D G E L I S T  #
#################################################################################

# Creating one-mode projection edgelists with customizable calculation
projection_el <- function(bipartite_list,
                          mode = 1,
                          within_fun = NULL,
                          agg_fun = sum,
                          directed = FALSE) {

  # browser()


  # Store a starting edgelist
  el1 <- bipartite_list$edgelist
  ### Name formatting
  colnames(el1) <- paste(colnames(el1), 1, sep = "")
  # Store `el2` as another copy of this edgelist
  el2 <- bipartite_list$edgelist
  colnames(el2) <- paste(colnames(el2), 2, sep = "")

  # Rename columns to enable merge
  if (mode == 1) {
    colnames(el1)[1:3] <- c("i_elements", "merge", "type")
    colnames(el2)[1:3] <- c("j_elements", "merge", "type")
  } else {
    colnames(el1)[1:3] <- c("merge", "i_elements", "type")
    colnames(el2)[1:3] <- c("merge", "j_elements", "type")
  }

  full_el <- el1 %>%
    dplyr::left_join(el2, by = c("merge", "type"),
                     relationship = "many-to-many") %>%
    dplyr::select(i_elements, j_elements, dplyr::everything()) %>%
    dplyr::filter(i_elements != j_elements) %>%
    dplyr::filter(!is.na(weight1) & !is.na(weight2))

  if (is.null(within_fun)) {
    full_el$weight3 <- 1
  } else {

    if (is.character(within_fun)) {
      within_fun <- eval(parse(text = within_fun))
    }

    full_el$weight3 <- mapply(within_fun, full_el$weight1, full_el$weight2)
  }


  # Dyad ID bit
  # Create dyad IDs
  full_el <- full_el %>%
    dplyr::mutate(node1 = ifelse(i_elements <= j_elements, i_elements, j_elements),
                  node2 = ifelse(i_elements <= j_elements, j_elements, i_elements)) %>%
    dplyr::group_by(node1, node2) %>%
    dplyr::mutate(undirected_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(i_elements, j_elements) %>%
    dplyr::mutate(directed_id = dplyr::cur_group_id())

  if (isTRUE(directed)) {
    full_el <- full_el %>%
      dplyr::select(i_elements, j_elements, dyad_id = directed_id, dplyr::everything()) %>%
      dplyr::select(-node1, -node2, -undirected_id)
  } else {
    full_el <- full_el %>%
      dplyr::filter(i_elements < j_elements) %>%
      dplyr::select(i_elements, j_elements, dyad_id = undirected_id, dplyr::everything()) %>%
      dplyr::select(-node1, -node2, -directed_id)
  }


  # Create a reference edgelist to merge back in
  ref_el <- full_el %>%
    dplyr::group_by(dyad_id, type) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(i_elements, j_elements, dyad_id, type)

  if (is.character(agg_fun)) {
    agg_fun <- eval(parse(text = agg_fun))
  }

  # Calculate aggregated weights
  agg_el <- full_el %>%
    dplyr::group_by(dyad_id, type) %>%
    dplyr::summarize(weight = agg_fun(weight3),
                     n_shared = dplyr::n()) %>%
    dplyr::ungroup()

  # Merge aggregated weights into reference edgelist
  agg_merge <- ref_el %>%
    dplyr::left_join(agg_el, by = c("dyad_id", "type"))


  return(agg_merge)

}


### Use Cases:
####### 1. Edge weights vary
weighted_list <- bipartite_list
weighted_list$edgelist$weight <- sample(c(-5, -4, -3, -2, -1,
                                    1, 2, 3, 4, 5),
                                  nrow(weighted_list$edgelist),
                                  replace = TRUE)

if (var(weighted_list$edgelist$weight) != 0) {

  proj1 <- projection_el(bipartite_list = weighted_list,
                         mode = 1,
                         within_fun = mean,
                         agg_fun = sum,
                         directed = FALSE)

  proj2 <- projection_el(bipartite_list = weighted_list,
                         mode = 2,
                         within_fun = mean,
                         agg_fun = sum,
                         directed = FALSE)

}

####### 2. Aggregating Multiple Edge Types at Once
types_list <- bipartite_list
new_edgelist <- bipartite_list$edgelist
new2 <- new_edgelist[sample(1:nrow(new_edgelist), 40, replace = FALSE),]
new2$type <- 2
new3 <- new_edgelist[sample(1:nrow(new_edgelist), 40, replace = FALSE),]
new3$type <- 3
new_edgelist <- dplyr::bind_rows(new_edgelist,
                                 new2, new3)
types_list$edgelist <- new_edgelist

if (length(unique(types_list$edgelist$type)) > 1) {
  proj1 <- projection_el(bipartite_list = types_list,
                         mode = 1,
                         within_fun = mean,
                         agg_fun = sum,
                         directed = FALSE)

  proj2 <- projection_el(bipartite_list = types_list,
                         mode = 2,
                         within_fun = mean,
                         agg_fun = sum,
                         directed = FALSE)

}

####### After generating these projection edgelists, you would then pass them
####### through standard `netwrite` to generate measures for each one-mode projection

###########################################################
#    5 .   C U S T O M I Z I N G   S O C I O G R A M S    #
###########################################################

bi_plot <- function(bigraph,
                    edge_type = NULL,
                    min_weight = NULL,
                    max_weight = NULL,
                    layout = "fr",
                    color_node = NULL) {

  # browser()

  # Confirm max_weight greater than or equal to min_weight
  if (!is.null(max_weight) & !is.null(min_weight)) {
    if (max_weight < min_weight) {
      stop("Minimum weight value exceeds maximum weight value")
    }
  }

  # Select Edge Type
  if (!is.null(edge_type)) {
    bigraph <- igraph::delete_edges(bigraph,
                                    which(is.na(
                                      eval(parse(
                                        text = paste("igraph::E(bigraph)$", edge_type, sep = "")))
                                    )
                                    )
    )
  }

  # Remove anything below minimum weight value
  if (!is.null(min_weight)) {
    bigraph <- igraph::delete_edges(bigraph,
                                    which(
                                      eval(parse(
                                        text = paste("igraph::E(bigraph)$", edge_type, " <= ", min_weight, sep = "")))

                                    )
    )
  }

  # Remove anything above maximum weight value
  if (!is.null(max_weight)) {
    bigraph <- igraph::delete_edges(bigraph,
                                    which(
                                      eval(parse(
                                        text = paste("igraph::E(bigraph)$", edge_type, " >= ", max_weight, sep = "")))

                                    )
    )
  }

  # Determine layout
  if (layout == "bipartite") {
    save_layout <- igraph::layout.bipartite(bigraph)
  } else {
    save_layout <- igraph::layout.fruchterman.reingold(bigraph)
  }


  # Update node color if necessary
  if (!is.null(color_node)) {

    # Get color variable values
    color_vec <- data.frame(value = eval(parse(text = paste("igraph::V(bigraph)$", color_node, sep = ""))))

    # Create data frame of color links
    color_link <- data.frame(value = unique(color_vec$value),
                             color = colorspace::qualitative_hcl(length(unique(color_vec$value)),
                                                                 palette = "Set 2"))

    # Merge in color values
    color_vec <- color_vec %>%
      dplyr::left_join(color_link, by = "value")

    igraph::V(bigraph)$color <- color_vec$color

  }


  # Plot updated graph
  plot(bigraph, layout = save_layout)

}


#################################################
#    6 .   B I P A R T I T E   D E N S I T Y    #
#################################################

# UCINet has degree, closeness, betweenness, and eigenvector centrality scores
# "The eigenvector centrality of a bipartite representation of a two mode network is the same as the singular vectors associated with the two mode data matrix."

#### Bipartite density (from Borgatti & Everett 1997)
bi_density <- function(bipartite_list, directed = FALSE) {

  if (!is.null(bipartite_list$adjmat)) {
    adjmat <- bipartite_list$adjmat

    if (isFALSE(directed)) {
      dens <- sum(adjmat)/(nrow(adjmat)*ncol(adjmat))
    } else {
      # a directed bipartite matrix is square with a lot of empty
      # cells. I don't really know if that's going to come up here.
      dens <- sum(adjmat)/(2*nrow(adjmat)*ncol(adjmat))
    }

  } else {

    numerator <- nrow(bipartite_list$edgelist)
    denom <- bipartite_list$nodelist %>%
      dplyr::group_by(mode) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      dplyr::ungroup()
    denom <- denom$count

    if (isFALSE(directed)) {
      dens <- numerator/(denom[1]*denom[2])
    } else {
      dens <- numerator/(2*denom[1]*denom[2])
    }

  }

  return(dens)
}

###############################################
#    7 .   B I P A R T I T E   D E G R E E    #
###############################################

#### Bipartite degree and normalized degree (from Borgatti & Everett 1997)
bi_degree <- function(bipartite_list) {

  # browser()

  # Mode 1 Degree Counts
  mode1_degree <- bipartite_list$edgelist %>%
    dplyr::group_by(mode1, type) %>%
    dplyr::summarize(degree = dplyr::n(),
                     weighted_degree = sum(weight)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(id = mode1)

  # Mode 2 Degree Counts
  mode2_degree <- bipartite_list$edgelist %>%
    dplyr::group_by(mode2, type) %>%
    dplyr::summarize(degree = dplyr::n(),
                     weighted_degree = sum(weight)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(id = mode2)

  # Combine
  degree_el <- dplyr::bind_rows(mode1_degree, mode2_degree) %>%
    dplyr::left_join(bipartite_list$nodelist, by = "id")

  # Get number of nodes in each mode, which is the divisor in the normalized
  # degree calculation
  n_sets <- bipartite_list$nodelist %>%
    dplyr::group_by(mode) %>%
    dplyr::summarize(n_set = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mode = ifelse(mode == 1, 2, 1))

  degree_el <- degree_el %>%
    dplyr::left_join(n_sets, by = "mode") %>%
    dplyr::mutate(norm_degree = (degree/n_set)*100) %>%
    dplyr::select(id, mode, type, degree, weighted_degree, norm_degree)

  # Need to pivot wider if multiple edge types
  if (length(unique(degree_el$type)) > 1) {
    degree_el <- degree_el %>%
      tidyr::pivot_wider(id_cols = id:mode,
                         names_from = type,
                         names_prefix = "type",
                         values_from = dplyr::ends_with("degree"),
                         values_fill = 0)
  }

  return(degree_el)

}


#####################################################
#    8 .   B I P A R T I T E   C L O S E N E S S    #
#####################################################

#### Bipartite closeness and normalized closeness (from Borgatti & Everett 1997)
bi_closeness <- function(bipartite_list,
                         weight_type) {

  # If weight type is "frequency", need to modify weight values
  # so they're interpreted as distances
  if (weight_type == "frequency") {
    bipartite_list$edgelist$weight <- 1/bipartite_list$edgelist$weight
  }

  # Create igraph object, needed to get distance matrix
  regular_graph <- bi_igraph(bipartite_list)

  # Create distance matrix
  dist <- igraph::distances(regular_graph)

  # Get farness by taking row sums of distance matrix
  farness <- rowSums(dist)

  # "F r e e m a n ' s normalization, the total distance score is divided into the
  # quantity n - 1, which represents the minimum score possible for a node in an
  # ordinary graph."
  freeman_norm_close <- length(igraph::V(regular_graph)) - 1

  freeman_closeness <- ((1/farness)*freeman_norm_close)*100

  # However, in the bipartite case, it is not possible for any node to be a distance
  # of 1 from all other nodes. Instead, a node may be distance 1 from all nodes in
  # the opposite vertex set, and distance 2 from all nodes in its own vertex set.
  # (Actually, for our connected bipartite graphs, any node which is at a distance 1
  # from all nodes in the opposite vertex set must be a distance of 2 from all the
  # nodes in its own vertex set.)

  # Therefore, the theoretical minimum raw score for a node is n i q-- 2 n o - 2
  # where n o is the size of the node's own vertex set and n i is the size of the
  # other vertex set. This formula, therefore, generates two different values for
  # any bipartite graph with unequal vertex sets. Hence, if we regard the size of
  # each vertex set as fixed, we should normalize closeness by dividing the raw score
  # into one of these quantities, as appropriate. Hence, as in the degree case,
  # we again obtain a nonlinear normalization.

  ### Create divisors for different modes
  n_o <- bipartite_list$nodelist %>%
    dplyr::group_by(mode) %>%
    dplyr::summarize(n_o = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate()

  n_i <- n_o %>% dplyr::mutate(mode = 2:1) %>%
    dplyr::rename(n_i = n_o)

  divisors <- n_o %>%
    dplyr::left_join(n_i, by = "mode") %>%
    dplyr::mutate(bi_norm = n_i + 2*n_o - 2) %>%
    dplyr::select(mode, bi_norm)


  closeness_df <- data.frame(id = names(farness),
                             inv_farness = 1/farness) %>%
    dplyr::left_join(bipartite_list$nodelist, by = "id") %>%
    dplyr::left_join(divisors, by = "mode") %>%
    dplyr::mutate(closeness = inv_farness * bi_norm * 100) %>%
    dplyr::select(id, closeness)

  return(closeness_df)
}


#########################################################
#    9 .   B I P A R T I T E   B E T W E E N N E S S    #
#########################################################

# Bipartite Betweenness(from Borgatti & Everett 1997)
bi_betweenness <- function(bipartite_list,
                           weight_type) {

  # browser()

  # If weight type is "frequency", need to modify weight values
  # so they're interpreted as distances
  if (weight_type == "frequency") {
    bipartite_list$edgelist$weight <- 1/bipartite_list$edgelist$weight
  }

  # Create igraph object
  regular_graph <- bi_igraph(bipartite_list)

  # Get unnormalized betweenness scores. `igraph` produces
  # different non-normalized betweenness scores than UCINet does.
  # This is a known difference that we've simply accepted in our
  # one-mode workflows
  nonnorm_betweenness <- data.frame(id = igraph::V(regular_graph)$name,
                                    betweenness = igraph::betweenness(regular_graph, directed = FALSE,
                                                                      normalized = FALSE)
  ) %>%
  # Merge in mode identification
  dplyr::left_join(bipartite_list$nodelist, by = "id")

  ### Get counts for each mode
  n_o <- bipartite_list$nodelist %>%
    dplyr::group_by(mode) %>%
    dplyr::summarize(n_o = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate()

  n_i <- n_o %>% dplyr::mutate(mode = 2:1) %>%
    dplyr::rename(n_i = n_o)

  divisors <- n_o %>%
    dplyr::left_join(n_i, by = "mode") %>%
    dplyr::mutate(which_max = n_o <= n_i) %>%
    dplyr::mutate(bi_max = dplyr::case_when(isFALSE(which_max) ~ 2*(n_o-1)*(n_i-1),
                                            TRUE ~ (.5*n_i*(n_i-1))+(.5*(n_o-1)*(n_o-2))+((n_o-1)*(n_i-1))
                                              )) %>%
    dplyr::select(mode, bi_max)

  betweenness_df <- nonnorm_betweenness %>%
    dplyr::left_join(divisors, by = "mode") %>%
    dplyr::mutate(norm_betweenness = (betweenness/bi_max)*100) %>%
    select(id, betweenness, norm_betweenness)

  return(betweenness_df)

}

bi_betweenness(bipartite_list)

# Create a data frame of the non-normalized betweenness scores seen in Table 2
# of Borgatti & Everett (1997), along with the modes each value correspond to
o_bet <- data.frame(bet = c(42.76, 22.86, 38.74, 22.01, 4.73, 4.75, 4.14, 2.98, 7.36,6.37, 5.94, 16.29, 25.30, 43.94, 30.73, 5.94, 2.09, 2.09,
           0.97, 0.94, 8.20, 3.45, 16.98, 28.01, 58.10, 108.26, 96.23, 6.82, 9.02, 10.24, 1.89, 1.89),
           mode = c(rep(1, 18), rep (2, 14)))

### Identify what the max value is for each mode
n_o <- o_bet %>%
  dplyr::group_by(mode) %>%
  dplyr::summarize(n_o = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate()

n_i <- n_o %>% dplyr::mutate(mode = 2:1) %>%
  dplyr::rename(n_i = n_o)

max_vals <- n_o %>%
  dplyr::left_join(n_i, by = "mode") %>%
  dplyr::mutate(which_max = n_o <= n_i) %>%
  dplyr::mutate(bi_max = dplyr::case_when(isFALSE(which_max) ~ 2*(n_o-1)*(n_i-1),
                                          TRUE ~ (.5*n_i*(n_i-1))+(.5*(n_o-1)*(n_o-2))+((n_o-1)*(n_i-1))
  ))

o_bet %>%
  dplyr::left_join(max_vals, by = "mode") %>%
  dplyr::mutate(norm_betweenness = (bet/bi_max)*100)


###########################################################
#    1 0 .   B I P A R T I T E   E I G E N V E C T O R    #
###########################################################

# We landed on just applying igraph::eigen_centrality to the entire bipartite
# network, and being sure to specify this choice in the vignette

bi_eigen <- function(bipartite_list) {

  # Create igraph object, needed to get distance matrix
  regular_graph <- bi_igraph(bipartite_list)

  eigen_df <- data.frame(id = igraph::V(regular_graph)$name,
                         eigen_centrality = igraph::eigen_centrality(regular_graph)$vector)

  return(eigen_df)

}
