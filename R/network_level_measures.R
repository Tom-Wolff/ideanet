#####################################################
#    L A R G E S T   W E A K   C O M P O N E N T    #
#####################################################

largest_weak_component_igraph <- function(g){

  # Create a list for storing output
  output_list <- list()

  # Isolating the graph's components
  components <- igraph::clusters(g, mode="weak")
  biggest_cluster_id <- which.max(components$csize)

  # Extracting the ids of the largest component
  largest_component_ids <- igraph::V(g)[components$membership == biggest_cluster_id]

  # Extracting Subgraph
  largest_component <- igraph::induced_subgraph(g, largest_component_ids)

  # Assigning the ID list and Subgraph to the Global Environment
  # assign(x = 'weak_membership', value = weak_membership,.GlobalEnv)
  output_list$largest_component_ids <- largest_component_ids
  # assign(x = 'largest_component_ids', value = largest_component_ids,.GlobalEnv)
  output_list$largest_component <- largest_component
  # assign(x = 'largest_component', value = largest_component,.GlobalEnv)

  return(output_list)
}



###############################################
#    L A R G E S T   B I C O M P O N E N T    #
###############################################

largest_bicomponent_igraph <- function(g) {

  # Create a list
  output_list <- list()

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
                                      size_bicomponent = sum(merge_df$in_largest_bicomponent, na.rm = TRUE),
                                      prop_bicomponent = sum(merge_df$in_largest_bicomponent, na.rm = TRUE)/nrow(merge_df))

    output_list$bicomponent_summary <- bicomponent_summary
    # assign(x = "bicomponent_summary", value = bicomponent_summary, .GlobalEnv)
    output_list$largest_bicomponent_memberships <- merge_df
    # assign(x = "largest_bicomponent_memberships", value = merge_df, .GlobalEnv)
    # rm(bicomponent_df, merge_df)


    # Assigning the ID list and Subgraph to the Global Environment
    output_list$largest_bicomponent_ids <- largest_bicomp_ids
    # assign(x = 'largest_bicomponent_ids', value = largest_bicomp_ids,.GlobalEnv)
    output_list$largest_bi_component <- largest_bi_component
    # assign(x = 'largest_bi_component', value = largest_bi_component,.GlobalEnv)

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
                             size_bicomponent = sum(bicomp_id_merge$in_largest_bicomponent, na.rm = TRUE),
                             prop_bicomponent = sum(bicomp_id_merge$in_largest_bicomponent, na.rm = TRUE)/length(igraph::V(g)$name))

        bicomponent_summary <- rbind(bicomponent_summary, sum_df)

      }


      bicomponent_df <- rbind(bicomponent_df, bicomp_id_merge)

      largest_bi_component[[i]] <- igraph::induced_subgraph(g, largest_bicomp_ids)

      output_list$largest_bicomponent_ids <- largest_bicomp_ids
      # assign(x = paste('largest_bicomponent_ids', i, sep = "_"), value = largest_bicomp_ids,.GlobalEnv)
      #      assign(x = paste('largest_bi_component', i, sep = "_"), value = largest_bi_component,.GlobalEnv)


    }

    output_list$largest_bi_component <- largest_bi_component
    # assign(x = 'largest_bi_component', value = largest_bi_component,.GlobalEnv)

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

    output_list$bicomponent_summary <- bicomponent_summary
    # assign(x = "bicomponent_summary", value = bicomponent_summary, .GlobalEnv)
    output_list$largest_bicomponent_memberships <- merge_df
    # assign(x = "largest_bicomponent_memberships", value = merge_df, .GlobalEnv)

    # rm(bi_components, bi_component_list, bi_lengths, largest_id, this_node, bicomponent_df, bicomponent_df2, merge_df, sum_df)
  }

  return(output_list)

}

#############################################################
#    B E T W E E N N E S S   C E N T R A L I Z A T I O N    #
#############################################################


betweenness_centralization <- function(g, weights, directed, weight_type) {

  # browser()

  # First step is to run the `betweenness` function and see wheter it produces
  # one measure or two
  bet_scores <- betweenness(g, weights = weights, directed = directed, weight_type = weight_type)

  # Might as well generate the star graph here too
  star_graph <- igraph::make_star(length(igraph::V(g)), mode = "undirected")
  star_bet <- betweenness(star_graph, weights = NULL, directed = FALSE)
  max_star <- max(star_bet, na.rm = TRUE)
  denom <- sum(max_star - star_bet)
  # What to do if `betweeness` produces betweenness and binarized betweenness
  if ("data.frame" %in% class(bet_scores)) {

    max_bet <- sapply(bet_scores, max, na.rm = TRUE)
    pre_num <- bet_scores
    pre_num[,1] <- max_bet[1] - pre_num[,1]
    pre_num[,2] <- max_bet[2] - pre_num[,2]
    numerator <- colSums(pre_num)
    betweenness_cent_vals <- numerator/denom

    betweenness_cent <- list(betweenness_centralization = betweenness_cent_vals["betweenness"],
                             binarized_betweenness_centralization = betweenness_cent_vals["binarized_betweenness"])


    # What to do if `betweenness` just produces a single vector
  } else {

    max_bet <- max(bet_scores, na.rm = TRUE)
    numerator <- sum(max_bet - bet_scores)

    betweenness_cent <- list(betweenness_centralization = numerator/denom,
                             binarized_betweenness_centralization = NA)
  }

  return(betweenness_cent)

}


#############################################################
#    E I G E N V E C T O R   C E N T R A L I Z A T I O N    #
#############################################################


eigen_centralization <- function(g, directed) {

  # See if multiple weak components
  if (length(unique(igraph::V(g)$weak_membership)) > 1) {
    warning("Eigenvector centralization calculated only for largest weak component.")
    ### Extract first largest weak component
    ##### Get component ID of first largest weak component
    if (length(unique(igraph::V(g)$weak_membership[igraph::V(g)$in_largest_weak])) > 1) {
      warning("Network has 2+ largest weak component of equal size. Only one of these will be used for calculating eigenvector centralization.")
    }

    g2 <- igraph::subgraph(g, vids = igraph::V(g)$weak_membership == min(igraph::V(g)$weak_membership[igraph::V(g)$in_largest_weak]))

    # If network is single component
  } else {
    g2 <- g
  }

  if (directed == TRUE) {
    # If we have a directed network
    eigen_cent_d <- eigen_igraph(g2, directed = TRUE, message = FALSE)
    eigen_cent_u <- eigen_igraph(igraph::as.undirected(g2), directed = FALSE, message = FALSE)

    numerator_d <- sum(max(eigen_cent_d, na.rm = TRUE) - eigen_cent_d, na.rm = TRUE)
    numerator_u <- sum(max(eigen_cent_u, na.rm = TRUE) - eigen_cent_u, na.rm = TRUE)

    star_d <- igraph::make_star(length(igraph::V(g2)), mode = "in")
    igraph::E(star_d)$weight <- 1
    igraph::V(star_d)$name <- 0:(length(igraph::V(g2))-1)
    eigen_star_d <- eigen_igraph(star_d, directed = TRUE, message = FALSE)
    denominator_d <- sum(max(eigen_star_d, na.rm = TRUE) - eigen_star_d, na.rm = TRUE)

    star_u <- igraph::make_star(length(igraph::V(g2)), mode = "undirected")
    igraph::E(star_u)$weight <- 1
    igraph::V(star_u)$name <- 0:(length(igraph::V(g2))-1)
    eigen_star_u <- eigen_igraph(star_u, directed = FALSE, message = FALSE)
    denominator_u <- sum(max(eigen_star_u, na.rm = TRUE) - eigen_star_u, na.rm = TRUE)

    centralization_d <- numerator_d/denominator_d
    centralization_u <- numerator_u/denominator_u


  } else {
    # If we have an undirected network
    eigen_cent_d <- NA
    centralization_d <- NA

    eigen_cent_u <- eigen_igraph(igraph::as.undirected(g2), directed = FALSE, message = FALSE)
    numerator_u <- sum(max(eigen_cent_u, na.rm = TRUE) - eigen_cent_u, na.rm = TRUE)

    star_u <- igraph::make_star(length(igraph::V(g2)), mode = "undirected")
    igraph::E(star_u)$weight <- 1
    igraph::V(star_u)$name <- 0:(length(igraph::V(g2))-1)
    eigen_star_u <- eigen_igraph(star_u, directed = FALSE, message = FALSE)
    denominator_u <- sum(max(eigen_star_u, na.rm = TRUE) - eigen_star_u, na.rm = TRUE)

    centralization_u <- numerator_u/denominator_u
  }

  return(list(directed = centralization_d,
              undirected = centralization_u))

}


###################################################
#    D E G R E E   C E N T R A L I Z A T I O N    #
###################################################

degree_centralization <- function(g, directed = directed) {

  # DIRECTED NETS
  if (directed == TRUE) {

    # Get degree scores
    degrees <- total_degree(g, directed = TRUE)
    ### Incoming ties
    max_in <- max(degrees$total_degree_in, na.rm = TRUE)
    numerator_in <- sum(max_in - degrees$total_degree_in)
    ### Outgoing ties
    max_out <- max(degrees$total_degree_out, na.rm = TRUE)
    numerator_out <- sum(max_out - degrees$total_degree_out)
    ### Undirected
    max_un <- max(degrees$total_degree_all, na.rm = TRUE)
    numerator_un <- sum(max_un - degrees$total_degree_all)

    # Generate star graphs for network of this size and calculate denominators
    ### Incoming Ties
    star_in <- igraph::make_star(length(igraph::V(g)), mode = "in")
    ##### For degree, maximum degree will be n-1
    star_in_degree <- igraph::degree(star_in, mode = "in")
    max_star_in_degree <- max(star_in_degree, na.rm = TRUE)
    denom_in <- sum(max_star_in_degree - star_in_degree)
    centralization_in <- numerator_in/denom_in
    ### Outgoing Ties
    star_out <- igraph::make_star(length(igraph::V(g)), mode = "out")
    ##### For degree, maximum degree will be n-1
    star_out_degree <- igraph::degree(star_out, mode = "out")
    max_star_out_degree <- max(star_out_degree, na.rm = TRUE)
    denom_out <- sum(max_star_out_degree - star_out_degree)
    centralization_out <- numerator_out/denom_out
    ### Undirected Ties
    star_undirected <- igraph::make_star(length(igraph::V(g)), mode = "undirected")
    star_un_degree <- igraph::degree(star_undirected, mode = "all")
    max_star_un_degree <- max(star_un_degree, na.rm = TRUE)
    denom_un <- sum(max_star_un_degree - star_un_degree)
    centralization_un <- numerator_un/denom_un

    centralization_scores <- list(centralization_in = centralization_in,
                                  centralization_out = centralization_out,
                                  centralization_un = centralization_un)

    return(centralization_scores)


    # UNDIRECTED NETS
  } else {

    degrees <- total_degree(g, directed = FALSE)
    max_degree <- max(degrees$total_degree_all, na.rm = TRUE)
    numerator <- sum(max_degree - degrees$total_degree_all)

    star_undirected <- igraph::make_star(length(igraph::V(g)), mode = "undirected")
    star_un_degree <- igraph::degree(star_undirected, mode = "all")
    max_star_un_degree <- max(star_un_degree, na.rm = TRUE)
    denom_un <- sum(max_star_un_degree - star_un_degree)

    degree_cent <- numerator/denom_un

    return(degree_cent)

  }

}



#########################################################
#    C L O S E N E S S   C E N T R A L I Z A T I O N    #
#########################################################


closeness_centralization <- function(g, directed = directed, weight_type) {

  # `closeness_igraph` calls upon a `weight` attribute in `g`. On the off-chance
  # that `g` lacks this attribute, go ahead and replace it with `1` values
  if (is.null(igraph::E(g)$weight)) {
    igraph::E(g)$weight <- 1
  }

  # DIRECTED NETS
  if (directed == TRUE) {

    # Get closeness scores
    g_closeness_scores <- closeness_igraph(g, directed = TRUE,
                                           weight_type = weight_type)
    ### Incoming Ties
    max_in <- max(g_closeness_scores$closeness_in, na.rm = TRUE)
    numerator_in <- sum(max_in - g_closeness_scores$closeness_in)
    ### Outgoing Ties
    max_out <- max(g_closeness_scores$closeness_out, na.rm = TRUE)
    numerator_out <- sum(max_out - g_closeness_scores$closeness_out)
    ### Undirected
    max_un <- max(g_closeness_scores$closeness_un, na.rm = TRUE)
    numerator_un <- sum(max_un - g_closeness_scores$closeness_un)

    # Generate star graphs for network of this size and calculate denominators
    ### Incoming Ties
    star_in <- igraph::make_star(length(igraph::V(g)), mode = "in")
    igraph::E(star_in)$weight <- 1
    star_in_closeness <- closeness_igraph(star_in, directed = TRUE,
                                          weight_type = weight_type)
    max_star_in_closeness <- max(star_in_closeness$closeness_in, na.rm = TRUE)
    denom_in <- sum(max_star_in_closeness - star_in_closeness$closeness_in)
    centralization_in <- numerator_in/denom_in
    ### Outgoing Ties
    star_out <- igraph::make_star(length(igraph::V(g)), mode = "out")
    igraph::E(star_out)$weight <- 1
    star_out_closeness <- closeness_igraph(star_out, directed = TRUE,
                                           weight_type = weight_type)
    max_star_out_closeness <- max(star_out_closeness$closeness_out, na.rm = TRUE)
    denom_out <- sum(max_star_out_closeness - star_out_closeness$closeness_out)
    centralization_out <- numerator_out/denom_out
    ### Undirected Ties
    star_undirected <- igraph::make_star(length(igraph::V(g)), mode = "undirected")
    igraph::E(star_undirected)$weight <- 1
    star_un_closeness <- closeness_igraph(star_undirected, directed = FALSE,
                                          weight_type = "distance")
    max_star_un_closeness <- max(star_un_closeness, na.rm = TRUE)
    denom_un <- sum(max_star_un_closeness - star_un_closeness)
    centralization_un <- numerator_un/denom_un

    centralization_scores <- list(centralization_in = centralization_in,
                                  centralization_out = centralization_out,
                                  centralization_un = centralization_un)

    return(centralization_scores)

    # UNDIRECTED NETS
  } else {
    g_closeness_scores <- closeness_igraph(g, directed = FALSE,
                                           weight_type = weight_type)
    max_closeness <- max(g_closeness_scores, na.rm = TRUE)
    numerator <- sum(max_closeness - g_closeness_scores)

    # Generate star graph for network of this size
    star_undirected <- igraph::make_star(length(igraph::V(g)), mode = "undirected")
    igraph::E(star_undirected)$weight <- 1

    star_closeness <- closeness_igraph(star_undirected, directed = FALSE,
                                       weight_type = "distance")
    max_star_closeness <- max(star_closeness, na.rm = TRUE)
    denominator <- sum(max_star_closeness - star_closeness)

    closeness_cent <- numerator/denominator
    return(closeness_cent)

  }

}

#######################################
#    S D   B Y   C O M P O N E N T    #
#######################################

# Function for handling multiple components
sd_component <- function(measure, components = NULL, largest_component = TRUE) {

  # browser()

  # If no list of components is given, just run `herfindahl_index`
  if (is.null(components)) {
    return(stats::sd(measure))
  } else {
    # Create reference dataframe of components by size
    comp_df <- as.data.frame(table(components)) %>%
      # We only work with components with 5 or more nodes
      dplyr::filter(.data$Freq >= 5) %>%
      dplyr::rename(component = .data$components, size = .data$Freq) %>%
      dplyr::mutate(sd = NA)
    comp_df[,1] <- as.numeric(comp_df[,1])

    # If multiple components remain, give warning
    if (nrow(comp_df) > 1 & isTRUE(largest_component)) {
      warning("Network consists of multiple isolated components. Standard deviation will only be calculated for largest isolated component.")
    }

    # Filter `comp_df` to largest component if `largest_component` is `TRUE`
    if (isTRUE(largest_component)) {
      comp_df <- comp_df %>%
        dplyr::filter(.data$size == max(.data$size))
      ### If two largest components of same size, just take top one
      comp_df <- comp_df[1,]
    }

    # Now calculate standard deviation for each component still listed in `comp_df`
    for (i in 1:nrow(comp_df)) {
      this_component <- comp_df[i, 1]
      these_measures <- measure[components == this_component]
      comp_df[i, "sd"] <- stats::sd(these_measures)
    }
    return(comp_df)
  }}

#########################################
#    H E R F I N D A H L   I N D E X    #
#########################################

# Function for handling multiple components
herfindahl <- function(measure, components = NULL, largest_component = TRUE) {

  # browser()

  # If no list of components is given, just run `herfindahl_index`
  if (is.null(components)) {
    return(herfindahl_index(measure))
  } else {
    # Create reference dataframe of components by size
    comp_df <- as.data.frame(table(components)) %>%
      # We only work with components with 5 or more nodes
      dplyr::filter(.data$Freq >= 5) %>%
      dplyr::rename(component = .data$components, size = .data$Freq) %>%
      dplyr::mutate(herfindahl = NA)
    comp_df[,1] <- as.numeric(comp_df[,1])

    # If multiple components remain, give warning
    if (nrow(comp_df) > 1 & isTRUE(largest_component)) {
      warning("Network consists of multiple isolated components. Herfindahl index will only be calculated for largest isolated component.")
    }

    # Filter `comp_df` to largest component if `largest_component` is `TRUE`
    if (isTRUE(largest_component)) {
      comp_df <- comp_df %>%
        dplyr::filter(.data$size == max(.data$size))
      ### If two largest components of same size, just take top one
      comp_df <- comp_df[1,]
    }

    # Now calculate Herfindahl for each component still listed in `comp_df`
    for (i in 1:nrow(comp_df)) {
      this_component <- comp_df[i, 1]
      these_measures <- measure[components == this_component]
      comp_df[i, "herfindahl"] <- herfindahl_index(these_measures)
    }
    return(comp_df)
  }}

# Core function
herfindahl_index <- function(x) {

  # Get sum of `x` measure vector
  sum_total <- sum(x, na.rm = TRUE)
  # Now divide `x` by `sum_total` and multiply by 100
  pct_shares <- (x/sum_total) * 100
  # If there are any `NA` values, substitute with zero
  pct_shares[is.na(pct_shares)] <- 0
  # Square values and sum
  squared <- sum(pct_shares^2)

  # Return `squared`
  return(squared)

}


#########################################
#    G I N I   C O E F F I C I E N T    #
#########################################

# Function generated by ChatGPT, adapted to work with netwrite's data structure
# and pay attention to components
gini <- function(measure, components = NULL, largest_component = TRUE) {

  # browser()

  # If no list of components is given, just run `gini_co`
  if (is.null(components)) {
    return(gini_co(measure))
  } else {
    # Create reference dataframe of components by size
    comp_df <- as.data.frame(table(components)) %>%
      # We only work with components with 5 or more nodes
      dplyr::filter(.data$Freq >= 5) %>%
      dplyr::rename(component = .data$components, size = .data$Freq) %>%
      dplyr::mutate(gini = NA)
    comp_df[,1] <- as.numeric(comp_df[,1])

    # If multiple components remain, give warning
    if (nrow(comp_df) > 1 & isTRUE(largest_component)) {
      warning("Network consists of multiple isolated components. Gini coefficient will only be calculated for largest isolated component.")
    }

    # Filter `comp_df` to largest component if `largest_component` is `TRUE`
    if (isTRUE(largest_component)) {
      comp_df <- comp_df %>%
        dplyr::filter(.data$size == max(.data$size))
      ### If two largest components of same size, just take top one
      comp_df <- comp_df[1,]
    }

    # Now calculate Gini for each component still listed in `comp_df`
    for (i in 1:nrow(comp_df)) {
      this_component <- comp_df[i, 1]
      these_measures <- measure[components == this_component]
      comp_df[i, "gini"] <- gini_co(these_measures)
    }
    return(comp_df)
  }}


gini_co <- function(x) {
  # browser()
  x <- as.numeric(x)
  n <- length(x)
  mu <- mean(x)
  num <- sum(abs(outer(x, x, "-")))
  gdp <- num / (2 * n^2 * mu)
  return(gdp)
}

#######################################################
#    T H E I L   I N D E X   ( U N G R O U P E D )    #
#######################################################


# Function generated by ChatGPT, adapted to work with netwrite's data structure
# and pay attention to components
theil <- function(measure, components = NULL, largest_component = TRUE) {

  # browser()

  # If no list of components is given, just run `theil_index`
  if (is.null(components)) {
    return(theil_index(measure, limit_case = TRUE))
  } else {
  # Create reference dataframe of components by size
    comp_df <- as.data.frame(table(components)) %>%
      # We only work with components with 5 or more nodes
      dplyr::filter(.data$Freq >= 5) %>%
      dplyr::rename(component = .data$components, size = .data$Freq) %>%
      dplyr::mutate(theil = NA)
    comp_df[,1] <- as.numeric(comp_df[,1])

    # If multiple components remain, give warning
    if (nrow(comp_df) > 1 & isTRUE(largest_component)) {
      warning("Network consists of multiple isolated components. Theil index will only be calculated for largest isolated component.")
    }

    # Filter `comp_df` to largest component if `largest_component` is `TRUE`
    if (isTRUE(largest_component)) {
      comp_df <- comp_df %>%
        dplyr::filter(.data$size == max(.data$size))
      ### If two largest components of same size, just take top one
      comp_df <- comp_df[1,]
    }

    # Now calculate Theil for each component still listed in `comp_df`
    for (i in 1:nrow(comp_df)) {
      this_component <- comp_df[i, 1]
      these_measures <- measure[components == this_component]
      comp_df[i, "theil"] <- theil_index(these_measures, limit_case = TRUE)
    }

return(comp_df)


  }}



# Function generated by ChatGPT, adapted to support different solutions to
# zero-handling
theil_index <- function(x, constant = NULL, limit_case = FALSE) {

  # browser()
  ### If a constant is specified, add constant to all values of `x`
  if (!is.null(constant)) {
    x <- x + constant
  }

  if (isFALSE(limit_case)) {
    ### Remove zero or negative values to avoid issues with log
    x <- x[x > 0]
  }

  ### Mean income
  mu <- mean(x)


  ### Calculate values to sum in numerator
  numerator_vals <- (x/mu)*log(x/mu)

  ### Some people treat zeroes as a "limit case" where they're simply treated as a
  ### (x/mu)*log(x/mu) = 0 value in calculation. If `isTRUE(limit_case)`,
  ### we recode any `NaN` values in `numerator_vals` as 0.
  if (isTRUE(limit_case)) {
    numerator_vals[is.nan(numerator_vals)] <- 0
  }

  # Theil index calculation
  theil <- sum(numerator_vals) / length(x)
  return(theil)
}


###########################################
#    T R A N S I T I V I T Y   R A T E    #
###########################################

trans_rate_igraph <- function(g, binarize = FALSE) {
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

  # Binarize two-paths if that's what we want
  if (binarize == TRUE) {
    i2[i2 > 1] <- 1
  }

  # Elementwise multiply by adjacency, will = 1 if two path is transitive
  t3 <- sum(i2*inmat)

  # Sum of two-paths, minus diagonal
  a3 <- sum(i2[row(i2) != col(i2)])

  # Calculate the proportion
  tv <- t3/a3

  # Assigning transitivity_rate to the global environment
  # assign(x = 'transitivity_rate', value = tv,.GlobalEnv)
  # rm(inmat, i2, t3, a3)

  return(tv)

}


###################################################################
#    G L O B A L   C L U S T E R I N G   C O E F F I C I E N T    #
###################################################################

# Jim specifies the calculation for GCC as the ratio of closed triads to open
# triads

gcc <- function(g) {

  t_census <- igraph::triad.census(g)

  # Numerator is number of closed triads
  closed_t <- sum(t_census[c(9:10, 12:16)])
  # Open triads
  open_t <- sum(t_census[c(4:8, 11)])

  gcc <- closed_t/(open_t + closed_t)

  return(gcc)

}


#################################################
#    D E G R E E   A S S O R T A T I V I T Y    #
#################################################

degree_assortativity <- function(g, directed) {

  degree_counts <- total_degree(g, directed = directed)

  # Get edgelist
  el1 <- as.data.frame(igraph::get.edgelist(g, names = TRUE))
  colnames(el1) <- c("ego", "alter")

  # Symmetrize Edgelist

  el2 <- el1
  colnames(el2) <- c("alter", "ego")

  sym_el <- dplyr::bind_rows(el1, el2) %>%
    unique()

  el2 <- el1
  colnames(el2) <- c("alter", "ego")
  sym_el <- dplyr::bind_rows(el1, el2) %>%
    unique()


  # Get edgewise correlation on total degree

  total_ego <- degree_counts[,c("id", "total_degree_all")]
  colnames(total_ego) <- c("ego", "ego_degree")
  total_alter <- total_ego
  colnames(total_alter) <- c("alter", "alter_degree")

  total_el <- sym_el %>%
    dplyr::left_join(total_ego, by = "ego") %>%
    dplyr::left_join(total_alter, by = "alter")

  total_cor <- stats::cor(total_el[,3], total_el[,4])

  # If directed, do the same for indegree and outdegree
  if (directed == TRUE) {

    # Indegree
    in_ego <- degree_counts[,c("id", "total_degree_in")]
    colnames(in_ego) <- c("ego", "ego_indegree")
    in_alter <- in_ego
    colnames(in_alter) <- c("alter", "alter_indegree")

    in_el <- el1 %>%
      dplyr::left_join(in_ego, by = "ego") %>%
      dplyr::left_join(in_alter, by = "alter")

    in_cor <- stats::cor(in_el[,3], in_el[,4])

    # Outdegre
    out_ego <- degree_counts[,c("id", "total_degree_out")]
    colnames(out_ego) <- c("ego", "ego_outdegree")
    out_alter <- out_ego
    colnames(out_alter) <- c("alter", "alter_outdegree")

    out_el <- el1 %>%
      dplyr::left_join(out_ego, by = "ego") %>%
      dplyr::left_join(out_alter, by = "alter")

    out_cor <- stats::cor(out_el[,3], out_el[,4])

  } else {

    in_cor <- NA
    out_cor <- NA

  }

  # Store `cor` values in list and return
  return(list(total = total_cor,
              indegree = in_cor,
              outdegree = out_cor))

}

# assortativity_degree <- function(g, directed = directed) {
#   # Extracting the graph's edgelist
#   edges <- as.data.frame(igraph::get.edgelist(g, names = FALSE))
#   colnames(edges) <- c("ego", "alter")
#
#   # Calculating the total degree for each node
#   # node_degree <- sna::degree(g2, gmode="digraph", cmode='freeman', ignore.eval=TRUE)
#   # node_degree <- as.data.frame(cbind(seq(1, length(node_degree), 1), node_degree))
#
#   node_degree <- total_degree(g, directed = TRUE)$total_degree_all
#   # node_degree <- igraph::degree(g, mode = "all", loops = FALSE)
#   node_degree <- data.frame("ego" = seq(1, length(node_degree), 1),
#                             "degree" = node_degree)
#
#     # as.data.frame(cbind(seq(1, length(node_degree), 1), node_degree))
#
#   # Joining i & j ids
#   colnames(node_degree)[[1]] <- colnames(edges)[[1]]
#   colnames(node_degree)[[2]] <- "degree"
#   edges <- dplyr::left_join(edges, node_degree, by="ego")
#   colnames(edges)[[3]] <- c('i_degree')
#
#   colnames(node_degree)[[1]] <- colnames(edges)[[2]]
#   edges <- dplyr::left_join(edges, node_degree, by=colnames(edges)[[2]])
#   colnames(edges)[[4]] <- c('j_degree')
#   rm(node_degree)
#
#   # Calculating the Pearson Correlation of i and j degree variables
#   degree_assortatvity <- stats::cor(edges$i_degree, edges$j_degree, method='pearson')
#
#   # Assigning correlation value to the global environment
#   assign(x = 'degree_assortatvity', value = degree_assortatvity,.GlobalEnv)
# }


#########################################
#    A V E R A G E   G E O D E S I C    #
#########################################

average_geodesic <- function(g) {

  # browser()

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
  return(average_path_length)
  # assign(x = 'average_path_length', value = average_path_length,.GlobalEnv)
  # rm(gd, geodesics)
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
      pairs <- t(utils::combn(paste0(types,'_','weight'), 2))
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
      pairs <- t(utils::combn(paste0(types,'_','weight'), 2))
      for(i in nrow(pairs)) {
        column_set <- pairs[i,]
        tie_set <- ties[,column_set]
        multiplex_edge_correlation <- paste0('Edge Correlation for ', paste(column_set, collapse= ' and '), ': ', round(stats::cor(tie_set)[1,2], digits=2))
        rm(column_set, tie_set)
      }
      rm(pairs, types, subnets, ties)
    }

    # Assigning final scores to global environment
    # assign(x = 'multiplex_edge_correlation', value = multiplex_edge_correlation,.GlobalEnv)
  }else{
    edgelist <- edgelist[,]
    multiplex_edge_correlation <- 'Simplex Network'
  }
  return(multiplex_edge_correlation)
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
  graph <- igraph::simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)

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
