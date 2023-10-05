#' Network Cleaning and Variable Calculation (`netwrite`)
#'
#' @description The `netwrite` function reads in relational data of several formats and processes them into a set of standardized outputs. These outputs include sets of commonly calculated measures at the individual node and network-wide levels.
#'
#' @param data_type A character value indicating the type of relational data being entered into `netwrite`. Available options are `edgelist`, `adjacency_matrix`, and `adjacency_list`.
#' @param adjacency_matrix If `data_type` is set to `adjacency_matrix`, a matrix object containing the adjacency matrix for the network being processed.
#' @param adjacency_list If `data_type` is set to `adjacency_list`, a data frame containing the adjacency list for the network being processed.
#' @param nodelist Either a vector of values indicating unique node/vertex IDs, or a data frame including all information about nodes in the network. If the latter, a value for `node_id` must be specified.
#' @param node_id If a data frame is entered for the `nodelist` arugment, `node_id` should be a character value indicating the name of the column in the node-level data frame containing unique node identifiers.
#' @param i_elements If `data_type` is set to `edgelist`, a numeric or character vector indicating the sender of ties in the edgelist.
#' @param j_elements If `data_type` is set to `edgelist`, a numeric or character vector indicating the receiver of ties in the edgelist.
#' @param fix_nodelist If `data_type` is set to `edgelist` and user inputs a vector or data frame into `nodelist`, a logical value indicating whether to include node IDs that do not appear in the nodelist but do appear in the edgelist in the nodelist used when processing network data. By default, `fix_nodelist` is set to `FALSE` to identify potential inconsistencies between the nodelist and edgelist to the user.
#' @param weights A numeric vector indicating the weight of ties in the edgelist.
#' @param type A numeric or character vector indicating the types of relationships represented in the edgelist. If `type` contains this vector, `netwrite` will treat the data as a multi-relational network and produce additional outputs reflecting the different types of ties occuring in the network.
#' @param remove_loops A logical value indicating whether "self-loops" (ties directed toward oneself) should be considered valid ties in the network being processed.
#' @param package (Deprecated) A character value indicating what format network objects should be produced by `netwrite`. Available options are `igraph` and `network`.
#' @param missing_code A numeric value indicating "missing" values in an edgelist. Such "missing" values are sometimes included to identify the presence of isolated nodes in an edgelist when a corresponding nodelist is unavailable.
#' @param weight_type A character value indicating whether edge weights should be treated as frequencies or distances. Available options are `frequency` and `distance`.
#' @param directed A logical value indicating whether edges should be treated as a directed or undirected when constructing the network.
#' @param net_name A character value indicating the name to which network/igraph objects should be given.
#' @param shiny A logical value indicating whether `netwrite` is being used in conjunction with IDEANet's Shiny-based visualization app. Should be set to its default (`FALSE`) when not being used with the Shiny app.
#' @param output A character vector indicating the kinds of objects `netwrite` should assign to the global environment. `netwrite` produces several outputs that may not all be necessary to a user's needs. Users can specify which outputs they specifically want in order to minimize the number of objects appearing in the global environment. Potential outputs include igraph object(s) (`"graph"`), subgraph(s) of only nodes that appear in the largest component and/or bicomponent of the network (`"largest_component"`, `"largest_bi_component"`), data frame(s) containing node-level measures (`"node_measure_plot"`), a processed edgelist of the network (`"edgelist"`), a data frame indicating network-level summaries (`"system_level_measures"`), and summary visualizations for node- and network-level measures (`"node_measure_plot"`, `"system_measure_plot"`).
#' @param message A logical value indicating whether warning messages should be displayed in the R console during processing.
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


##########################################################
#   PERFORMING MULTI-RELATIONAL FUNCTIONS IF SPECIFIED   #
##########################################################

# This is `netwrite` proper, as users encounter it. `netwrite` proper is effectively
# a wrapper for `basic_netwrite` that detects whether there's a multirelational
# edgelist present for which `basic_netwrite` needs to be applied to each relation type's
# respective subgraph

netwrite <- function(data_type = c('edgelist'), adjacency_matrix=FALSE,
                     adjacency_list=FALSE,
                     nodelist=FALSE,
                     # `node_id` takes a character argument specifying
                     # how the original node ID variable should be named
                     # in output
                     node_id = NULL,
                     i_elements=FALSE,
                     j_elements=FALSE,
                     # In the rare event that an edgelist contains node IDs that are
                     # not in the nodelist, `fix_nodelist` will add these node IDs to the
                     # nodelist used in network processing
                     fix_nodelist = TRUE,
                     # I THINK the `weights` argument should work for adjmats if we just have users set to TRUE when using a weighted adjmat
                     weights=NULL, type=NULL,
                     remove_loops = FALSE,
                     package='igraph', missing_code=99999,
                     weight_type='frequency', directed=FALSE,
                     net_name='network',
                     shiny = FALSE,
                     output = c("graph",
                                "largest_bi_component",
                                "largest_component",
                                "node_measure_plot",
                                "nodelist",
                                "edgelist",
                                "system_level_measures",
                                "system_measure_plot"),
                     message = TRUE) {


  # We might need to store `output` in a separate object given that it also gets
  # defined in the subsequent `basic_netwrite` calls.
  final_output <- output

  # If the data frame containing a nodelist is a tibble, it creates problems
  # downstream. So detect whether or not `nodelist` is a tibble and remove that
  # property

  if (("data.frame" %in% class(nodelist)) == TRUE) {
    nodelist <- as.data.frame(nodelist)
  }

  # If `node_id` is set to be `"id"`, change its value to `"original_id"`
  if ((!is.null(node_id) == TRUE)) {
    if (node_id == "id") {
      node_id <- "original_id"
    }

  }

  # Running `basic_netwrite` multiple times will crash because of current
  # naming conventions. After one application, the global environment has
  # an object named `nodelist` that isn't a vector of node IDs but rather
  # a full dataframe containing centrality measures and component memberships.
  # Feeding this into a subsequent iteration of `basic_netwrite` will cause
  # a crash. Consequently, for handling multi-relational networks, we'll
  # want to store the originally inputted `nodelist` as a specially named
  # object to preven this from happening.

  original_nodelist <- nodelist


  # First things first: if `nodelist` is a data frame that contains a variable
  # named `id`, it needs to be renamed to `original_id`

  if (("data.frame" %in% class(original_nodelist)) == TRUE) {

    # If any column in the nodelist dataframe is named `"id"`,
    # rename to `"original_id"`
    original_nodelist_names <- colnames(original_nodelist)
    original_nodelist_names[which(original_nodelist_names == "id")] <- "original_id"
    colnames(original_nodelist) <- original_nodelist_names

    # When we iterate over each relation type, we only want to input
    # the vector of node IDs rather than the full nodelist dataframe
    # (if available). If we keep running netwrite over the full
    # nodelist data frame, we'll end up with a lot of copies of the
    # basic node variables
    just_ids <- as.data.frame(original_nodelist)[, node_id]
    # We also need a copy of this to check for ratio of node IDs
    # appearing in nodelist vs. edgelist
    just_ids_check <- just_ids

    # If `fix_nodelist` is set to `TRUE`, we need to incorporate node IDs from the edgelist
    if (fix_nodelist == TRUE) {
      el_ids <- sort(unique(c(i_elements, j_elements)))
      just_ids <- sort(unique(c(just_ids, el_ids)))
    }

  } else {
    just_ids <- original_nodelist
    just_ids_check <- just_ids

    # If `fix_nodelist` is set to `TRUE`, we need to incorporate node IDs from the edgelist
    if (fix_nodelist == TRUE) {
      el_ids <- sort(unique(c(i_elements, j_elements)))
      just_ids <- sort(unique(c(just_ids, el_ids)))
    }
  }


  # If no original nodelist is given, it'll create issues with
  # where isolates in subgraphs will be removed. This creates
  # issues down the line, including problems with adjacency matrices
  # needed for role analysis. To avoid this, we'll create an ad hoc
  # nodelist from unique node IDs in the edgelist

  # if (is.null(nodelist) == TRUE) {
  if (is.logical(nodelist)) {

    just_ids <- sort(unique(c(i_elements, j_elements)))


  }


  # Sometimes users might enter a network with an erroneously high number of
  # nodes relative to edges in the network. If this should occur, netwrite
  # should warn the user of this being the case and ask the user if they would
  # like to proceed.
  ##### We'll need different versions of handling this for each type of data
  ##### structure. Edgelists and adjacency matrices are pretty easy to handle,
  ##### but we'll need to think of a systematic way to check for this when given
  ##### and adjacency list.

  if (shiny == FALSE & data_type == "edgelist" & !is.logical(nodelist)) {

    ratio1 <- length(just_ids_check)/length(unique(c(i_elements, j_elements)))
    ratio2 <- length(unique(c(i_elements, j_elements)))/length(just_ids_check)

    if (ratio1 > 2) {
      base::message("It appears that the number of nodes identified in your nodelist far exceeds the number of nodes identified in your edgelist. Is this correct?")
      yes_no <- readline(prompt = "Enter 'Y' to proceed or 'N' to terminate netwrite: ")
      yes_no <- as.character(yes_no)

      while(!(yes_no %in% c("Yes", "Y", "No", "N", "yes", "y", "no", "n"))) {
        yes_no <- readline(prompt = "Please enter an appropriate response ('Y' or 'N'): ")
        yes_no <- as.character(yes_no)
      }

      tryCatch({
        base::stopifnot(yes_no == "Yes" | yes_no == "Y" | yes_no == "yes" | yes_no == "y")
      }, error = function(e) {
        stop("netwrite has been terminated.", call. = FALSE)})

    } else if (ratio2 > 2) {

      base::message("It appears that the number of nodes identified in your edgelist far exceeds the number of nodes identified in your nodelist Is this correct?")
      yes_no <- readline(prompt = "Enter 'Y' to proceed or 'N' to terminate netwrite: ")
      yes_no <- as.character(yes_no)

      while(!(yes_no %in% c("Yes", "Y", "No", "N", "yes", "y", "no", "n"))) {
        yes_no <- readline(prompt = "Please enter an appropriate response ('Y' or 'N'): ")
        yes_no <- as.character(yes_no)
      }

      tryCatch({
        base::stopifnot(yes_no == "Yes" | yes_no == "Y" | yes_no == "yes" | yes_no == "y")
      }, error = function(e) {
        stop("netwrite has been terminated.", call. = FALSE)})
    }
  }








  # If the `type` argument is left at its default (`NULL`), netwrite assumes
  # the data entered does not reflect a multi-relational net and
  if (is.null(type) == TRUE) {


    # If `nodelist` is a data frame, we'll want to make sure that the node-level
    # attributes it already stores are included in the igraph objects that
    # `netwrite` produces

    if (("data.frame" %in% class(nodelist)) == TRUE) {

      basic_netwrite(data_type = data_type,
                     adjacency_matrix = adjacency_matrix,
                     adjacency_list = adjacency_list,
                     nodelist = original_nodelist,
                     node_id = node_id,
                     i_elements = i_elements,
                     j_elements = j_elements,
                     weights = weights,
                     type = type,
                     remove_loops = remove_loops,
                     package = package,
                     missing_code = missing_code,
                     weight_type = weight_type,
                     directed = directed,
                     net_name = net_name,
                     shiny = shiny,
                     output = output,
                     message = message)

    } else {

      basic_netwrite(data_type = data_type,
                     adjacency_matrix = adjacency_matrix,
                     adjacency_list = adjacency_list,
                     nodelist = just_ids,
                     node_id = node_id,
                     i_elements = i_elements,
                     j_elements = j_elements,
                     weights = weights,
                     type = type,
                     remove_loops = remove_loops,
                     package = package,
                     missing_code = missing_code,
                     weight_type = weight_type,
                     directed = directed,
                     net_name = net_name,
                     shiny = shiny,
                     output = output,
                     message = message)

    }


# The below code chunk is presumably deprecated, but saving here in case
    # merge issues reappear.

    # If `nodelist` is a data frame, we'll want to merge it into `node_measures`
    if (("data.frame" %in% class(nodelist)) == TRUE) {

      # Sometimes the original ID column we need to join on will be of a different class
      # between the two dataframes we're trying to merge here. To be safe, we'll convert both columns
      # into characters and merge
      original_nodelist[, node_id] <- as.character(unlist(original_nodelist[, node_id]))
      node_measures[, node_id] <- as.character(unlist(node_measures[, node_id]))

      node_measures <- dplyr::left_join(original_nodelist, node_measures, by = node_id)
      # Rearrange columns
      node_measures <- dplyr::select(node_measures, id, dplyr::everything())

      assign(x = "node_measures", value = node_measures, .GlobalEnv)

    }

  } else {

    # If there is a vector of tie type assignments put into the `type` argument,
    # `netwrite` uses the `net_splitter` function to identify subgraphs based on
    # types of relations and then applies `basic_netwrite` to each, plus the aggregate
    # graph of all relations

    # Apply `net_splitter`
    edges_list <- net_splitter(i_elements = i_elements,
                               j_elements = j_elements,
                               type = type,
                               weights = weights)


    # Creating output lists
    graphs_list <- vector('list', length(edges_list))
    names(graphs_list) <- names(edges_list)

    bicomponent_list <- vector('list', length(edges_list))
    names(bicomponent_list) <- names(edges_list)

    lcomponent_list <- vector('list', length(edges_list))
    names(lcomponent_list) <- names(edges_list)

    nplot_list <- vector('list', length(edges_list))
    names(nplot_list) <- names(edges_list)

    node_measures_list <- vector('list', length(edges_list))
    names(node_measures_list) <- names(edges_list)

    f_edges_list <- vector('list', length(edges_list))
    names(f_edges_list) <- names(edges_list)

    s_measures_list <- vector('list', length(edges_list))
    names(s_measures_list) <- names(edges_list)

    splot_list <- vector('list', length(edges_list))

    names(splot_list) <- names(edges_list)


    # Looping Over netwrite
    for (i in seq_along(edges_list)) {

      if (names(edges_list)[[i]] == "summary_graph") {
        base::message("Processing aggregate network of all edge types")
      } else {
        base::message(paste("Processing network for edge type", names(edges_list)[[i]], sep = " "))
      }


      # May need to take out this `summary_graph` condition:
      # First-level netwrite's going to do all this stuff anyway

      if(names(edges_list)[[i]] == c('summary_graph')) {

        # If the `type` argument is a character vector, will
        # cause issues with merging. To fix, ensure `just_ids`
        # vector going into `basic_netwrite` is also a character
        if (class(edges_list[[i]]$type) == "character") {
          just_ids2 <- as.character(just_ids)
        } else {
          just_ids2 <- just_ids
        }


        # If we have a data frame for a nodelist, use that. Otherwise
        # `just_ids`
        if ("data.frame" %in% class(nodelist) == TRUE) {
              nodelist_condition <- original_nodelist
        } else {
              nodelist_condition <- just_ids2
        }


        # If handling aggregate graph that has weights
        if ('weights' %in% colnames(edges_list[[i]]) == TRUE) {

          basic_netwrite(data_type = data_type,
                         adjacency_matrix = adjacency_matrix,
                         adjacency_list = adjacency_list,
                         nodelist = nodelist_condition,
                         node_id = node_id,
                         i_elements=edges_list[[i]]$i_elements,
                         j_elements=edges_list[[i]]$j_elements,
                         weights=edges_list[[i]]$weights,
                         type=edges_list[[i]]$type,
                         remove_loops = remove_loops,
                         package=package,
                         missing_code = missing_code,
                         weight_type = weight_type, directed=directed,
                         net_name = "this_igraph", # Giving a consistent name for igraph object
                         # Makes it easier to place into the list of
                         # igraph objects down the line.
                         shiny = TRUE,
                         # For processing multi-relational nets, we'll want to collect all the
                         # possible output objects from `basic_netwrite` at first, then filter out
                         # based on the original outputs specified in the `netwrite` call
                         output = c("graph",
                                    "largest_bi_component",
                                    "largest_component",
                                    "node_measure_plot",
                                    "nodelist",
                                    "edgelist",
                                    "system_level_measures",
                                    "system_measure_plot"),
                         message = message)

        }else{
          # Aggregate graph, unweighted
          basic_netwrite(data_type = data_type,
                         adjacency_matrix = adjacency_matrix,
                         adjacency_list = adjacency_list,
                         nodelist = nodelist_condition,
                         node_id = node_id,
                         i_elements=edges_list[[i]]$i_elements,
                         j_elements=edges_list[[i]]$j_elements,
                         weights = NULL,
                         type=edges_list[[i]]$type,
                         remove_loops = remove_loops,
                         package=package,
                         missing_code = missing_code,
                         weight_type = weight_type, directed=directed,
                         net_name = "this_igraph",
                         shiny = TRUE,
                         output = c("graph",
                                    "largest_bi_component",
                                    "largest_component",
                                    "node_measure_plot",
                                    "nodelist",
                                    "edgelist",
                                    "system_level_measures",
                                    "system_measure_plot"),
                         message = message)


        }

      } else {

        # If we have a data frame for a nodelist, use that. Otherwise
        # `just_ids`
        if ("data.frame" %in% class(nodelist) == TRUE) {
          nodelist_condition <- original_nodelist
        } else {
          nodelist_condition <- just_ids
        }

        if('weights' %in% colnames(edges_list[[i]]) == TRUE){
          # Subgraphs, with weights
          basic_netwrite(data_type = data_type,
                         adjacency_matrix = adjacency_matrix,
                         adjacency_list = adjacency_list,
                         nodelist = nodelist_condition,
                         node_id = node_id,
                         i_elements=edges_list[[i]]$i_elements,
                         j_elements=edges_list[[i]]$j_elements,
                         weights = edges_list[[i]]$weights,
                         type=edges_list[[i]]$type,
                         remove_loops = remove_loops,
                         package=package,
                         missing_code = missing_code,
                         weight_type = weight_type, directed=directed,
                         net_name = "this_igraph",
                         shiny = TRUE,
                         output = c("graph",
                                    "largest_bi_component",
                                    "largest_component",
                                    "node_measure_plot",
                                    "nodelist",
                                    "edgelist",
                                    "system_level_measures",
                                    "system_measure_plot"),
                         message = message)

        } else {
          # Subgraphs, without weights
          basic_netwrite(data_type = data_type,
                         adjacency_matrix = adjacency_matrix,
                         adjacency_list = adjacency_list,
                         nodelist = nodelist_condition,
                         node_id = node_id,
                         i_elements=edges_list[[i]]$i_elements,
                         j_elements=edges_list[[i]]$j_elements,
                         weights = NULL,
                         type=edges_list[[i]]$type,
                         remove_loops = remove_loops,
                         package=package,
                         missing_code = missing_code,
                         weight_type = weight_type, directed=directed,
                         net_name = "this_igraph",
                         shiny = TRUE,
                         output = c("graph",
                                    "largest_bi_component",
                                    "largest_component",
                                    "node_measure_plot",
                                    "nodelist",
                                    "edgelist",
                                    "system_level_measures",
                                    "system_measure_plot"),
                         message = message)

        }
      }

      # Store netwrite outputs into respective lists

      # igraph object list
      graphs_list[[i]] <- this_igraph
      suppressWarnings(rm(this_igraph))

      # Largest bicomponent list
      bicomponent_list[[i]] <- largest_bi_component
      suppressWarnings(rm(largest_bi_component))

      # Largest component list
      lcomponent_list[[i]] <- largest_component
      suppressWarnings(rm(largest_component))

      # Node measure plot list
      nplot_list[[i]] <- node_measure_plot
      suppressWarnings(rm(node_measure_plot))

      # Node measures list
      ### To ensure successful merging downstream,
      ### convert `attr` to character before adding
      ### `node_measures_list`
      node_measures[,2] <- as.character(node_measures[,2])
      node_measures_list[[i]] <- node_measures
      suppressWarnings(rm(node_measures))

      # List of final processes edgelists
      f_edges_list[[i]] <- edgelist
      suppressWarnings(rm(edgelist))

      # System-level measures summaries list
      s_measures_list[[i]] <- system_level_measures
      suppressWarnings(rm(system_level_measures))

      # System-level plot list
      splot_list[[i]] <- system_measure_plot
      suppressWarnings(rm(system_measure_plot))




    } # End for Loop

    # Finishing steps

    # 1. Reorder lists so that the aggregate network's outputs come first

    # Make new ordering vector
    new_order <- c(
      names(edges_list)[[length(names(edges_list))]],
      names(edges_list)[1:(length(names(edges_list))-1)])

    graphs_list <- graphs_list[new_order]
    bicomponent_list <- bicomponent_list[new_order]
    lcomponent_list <- lcomponent_list[new_order]
    nplot_list <- nplot_list[new_order]
    node_measures_list <- node_measures_list[new_order]
    f_edges_list <- f_edges_list[new_order]
    s_measures_list <- s_measures_list[new_order]
    splot_list <- splot_list[new_order]

    # 2. Convert node measures list into dataframe

    #nodelist <- Reduce(dplyr::full_join, node_measures_list)
    #rm(node_measures_list)

    # 3. Store aggregate info as their own objects in the global environment

    graph <- graphs_list$summary_graph
    largest_bi_component <- bicomponent_list$summary_graph
    largest_component <- lcomponent_list$summary_graph
    node_measure_plot <- nplot_list$summary_graph
    edgelist <- f_edges_list$summary_graph
    system_level_measures <- s_measures_list$summary_graph
    system_measure_plot <- splot_list$summary_graph

    # 4. Keep only those objects that the original `output` argument specified

    if ("graph" %in% final_output) {
      assign(x = net_name, value = graph, .GlobalEnv)
      assign(x = "network_list", value = graphs_list, .GlobalEnv)
      suppressWarnings(rm(graph))
    } else {
      suppressWarnings(rm(graph, graphs_list))
    }

    if ("largest_bi_component" %in% final_output) {
      assign(x = "largest_bi_component", value = largest_bi_component, .GlobalEnv)
      assign(x = "bicomponent_list", value = bicomponent_list, .GlobalEnv)
    } else {
      suppressWarnings(rm(largest_bi_component, bicomponent_list))
    }

    if ("largest_component" %in% final_output) {
      assign(x = "largest_component", value = largest_component, .GlobalEnv)
      assign(x = "largest_component_list", value = lcomponent_list, .GlobalEnv)
    } else {
      suppressWarnings(rm(largest_component, lcomponent_list))
    }

    if ("node_measure_plot" %in% final_output) {
      assign(x = "node_measure_plot", value = node_measure_plot, .GlobalEnv)
      assign(x = "node_measure_plot_list", value = nplot_list, .GlobalEnv)
    } else {
      suppressWarnings(rm(node_measure_plot, nplot_list))
    }

    # For `nodelist`, we just want a dataframe that has the node-level
    # measures for each relation type. To do this, we're going to want
    # to rename the columns in each element of `node_measures_list` and
    # then merge these elements.

    for (i in 2:length(node_measures_list)) {

      #  print(i)

      this_name <- names(node_measures_list)[[i]]
      new_column_names <- paste(this_name,
                                colnames(node_measures_list[[i]]),
                                sep = "_")

      colnames(node_measures_list[[i]]) <- c(colnames(node_measures_list[[i]])[1:2],
                                             new_column_names[3:length(new_column_names)])
    }

    node_measures <- Reduce(dplyr::full_join, node_measures_list)

    # If `nodelist` is a data frame, we'll want to merge it into `node_measures`
    if (("data.frame" %in% class(nodelist)) == TRUE) {

      # Sometimes the original ID column we need to join on will be of a different class
      # between the two dataframes we're trying to merge here. To be safe, we'll convert both columns
      # into characters and merge
      original_nodelist[, node_id] <- as.character(unlist(original_nodelist[, node_id]))
      node_measures[, node_id] <- as.character(unlist(node_measures[, node_id]))

      node_measures <- dplyr::left_join(original_nodelist, node_measures, by = node_id)
      # Rearrange columns
      node_measures <- dplyr::select(node_measures, id, dplyr::everything())

    }

    if ("nodelist" %in% final_output) {
      assign(x = "node_measures", value = node_measures, .GlobalEnv)
      assign(x = "node_measures_list", value = node_measures_list, .GlobalEnv)
    } else {
      suppressWarnings(rm(node_measures))
      suppressWarnings(rm(node_measures_list))
    }

    if ("edgelist" %in% final_output) {
      assign(x = "edgelist", value = edgelist, .GlobalEnv)
      assign(x = "edgelist_list", value = f_edges_list, .GlobalEnv)
    } else {
      suppressWarnings(rm(edgelist, f_edges_list))
    }

    if ("system_level_measures" %in% final_output) {
      assign(x = "system_level_measures", value = system_level_measures, .GlobalEnv)
      assign(x = "system_level_measures_list", value = s_measures_list, .GlobalEnv)
    } else {
      suppressWarnings(rm(system_level_measures, s_measures_list))
    }

    if ("system_measure_plot" %in% final_output) {
      assign(x = "system_measure_plot", value = system_measure_plot, .GlobalEnv)
      assign(x = "system_measure_plot_list", value = splot_list, .GlobalEnv)
    } else {
      suppressWarnings(rm(system_measure_plot, splot_list))
    }




  } # End multi-relational network processing

  # Remove objects not specific as being included in `output`, based on
  # values stored in `final_output`. Although the above set of conditionals should
  # be taking care of this, it doesn't always succeed and this ensures that only the
  # desired outputs specified by the user remain in the Global Environment.

  if (!("graph" %in% final_output)) {
    suppressWarnings(rm(list = c("graph", "graphs_list"), envir = .GlobalEnv))
  }

  if (!("largest_bi_component" %in% final_output)) {
    suppressWarnings(rm(list = c("largest_bi_component", "bicomponent_list"), envir = .GlobalEnv))
  }

  if (!("largest_component" %in% final_output)) {
    suppressWarnings(rm(list = c("largest_component", "lcomponent_list"), envir = .GlobalEnv))
  }

  if (!("node_measure_plot" %in% final_output)) {
    suppressWarnings(rm(list = c("node_measure_plot", "nplot_list"), envir = .GlobalEnv))
  }

  if (!("nodelist" %in% final_output)) {
    suppressWarnings(rm(list = c("node_measures", "node_measures_list"), envir = .GlobalEnv))
  }

  if (!("edgelist" %in% final_output)) {
    suppressWarnings(rm(list = c("edgelist", "f_edges_list"), envir = .GlobalEnv))
  }

  if (!("system_level_measures" %in% final_output)) {
    suppressWarnings(rm(list = c("system_level_measures", "s_measures_list"), envir = .GlobalEnv))
  }

  if (!("system_measure_plot" %in% final_output)) {
    suppressWarnings(rm(list = c("system_measure_plot", "system_measure_plot_list"), envir = .GlobalEnv))
  }


}

#######################################################



# The below function is the core script for netwrite, entitled `basic_netwrite`.
# This function does everything that netwrite is supposed to do, but only for the entire
# network entered in as arguments. As such, this function is not fully equipped to handle
# multi-relational networks and perform calculations for subgraphs of specific
# relation types. To handle multi-relational networks, we create another function
# later on (`netwrite` proper), which applies `basic_netwrite` to all subgraphs.


basic_netwrite <- function(data_type = c('edgelist'), adjacency_matrix=FALSE,
                           adjacency_list=FALSE,
                           nodelist=NULL,
                           # `node_id` takes a character argument specifying
                           # how the original node ID variable should be named
                           # in output
                           node_id = NULL,
                           i_elements=FALSE,
                           j_elements=FALSE,

                           # I THINK the `weights` argument should work for adjmats if we just have users set to TRUE when using a weighted adjmat
                           weights=NULL, type=NULL,
                           remove_loops = FALSE,
                           package='igraph', missing_code=99999,
                           weight_type='frequency', directed=FALSE,
                           net_name='network',
                           shiny = FALSE,
                           output = c("graph",
                                      "largest_bi_component",
                                      "largest_component",
                                      "node_measure_plot",
                                      "nodelist",
                                      "edgelist",
                                      "system_level_measures",
                                      "system_measure_plot"),
                           message = TRUE) {

  # Installing Necessary Packages
  list.of.packages <- c('dplyr', 'igraph', 'network', 'ggplot2', 'cowplot', 'moments')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  rm(list.of.packages, new.packages)


  #########################################
  #    C O N D I T I O N A L   F L O W    #
  #########################################

  # Setting Data Type: Adjacency Matrix, Adjacency List, or Edgelist

  # ADJACENCY MATRIX
  if(data_type == 'adjacency_matrix'){
    # Checking for ID Column
    if (dim(adjacency_matrix)[[1]] != dim(adjacency_matrix)[[2]]){
      adjacency_matrix <- adjacency_matrix[,c(2:ncol(adjacency_matrix))]
    }else{
      adjacency_matrix <- adjacency_matrix[,]
    }

    if(as.logical(directed) == TRUE){

      # Set up conditional for generating weighted graph from weighted adjacency matrix
      if (is.null(weights) == TRUE) {
        adj_weight <- NULL
      } else {
        adj_weight <- ifelse(weights[[1]] == TRUE, TRUE, NULL)
      }

     # adj_weight = ifelse((!is.null(weights) == TRUE & weights[[1]] == TRUE), TRUE, FALSE)

      # Also need to adjust for type of weight
      # Make Weights Reflect Frequency Rather than Distance
      if(weight_type == 'frequency') {
        adjacency_matrix <- 1/adjacency_matrix
        adjacency_matrix[is.infinite(adjacency_matrix)] <- 0
      }else{
        adjacency_matrix <- adjacency_matrix
      }

      # If `remove_loops == TRUE`, remove self-loops
      if (remove_loops == TRUE) {
        diag(adjacency_matrix) <- NA
      }


      # Generating directed graph
      g <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode=c('directed'), diag = TRUE,
                                               weighted = adj_weight)



      # Create Nodes file with Node-level measures
      edges <- as.data.frame(igraph::as_edgelist(g, names=FALSE))

      # nodes <- as.data.frame(sort(unique(c(edges$V1, edges$V2))))
      # colnames(nodes) <- c('id')
      ### The above two lines were dropping isolates from the nodelist.
      ### I think this is a better alternative
      nodes <- as.data.frame(1:length(igraph::V(g)))
      colnames(nodes) <- "id"
      nodes$id <- nodes$id - 1
      nodes$label <- igraph::V(g)$name

      # To keep things consistent across code, we're going to reassign node names
      # to a `label` vertex attribute in `igraph` and replace the `name` attribute
      # with numeric IDs
      igraph::V(g)$name <- nodes$id
      igraph::V(g)$label <- nodes$label

      # Create alternate closeness function
      # Reachability function (eliminating loops)

      # Adding Node-level measures
      if ("nodelist" %in% output | "node_measure_plot" %in% output) {
        nodes <- node_level_igraph(nodes = nodes, g = g, directed = directed,
                                   message = message)

        # If original `node_id` name is specified, rename column `attr` to match
        if (!is.null(node_id)) {
          # Get original column names
          nodelist_names <- colnames(nodes)
          nodelist_names[which(nodelist_names == "attr")] <- node_id
          colnames(nodes) <- nodelist_names
        }

      }
      # Extracting largest weakly-connected component
      # Extracting largest bicomponent
      # Calculating proportion of two-step paths that are also one-step paths (trans_rate)

      # Calculating system-level measures
      if ("system_level_measures" %in% output) {
        largest_weak_component_igraph(g)
        largest_bicomponent_igraph(g)
        ### Merge in largest bicomponent memberships
        if ("nodelist" %in% output | "node_measure_plot" %in% output) {
          nodes <- dplyr::left_join(nodes, largest_bicomponent_memberships, by = "id")
        }
        degree_assortativity <- igraph::assortativity.degree(g, directed=as.logical(directed))
        reciprocity_rate <- igraph::reciprocity(g, ignore.loops = TRUE, mode='ratio')
        trans_rate_igraph(g)
        global_clustering_coefficient <- igraph::transitivity(g, type='global')
        average_path_length <- igraph::average.path.length(g, directed=as.logical(directed))
      }
      # UNDIRECTED
    } else {
      # Set up conditional for generating weighted graph from weighted adjacency matrix
      if (is.null(weights) == TRUE) {
        adj_weight <- NULL
      } else {
        adj_weight <- ifelse(weights[[1]] == TRUE, TRUE, NULL)
      }
     # adj_weight = ifelse((!is.null(weights) == TRUE & weights[[1]] == TRUE), TRUE, FALSE)

      # Also need to adjust for type of weight
      # Make Weights Reflect Frequency Rather than Distance
      if(weight_type == 'frequency') {
        adjacency_matrix <- 1/adjacency_matrix
        adjacency_matrix[is.infinite(adjacency_matrix)] <- 0
      }else{
        adjacency_matrix <- adjacency_matrix
      }

      # If `remove_loops == TRUE`, remove self-loops
      if (remove_loops == TRUE) {
        diag(adjacency_matrix) <- NA
      }

      # Generating undirected graph
      g <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode=c('undirected'), diag = FALSE,
                                               weighted = adj_weight)

      # Create Nodes file with Node-level measures
      edges <- as.data.frame(igraph::as_edgelist(g, names=FALSE))

      # nodes <- as.data.frame(sort(unique(c(edges$V1, edges$V2))))
      # colnames(nodes) <- c('id')
      ### The above two lines were dropping isolates from the nodelist.
      ### I think this is a better alternative
      nodes <- as.data.frame(1:length(igraph::V(g)))
      colnames(nodes) <- "id"
      nodes$id <- nodes$id - 1
      nodes$label <- igraph::V(g)$name

      # To keep things consistent across code, we're going to reassign node names
      # to a `label` vertex attribute in `igraph` and replace the `name` attribute
      # with numeric IDs
      igraph::V(g)$name <- nodes$id
      igraph::V(g)$label <- nodes$label

      # Create an alternate closeness function
      # Reachablility function (Eliminate Loops, reaching yourself isn't that useful)

      # Adding Node-Level Measures
      if ("nodelist" %in% output | "node_measure_plot" %in% output) {
        nodes <- node_level_igraph(nodes = nodes, g = g, directed = directed,
                                   message = message)

        # If original `node_id` name is specified, rename column `attr` to match
        if (!is.null(node_id)) {
          # Get original column names
          nodelist_names <- colnames(nodes)
          nodelist_names[which(nodelist_names == "attr")] <- node_id
          colnames(nodes) <- nodelist_names
        }

      }
      # Extracting the largest weakly connected component
      # Extracting the largest bi-component
      # Calculating the Proportion of Two-Step Path that Are Also One-Step Paths

      # Calculating System-Level Measures
      if ("system_level_measures" %in% output) {
        largest_weak_component_igraph(g)
        largest_bicomponent_igraph(g)
        ### Merge in largest bicomponent memberships
        if ("nodelist" %in% output | "node_measure_plot" %in% output) {
          nodes <- dplyr::left_join(nodes, largest_bicomponent_memberships, by = "id")
        }
        degree_assortativity <- igraph::assortativity.degree(g, directed=as.logical(directed))
        reciprocity_rate <- igraph::reciprocity(g, ignore.loops = TRUE, mode='ratio')
        trans_rate_igraph(g)
        global_clustering_coefficient <- igraph::transitivity(g, type='global')
        average_path_length <- igraph::average.path.length(g, directed=as.logical(directed))
      }




    }

    # Outputting Network Objects
    ### Note: Jim wants the outputted edgelist and igraph object to have the original weights.
    ### I've added some code that reverts the weights back to original if need be.
    if(weight_type == 'frequency') {
      adjacency_matrix <- 1/adjacency_matrix
      adjacency_matrix[is.infinite(adjacency_matrix)] <- 0
    }else{
      adjacency_matrix <- adjacency_matrix
    }

    if ("adjacency_matrix" %in% output) {
      assign(x = 'adjacency_matrix', value = adjacency_matrix,.GlobalEnv)
    }
    if ("nodelist" %in% output) {
      assign(x = 'node_measures', value = nodes,.GlobalEnv)
    }

    ### Note: Jim wants the outputted edgelist and igraph object to have the original weights.
    ### I've added some code that reverts the weights back to original if need be.
    if(weight_type == 'frequency' & !is.null(adj_weight) == TRUE) {
      igraph::E(g)$weight <- 1/igraph::E(g)$weight
    }


    # # It might be easier to just add everyting from `nodelist` to the igraph object as
    # # node-level properties. The below loop does this:
    nodelist_names <- names(nodes)

    for (i in 1:length(nodelist_names)) {

      eval(parse(
        text = paste("igraph::V(g)$", nodelist_names[[i]], "<- nodes[,i]", sep = "")
      ))

    }

    # Assign to global environment
    if ("graph" %in% output) {
      assign(x = net_name, value = g,.GlobalEnv)
    }


    # ADJACENCY LIST
  }else if (data_type == 'adjacency_list') {
    # Is the adjacency list a list
    if (class(adjacency_list) == 'list') {
      g <- igraph::graph_from_adj_list(adjacency_list, mode="out")

      # If `remove_loops == TRUE`, remove self-loops
      if (remove_loops == TRUE) {
        g <- igraph::simplify(g, remove.multiple = FALSE,
                              remove.loops = TRUE)
      }
    } else {
      # IF NOT, Converting to a list
      adj_list <- vector('list', dim(adjacency_list)[[1]])
      names(adj_list) <- as.character(adjacency_list[,1])
      for(i in seq_along(adj_list)){
        adj_row <- unique(as.integer(strsplit(adjacency_list[i,2], ' ')[[1]]))
        adj_list[[i]] <- vector('list', length(adj_row))
        for(j in seq_along(adj_row)) {
          adj_list[[i]][[j]] <- adj_row[[j]]
        }
        rm(adj_row)
      }

      # Generating network from adjacency list
      g <- igraph::graph_from_adj_list(adj_list, mode="out")

      # If `remove_loops == TRUE`, remove self-loops
      if (remove_loops == TRUE) {
        g <- igraph::simplify(g, remove.multiple = FALSE,
                              remove.loops = TRUE)
      }
    }

    # Copying igraph object
    g <- g

    # Creating Nodes File with Node-Level Measures
    edges <- as.data.frame(igraph::as_edgelist(g, names=FALSE))
    nodes <- as.data.frame(sort(unique(c(edges$V1, edges$V2))))
    colnames(nodes) <- c('id')
    nodes$id <- nodes$id - 1

    # Create an alternate closeness function
    # Reachablility function (Eliminate Loops, reaching yourself isn't that useful)
    # Adding Node-Level Measures
    if ("nodelist" %in% output | "node_measure_plot" %in% output) {
      nodes <- node_level_igraph(nodes = nodes, g = g, directed = directed,
                                 message = message)

      # If original `node_id` name is specified, rename column `attr` to match
      if (!is.null(node_id)) {
        # Get original column names
        nodelist_names <- colnames(nodes)
        nodelist_names[which(nodelist_names == "attr")] <- node_id
        colnames(nodes) <- nodelist_names
      }

    }
    # Extracting the largest weakly connected component
    # Extracting the largest bi-component
    # Calculating the Proportion of Two-Step Path that Are Also One-Step Paths
    # Calculating System-Level Measures
    if ("system_level_measures" %in% output) {
      largest_weak_component_igraph(g)
      largest_bicomponent_igraph(g)
      ### Merge in largest bicomponent memberships
      if ("nodelist" %in% output | "node_measure_plot" %in% output) {
        nodes <- dplyr::left_join(nodes, largest_bicomponent_memberships, by = "id")
      }
      degree_assortativity <- igraph::assortativity.degree(g, directed=as.logical(directed))
      reciprocity_rate <- igraph::reciprocity(g, ignore.loops = TRUE, mode='ratio')
      trans_rate_igraph(g)
      global_clustering_coefficient <- igraph::transitivity(g, type='global')
      average_path_length <- igraph::average.path.length(g, directed=as.logical(directed))
    }
    # EDGELIST
  } else {

    # print("creating edgelist")

    # Creating Canonical Node and Edgelists
    if (is.null(weights) == TRUE){
      edgelist <-as.matrix(cbind(i_elements, j_elements))
      edgelist <-cbind(edgelist, rep(1,nrow(edgelist)))
      colnames(edgelist)[[3]] <- c('weight')
    }else{
      edgelist <-as.matrix(cbind(i_elements, j_elements, weights))
      colnames(edgelist)[[3]] <- c('weight')
    }

    # Checking for Edge Type
    if(is.null(type) == TRUE){
      edgelist <- edgelist
    }else if (length(type) == length(i_elements)){
      edgelist <- cbind(edgelist, type)
    }else {
      writeLines("The type indicator variable is not the same length as the network's edgelist.\nTo calculate the network's multilevel edge correlation, please supply a vector of the same length.")
    }

    edgelist <- edgelist[!(rowSums(is.na(edgelist))), ]
    edgelist <- edgelist[edgelist[,1] != missing_code & edgelist[,2] != missing_code, ]
    edgelist <- cbind(seq(1,nrow(edgelist), 1), edgelist)
    colnames(edgelist)[[1]] <- c('Obs_ID')

    # Adding Nodes
     if(is.null(nodelist[[1]]) == TRUE) {
    #if(length(nodelist) == 1 & nodelist[[1]] == FALSE) {
      print("make nodes no nodelist")
      nodes <- as.data.frame(sort(unique(c(edgelist[,2], edgelist[,3]))))
      nodes <- cbind(seq(1,nrow(nodes),1), nodes)
      colnames(nodes) <- c('id', 'label')

      senders <- as.data.frame(edgelist[,c(1:2)])
      colnames(senders)[[2]] <- c('label')
      senders <- dplyr::left_join(senders, nodes, by='label')
      colnames(senders)[c(2,3)] <- c('i_elements', 'i_id')

      if (is.null(type) == TRUE){
        targets <- as.data.frame(edgelist[,c(1,3,4)])
      }else {
        targets <- as.data.frame(edgelist[,c(1,3,4, 5)])
      }

      colnames(targets)[[2]] <- c('label')
      targets <- dplyr::left_join(targets, nodes, by='label')
      if (is.null(type) == TRUE){
        colnames(targets)[c(2,4)] <- c('j_elements', 'j_id')
        targets <- targets[c(1,2,4,3)]
      } else {
        colnames(targets)[c(2,5)] <- c('j_elements', 'j_id')
        targets <- targets[c(1,2,5,3,4)]
      }

      edgelist <- dplyr::left_join(senders, targets, by='Obs_ID')
      edgelist <- edgelist[order(edgelist$i_id, edgelist$j_id), ]
      edgelist <- as.matrix(edgelist)
      rm(senders, targets)

    } else {

      # If `nodelist` is a data frame, extract vector of
      # node_ids
      if ("data.frame" %in% class(nodelist)) {
            nodes <- nodelist[,node_id]
      } else {
            nodes <- nodelist
      }


      nodes <- cbind(as.data.frame(seq(1, length(nodes), 1)), nodes)
      colnames(nodes) <- c('id', 'label')
      # Make `label` a character to ensure merging
      nodes$label <- as.character(nodes$label)

      senders <- as.data.frame(edgelist[,c(1:2)])
      colnames(senders)[[2]] <- c('label')
      senders$label <- as.character(senders$label)
      senders <- dplyr::left_join(senders, nodes, by='label')
      colnames(senders)[c(2,3)] <- c('i_elements', 'i_id')

      if(is.null(type) == TRUE){
        targets <- as.data.frame(edgelist[,c(1,3,4)])
      }else{
        targets <- as.data.frame(edgelist[,c(1,3,4, 5)])
      }

      colnames(targets)[[2]] <- c('label')
      targets$label <- as.character(targets$label)
      targets <- dplyr::left_join(targets, nodes, by='label')
      if (is.null(type) == TRUE) {
        colnames(targets)[c(2,4)] <- c('j_elements', 'j_id')
        targets <- targets[c(1,2,4,3)]
      }else{
        colnames(targets)[c(2,5)] <- c('j_elements', 'j_id')
        targets <- targets[c(1,2,5,3,4)]
      }

      edgelist <- dplyr::left_join(senders, targets, by='Obs_ID')
      edgelist <- edgelist[order(edgelist$i_id, edgelist$j_id), ]
      edgelist <- as.matrix(edgelist)
      rm(senders, targets)
    }

    # Convert edgelist to data frame class and make `Obs_ID`, `i_id`, `j_id`, and `weight` numeric
    edgelist <- as.data.frame(edgelist)
    edgelist$Obs_ID <- as.numeric(edgelist$Obs_ID)
    edgelist$i_id <- as.numeric(edgelist$i_id)
    edgelist$j_id <- as.numeric(edgelist$j_id)
    edgelist$weight <- as.numeric(edgelist$weight)

    # Create graph objects

    # Make Zero-Indexed
    nodes$id <- nodes$id - 1
    edgelist[,3] <- edgelist[,3] - 1
    edgelist[,5] <- edgelist[,5] - 1

    # Make Weights Reflect Frequency Rather than Distance
    if(weight_type == 'frequency') {
      edgelist[,6] <- as.numeric(1/edgelist[,6])
    }else{
      edgelist[,6] <- edgelist[,6]
    }

    # If `remove_loops == TRUE`, remove self-loops
    if (remove_loops == TRUE) {
      # identify loops
      edgelist$loop <- edgelist$i_id == edgelist$j_id
      # Filter out loops
      edgelist <- edgelist[!edgelist$loop, ]
      # Remove `loop` column
      edgelist$loop <- NULL
    }


    # print('creating igraph object')
    # Creating igraph object
    colnames(nodes)[[2]] <- c('attr')
    # If the nodelist is a data frame, we'll want to add the node attributes
    # back into the newly constructed, zero-indexed nodelist so that they
    # also appear in the igraph object constructed below
    if ("data.frame" %in% class(nodelist)) {
      colnames(nodelist)[[which(colnames(nodelist) == node_id)]] <- "attr"
      nodelist$attr <- as.character(nodelist$attr)
      nodes <- nodes %>% dplyr::left_join(nodelist, by = "attr")
    }


    g <- igraph::graph_from_data_frame(d = edgelist[,c(3,5)], directed = as.logical(directed), vertices = nodes)

    # Once you've created the igraph object, you can remove the other
    # node level attributes for now
    nodes <- nodes[,1:2]

    # Adding edge weights
    igraph::edge.attributes(g)$weight <- edgelist[,6]


    #   print('node-level measures')
    # Create an alternate closeness function
    # Reachablility function (Eliminate Loops, reaching yourself isn't that useful)
    # Adding Node-Level Measures
    if ("nodelist" %in% output | "node_measure_plot" %in% output) {
      nodes <- node_level_igraph(nodes = nodes, g = g, directed = directed,
                                 message = message)

      # If original `node_id` name is specified, rename column `attr` to match
      if (!is.null(node_id)) {
        # Get original column names
        nodelist_names <- colnames(nodes)
        nodelist_names[which(nodelist_names == "attr")] <- node_id
        colnames(nodes) <- nodelist_names
      }

    }
    # Extracting the largest weakly connected component
    # Extracting the largest bi-component
    # Calculating the Proportion of Two-Step Path that Are Also One-Step Paths
    # Calculating Multiplex Edge Correlation
    # Calculating System-Level Measures
    # print('system level measures')
    if ("system_level_measures" %in% output | "system_measure_plot" %in% output) {
      largest_weak_component_igraph(g)
      largest_bicomponent_igraph(g)
      ### Merge in largest bicomponent memberships
      if ("nodelist" %in% output | "node_measure_plot" %in% output) {
        nodes <- dplyr::left_join(nodes, largest_bicomponent_memberships, by = "id")
      }
      degree_assortativity <- igraph::assortativity.degree(g, directed=as.logical(directed))
      reciprocity_rate <- igraph::reciprocity(g, ignore.loops = TRUE, mode='ratio')
      trans_rate_igraph(g)
      global_clustering_coefficient <- igraph::transitivity(g, type='global')
      average_path_length <- igraph::average.path.length(g, directed=as.logical(directed))

      multiplex_edge_corr_igraph(edgelist = edgelist, directed = as.logical(directed),
                                 weight_type = weight_type,
                                 type = type)

    }
    # Outputting Network Objects
    ### Note: Jim wants the outputted edgelist and igraph object to have the original weights.
    ### I've added some code that reverts the weights back to original if need be.
    if(weight_type == 'frequency') {
      edgelist[,6] <- as.numeric(1/edgelist[,6])
    }else{
      edgelist[,6] <- edgelist[,6]
    }
    if ("edgelist" %in% output) {
      assign(x = 'edgelist', value = edgelist,.GlobalEnv)
    }
    if ("nodelist" %in% output) {
      # We need to force consistency across `ideanet's` various functions to make sure they
      # produce dataframes that can easily be merged into one another. For now, all `id` columns
      # will be designated as a numeric vector
      nodes$id <- as.numeric(nodes$id)
      assign(x = 'node_measures', value = nodes,.GlobalEnv)
    }

    ### Note: Jim wants the outputted edgelist and igraph object to have the original weights.
    ### I've added some code that reverts the weights back to original if need be.
    if(weight_type == 'frequency') {
      igraph::E(g)$weight <- edgelist[,6]
    }

    if ("nodelist" %in% output | "node_measure_plot" %in% output) {
      # It might be easier to just add everything from `nodelist` to the igraph object as
      # node-level properties. The below loop does this:
      nodelist_names <- names(nodes)

      for (i in 1:length(nodelist_names)) {

        eval(parse(
          text = paste("igraph::V(g)$", nodelist_names[[i]], "<- nodes[,i]", sep = "")
        ))

      }
    }

    # Assign to global environment
    if ("graph" %in% output) {
      assign(x = net_name, value = g,.GlobalEnv)
    }
  } # End edgelist condition

  ###########################################
  #    G E N E R A T I N G   R E P O R T    #
  ###########################################


  if ("system_level_measures" %in% output | "system_measure_plot" %in% output) {
    # Weak component summaries
    num_clusters <- igraph::clusters(g, mode="weak")[[3]]
    largest_size <- max(igraph::clusters(g, mode="weak")[[2]])
    proportion_largest <- max(igraph::clusters(g, mode="weak")[[2]])/nrow(nodes)

    # Strong component summaries
    strong_num_clusters <- igraph::clusters(g, mode="strong")[[3]]
    strong_largest_size <- max(igraph::clusters(g, mode="strong")[[2]])
    strong_proportion_largest <- max(igraph::clusters(g, mode="strong")[[2]])/nrow(nodes)

    # Indicating if Adjacency matrix is singular, thus having eigen and
    # bonacich centralities calculated on undirected adjacency matrix
    if (directed == TRUE & "nodelist" %in% output & "bon_cent_in" %in% names(nodelist)) {
      singular <- "No"
    } else {
      singular <- "Yes"
    }


    # Undirected and directed pairwise reachability

    if (directed == TRUE) {

      g_undir <- igraph::as.undirected(g)

      # Pairwise reachability (weak, undirected)
      weak_clusters_un <- igraph::clusters(g_undir, mode = "weak")
      pairwise_weak_un <- sum(weak_clusters_un$csize * (weak_clusters_un$csize-1)) / (length(igraph::V(g))*(length(igraph::V(g)) - 1))

      # Pairwise reachability (strong, undirected)
      strong_clusters_un <- igraph::clusters(g_undir, mode = "strong")
      pairwise_strong_un <- sum(strong_clusters_un$csize * (strong_clusters_un$csize-1)) / (length(igraph::V(g))*(length(igraph::V(g)) - 1))


      # Pairwise reachability (weak, directed)
      weak_clusters_dir <- igraph::clusters(g, mode = "weak")
      pairwise_weak_dir <- sum(weak_clusters_dir$csize * (weak_clusters_dir$csize-1)) / (length(igraph::V(g))*(length(igraph::V(g)) - 1))

      # Pairwise reachability (strong, directed)
      strong_clusters_dir <- igraph::clusters(g, mode = "strong")
      pairwise_strong_dir <- sum(strong_clusters_dir$csize * (strong_clusters_dir$csize-1)) / (length(igraph::V(g))*(length(igraph::V(g)) - 1))

    } else {

      # Pairwise reachability (weak, undirected)
      weak_clusters_un <- igraph::clusters(g, mode = "weak")
      pairwise_weak_un <- sum(weak_clusters_un$csize * (weak_clusters_un$csize-1)) / (length(igraph::V(g))*(length(igraph::V(g)) - 1))

      # Pairwise reachability (strong, undirected)
      strong_clusters_un <- igraph::clusters(g, mode = "strong")
      pairwise_strong_un <- sum(strong_clusters_un$csize * (strong_clusters_un$csize-1)) / (length(igraph::V(g))*(length(igraph::V(g)) - 1))


      # Pairwise reachability (weak, directed)
      pairwise_weak_dir <- NA

      # Pairwise reachability (strong, directed)
      pairwise_strong_dir <- NA


    }




    # Creating system-level data object
    multiplex_edge_correlation <- ifelse((is.null(type) == TRUE), 'Singleplex Network', multiplex_edge_correlation)
    multiplex_edge_correlation <- multiplex_edge_correlation[[1]]


    # Basic Graph Info
    graph_type <- ifelse(directed == TRUE, "Directed", "Undirected")
    weighted_graph <- ifelse((is.null(weights) == TRUE), "No", "Yes")[1]

    num_nodes <- length(igraph::V(g))
    num_ties <- length(igraph::E(g))

    num_types <- ifelse((is.null(type) == TRUE), NA, length(unique(type)))

    mutual <- suppressWarnings(igraph::dyad_census(g)$mut)
    asym <- suppressWarnings(igraph::dyad_census(g)$asym)
    null_ties <- suppressWarnings(igraph::dyad_census(g)$null)

    avg_geodesic <- igraph::average.path.length(g, directed = directed)

    ### Jim wanted to add transitivity correlation score as an additional
    ### network-level measure. This is readymade in `sna`, so we'll use
    ### `intergraph` and `sna` to call the package
    ###### The requisite function in `sna` needs specification as to whether
    ###### the network in question is directed or undirected. We'll make an
    ###### object here with that specification
    # print("trans_cor")
    # assign("broken_graph", g, .GlobalEnv)
    sna_mode <- ifelse(directed == T, "digraph", "graph")
    trans_cor <- sna::gtrans(intergraph::asNetwork(igraph::simplify(g, remove.multiple = T)), mode = sna_mode, measure = "correlation")


    # print("density")
    if (directed == TRUE){
      density_directed <- igraph::edge_density(g)
      density_undirected <- igraph::edge_density(igraph::as.undirected(g))
    } else {
      density_directed <- NA
      density_undirected <- igraph::edge_density(g)
    }

    num_isolates <- sum(nodes$total_degree == 0)

    # print("self_loops")
    num_self_loops <- sum(igraph::is.loop(g, eids = igraph::E(g)))


    # Centralization scores
    #### First, need to remove isolates from graph for select measures
    g_no_iso <- igraph::delete.vertices(g, v = (igraph::degree(g, mode = "all") == 0))

    if (directed == TRUE) {

      ### Betweenness
      cent_bet_undir <- igraph::centralization.betweenness(g_no_iso,
                                                           directed = FALSE,
                                                           normalized = TRUE)$centralization
      cent_bet_dir <- igraph::centralization.betweenness(g_no_iso,
                                                           directed = TRUE,
                                                           normalized = TRUE)$centralization

      ### Degree
      cent_deg_undir <- igraph::centralization.degree(g, mode = "all",
                                                      loops = FALSE,
                                                      normalized = TRUE)$centralization
      cent_deg_out <- igraph::centralization.degree(g, mode = "out",
                                                      loops = FALSE,
                                                      normalized = TRUE)$centralization
      cent_deg_in <- igraph::centralization.degree(g, mode = "in",
                                                      loops = FALSE,
                                                      normalized = TRUE)$centralization

      ### Closeness
      cent_close_undir <- igraph::centralization.closeness(g_no_iso,
                                                         mode = "all",
                                                         normalized = TRUE)$centralization
      cent_close_out <- igraph::centralization.closeness(g_no_iso,
                                                       mode = "out",
                                                       normalized = TRUE)$centralization
      cent_close_in <- igraph::centralization.closeness(g_no_iso,
                                                      mode = "in",
                                                      normalized = TRUE)$centralization

      ### Eigen
      cent_eigen_undir <- igraph::centralization.evcent(g_no_iso,
                                                        directed = FALSE,
                                                        normalized = TRUE)$centralization
      cent_eigen_dir <- igraph::centralization.evcent(g_no_iso,
                                                      directed = TRUE,
                                                      normalized = TRUE)$centralization

    } else {

      ### Betweenness
      cent_bet_undir <- igraph::centralization.betweenness(g_no_iso,
                                                           directed = FALSE,
                                                           normalized = TRUE)$centralization
      cent_bet_dir <- NA

      ### Degree
      cent_deg_undir <- igraph::centralization.degree(g, mode = "all",
                                                      loops = FALSE,
                                                      normalized = TRUE)$centralization
      cent_deg_out <- NA
      cent_deg_in <- NA

      ### Closeness
      cent_close_undir <- igraph::centralization.closeness(g_no_iso,
                                                         mode = "all",
                                                         normalized = TRUE)$centralization
      cent_close_out <- NA
      cent_close_in <- NA

      ### Eigen
      cent_eigen_undir <- igraph::centralization.evcent(g_no_iso,
                                                        directed = FALSE,
                                                        normalized = TRUE)$centralization
      cent_eigen_dir <- NA

    }


    # K-core cohesion (ask Jim for best name for this measure)
    k_core_cohesion <- k_cohesion(graph = g)



    measure_labels <- c('Type of Graph', 'Weighted', 'Number of Nodes', 'Number of Ties',
                        'Number of Tie Types',

                        "Number of isolates",
                        "Number of self-loops",

                        "Density (Undirected)", "Density (Directed)",

                        'Number of Weak Components', 'Size of Largest Weak Component', 'Proportion in the Largest Weak Component',
                        'Number of Strong Components', 'Size of Largest Strong Component', 'Proportion in the Largest Strong Component',
                        "Number of Largest Bicomponents", "Size of Largest Bicomponent(s)", "Proportion in the Largest Bicomponent(s)",

                        'Number of Mutual Ties', 'Number of Asymmetric Ties',
                        'Number of Null Ties',

                        'Degree Assortativity', 'Reciprocity Rate', 'Transitivity Rate',

                        'Transitivity Correlation',



                        'Global Clustering Coefficient', 'Average Geodesic',
                        'Multi-Level Edge Correlation',

                        'Pairwise Reachability (Weak, Undirected)', 'Pairwise Reachability (Strong, Undirected)',
                        'Pairwise Reachability (Weak, Directed)', 'Pairwise Reachability (Strong, Directed)',

                        "Betweenness Centralization (Undirected)", "Betweenness Centralization (Directed)",
                        "Degree Centralization (Undirected)", "Degree Centralization (In)", "Degree Centralization (Out)",
                        "Closeness Centralization (Undirected)", "Closeness Centralization (In)", "Closeness Centralization (Out)",
                        "Eigenvector Centralization (Undirected)", "Eigenvector Centralization (Directed)",
                        "K-Core Cohesion"
    )

    measure_descriptions <- c("Type of graph (either directed or undirected)",
                              "Whether or not edges in the graph have weights",
                              'The number of nodes in the graph',
                              'The number of ties in the graph',
                              'The number of types of tie in the graph (if multi-relational)',

                              "The number of nodes in the network without any ties to other nodes",
                              "The number of edges in the network that whose origin and target are the same node",

                              "The proportion of possible ties in the network that actually exist when treating edges as being undirected",
                              "The proportion of possible ties in the network that actually exist when treating edges as being directed",

                              'The number of weak components in the graph',
                              'The number of nodes in the largest weak component of the graph',
                              'The proportion of nodes in the largest weak component of the graph',

                              'The number of strong components in the graph',
                              'The number of nodes in the largest strong component of the graph',
                              'The proportion of nodes in the largest strong component of the graph',

                              'The number of maximally-sized bicomponents in the graph',
                              'The number of nodes in the largest bicomponent(s) of the graph',
                              'The proportion of nodes in the largest bicomponent(s) of the graph',

                              'The number of mutual ties in the graph',
                              'The number of asymmetric ties in the graph',
                              'The number of null ties in the graph',



                              'Edgewise correlation of degree', 'The proportion of directed ties that are reciprocated',
                              'The proportion of two-step paths that are also one-step paths',

                              "The observed correlation between a tie and the number of two-step paths connecting the two nodes in a tie",



                              'The proportion of closed triangles to all triangles', 'The average shortest path length',
                              'Multiplex networks edgwise correlation of relations',

                              'The proportion of nodes that share a weak component (undirected)',
                              'The proportion of nodes that share a strong component (undirected)',
                              'The proportion of nodes that share a weak component (directed)',
                              'The proportion of nodes that share a strong component (directed)',

                              # Betweeness
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by betweenness centrality scores (Undirected shortest paths used when calculating betweenness)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by betweenness centrality scores (Directed shortest paths used when calculating betweenness)",

                              #Degree
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by degree centrality scores (Undirected edges used when calculating degree)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by degree centrality scores (Incoming edges used when calculating degree)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by degree centrality scores (Outgoing edges used when calculating degree)",

                              # Closeness
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by closeness centrality scores (Undirected edges used when calculating closeness)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by closeness centrality scores (Incoming edges used when calculating closeness)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by closeness centrality scores (Outgoing edges used when calculating closeness)",

                              # Eigenvector centrality
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by eigenvector centrality scores (Undirected edges used when calculating eigenvector centrality)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by eigenvector centrality scores (Directed edges paths used when calculating eigenvector centrality)",

                              # K-core Cohesion
                              "The average across all pairs of the maximum k-core to which each pair is a joint member (Measures the average level of shared contacts)"
                              )

    measures <- c(graph_type, weighted_graph, as.character(num_nodes), as.character(num_ties), as.character(num_types),

                  as.character(num_isolates), as.character(num_self_loops),

                  as.character(density_undirected), as.character(density_directed),

                  as.character(num_clusters), as.character(largest_size), as.character(proportion_largest),
                  as.character(strong_num_clusters), as.character(strong_largest_size), as.character(strong_proportion_largest),
                  as.character(bicomponent_summary$num_bicomponents), as.character(bicomponent_summary$size_bicomponent), as.character(bicomponent_summary$prop_bicomponent),

                  as.character(mutual), as.character(asym), as.character(null_ties),

                  as.character(degree_assortativity), as.character(reciprocity_rate),
                  as.character(transitivity_rate), as.character(trans_cor),

                  as.character(global_clustering_coefficient), average_path_length,
                  as.character(multiplex_edge_correlation),
                  as.character(pairwise_weak_un), as.character(pairwise_strong_un),
                  as.character(pairwise_weak_dir), as.character(pairwise_strong_dir),

                  as.character(cent_bet_undir), as.character(cent_bet_dir),
                  as.character(cent_deg_undir), as.character(cent_deg_in), as.character(cent_deg_out),
                  as.character(cent_close_undir), as.character(cent_close_in), as.character(cent_close_out),
                  as.character(cent_eigen_undir), as.character(cent_eigen_dir),
                  as.character(k_core_cohesion))

    system_level_measures <- cbind(as.data.frame(measure_labels), measure_descriptions, measures)

    # If nodelist is in output, create indicator of singular Adjmat
    if ("nodelist" %in% output & directed == TRUE) {
      singular_df <- data.frame(measure_labels = "Singular Matrix",
                                measure_descriptions = "Whether corresponding adjacency matrix is singular. If 'Yes', network is treated as undirected when calculating Eigenvector and Bonacich centrality measures.",
                                measures = singular)

      system_level_measures <- rbind(system_level_measures, singular_df)
    }


    # Removing node-level and system-level data objects for clarity
    suppressWarnings(rm(measure_labels, measure_descriptions, num_clusters, proportion_largest, degree_assortativity,
       reciprocity_rate, global_clustering_coefficient, average_path_length,
       multiplex_edge_correlation, measures, singular, singular_df))

    suppressWarnings(rm(transitivity_rate, reachability, bicomponent_summary, largest_bicomponent_memberships, envir = .GlobalEnv))

  }


  # System & Node-Level Visualizations

  if ("system_measure_plot" %in% output) {

    if (shiny == FALSE) {
      grDevices::dev.new(width = 10.6806, height = 7.30556)
    }
    options(scipen = 0, digits = 7)
    system_plot <- function() {
      # Creating Layout
      viz_matrix <- matrix(c(10,10,10,10,10,10,10,10,10,
                             2,2,2,3,3,3,0,0,0,
                             1,1,1,1,1,1,4,4,4,
                             1,1,1,1,1,1,0,0,0,
                             1,1,1,1,1,1,5,5,5,
                             1,1,1,1,1,1,6,6,6,
                             1,1,1,1,1,1,0,0,0,
                             1,1,1,1,1,1,9,9,9,
                             7,7,7,8,8,8,0,0,0),
                           ncol  = 9, byrow = TRUE)
      layout(viz_matrix)
      layout.show(10)

      # Defining degree distribution coordinates
      y_axis <- density(nodes$total_degree)$y
      x_axis <- density(nodes$total_degree)$x
      coordinates <- cbind(as.data.frame(x_axis), y_axis)
      coordinates <- coordinates[(coordinates$x_axis >= 0), ]
      x_axis <- pretty(coordinates$x_axis)
      y_axis <- pretty(coordinates$y_axis)
      x_spacer <- x_axis[c(length(x_axis))] - x_axis[c(length(x_axis)-1)]
      x_spacer <- x_spacer*0.5
      y_spacer <- y_axis[c(length(y_axis))] - y_axis[c(length(y_axis)-1)]
      y_spacer <- y_spacer*0.5

      # Defining Base Degree Plot
      par(mar = c(5,6,2,2),  family='HersheySerif')
      plot(0, type='n', xlab=' ', ylab=' ', xlim=c(min(x_axis), max(x_axis)),
           ylim=c(min(y_axis), max(y_axis)), cex.axis=1.3, family='HersheySerif',
           las=1, main=' ', bty='n')
      grid(lwd = 2)

      # Adding Margin Text
      mtext(side = 1, text = 'Total Degree', col = "black", line = 3, cex = 1.5, family='HersheySerif')
      mtext(side = 2, text = 'Density', col = "black", line = 4.5, cex = 1.5, family='HersheySerif')

      # Plotting Degree
      lines(coordinates$x_axis, coordinates$y_axis, col='brown', lwd=1.5)

      # Adding Skew and Kurtosis
      skewness <- moments::skewness(nodes$total_degree)
      kurtosis <- moments::kurtosis(nodes$total_degree)
      text(x = (max(x_axis)-x_spacer), y = (max(y_axis)-y_spacer), paste('Skewness',round(skewness, digits=2)), cex=1.3)
      text(x = (max(x_axis)-x_spacer), y = (max(y_axis)-(y_spacer*2)), paste('Kurtosis',round(kurtosis, digits=2)), cex=1.3)

      # Adding Title
      title(c("Total Degree Distribution"), family='serif', cex.main=2)


      # Populating Subplots
      system_plot_names <- c("Number of Weak Components",
                             "Proportion in the Largest Weak Component",
                             "Degree Assortativity",
                             "Reciprocity Rate",
                             "Transitivity Rate",
                             "Global Clustering Coefficient",
                             "Average Geodesic",
                             "Multi-Level Edge Correlation")

      system_plot_labels <- c("# of Weak Components",
                              "% in the Largest Weak Component",
                              "Degree Assortativity",
                              "Reciprocity Rate",
                              "Transitivity Rate",
                              "Global Clustering Coefficient",
                              "Average Geodesic",
                              "Multi-Level Edge Correlation")


      for(i in seq_along(system_plot_names)) {
        plot_measure <- system_level_measures[system_level_measures$measure_labels == system_plot_names[[i]], 3]

        plot_measure <- ifelse(i < 8, as.numeric(plot_measure), plot_measure)
        plot_measure <- ifelse(i < 8, round(plot_measure, digits=2), plot_measure)
        plot_measure <- ifelse(i == 8, trimws(gsub('Edge', '', plot_measure)), plot_measure)

        par(mar=c(0,0,0,0), family='serif')
        plot(0, type='n', xlab=' ', ylab=' ', xlim=c(1,10),
             ylim=c(1,10), axes=FALSE, main='', bty='n')

        text(x=5, y=9, system_plot_labels[[i]], family='serif', font=2, cex=1.3)
        text(x=5, y=6.5, plot_measure, family='serif', cex=1.5)
        rm(plot_measure)
      }

      # Adding Plot Title
      par(mar=c(0,0,0,0), family='serif')
      plot(0, type='n', xlab=' ', ylab=' ', xlim=c(1,10),
           ylim=c(1,10), axes=FALSE, main='', bty='n')
      text(x=5.5, y=5, 'System-Level Measures', family='serif', font=2, cex=3)
    }

    g <- cowplot::as_grob(system_plot)
    p_1 <- cowplot::ggdraw(g)

    p_1

  }


  if ("node_measure_plot" %in% output) {

    node_measures_plot <- function() {
      # Creating Group Index of Measures
      columns <- colnames(nodes)
      columns <- columns[columns != "id" & columns != "attr" ]
      weighted_degree_measures <- columns[grepl("weighted", columns)]
      weighted_degree_measures <- cbind(rep("weighted_degree", length(weighted_degree_measures)), weighted_degree_measures)
      colnames(weighted_degree_measures) <- c('type', 'measure')

      degree_measures <- columns[grepl("_degree", columns)]
      degree_measures <- degree_measures[!grepl("weighted", degree_measures)]
      degree_measures <- cbind(rep("degree", length(degree_measures)), degree_measures)
      colnames(degree_measures) <- c('type', 'measure')

      closeness_measures <- columns[grepl("closeness", columns)]
      closeness_measures <- cbind(rep("closeness", length(closeness_measures)), closeness_measures)
      colnames(closeness_measures) <- c('type', 'measure')

      betweenness_measures <- columns[grepl("betweenness", columns)]
      betweenness_measures <- cbind(rep("betweenness", length(betweenness_measures)), betweenness_measures)
      colnames(betweenness_measures) <- c('type', 'measure')

      bon_measures <- columns[grepl("bon", columns)]
      bon_measures <- cbind(rep("bonacich", length(bon_measures)), bon_measures)
      colnames(bon_measures) <- c('type', 'measure')

      eigen_measures <- columns[grepl("eigen", columns)]
      eigen_measures <- cbind(rep("eigen", length(eigen_measures)), eigen_measures)
      colnames(eigen_measures) <- c('type', 'measure')

      burt_measures <- columns[grepl("burt", columns)]
      burt_measures <- cbind(rep("burt", length(burt_measures)), burt_measures)
      colnames(burt_measures) <- c('type', 'measure')

      reachable_measures <- columns[grepl("reach", columns)]
      reachable_measures <- cbind(rep("reachable", length(reachable_measures)), reachable_measures)
      colnames(reachable_measures) <- c('type', 'measure')

      measure_index <- as.data.frame(rbind(weighted_degree_measures,  degree_measures,
                                           closeness_measures, betweenness_measures,
                                           bon_measures, eigen_measures,
                                           burt_measures, reachable_measures))

      rm(columns, weighted_degree_measures, degree_measures, closeness_measures,
         betweenness_measures, bon_measures, eigen_measures, burt_measures,
         reachable_measures)



      # Specifying nicer labels
      plot_labels <- c('Weighted Degree', 'Degree', 'Closeness',
                       'Betweenness', 'Bon. Power Cent.',
                       'Eigenvector Cent.', 'Burt Measures', 'Reachability')

      # Font setting for legend elements
      par(family='serif')



      # Legend elements
      legend_elements <- vector("list", length(plot_labels))
      legend_elements[[1]] <- c("degree", "indegree", "outdegree")
      legend_elements[[2]] <- c("degree", "indegree", "outdegree")
      legend_elements[[3]] <- c()
      legend_elements[[4]] <- c("binarized", "weighted")

      # Handling different amounts of bonacich measures
      if ("bonpow_in" %in% names(nodes)) {
        legend_elements[[5]] <- c("in, beta 0.75",
                                  "out, beta 0.75",
                                  "sym, beta 0.75",
                                  "in beta -0.75",
                                  "out beta -0.75",
                                  "sym beta -0.75")

      } else {

        legend_elements[[5]] <- c("beta 0.75", "beta -0.75")

      }

      if ("eigen_centrality" %in% names(nodes)) {
        legend_elements[[6]] <- c()
      } else {
        legend_elements[[6]] <- c("incoming", "outgoing", "symmetrized")
      }
      legend_elements[[7]] <- c("constraint", "hierarchy")
      legend_elements[[8]] <- c("incoming", "outgoing", "all")

      # Isolating the measure being visualized based on whether it's directed or not
      plot_measures <- unique(measure_index$type)

      # Defining the layout used
      viz_matrix <- matrix(c(10,10,10,10,10,10,10,10,10,
                             1,1,1,2,2,2,3,3,3,
                             1,1,1,2,2,2,3,3,3,
                             1,1,1,2,2,2,3,3,3,
                             4,4,4,5,5,5,6,6,6,
                             4,4,4,5,5,5,6,6,6,
                             4,4,4,5,5,5,6,6,6,
                             7,7,7,8,8,8,9,9,9,
                             7,7,7,8,8,8,9,9,9,
                             7,7,7,8,8,8,9,9,9),
                           ncol  = 9, byrow = TRUE)
      layout(viz_matrix)
      #layout.show(viz_matrix)



      # Generating Subplot
      for(i in seq_along(plot_measures)){

        # Colors
        colors <- c('brown', 'blue', 'forestgreen', 'purple', 'goldenrod2', 'pink')

        # Identifying measure group & associated measures
        measures <- measure_index[(measure_index[,1] == plot_measures[[i]]), 2]



        # Eliminating NA Values
        sub_measures <- vector('list', length(measures))
        for(j in seq_along(measures)){
          plot_measure <- nodes[,measures[[j]]]
          plot_measure <- plot_measure[!is.na(plot_measure)]
          sub_measures[[j]] <- plot_measure
        }


        #measure_ranges <- lapply(sub_measures, function(x)range(x))
        measure_ranges <- numeric(length = length(sub_measures))

        if (length(measure_ranges) > 1) {

          for (j in 1:length(measure_ranges)){
            this_range <- range(sub_measures[[j]])
            measure_ranges[[j]] <- this_range[[2]]-this_range[[1]]

          }

          measure_ranges <- as.data.frame(cbind(legend_elements[[i]], measure_ranges))
          colnames(measure_ranges)[[1]] <- c("name")
          measure_ranges <- measure_ranges[!duplicated(measure_ranges[[2]]), ]
          colors <- colors[1:nrow(measure_ranges)]

        }


        # Defining degree distribution coordinates
        y_axis <- NULL
        x_axis <- NULL
        for(j in seq_along(sub_measures)){
          plot_measure <- sub_measures[[j]]
          y <- density(plot_measure)$y
          x <- density(plot_measure)$x

          coordinates <- cbind(as.data.frame(x), y)
          coordinates <- coordinates[(coordinates[,1] >= min(plot_measure)), ]
          coordinates <- coordinates[(coordinates[,1] <= max(plot_measure)), ]
          x <- c(min(plot_measure), coordinates$x, max(plot_measure))
          ymax <- max(coordinates$y)
          y <- c(0, coordinates$y, ymax)

          y_axis <- c(y_axis, y)
          x_axis <- c(x_axis, x)
        }

        y_axis <- pretty(y_axis)
        x_axis <- pretty(x_axis)
        colors <- colors[1:length(measures)]

        # Defining Base Degree Plot
        par(mar = c(5,6,2,2), family='HersheySerif')
        plot(0, type='n', xlab=' ', ylab=' ', xlim=c(min(x_axis), max(x_axis)),
             ylim=c(min(y_axis), max(y_axis)), cex.axis=1.3, family='HersheySerif',
             las=1, main=' ', bty='n')
        grid(lwd = 2)

        # Adding Margin Text
        mtext(side = 1, text = plot_labels[[i]], col = "black", line = 3, cex = 1.2, family='HersheySerif')

        # Plotting Degree
        for(j in seq_along(sub_measures)){
          plot_measure <- sub_measures[[j]]
          y_axis <- density(plot_measure)$y
          x_axis <- density(plot_measure)$x
          coordinates <- cbind(as.data.frame(x_axis), y_axis)
          coordinates <- coordinates[(coordinates$x_axis >= min(plot_measure)), ]
          x_axis <- coordinates$x_axis
          y_axis <- coordinates$y_axis
          lines(x_axis, y_axis, col=colors[[j]], lwd=1.5)

        }

        if (length(legend_elements[[i]] > 0)) {


          par(family='serif')
          legend("topright", legend=measure_ranges[[1]], col=colors, lty=1, cex=.9, bty='n')
        }



      }

      # Adding Legend
      #  if(directed == TRUE){
      par(mar=c(0,0,0,0), family='serif')
      plot(0, type='n', xlab=' ', ylab=' ', xlim=c(1,10),
           ylim=c(1,10), axes=FALSE, main='', bty='n')

      # segments(3, 8, 4, 8, col="blue")
      # segments(3, 5, 4, 5, col="forestgreen")
      #
      text(x=6, y=7, 'Note: Overlapping lines may ', family='serif', cex=1.3)
      text(x=6, y=6, 'merely indicate nearly', family='serif', cex=1.3)
      text(x=6, y=5, 'duplicate values', family='serif', cex=1.3)
      # }else{
      #   par(mar=c(0,0,0,0), family='serif')
      #   plot(0, type='n', xlab=' ', ylab=' ', xlim=c(1,10),
      #        ylim=c(1,10), axes=FALSE, main='', bty='n')
      # }

      # Adding Title
      par(mar=c(0,0,0,0), family='serif')
      plot(0, type='n', xlab=' ', ylab=' ', xlim=c(1,10),
           ylim=c(1,10), axes=FALSE, main='', bty='n')
      text(x=5.5, y=5, 'Node-Level Measures', family='serif', font=2, cex=3)
    }

    g <- cowplot::as_grob(node_measures_plot)
    p_2 <- cowplot::ggdraw(g)

    p_2

  }

  # Assigning Report Elements to the Global Environment
  if ("system_measure_plot" %in% output) {
    assign(x = 'system_measure_plot', value = p_1,.GlobalEnv)
  }
  if ("node_measure_plot" %in% output) {
    assign(x = 'node_measure_plot', value = p_2,.GlobalEnv)
  }
  if ("system_level_measures" %in% output) {
    assign(x = 'system_level_measures', value = system_level_measures, .GlobalEnv)
  }

  # Remove largest component/bicomponent if not specified
  if (!("largest_component" %in% output)) {
    suppressWarnings(rm(largest_component, largest_component_ids))
  }

  if (!("largest_bi_component" %in% output)) {
    suppressWarnings(rm(largest_bi_component, largest_bi_component_ids))
  }

}


# Quick update
