#' Network Cleaning and Variable Calculation (\code{netwrite})
#'
#' @description The \code{netwrite} function reads in relational data of several formats and processes them into a set of standardized outputs. These outputs include sets of commonly calculated measures at the individual node and network-wide levels.
#'
#' @param data_type A character value indicating the type of relational data being entered into \code{netwrite}. Available options are \code{edgelist}, \code{adjacency_matrix}, and \code{adjacency_list}.
#' @param adjacency_matrix If \code{data_type} is set to \code{adjacency_matrix}, a matrix object containing the adjacency matrix for the network being processed.
#' @param adjacency_list If \code{data_type} is set to \code{adjacency_list}, a data frame containing the adjacency list for the network being processed.
#' @param edgelist A data frame including all ties in the network. If this argument is specified, \code{i_elements}, \code{j_elements}, \code{edge_netid} (if applicable), \code{weights} (if applicable), and \code{type} (if applicable), must be specified as single character values indicating the names of their respective columns.
#' @param edge_netid If \code{data_type} is set to \code{"edgelist"}, a vector of identifiers indicating the specific network to which a particular tie in the edgelist belongs, or a single character value indicating the name of the column in \code{edgelist} containing network identifiers.
#' @param i_elements If \code{data_type} is set to \code{"edgelist"}, a vector of identifiers indicating the senders of ties in the edgelist, or a single character value indicating the name of the column in \code{edgelist} containing these identifiers.
#' @param j_elements If \code{data_type} is set to \code{"edgelist"}, a vector of identifiers indicating the receivers of ties in the edgelist, or a single character value indicating the name of the column in \code{edgelist} containing these identifiers.
#' @param weights If \code{data_type} is set to \code{"edgelist"}, a numeric vector indicating the weight of ties in the edgelist, or a single character value indicating the name of the column in \code{edgelist} containing tie weights. \code{netwrite} requires that all edge weights be positive values.
#' @param type If \code{data_type} is set to \code{"edgelist"}, a numeric or character vector indicating the types of relationships represented in the edgelist, or a single character value indicating the name of the column in \code{edgelist} containing tie types. If \code{type} is specified, \code{netwrite} will treat network(s) as multi-relational and produce additional outputs reflecting the different types of ties appearing in the data.
#' @param nodelist Either a vector of values indicating unique node/vertex IDs, or a data frame including all information about nodes in the network. If the latter, a value for \code{node_id} must be specified.
#' @param node_id If a data frame is entered for the \code{nodelist} argument, \code{node_id} should be a character value indicating the name of the column in the node-level data frame containing unique node identifiers.
#' @param node_netid If a data frame is entered for the \code{nodelist} argument, \code{node_netid} should be a character value indicating the name of the column in the node-level data frame containing unique network identifiers. This argument should be specified if a value is given for \code{edge_netid}.
#' @param fix_nodelist If \code{data_type} is set to \code{"edgelist"} and user inputs a vector or data frame into \code{nodelist}, a logical value indicating whether to include node IDs that do not appear in the nodelist but do appear in the edgelist in the nodelist used when processing network data. By default, \code{fix_nodelist} is set to \code{FALSE} to identify potential inconsistencies between the nodelist and edgelist to the user.
#' @param remove_loops A logical value indicating whether "self-loops" (ties directed toward oneself) should be considered valid ties in the network being processed.
#' @param missing_code A numeric value indicating "missing" values in an edgelist. Such "missing" values are sometimes included to identify the presence of isolated nodes in an edgelist when a corresponding nodelist is unavailable.
#' @param weight_type A character value indicating whether edge weights should be treated as frequencies or distances. Available options are \code{"frequency"}, indicating that higher values represent stronger ties, and \code{"distance"}, indicating that higher values represent weaker ties. Note: some underlying functions assume that edges represent distances. If \code{weight_type} is set to \code{"frequency"}, these functions will use the reciprocal of \code{weights} as distance values in calculation.
#' @param directed A logical value indicating whether edges should be treated as a directed or undirected when constructing the network.
#' @param net_name A character value indicating the name to which network/igraph objects should be given.
#' @param shiny A logical value indicating whether \code{netwrite} is being used in conjunction with IDEANet's Shiny-based visualization app. \code{shiny} should also be set to \code{TRUE} when using \code{ideanet} in an R Markdown file that users expect to knit into a document.
#' @param output A character vector indicating the kinds of objects \code{netwrite} should assign to the global environment. \code{netwrite} produces several outputs that may not all be necessary to a user's needs. Users can specify which outputs they specifically want in order to minimize the number of objects appearing in the global environment. Potential outputs include igraph object(s) (\code{"graph"}), subgraph(s) of only nodes that appear in the largest component and/or bicomponent of the network (\code{"largest_component"}, \code{"largest_bi_component"}), data frame(s) containing node-level measures (\code{"node_measure_plot"}), a processed edgelist of the network (\code{"edgelist"}), a data frame indicating network-level summaries (\code{"system_level_measures"}), and summary visualizations for node- and network-level measures (\code{"node_measure_plot"}, \code{"system_measure_plot"}).
#' @param message A logical value indicating whether warning messages should be displayed in the R console during processing.
#'
#' @return \code{netwrite} returns a list containing several output objects. Users may find it easier to access and work with outputs by applying \link{list2env} to this list, which will separate outputs and store them in the R Global Environment. Note, however, that this risks overwriting existing objects in the Global Environment should those objects share names with objects in \code{netwrite}'s output. Depending on the values assigned to the \code{output} argument, \code{netwrite} will produce any or all of the following:
#'
#' If \code{output} contains \code{graph}, \code{netwrite} will return an igraph object of the network represented in the original data.
#' If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing igraph objects for each unique relation type as well as the overall network. These output objects are named according to the value specified in the \code{net_name} argument.
#'
#' If \code{output} contains \code{"nodelist"}, \code{netwrite} will return a dataframe containing individual-level information for each node in the network. This dataframe contains a set of frequently used node-level measures for each node in the network. If a vector is entered into the \code{type} argument, \code{netwrite} will produce these node-level measures for each unique relation type.
#'
#' If \code{output} contains \code{"edgelist"}, \code{netwrite} will return a formatted edgelist for the network represented in the original data. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing edgelists for each unique relation type as well as the overall network.
#'
#' If \code{output} contains \code{"system_level_measures"}, \code{netwrite} will return a data frame providing network-level summary information.
#'
#' If \code{output} contains \code{"node_measure_plot"}, \code{netwrite} will return a plot summarizing the distribution of frequently used node-level measures across all nodes in the network. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing node-level summary plots for each unique relation type as well as the overall network.
#'
#' If \code{output} contains \code{"system_measure_plot"}, \code{netwrite} will return a plot summarizing the distribution of frequently used network-level measures. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing network-level summary plots for each unique relation type as well as the overall network.
#'
#' If \code{output} contains \code{"largest_bi_component"}, \code{netwrite} will return an igraph object of the largest bicomponent in the network represented in the original data. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing the largest bicomponent for each unique relation type as well as the overall network.
#'
#' If \code{output} contains \code{"largest_bi_component"}, \code{netwrite} will return an igraph object of the largest main component in the network represented in the original data. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing the largest main component for each unique relation type as well as the overall network.
#'
#' If users are working with data containing multiple independent networks, \code{netwrite} will return a list containing the above outputs for each network in their data, provided that users have passed a vector of network identifiers to the \code{edge_netid} argument. Each network's output will be labeled according to its corresponding value in \code{edge_netid}.
#'
#' @export
#'
#' @examples
#' # Use netwrite on an edgelist
#' nw_fauxmesa <- netwrite(nodelist = fauxmesa_nodes,
#'                       node_id = "id",
#'                       i_elements = fauxmesa_edges$from,
#'                       j_elements = fauxmesa_edges$to,
#'                       directed = TRUE,
#'                       net_name = "faux_mesa")
#'
#' ### Inspect updated edgelist
#' head(nw_fauxmesa$edgelist)
#'
#' ### Inspect data frame of node-level measures
#' head(nw_fauxmesa$node_measures)
#'
#' ### Inspect system-level summary
#' head(nw_fauxmesa$system_level_measures)
#'
#' ### Plot sociogram of network
#' plot(nw_fauxmesa$faux_mesa)
#'
#' ### View node-level summary visualization
#' nw_fauxmesa$node_measure_plot
#'
#' ### View system-level summary visualization
#' nw_fauxmesa$system_measure_plot
#'
#'
#'
#' # Run netwrite on an adjacency matrix
#'
#' nw_triad <- netwrite(data_type = "adjacency_matrix",
#'                      adjacency_matrix = triad,
#'                      directed = TRUE,
#'                      net_name = "triad_igraph")


#################################################
#    U S E R - F A C I N G   F U N C T I O N    #
#################################################

netwrite <- function(data_type = c('edgelist'), adjacency_matrix=FALSE,
                     adjacency_list=FALSE,
                     nodelist=FALSE,
                     # `node_id` takes a character argument specifying
                     # how the original node ID variable should be named
                     # in output
                     node_id = NULL,
                     # `node_netid` takes a character argument specifying
                     # which column in the nodelist contains the network identifier
                     node_netid = NULL,

                     edgelist = FALSE,
                     i_elements=FALSE,
                     j_elements=FALSE,
                     # `edge_netid` takes the column from the edgelist containing
                     # network identifiers
                     edge_netid = NULL,
                     # In the rare event that an edgelist contains node IDs that are
                     # not in the nodelist, `fix_nodelist` will add these node IDs to the
                     # nodelist used in network processing
                     fix_nodelist = TRUE,
                     # I THINK the `weights` argument should work for adjmats if we just have users set to TRUE when using a weighted adjmat
                     weights=NULL, type=NULL,
                     remove_loops = FALSE,
                     missing_code=99999,
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

  # browser()

  # `netwrite` doesn't play nicely with tibbles, so if `nodelist` or `edgelist`
  # are tibbles we'll need to convert them to data.frames

  if ("tbl" %in% class(edgelist)) {
    edgelist <- as.data.frame(edgelist)
  }

  if ("tbl" %in% class(nodelist)) {
    nodelist <- as.data.frame(nodelist)
  }

# Some measures in the system level measures data frame calculated from the
  # distribution of centrality scores at the node level. If users don't choose
  # to create an output built on node-level measures, however, we should exclude
  # system-level measures that depend on centrality scores. When this occurs
  # we display a warning to the user:
  if (!("nodelist" %in% output | "node_measure_plot" %in% output) & "system_level_measures" %in% output) {
    base::warning("Outputs related to node-level measures not selected for inclusion in `netwrite` output. System-level measures based on node-level centrality scores (e.g. centralization, Herfindahl index) will be excluded from system-level measures output.")
  }


  if (data_type == "edgelist") {

    # Check if edgelist-related arguments are names of columns rather than vectors
    # If this is the case, extract vectors
    if (length(i_elements) == 1 & is.character(i_elements)) {
      if ("data.frame" %in% class(edgelist)) {
        i_elements <- edgelist[, i_elements]
      } else {
        stop("Edgelist data frame not given.")
      }
    }

    if (length(j_elements) == 1 & is.character(j_elements)) {
      if ("data.frame" %in% class(edgelist)) {
        j_elements <- edgelist[, j_elements]
      } else {
        stop("Edgelist data frame not given.")
      }
    }

    if (length(weights) == 1 & is.character(weights)) {
      if ("data.frame" %in% class(edgelist)) {
        weights <- edgelist[, weights]
      } else {
        stop("Edgelist data frame not given.")
      }
    }

    if (length(type) == 1 & is.character(type)) {
      if ("data.frame" %in% class(edgelist)) {
        type <- edgelist[, type]
      } else {
        stop("Edgelist data frame not given.")
      }
    }

    if (length(edge_netid) == 1 & is.character(edge_netid)) {
      if ("data.frame" %in% class(edgelist)) {
        edge_netid <- edgelist[, edge_netid]
      } else {
        stop("Edgelist data frame not given.")
      }
    }

    # Create temporary edgelist to pass to sub-functions
    temp_el <- data.frame(i_elements = i_elements,
                          j_elements = j_elements)

    if (!is.null(weights)) {
      temp_el$weights = weights
    } else {
      temp_el$weights <- 1
    }

    if (!is.null(type)) {
      temp_el$type = type
    } else {
      temp_el$type <- NA
    }

    if (!is.null(edge_netid)) {

      # If network identifiers are numerics, make into a character
      if (is.numeric(edge_netid)) {
        edge_netid <- paste("network", edge_netid, sep = "")
      }

      temp_el$edge_netid = edge_netid
    } else {
      temp_el$edge_netid <- NA
    }


    if (!is.null(node_netid) ) {
      if (is.numeric(nodelist[, node_netid])) {
        nodelist[, node_netid] <- paste("network", nodelist[, node_netid], sep = "")
      }
    }



  }

  if (!is.null(edge_netid)) {

    # Create list for storing each context's output
    context_list <- list()

    # Get unique netid values
    netid_vals <- unique(edge_netid)

    # For each unique network ID, extract edgelist and, if applicable, nodelist
    for (i in 1:length(netid_vals)) {

      base::message(paste("Processing network ", netid_vals[[i]], sep = ""))

      # FIX THIS, NEED TO CONSTRUCT THE EDGELIST HERE AT THE TOP
      these_edges <- temp_el[edge_netid == netid_vals[[i]],]
      these_nodes <- FALSE

      if ("data.frame" %in% class(nodelist)) {
        these_nodes <- nodelist[nodelist[, node_netid] == netid_vals[[i]], ]
      }

      if (min(these_edges$weights) == 1 & max(these_edges$weights) == 1) {
        these_weights <- NULL
      } else {
        these_weights <- these_edges$weights
      }

      if (sum(is.na(these_edges$type)) == nrow(these_edges)) {
        these_types <- NULL
      } else {
        these_types <- these_edges$type
      }


      context_list[[i]] <- multi_netwrite(nodelist = these_nodes,
                                          node_id = node_id,
                                          i_elements = these_edges$i_elements,
                                          j_elements = these_edges$j_elements,
                                          fix_nodelist = fix_nodelist,
                                          weights = these_weights,
                                          type = these_types,
                                          remove_loops = remove_loops,
                                          missing_code = missing_code,
                                          weight_type = weight_type,
                                          directed = directed,
                                          net_name = as.character(netid_vals[[i]]),
                                          shiny = shiny,
                                          output = output,
                                          message = message)

    }

    names(context_list) <- netid_vals
    return(context_list)


  } else {

    return(multi_netwrite(data_type = data_type,
                          adjacency_matrix = adjacency_matrix,
                          adjacency_list = adjacency_list,
                          nodelist = nodelist,
                          node_id = node_id,
                          i_elements = i_elements,
                          j_elements = j_elements,
                          fix_nodelist = fix_nodelist,
                          weights = weights,
                          type = type,
                          remove_loops = remove_loops,
                          missing_code = missing_code,
                          weight_type = weight_type,
                          directed = directed,
                          net_name = net_name,
                          shiny = shiny,
                          output = output,
                          message = message))

  }

}


##########################################################
#   PERFORMING MULTI-RELATIONAL FUNCTIONS IF SPECIFIED   #
##########################################################

# This is `netwrite` proper, as users encounter it. `netwrite` proper is effectively
# a wrapper for `basic_netwrite` that detects whether there's a multirelational
# edgelist present for which `basic_netwrite` needs to be applied to each relation type's
# respective subgraph

multi_netwrite <- function(data_type = c('edgelist'), adjacency_matrix=FALSE,
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
                           missing_code=99999,
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

  # browser()
  # netwrite_start <- Sys.time()

  # If a vector of edge weights are passed, check to see that all weights exceed
  # zero, otherwise return an error
  if (!is.null(weights)) {
    if (sum(weights <= 0) > 0) {
      stop("Detected edge weight values of 0 or lower. netwrite only supports processing of positive edge weights.")
    }
  }
  ### Need to add a similar check for values in adjacency matrix if used:
  if (is.matrix(adjacency_matrix)) {
    if (min(adjacency_matrix) < 0) {
      stop("Detected edge weight values of 0 or lower. netwrite only supports processing of positive edge weights.")
    }
  }



  # Since we can't seem to create an environment that will go outside the function, we'll have to
  # create a list to store all output
  netwrite_output <- list()




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
    if (node_id == "name") {
      node_id <- "original_name"
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
    # If any column in the nodelist dataframe is named `"name"`,
    # rename to `"original_name"`
    original_nodelist_names[which(original_nodelist_names == "name")] <- "original_name"
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

      # Create an expanded dataframe to include nodes not originally appearing in nodelist
      nodelist_union <- data.frame(id = just_ids)
      # Add a variable indicating whether nodes appear in `original_nodelist`
      nodelist_union$in_original_nodelist <- nodelist_union$id %in% original_nodelist[, node_id]
      # Rename columns
      colnames(nodelist_union) <- c(node_id, "in_original_nodelist")
      # Merge into `original_nodelist`
      original_nodelist <- dplyr::left_join(nodelist_union, original_nodelist, by = node_id)
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
  # the data entered does not reflect a multi-relational net
  if (is.null(type) == TRUE) {


    # If `nodelist` is a data frame, we'll want to make sure that the node-level
    # attributes it already stores are included in the igraph objects that
    # `netwrite` produces

    if (("data.frame" %in% class(nodelist)) == TRUE) {

      if (message == FALSE) {
        netwrite_output <- suppressWarnings(
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
                         missing_code = missing_code,
                         weight_type = weight_type,
                         directed = directed,
                         net_name = net_name,
                         shiny = shiny,
                         output = output,
                         message = message)
        )
      } else {

        netwrite_output <- basic_netwrite(data_type = data_type,
                                          adjacency_matrix = adjacency_matrix,
                                          adjacency_list = adjacency_list,
                                          nodelist = original_nodelist,
                                          node_id = node_id,
                                          i_elements = i_elements,
                                          j_elements = j_elements,
                                          weights = weights,
                                          type = type,
                                          remove_loops = remove_loops,
                                          missing_code = missing_code,
                                          weight_type = weight_type,
                                          directed = directed,
                                          net_name = net_name,
                                          shiny = shiny,
                                          output = output,
                                          message = message)
      }

    } else {

      if (message == FALSE) {
        netwrite_output <- suppressWarnings(
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
                         missing_code = missing_code,
                         weight_type = weight_type,
                         directed = directed,
                         net_name = net_name,
                         shiny = shiny,
                         output = output,
                         message = message)
        )
      } else {
        netwrite_output <- basic_netwrite(data_type = data_type,
                                          adjacency_matrix = adjacency_matrix,
                                          adjacency_list = adjacency_list,
                                          nodelist = just_ids,
                                          node_id = node_id,
                                          i_elements = i_elements,
                                          j_elements = j_elements,
                                          weights = weights,
                                          type = type,
                                          remove_loops = remove_loops,
                                          missing_code = missing_code,
                                          weight_type = weight_type,
                                          directed = directed,
                                          net_name = net_name,
                                          shiny = shiny,
                                          output = output,
                                          message = message)
      }

    }


    # The below code chunk is presumably deprecated, but saving here in case
    # merge issues reappear.

    # If `nodelist` is a data frame, we'll want to merge it into `node_measures`
    if (("data.frame" %in% class(nodelist)) == TRUE & "nodelist" %in% output) {

      # Sometimes the original ID column we need to join on will be of a different class
      # between the two dataframes we're trying to merge here. To be safe, we'll convert both columns
      # into characters and merge
      original_nodelist[, node_id] <- as.character(unlist(original_nodelist[, node_id]))

      # Need to rename the `label` column in the node measure dataframe if working with an adjmat
      if (data_type == "adjacency_matrix") {
        colnames(netwrite_output$node_measures)[[2]] <- node_id
      }

      netwrite_output$node_measures[, node_id] <- as.character(unlist(netwrite_output$node_measures[, node_id]))



      node_measures <- dplyr::left_join(original_nodelist, netwrite_output$node_measures, by = node_id)
      # Rearrange columns
      node_measures <- dplyr::select(node_measures, .data$id, dplyr::everything())

      netwrite_output$node_measures <- node_measures


      # We'll also want to store original node attributes in the igraph object here if we're working with an adjmat
      if (data_type == "adjacency_matrix") {
        nodelist_names <- names(netwrite_output$node_measures)

        for (i in 2:length(nodelist_names)) {

          eval(parse(
            text = paste("igraph::V(netwrite_output[[net_name]])$", nodelist_names[[i]], "<- netwrite_output$node_measures[,i]", sep = "")
          ))

        }
      }

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
        if (methods::is(edges_list[[i]]$type, "character")) {
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

          if (message == FALSE) {
            this_netwrite <- suppressWarnings(
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
                             missing_code = missing_code,
                             weight_type = weight_type, directed=directed,
                             net_name = "this_igraph", # Giving a consistent name for igraph object
                             # Makes it easier to place into the list of
                             # igraph objects down the line.
                             shiny = TRUE,
                             # For processing multi-relational nets, we'll want to collect all the
                             # possible output objects from `basic_netwrite` at first, then filter out
                             # based on the original outputs specified in the `netwrite` call
                             # output = c("graph",
                             #            "largest_bi_component",
                             #            "largest_component",
                             #            "node_measure_plot",
                             #            "nodelist",
                             #            "edgelist",
                             #            "system_level_measures",
                             #            "system_measure_plot"),
                             output = output,
                             message = message)
            )
          } else {
            this_netwrite <- basic_netwrite(data_type = data_type,
                                            adjacency_matrix = adjacency_matrix,
                                            adjacency_list = adjacency_list,
                                            nodelist = nodelist_condition,
                                            node_id = node_id,
                                            i_elements=edges_list[[i]]$i_elements,
                                            j_elements=edges_list[[i]]$j_elements,
                                            weights=edges_list[[i]]$weights,
                                            type=edges_list[[i]]$type,
                                            remove_loops = remove_loops,
                                            missing_code = missing_code,
                                            weight_type = weight_type, directed=directed,
                                            net_name = "this_igraph", # Giving a consistent name for igraph object
                                            # Makes it easier to place into the list of
                                            # igraph objects down the line.
                                            shiny = TRUE,
                                            # For processing multi-relational nets, we'll want to collect all the
                                            # possible output objects from `basic_netwrite` at first, then filter out
                                            # based on the original outputs specified in the `netwrite` call
                                            # output = c("graph",
                                            #            "largest_bi_component",
                                            #            "largest_component",
                                            #            "node_measure_plot",
                                            #            "nodelist",
                                            #            "edgelist",
                                            #            "system_level_measures",
                                            #            "system_measure_plot"),
                                            output = output,
                                            message = message)
          }



        }else{
          # Aggregate graph, unweighted
          if (message == FALSE) {
            this_netwrite <- suppressWarnings(
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
                             missing_code = missing_code,
                             weight_type = weight_type, directed=directed,
                             net_name = "this_igraph",
                             shiny = TRUE,
                             # output = c("graph",
                             #            "largest_bi_component",
                             #            "largest_component",
                             #            "node_measure_plot",
                             #            "nodelist",
                             #            "edgelist",
                             #            "system_level_measures",
                             #            "system_measure_plot"),
                             output = output,
                             message = message)
            )
          } else {
            this_netwrite <- basic_netwrite(data_type = data_type,
                                            adjacency_matrix = adjacency_matrix,
                                            adjacency_list = adjacency_list,
                                            nodelist = nodelist_condition,
                                            node_id = node_id,
                                            i_elements=edges_list[[i]]$i_elements,
                                            j_elements=edges_list[[i]]$j_elements,
                                            weights = NULL,
                                            type=edges_list[[i]]$type,
                                            remove_loops = remove_loops,
                                            missing_code = missing_code,
                                            weight_type = weight_type, directed=directed,
                                            net_name = "this_igraph",
                                            shiny = TRUE,
                                            # output = c("graph",
                                            #            "largest_bi_component",
                                            #            "largest_component",
                                            #            "node_measure_plot",
                                            #            "nodelist",
                                            #            "edgelist",
                                            #            "system_level_measures",
                                            #            "system_measure_plot"),
                                            output = output,
                                            message = message)
          }
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

          if (message == FALSE) {
            this_netwrite <- suppressWarnings(
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
                             missing_code = missing_code,
                             weight_type = weight_type, directed=directed,
                             net_name = "this_igraph",
                             shiny = TRUE,
                             # output = c("graph",
                             #            "largest_bi_component",
                             #            "largest_component",
                             #            "node_measure_plot",
                             #            "nodelist",
                             #            "edgelist",
                             #            "system_level_measures",
                             #            "system_measure_plot"),
                             output = output,
                             message = message)
            )
          } else {
            this_netwrite <- basic_netwrite(data_type = data_type,
                                            adjacency_matrix = adjacency_matrix,
                                            adjacency_list = adjacency_list,
                                            nodelist = nodelist_condition,
                                            node_id = node_id,
                                            i_elements=edges_list[[i]]$i_elements,
                                            j_elements=edges_list[[i]]$j_elements,
                                            weights = edges_list[[i]]$weights,
                                            type=edges_list[[i]]$type,
                                            remove_loops = remove_loops,
                                            missing_code = missing_code,
                                            weight_type = weight_type, directed=directed,
                                            net_name = "this_igraph",
                                            shiny = TRUE,
                                            # output = c("graph",
                                            #            "largest_bi_component",
                                            #            "largest_component",
                                            #            "node_measure_plot",
                                            #            "nodelist",
                                            #            "edgelist",
                                            #            "system_level_measures",
                                            #            "system_measure_plot"),
                                            output = output,
                                            message = message)
          }

        } else {
          # Subgraphs, without weights
          if (message == FALSE) {
            this_netwrite <- suppressWarnings(
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
                             missing_code = missing_code,
                             weight_type = weight_type, directed=directed,
                             net_name = "this_igraph",
                             shiny = TRUE,
                             # output = c("graph",
                             #            "largest_bi_component",
                             #            "largest_component",
                             #            "node_measure_plot",
                             #            "nodelist",
                             #            "edgelist",
                             #            "system_level_measures",
                             #            "system_measure_plot"),
                             output = output,
                             message = message)
            )
          } else {
            this_netwrite <- basic_netwrite(data_type = data_type,
                                            adjacency_matrix = adjacency_matrix,
                                            adjacency_list = adjacency_list,
                                            nodelist = nodelist_condition,
                                            node_id = node_id,
                                            i_elements=edges_list[[i]]$i_elements,
                                            j_elements=edges_list[[i]]$j_elements,
                                            weights = NULL,
                                            type=edges_list[[i]]$type,
                                            remove_loops = remove_loops,
                                            missing_code = missing_code,
                                            weight_type = weight_type, directed=directed,
                                            net_name = "this_igraph",
                                            shiny = TRUE,
                                            # output = c("graph",
                                            #            "largest_bi_component",
                                            #            "largest_component",
                                            #            "node_measure_plot",
                                            #            "nodelist",
                                            #            "edgelist",
                                            #            "system_level_measures",
                                            #            "system_measure_plot"),
                                            output = output,
                                            message = message)
          }
        }
      }

      # Store netwrite outputs into respective lists, if they've been generated

      # igraph object list
      if ("graph" %in% output) {
        graphs_list[[i]] <- this_netwrite$this_igraph
      }


      # Largest bicomponent list
      if ("largest_bi_component" %in% output) {
        bicomponent_list[[i]] <- this_netwrite$largest_bi_component
      }


      # Largest component list
      if ("largest_component" %in% output) {
        lcomponent_list[[i]] <- this_netwrite$largest_component
      }

      # Node measure plot list
      if ("node_measure_plot" %in% output) {
        nplot_list[[i]] <- this_netwrite$node_measure_plot
      }


      # Node measures list
      ### To ensure successful merging downstream,
      ### convert `attr` to character before adding
      ### `node_measures_list`
      if ("nodelist" %in% output) {
        this_netwrite$node_measures[,2] <- as.character(this_netwrite$node_measures[,2])
        node_measures_list[[i]] <- this_netwrite$node_measures
      }

      # List of final processes edgelists
      if ("edgelist" %in% output) {
        f_edges_list[[i]] <- this_netwrite$edgelist
      }



      # System-level measures summaries list
      if ("system_level_measures" %in% output) {
        ### Rename the `measures` column to the edge type
        colnames(this_netwrite$system_level_measures) <- c("measure_labels", "description", names(edges_list)[[i]])
        s_measures_list[[i]] <- this_netwrite$system_level_measures
      }

      # System-level plot list
      if ("system_measure_plot" %in% output) {
        splot_list[[i]] <- this_netwrite$system_measure_plot
      }


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
      netwrite_output[[`net_name`]] <- graph
      netwrite_output$igraph_list <- graphs_list

    }

    if ("largest_bi_component" %in% final_output) {
      netwrite_output$largest_bi_component <- bicomponent_list
    }

    if ("largest_component" %in% final_output) {
      netwrite_output$largest_component <- lcomponent_list
    }

    if ("node_measure_plot" %in% final_output) {
      netwrite_output$node_measure_plot <- nplot_list
    }

    # For `nodelist`, we just want a dataframe that has the node-level
    # measures for each relation type. To do this, we're going to want
    # to rename the columns in each element of `node_measures_list` and
    # then merge these elements.

    # browser()

    if ("nodelist" %in% final_output) {

    for (i in 2:length(node_measures_list)) {


      this_name <- names(node_measures_list)[[i]]
      new_column_names <- paste(this_name,
                                colnames(node_measures_list[[i]]),
                                sep = "_")

      colnames(node_measures_list[[i]]) <- c(colnames(node_measures_list[[i]])[1:2],
                                             new_column_names[3:length(new_column_names)])
    }

    node_measures <- suppressMessages(Reduce(dplyr::full_join, node_measures_list))

    # If `nodelist` is a data frame, we'll want to merge it into `node_measures`
    if (("data.frame" %in% class(nodelist)) == TRUE) {

      # Sometimes the original ID column we need to join on will be of a different class
      # between the two dataframes we're trying to merge here. To be safe, we'll convert both columns
      # into characters and merge
      original_nodelist[, node_id] <- as.character(unlist(original_nodelist[, node_id]))
      node_measures[, node_id] <- as.character(unlist(node_measures[, node_id]))

      node_measures <- dplyr::left_join(original_nodelist, node_measures, by = node_id)
      # Rearrange columns
      node_measures <- dplyr::select(node_measures, .data$id, dplyr::everything())

    }


      netwrite_output$node_measures <- node_measures
    }

    if ("edgelist" %in% final_output) {
      netwrite_output$edgelist <- f_edges_list
    }

    # COME BACK TO THIS ONE AND SEE IF WE CAN COMBINE INTO A SINGLE DATAFRAME

    if ("system_level_measures" %in% final_output) {
      s_measures_reduce <- suppressMessages(Reduce(dplyr::full_join, s_measures_list))
      netwrite_output$system_level_measures <- s_measures_reduce
    }

    if ("system_measure_plot" %in% final_output) {
      netwrite_output$system_measure_plot <- splot_list
    }




  } # End multi-relational network processing

  # Remove objects not specific as being included in `output`, based on
  # values stored in `final_output`. Although the above set of conditionals should
  # be taking care of this, it doesn't always succeed and this ensures that only the
  # desired outputs specified by the user remain in the Global Environment.

  # if (!("graph" %in% final_output)) {
  #   suppressWarnings(rm(list = c("graph", "graphs_list"), envir = .GlobalEnv))
  # }
  #
  # if (!("largest_bi_component" %in% final_output)) {
  #   suppressWarnings(rm(list = c("largest_bi_component", "bicomponent_list"), envir = .GlobalEnv))
  # }
  #
  # if (!("largest_component" %in% final_output)) {
  #   suppressWarnings(rm(list = c("largest_component", "lcomponent_list"), envir = .GlobalEnv))
  # }
  #
  # if (!("node_measure_plot" %in% final_output)) {
  #   suppressWarnings(rm(list = c("node_measure_plot", "nplot_list"), envir = .GlobalEnv))
  # }
  #
  # if (!("nodelist" %in% final_output)) {
  #   suppressWarnings(rm(list = c("node_measures", "node_measures_list"), envir = .GlobalEnv))
  # }
  #
  # if (!("edgelist" %in% final_output)) {
  #   suppressWarnings(rm(list = c("edgelist", "f_edges_list"), envir = .GlobalEnv))
  # }
  #
  # if (!("system_level_measures" %in% final_output)) {
  #   suppressWarnings(rm(list = c("system_level_measures", "s_measures_list"), envir = .GlobalEnv))
  # }
  #
  # if (!("system_measure_plot" %in% final_output)) {
  #   suppressWarnings(rm(list = c("system_measure_plot", "system_measure_plot_list"), envir = .GlobalEnv))
  # }


  return(netwrite_output)

}

#######################################################



# The below function is the core script for netwrite, entitled `basic_netwrite`.
# This function does everything that netwrite is supposed to do, but only for the entire
# network entered in as arguments. As such, this function is not fully equipped to handle
# multi-relational networks and perform calculations for subgraphs of specific
# relation types. To handle multi-relational networks, we create another function
# later on (`netwrite` proper), which applies `basic_netwrite` to all subgraphs.

# GET BASIC

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
                           missing_code=99999,
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

  # browser()
  # basic_start <- Sys.time()

  # Need to create a list for storing output
  basic_output <- list()


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
        adjacency_matrix <- adjacency_matrix
      }else{
        adjacency_matrix <- 1/adjacency_matrix
        adjacency_matrix[is.infinite(adjacency_matrix)] <- 0
      }

      # If `remove_loops == TRUE`, remove self-loops
      if (remove_loops == TRUE) {
        diag(adjacency_matrix) <- NA
      }


      # Generating directed graph
      g <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode=c('directed'), diag = TRUE,
                                               weighted = adj_weight)

      if (is.null(adj_weight)) {
        igraph::E(g)$weight <- 1
      }

      # Create Nodes file with Node-level measures
      edges <- as.data.frame(igraph::as_edgelist(g, names=FALSE))

      # nodes <- as.data.frame(sort(unique(c(edges$V1, edges$V2))))
      # colnames(nodes) <- c('id')
      ### The above two lines were dropping isolates from the nodelist.
      ### I think this is a better alternative
      nodes <- as.data.frame(1:length(igraph::V(g)))
      colnames(nodes) <- "id"
      nodes$id <- nodes$id - 1
      if (!is.null(nodes$label)) {
          nodes$label <- igraph::V(g)$name
      } else {
          nodes$label <- nodes$id
      }

      # To keep things consistent across code, we're going to reassign node names
      # to a `label` vertex attribute in `igraph` and replace the `name` attribute
      # with numeric IDs
      igraph::V(g)$name <- nodes$id
      igraph::V(g)$label <- nodes$label

      # Create alternate closeness function
      # Reachability function (eliminating loops)

      # processing <- Sys.time()

      # Adding Node-level measures
      if ("nodelist" %in% output | "node_measure_plot" %in% output) {

        nodes <- node_level_igraph(nodes = nodes, g = g, directed = directed,
                                   message = message, weights = weights,
                                   weight_type = weight_type)


        # If original `node_id` name is specified, rename column `attr` to match
        if (!is.null(node_id)) {
          # Get original column names
          nodelist_names <- colnames(nodes)
          nodelist_names[which(nodelist_names == "attr")] <- node_id
          colnames(nodes) <- nodelist_names
        }

      }

      # node_level_time <- Sys.time()

      # Extracting largest weakly-connected component
      # Extracting largest bicomponent
      # Calculating proportion of two-step paths that are also one-step paths (trans_rate)

      # Calculating system-level measures
      if ("system_level_measures" %in% output) {
        weak_component <- largest_weak_component_igraph(g)
        bicomponent <- largest_bicomponent_igraph(g)
        ### Merge in largest bicomponent memberships
        if ("nodelist" %in% output | "node_measure_plot" %in% output) {
          nodes <- dplyr::left_join(nodes, bicomponent$largest_bicomponent_memberships, by = "id")
        }
        degree_assortativity <- degree_assortativity(g, directed=as.logical(directed))
        reciprocity_rate <- igraph::reciprocity(g, ignore.loops = TRUE, mode='ratio')
        transitivity_rate <- trans_rate_igraph(g)
        transitivity_rate_b <- trans_rate_igraph(g, binarize = TRUE)
        global_clustering_coefficient <- gcc(g)
        if (weight_type == "frequency") {
          average_path_length <- igraph::mean_distance(g, directed=as.logical(directed),
                                                       weights = 1/igraph::E(g)$weight)
        } else {
          average_path_length <- igraph::mean_distance(g, directed=as.logical(directed))
        }
      }

      # system_level_time1 <- Sys.time()

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
        adjacency_matrix <- adjacency_matrix
      }else{
        adjacency_matrix <- 1/adjacency_matrix
        adjacency_matrix[is.infinite(adjacency_matrix)] <- 0
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

      # processing <- Sys.time()

      # Adding Node-Level Measures
      if ("nodelist" %in% output | "node_measure_plot" %in% output) {
        nodes <- node_level_igraph(nodes = nodes, g = g, directed = directed,
                                   message = message, weights = weights,
                                   weight_type = weight_type)

        # If original `node_id` name is specified, rename column `attr` to match
        if (!is.null(node_id)) {
          # Get original column names
          nodelist_names <- colnames(nodes)
          nodelist_names[which(nodelist_names == "attr")] <- node_id
          colnames(nodes) <- nodelist_names
        }

      }

      # node_level_time <- Sys.time()

      # Extracting the largest weakly connected component
      # Extracting the largest bi-component
      # Calculating the Proportion of Two-Step Path that Are Also One-Step Paths

      # Calculating System-Level Measures
      if ("system_level_measures" %in% output) {
        weak_component <- largest_weak_component_igraph(g)
        bicomponent <- largest_bicomponent_igraph(g)
        ### Merge in largest bicomponent memberships
        if ("nodelist" %in% output | "node_measure_plot" %in% output) {
          nodes <- dplyr::left_join(nodes, bicomponent$largest_bicomponent_memberships, by = "id")
        }
        degree_assortativity <- degree_assortativity(g, directed=as.logical(directed))
        reciprocity_rate <- igraph::reciprocity(g, ignore.loops = TRUE, mode='ratio')
        transitivity_rate <- trans_rate_igraph(g)
        transitivity_rate_b <- trans_rate_igraph(g, binarize = TRUE)
        global_clustering_coefficient <- gcc(g)

        if (weight_type == "frequency") {
          average_path_length <- igraph::mean_distance(g, directed=as.logical(directed),
                                                       weights = 1/igraph::E(g)$weight)
        } else {
          average_path_length <- igraph::mean_distance(g, directed=as.logical(directed))
        }
      }

      #.system_level_time1 <- Sys.time()




    }

    # Outputting Network Objects
    ### Note: Jim wants the outputted edgelist and igraph object to have the original weights.
    ### I've added some code that reverts the weights back to original if need be.
    if(weight_type == 'frequency') {
      adjacency_matrix <- adjacency_matrix
    }else{
      adjacency_matrix <- 1/adjacency_matrix
      adjacency_matrix[is.infinite(adjacency_matrix)] <- 0
    }

    if ("adjacency_matrix" %in% output) {
      basic_output$adjacency_matrix <- adjacency_matrix
    }




    if ("nodelist" %in% output) {
      basic_output$node_measures <- nodes
    }

    ### Note: Jim wants the outputted edgelist and igraph object to have the original weights.
    ### I've added some code that reverts the weights back to original if need be.
    if(weight_type == 'distance' & !is.null(adj_weight) == TRUE) {
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
      basic_output[[`net_name`]] <- g
    }

    # Create an edgelist and store in output list
    if ("edgelist" %in% output) {

      new_edgelist <- as.data.frame(igraph::as_edgelist(g))
      colnames(new_edgelist) <- c("i_id", "j_id")
      new_edgelist2 <- as.data.frame(igraph::as_edgelist(g, names = FALSE))
      colnames(new_edgelist2) <- c("i_elements", "j_elements")
      new_edgelist$Obs_ID <- 1:nrow(new_edgelist)

      if (is.null(igraph::E(g)$weight)) {
        new_edgelist$weight <- 1
      } else {
        new_edgelist$weight <- igraph::E(g)$weight
      }

      new_edgelist <- cbind(new_edgelist, new_edgelist2)

      new_edgelist <- new_edgelist[,c("Obs_ID", "i_elements", "i_id", "j_elements", "j_id", "weight")]

      basic_output$edgelist <- new_edgelist

    }


    # ADJACENCY LIST
  } else if (data_type == 'adjacency_list') {
    # Is the adjacency list a list
    if (methods::is(adjacency_list, 'list')) {
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

    # processing <- Sys.time()

    # Create an alternate closeness function
    # Reachablility function (Eliminate Loops, reaching yourself isn't that useful)
    # Adding Node-Level Measures
    if ("nodelist" %in% output | "node_measure_plot" %in% output) {
      nodes <- node_level_igraph(nodes = nodes, g = g, directed = directed,
                                 message = message, weights = weights,
                                 weight_type = weight_type)

      # If original `node_id` name is specified, rename column `attr` to match
      if (!is.null(node_id)) {
        # Get original column names
        nodelist_names <- colnames(nodes)
        nodelist_names[which(nodelist_names == "attr")] <- node_id
        colnames(nodes) <- nodelist_names
      }

    }

    # node_level_time <- Sys.time()

    # Extracting the largest weakly connected component
    # Extracting the largest bi-component
    # Calculating the Proportion of Two-Step Path that Are Also One-Step Paths
    # Calculating System-Level Measures
    if ("system_level_measures" %in% output) {
      weak_component <- largest_weak_component_igraph(g)
      bicomponent <- largest_bicomponent_igraph(g)
      ### Merge in largest bicomponent memberships
      if ("nodelist" %in% output | "node_measure_plot" %in% output) {
        nodes <- dplyr::left_join(nodes, bicomponent$largest_bicomponent_memberships, by = "id")
      }
      degree_assortativity <- degree_assortativity(g, directed=as.logical(directed))
      reciprocity_rate <- igraph::reciprocity(g, ignore.loops = TRUE, mode='ratio')
      transitivity_rate <- trans_rate_igraph(g)
      transitivity_rate_b <- trans_rate_igraph(g, binarize = TRUE)
      global_clustering_coefficient <- gcc(g)

      if (weight_type == "frequency") {
        average_path_length <- igraph::mean_distance(g, directed=as.logical(directed),
                                                     weights = 1/igraph::E(g)$weight)
      } else {
        average_path_length <- igraph::mean_distance(g, directed=as.logical(directed))
      }
    }

    # system_level_time1 <- Sys.time()

    # EDGELIST
  } else {

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
    if(weight_type != 'frequency') {
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

    # processing <- Sys.time()

    # Create an alternate closeness function
    # Reachablility function (Eliminate Loops, reaching yourself isn't that useful)
    # Adding Node-Level Measures
    if ("nodelist" %in% output | "node_measure_plot" %in% output) {
      nodes <- node_level_igraph(nodes = nodes, g = g, directed = directed,
                                 message = message, weights = weights,
                                 weight_type = weight_type)

      # If original `node_id` name is specified, rename column `attr` to match
      if (!is.null(node_id)) {
        # Get original column names
        nodelist_names <- colnames(nodes)
        nodelist_names[which(nodelist_names == "attr")] <- node_id
        colnames(nodes) <- nodelist_names
      }

    } else if (!("nodelist" %in% output | "node_measure_plot" %in% output) & "system_measure_plot" %in% output) {
      # We still need degree distribution for the system-level plot. Calculate
      # this if missing
      custom_degree <- total_degree(g, directed = directed)
      nodes$total_degree <- custom_degree$total_degree_all
    }

    # node_level_time <- Sys.time()

    # Extracting the largest weakly connected component
    # Extracting the largest bi-component
    # Calculating the Proportion of Two-Step Path that Are Also One-Step Paths
    # Calculating Multiplex Edge Correlation
    # Calculating System-Level Measures
    if ("system_level_measures" %in% output | "system_measure_plot" %in% output | "largest_bi_component" %in% output | "largest_component" %in% output) {
      weak_component <- largest_weak_component_igraph(g)
      bicomponent <- largest_bicomponent_igraph(g)
      ### Merge in largest bicomponent memberships
      if ("nodelist" %in% output | "node_measure_plot" %in% output) {
        nodes <- dplyr::left_join(nodes, bicomponent$largest_bicomponent_memberships, by = "id")
      }
      degree_assortativity <- degree_assortativity(g, directed=as.logical(directed))
      # deg_asst_time <- Sys.time()
      reciprocity_rate <- igraph::reciprocity(g, ignore.loops = TRUE, mode='ratio')
      # rec_rate_time <- Sys.time()
      transitivity_rate <- trans_rate_igraph(g)
      # trans_rate_time <- Sys.time()
      transitivity_rate_b <- trans_rate_igraph(g, binarize = TRUE)
      # trans_rate_b_time <- Sys.time()
      global_clustering_coefficient <- gcc(g)
      # gcc_time <- Sys.time()
      if (weight_type == "frequency") {
        average_path_length <- igraph::mean_distance(g, directed=as.logical(directed),
                                                     weights = 1/igraph::E(g)$weight)
      } else {
        average_path_length <- igraph::mean_distance(g, directed=as.logical(directed))
      }
      # avg_path_time <- Sys.time()

      multiplex <- multiplex_edge_corr_igraph(edgelist = edgelist, directed = as.logical(directed),
                                              weight_type = weight_type,
                                              type = type)
      #.multiplex_time <- Sys.time()


    }

    # system_level_time1 <- Sys.time()

    # Outputting Network Objects
    ### Note: Jim wants the outputted edgelist and igraph object to have the original weights.
    ### I've added some code that reverts the weights back to original if need be.
    if(weight_type != 'frequency') {
      edgelist[,6] <- as.numeric(1/edgelist[,6])
    }else{
      edgelist[,6] <- edgelist[,6]
    }
    if ("edgelist" %in% output) {
      basic_output$edgelist <- edgelist
    }
    if ("nodelist" %in% output) {
      # We need to force consistency across `ideanet's` various functions to make sure they
      # produce dataframes that can easily be merged into one another. For now, all `id` columns
      # will be designated as a numeric vector
      nodes$id <- as.numeric(nodes$id)
      basic_output$node_measures <- nodes
    }

    ### Note: Jim wants the outputted edgelist and igraph object to have the original weights.
    ### I've added some code that reverts the weights back to original if need be.
    if(weight_type != 'frequency') {
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
      basic_output[[`net_name`]] <- g
    }

  } # End edgelist condition


  ###########################################
  #    G E N E R A T I N G   R E P O R T    #
  ###########################################


  if ("system_level_measures" %in% output | "system_measure_plot" %in% output) {
    # Weak component summaries
    num_clusters <- igraph::clusters(g, mode="weak")[[3]]
    # num_clust_time <- Sys.time()
    largest_size <- max(igraph::clusters(g, mode="weak")[[2]])
    # largest_time <- Sys.time()
    proportion_largest <- max(igraph::clusters(g, mode="weak")[[2]])/nrow(nodes)
    # prop_largest_time <- Sys.time()

    # Strong component summaries
    strong_num_clusters <- igraph::clusters(g, mode="strong")[[3]]
    strong_largest_size <- max(igraph::clusters(g, mode="strong")[[2]])
    strong_proportion_largest <- max(igraph::clusters(g, mode="strong")[[2]])/nrow(nodes)
    # strong_time <- Sys.time()

    # Indicating if Adjacency matrix is singular, thus having eigen and
    # bonacich centralities calculated on undirected adjacency matrix
    if (directed == TRUE & "nodelist" %in% output & "bon_cent_in" %in% names(nodelist)) {
      singular <- "No"
    } else {
      singular <- "Yes"
    }
    singular_time <- Sys.time()


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
    # pairwise_time <- Sys.time()



    # Creating system-level data object
    multiplex <- ifelse((is.null(type) == TRUE), 'Singleplex Network', multiplex)
    multiplex <- multiplex[[1]]


    # Basic Graph Info
    graph_type <- ifelse(directed == TRUE, "Directed", "Undirected")
    weighted_graph <- ifelse((is.null(weights) == TRUE), "No", "Yes")[1]

    num_nodes <- length(igraph::V(g))
    num_ties <- length(igraph::E(g))

    num_types <- ifelse((is.null(type) == TRUE), NA, length(unique(type)))

    # basic_system_time <- Sys.time()

    mutual <- suppressWarnings(igraph::dyad_census(g)$mut)
    asym <- suppressWarnings(igraph::dyad_census(g)$asym)
    null_ties <- suppressWarnings(igraph::dyad_census(g)$null)

    # dyad_census_time <- Sys.time()

    t_census <- suppressWarnings(igraph::triad_census(g))

    # Triad census
    triad_003 =  t_census[[1]]
    triad_012 =  t_census[[2]]
    triad_102 =  t_census[[3]]
    triad_021D = t_census[[4]]
    triad_021U = t_census[[5]]
    triad_021C = t_census[[6]]
    triad_111D = t_census[[7]]
    triad_111U = t_census[[8]]
    triad_030T = t_census[[9]]
    triad_030C = t_census[[10]]
    triad_201 =  t_census[[11]]
    triad_120D = t_census[[12]]
    triad_120U = t_census[[13]]
    triad_120C = t_census[[14]]
    triad_210 =  t_census[[15]]
    triad_300 =  t_census[[16]]

    # triad_census_time <- Sys.time()

    avg_geodesic <- igraph::mean_distance(g, directed = directed)

    # avg_geo_time <- Sys.time()

    ### Jim wanted to add transitivity correlation score as an additional
    ### network-level measure. This is readymade in `sna`, so we'll use
    ### `intergraph` and `sna` to call the package
    ###### The requisite function in `sna` needs specification as to whether
    ###### the network in question is directed or undirected. We'll make an
    ###### object here with that specification
    sna_mode <- ifelse(directed == TRUE, "digraph", "graph")
    trans_cor <- sna::gtrans(intergraph::asNetwork(igraph::simplify(g, remove.multiple = TRUE)), mode = sna_mode, measure = "correlation")

    # trans_cor_time <- Sys.time()

    if (directed == TRUE){
      density_directed <- igraph::edge_density(g)
      density_undirected <- igraph::edge_density(igraph::as.undirected(g))
    } else {
      density_directed <- NA
      density_undirected <- igraph::edge_density(g)
    }

    # density_time <- Sys.time()

    num_isolates <- sum(nodes$total_degree == 0)

    num_self_loops <- sum(igraph::is.loop(g, eids = igraph::E(g)))

    # iso_time <- Sys.time()


    # Centralization scores
    #### First, need to remove isolates from graph for select measures
    g_no_iso <- igraph::delete.vertices(g, v = (igraph::degree(g, mode = "all") == 0))

    if (directed == TRUE) {

      ### Betweenness
      # cent_bet_undir <- igraph::centralization.betweenness(g_no_iso,
      #                                                      directed = FALSE,
      #                                                      normalized = TRUE)$centralization
      # cent_bet_dir <- igraph::centralization.betweenness(g_no_iso,
      #                                                      directed = TRUE,
      #                                                      normalized = TRUE)$centralization

      bet_centr_u <- betweenness_centralization(g = g_no_iso,
                                                weights = igraph::E(g_no_iso)$weight,
                                                directed = FALSE,
                                                weight_type = weight_type)


      cent_bet_undir <- bet_centr_u$betweenness_centralization
      cent_bet_undir_bin <- bet_centr_u$binarized_betweenness_centralization

      bet_centr_d <- betweenness_centralization(g = g_no_iso,
                                                weights = igraph::E(g_no_iso)$weight,
                                                directed = TRUE,
                                                weight_type = weight_type)




      cent_bet_dir <- bet_centr_d$betweenness_centralization
      cent_bet_dir_bin <- bet_centr_d$binarized_betweenness_centralization

      ##### Standard deviations for betweenness
      sd_bet <- stats::sd(nodes$betweenness, na.rm = TRUE)
      sd_bet_bin <- stats::sd(nodes$binarized_betweenness, na.rm = TRUE)
      ##### Herfindahl Index for betweenness
      herf_bet <- herfindahl(nodes$betweenness)
      herf_bet_bin <- herfindahl(nodes$binarized_betweenness)


      ### Degree
      # cent_deg_undir <- igraph::centralization.degree(g, mode = "all",
      #                                                 loops = FALSE,
      #                                                 normalized = TRUE)$centralization
      # cent_deg_out <- igraph::centralization.degree(g, mode = "out",
      #                                                 loops = FALSE,
      #                                                 normalized = TRUE)$centralization
      # cent_deg_in <- igraph::centralization.degree(g, mode = "in",
      #                                                 loops = FALSE,
      #                                                 normalized = TRUE)$centralization

      deg_centr <-  degree_centralization(g, directed = TRUE)

      cent_deg_undir <- deg_centr$centralization_un
      cent_deg_out <- deg_centr$centralization_out
      cent_deg_in <- deg_centr$centralization_in

      ##### Standard deviations for degree
      sd_indegree <- stats::sd(nodes$in_degree, na.rm = TRUE)
      sd_outdegree <- stats::sd(nodes$out_degree, na.rm = TRUE)
      sd_total_degree <- stats::sd(nodes$total_degree, na.rm = TRUE)
      ##### Herfindahl Index for Degree
      herf_indegree <- herfindahl(nodes$in_degree)
      herf_outdegree <- herfindahl(nodes$out_degree)
      herf_total_degree <- herfindahl(nodes$total_degree)


      ### Closeness
      # cent_close_undir <- igraph::centralization.closeness(g_no_iso,
      #                                                    mode = "all",
      #                                                    normalized = TRUE)$centralization
      # cent_close_out <- igraph::centralization.closeness(g_no_iso,
      #                                                  mode = "out",
      #                                                  normalized = TRUE)$centralization
      # cent_close_in <- igraph::centralization.closeness(g_no_iso,
      #                                                 mode = "in",
      #                                                 normalized = TRUE)$centralization

      close_centr <- closeness_centralization(g, directed = TRUE, weight_type = weight_type)


      cent_close_undir <- close_centr$centralization_un
      cent_close_out <- close_centr$centralization_out
      cent_close_in <- close_centr$centralization_in

      ##### Standard deviations for closeness
      sd_closeness_in <- stats::sd(nodes$closeness_in, na.rm = TRUE)
      sd_closeness_out <- stats::sd(nodes$closeness_out, na.rm = TRUE)
      sd_closeness_un <- stats::sd(nodes$closeness_undirected, na.rm = TRUE)
      #####. Herfindahl Index for Closeness
      herf_close_in <- herfindahl(nodes$closeness_in)
      herf_close_out <- herfindahl(nodes$closeness_out)
      herf_close_un <- herfindahl(nodes$closeness_undirected)

      ### Eigen
      # cent_eigen_undir <- igraph::centralization.evcent(g_no_iso,
      #                                                   directed = FALSE,
      #                                                   normalized = TRUE)$centralization
      # cent_eigen_dir <- igraph::centralization.evcent(g_no_iso,
      #                                                 directed = TRUE,
      #                                                 normalized = TRUE)$centralization
      eigen_centr <- eigen_centralization(g, directed = TRUE)

      cent_eigen_undir <- eigen_centr$undirected
      cent_eigen_dir <- eigen_centr$directed

      ##### Standard deviation for eigen
      sd_eigen <- stats::sd(nodes$eigen_centrality, na.rm = TRUE)
      ##### Herfindahl Index for Eigen
      herf_eigen <- herfindahl(nodes$eigen_centrality)

    } else {

      ### Betweenness
      # cent_bet_undir <- igraph::centralization.betweenness(g_no_iso,
      #                                                      directed = FALSE,
      #                                                      normalized = TRUE)$centralization
      bet_centr_u <- betweenness_centralization(g = g_no_iso,
                                                weights = igraph::E(g_no_iso)$weight,
                                                directed = FALSE,
                                                weight_type = weight_type)

      cent_bet_undir <- bet_centr_u$betweenness_centralization
      cent_bet_undir_bin <- bet_centr_u$binarized_betweenness_centralization

      # cent_bet_dir <- NA

      cent_bet_dir <- NA
      cent_bet_dir_bin <- NA

      ##### Standard deviations for betweenness
      sd_bet <- stats::sd(nodes$betweenness, na.rm = TRUE)
      sd_bet_bin <- stats::sd(nodes$binarized_betweenness, na.rm = TRUE)
      ##### Herfindahl Index for betweenness
      herf_bet <- herfindahl(nodes$betweenness)
      herf_bet_bin <- herfindahl(nodes$binarized_betweenness)


      ### Degree

      # cent_deg_undir <- igraph::centralization.degree(g, mode = "all",
      #                                                 loops = FALSE,
      #                                                 normalized = TRUE)$centralization

      cent_deg_undir <- degree_centralization(g, directed = FALSE)

      cent_deg_out <- NA
      cent_deg_in <- NA

      ##### Standard deviations for degree
      sd_indegree <- NA
      sd_outdegree <- NA
      sd_total_degree <- stats::sd(nodes$total_degree, na.rm = TRUE)
      ##### Herfindahl Index for Degree
      herf_indegree <- NA
      herf_outdegree <- NA
      herf_total_degree <- herfindahl(nodes$total_degree)

      ### Closeness
      # cent_close_undir <- igraph::centralization.closeness(g_no_iso,
      #                                                    mode = "all",
      #                                                    normalized = TRUE)$centralization
      cent_close_undir <- closeness_centralization(g_no_iso,
                                                   directed = FALSE, weight_type = weight_type)
      cent_close_out <- NA
      cent_close_in <- NA

      ##### Standard Deviations for Closeness
      sd_closeness_in <- NA
      sd_closeness_out <- NA
      sd_closeness_un <- stats::sd(nodes$closeness, na.rm = TRUE)
      #####. Herfindahl Index for Closeness
      herf_close_in <- NA
      herf_close_out <- NA
      herf_close_un <- herfindahl(nodes$closeness)

      ### Eigen
      # cent_eigen_undir <- igraph::centralization.evcent(g_no_iso,
      #                                                   directed = FALSE,
      #                                                   normalized = TRUE)$centralization
      eigen_centr <- eigen_centralization(g, directed = FALSE)
      cent_eigen_undir <- eigen_centr$undirected

      cent_eigen_dir <- NA

      ##### Standard deviation for eigen
      sd_eigen <- stats::sd(nodes$eigen_centrality, na.rm = TRUE)
      ##### Herfindahl Index for Eigen
      herf_eigen <- herfindahl(nodes$eigen_centrality)

    }

    # centralization_time <- Sys.time()


    # K-core cohesion (ask Jim for best name for this measure)
    k_core_cohesion <- k_cohesion(graph = g)

    # kcore_time <- Sys.time()


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

                        "Number of 003 Triads",
                        "Number of 012 Triads",
                        "Number of 102 Triads",
                        "Number of 021D Triads",
                        "Number of 021U Triads",
                        "Number of 021C Triads",
                        "Number of 111D Triads",
                        "Number of 111U Triads",
                        "Number of 030T Triads",
                        "Number of 030C Triads",
                        "Number of 201 Triads",
                        "Number of 120D Triads",
                        "Number of 120U Triads",
                        "Number of 120C Triads",
                        "Number of 210 Triads",
                        "Number of 300 Triads",

                        'Degree Assortativity (Total)', 'Degree Assortativity (Indegree)', 'Degree Assortativity (Outdegree)',
                        'Reciprocity Rate', 'Transitivity Rate', 'Transitivity Rate (Binarized)',

                        'Transitivity Correlation',



                        'Global Clustering Coefficient', 'Average Geodesic',
                        'Multi-Level Edge Correlation',

                        'Pairwise Reachability (Weak, Undirected)', 'Pairwise Reachability (Strong, Undirected)',
                        'Pairwise Reachability (Weak, Directed)', 'Pairwise Reachability (Strong, Directed)',

                        "Betweenness Centralization (Undirected)", "Betweenness Centralization (Undirected, Binarized)",
                        "Betweenness Centralization (Directed)", "Betweenness Centralization (Directed, Binarized)",
                        "Standard Deviation, Betweenness", "Standard Deviation, Binarized Betweeness",
                        "Herfindahl Index, Betweenness", "Herfindahl Index, Binarized Betweeness",
                        "Degree Centralization (Undirected)", "Degree Centralization (In)", "Degree Centralization (Out)",
                        "Standard Deviation, Total Degree", "Standard Deviation, Indegree", "Standard Deviation, Outdegree",
                        "Herfindahl Index, Total Degree", "Herfindahl Index, Indegree", "Herfindahl Index, Outdegree",
                        "Closeness Centralization (Undirected)", "Closeness Centralization (In)", "Closeness Centralization (Out)",
                        "Standard Deviation, Closeness (Undirected)", "Standard Deviation, Closeness (Indegree)", "Standard Deviation, Closeness (Outdegree)",
                        "Herfindahl Index, Closeness (Undirected)", "Herfindahl Index, Closeness (Indegree)", "Herfindahl Index, Closeness (Outdegree)",
                        "Eigenvector Centralization (Undirected)", "Eigenvector Centralization (Directed)",
                        "Standard Deviation, Eigenvector Centrality",
                        "Herfindahl Index, Eigenvector Centrality",

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

                              "The number of 003 triads in the graph",
                              "The number of 012 triads in the graph",
                              "The number of 102 triads in the graph",
                              "The number of 021D triads in the graph",
                              "The number of 021U triads in the graph",
                              "The number of 021C triads in the graph",
                              "The number of 111D triads in the graph",
                              "The number of 111U triads in the graph",
                              "The number of 030T triads in the graph",
                              "The number of 030C triads in the graph",
                              "The number of 201 triads in the graph",
                              "The number of 120D triads in the graph",
                              "The number of 120U triads in the graph",
                              "The number of 120C triads in the graph",
                              "The number of 210 triads in the graph",
                              "The number of 300 triads in the graph",

                              'Edgewise correlation of total degree',
                              'Edgewise correlation of total indegree',
                              'Edgewise correlation of total outdegree',

                              'The proportion of directed ties that are reciprocated',
                              'The proportion of two-step paths that are also one-step paths',
                              'The proportion of two-step paths that are also one-step paths (two-paths between nodes are only counted once)',

                              "The observed correlation between a tie and the number of two-step paths connecting the two nodes in a tie",



                              'The proportion of closed triangles to all triangles', 'The average shortest path length',
                              'Multiplex networks edgewise correlation of relations',

                              'The proportion of nodes that share a weak component (undirected)',
                              'The proportion of nodes that share a strong component (undirected)',
                              'The proportion of nodes that share a weak component (directed)',
                              'The proportion of nodes that share a strong component (directed)',

                              # Betweeness
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by betweenness centrality scores (Undirected shortest paths used when calculating betweenness)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by betweenness centrality scores (Undirected shortest paths used when calculating betweenness, edge weights are binarized)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by betweenness centrality scores (Directed shortest paths used when calculating betweenness)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by betweenness centrality scores (Directed shortest paths used when calculating betweenness, edge weights are binarized)",
                              ##### SD Betweenness
                              "Standard deviation of betweenness centrality scores",
                              "Standard deviation of binarized betweenness centrality scores",
                              ##### Herf Betweenness
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by betweenness centrality scores",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by betweenness centrality scores (edge weights are binarized)",

                              # Degree
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by degree centrality scores (Undirected edges used when calculating degree)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by degree centrality scores (Incoming edges used when calculating degree)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by degree centrality scores (Outgoing edges used when calculating degree)",
                              ##### SD Degree
                              "Standard deviation of total degree centrality scores",
                              "Standard deviation of indegree centrality scores",
                              "Standard deviation of outdegree centrality scores",
                              ##### Herf Degree
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by degree centrality scores (Undirected edges used when calculating degree)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by degree centrality scores (Incoming edges used when calculating degree)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by degree centrality scores (Outgoing edges used when calculating degree)",

                              # Closeness
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by closeness centrality scores (Undirected edges used when calculating closeness)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by closeness centrality scores (Incoming edges used when calculating closeness)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by closeness centrality scores (Outgoing edges used when calculating closeness)",
                              ##### SD Closeness
                              "Standard deviation of closeness centrality scores (undirected)",
                              "Standard deviation of closeness centrality scores (incoming edges used when calculating closeness)",
                              "Standard deviation of closeness centrality scores (outgoing edges used when calculating closeness)",
                              ##### Herf Closeness
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by closeness centrality scores (Undirected edges used when calculating closeness)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by closeness centrality scores (Incoming edges used when calculating closeness)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by closeness centrality scores (Outgoing edges used when calculating closeness)",

                              # Eigenvector centrality
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by eigenvector centrality scores (Undirected edges used when calculating eigenvector centrality)",
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by eigenvector centrality scores (Directed edges paths used when calculating eigenvector centrality)",
                              ##### SD Eigenvector centrality
                              "Standard deviation of eigenvector centrality scores",
                              ##### Herf Eigen
                              "The extent to which ties in the network are concentrated on a single actor or group of actors, as determined by eigenvector centrality scores",

                              # K-core Cohesion
                              "The average across all pairs of the maximum k-core to which each pair is a joint member (Measures the average level of shared contacts)"
    )

    measures <- c(graph_type, weighted_graph, as.character(num_nodes), as.character(num_ties), as.character(num_types),

                  as.character(num_isolates), as.character(num_self_loops),

                  as.character(density_undirected), as.character(density_directed),

                  as.character(num_clusters), as.character(largest_size), as.character(proportion_largest),
                  as.character(strong_num_clusters), as.character(strong_largest_size), as.character(strong_proportion_largest),
                  as.character(bicomponent$bicomponent_summary$num_bicomponents), as.character(bicomponent$bicomponent_summary$size_bicomponent), as.character(bicomponent$bicomponent_summary$prop_bicomponent),

                  as.character(mutual), as.character(asym), as.character(null_ties),

                  as.character(triad_003),
                  as.character(triad_012),
                  as.character(triad_102),
                  as.character(triad_021D),
                  as.character(triad_021U),
                  as.character(triad_021C),
                  as.character(triad_111D),
                  as.character(triad_111U),
                  as.character(triad_030T),
                  as.character(triad_030C),
                  as.character(triad_201),
                  as.character(triad_120D),
                  as.character(triad_120U),
                  as.character(triad_120C),
                  as.character(triad_210),
                  as.character(triad_300),


                  as.character(degree_assortativity$total),
                  as.character(degree_assortativity$indegree),
                  as.character(degree_assortativity$outdegree),


                  as.character(reciprocity_rate),
                  as.character(transitivity_rate),
                  as.character(transitivity_rate_b),
                  as.character(trans_cor),

                  as.character(global_clustering_coefficient), average_path_length,
                  as.character(multiplex),
                  as.character(pairwise_weak_un), as.character(pairwise_strong_un),
                  as.character(pairwise_weak_dir), as.character(pairwise_strong_dir),

                  as.character(cent_bet_undir), as.character(cent_bet_undir_bin),
                  as.character(cent_bet_dir), as.character(cent_bet_dir_bin),
                  as.character(sd_bet), as.character(sd_bet_bin),
                  as.character(herf_bet), as.character(herf_bet_bin),
                  as.character(cent_deg_undir), as.character(cent_deg_in), as.character(cent_deg_out),
                  as.character(sd_total_degree), as.character(sd_indegree), as.character(sd_outdegree),
                  as.character(herf_total_degree), as.character(herf_indegree), as.character(herf_outdegree),
                  as.character(cent_close_undir), as.character(cent_close_in), as.character(cent_close_out),
                  as.character(sd_closeness_un), as.character(sd_closeness_in), as.character(sd_closeness_out),
                  as.character(herf_close_un), as.character(herf_close_in), as.character(herf_close_out),
                  as.character(cent_eigen_undir), as.character(cent_eigen_dir),
                  as.character(sd_eigen),
                  as.character(herf_eigen),
                  as.character(k_core_cohesion))


    system_level_measures <- cbind(as.data.frame(measure_labels), measure_descriptions, measures)

    # If network is undirected, we don't need a lot of the triad counts. Let's filter those out.
    if (directed == FALSE) {
      system_level_measures <- system_level_measures[!(system_level_measures$measure_labels %in% c("Number of 012 Triads",
                                                                                                   "Number of 021D Triads",
                                                                                                   "Number of 021U Triads",
                                                                                                   "Number of 021C Triads",
                                                                                                   "Number of 111D Triads",
                                                                                                   "Number of 111U Triads",
                                                                                                   "Number of 030T Triads",
                                                                                                   "Number of 030C Triads",
                                                                                                   "Number of 120D Triads",
                                                                                                   "Number of 120U Triads",
                                                                                                   "Number of 120C Triads",
                                                                                                   "Number of 210 Triads")), ]
    }

    # make_df_time <- Sys.time()



    # sys_level_time_vec <- c(deg_asst_time-node_level_time,
    #                         rec_rate_time-deg_asst_time,
    #                         trans_rate_time-rec_rate_time,
    #                         trans_rate_b_time-trans_rate_time,
    #                         gcc_time-trans_rate_b_time,
    #                         avg_path_time-gcc_time,
    #                         system_level_time1-avg_path_time,
    #
    #                         num_clust_time-system_level_time1,
    #                         largest_time-num_clust_time,
    #                         prop_largest_time-largest_time,
    #                         strong_time-prop_largest_time,
    #                         singular_time-strong_time,
    #                         pairwise_time-singular_time,
    #                         basic_system_time-pairwise_time,
    #                         dyad_census_time-basic_system_time,
    #                         triad_census_time-dyad_census_time,
    #                         avg_geo_time-triad_census_time,
    #                         density_time-avg_geo_time,
    #                         iso_time-density_time,
    #                         centralization_time-iso_time,
    #                         kcore_time-centralization_time,
    #                         make_df_time-kcore_time)
    #
    # basic_output$sys_level_time_df <- data.frame(stage = c(
    #   "deg_asst_time",
    #   "rec_rate_time",
    #   "trans_rate_time",
    #   "trans_rate_b_time",
    #   "gcc_time",
    #   "avg_path_time",
    #   "system_level_time1",
    #
    #   "num_clust_time",
    #   "largest_time",
    #   "prop_largest_time",
    #   "strong_time",
    #   "singular_time",
    #   "pairwise_time",
    #   "basic_system_time",
    #   "dyad_census_time",
    #   "triad_census_time",
    #   "avg_geo_time",
    #   "density_time",
    #   "iso_time",
    #   "centralization_time",
    #   "kcore_time",
    #   "make_df_time"),
    #   bigger_stage = c(rep(1, 7), rep(2, 15)),
    #   duration = sys_level_time_vec)



    # If nodelist is in output, create indicator of singular Adjmat
    if ("nodelist" %in% output & directed == TRUE) {
      singular_df <- data.frame(measure_labels = "Singular Matrix",
                                measure_descriptions = "Whether corresponding adjacency matrix is singular. If 'Yes', network is treated as undirected when calculating Eigenvector and Bonacich centrality measures.",
                                measures = singular)

      system_level_measures <- rbind(system_level_measures, singular_df)
    }

    # If nodelist isn't in output, remove all standard deviation, centralization,
    # and Herfindahl scores and display warning
    if (!("nodelist" %in% output | "node_measure_plot" %in% output)) {

      keep_rows <- !(stringr::str_detect(system_level_measures$measure_labels, "^Herfindahl") +
                     stringr::str_detect(system_level_measures$measure_labels, "^Standard Deviation") +
                     stringr::str_detect(system_level_measures$measure_labels, "Centralization"))

      ### Degree centralization is still calculated, so might as well keep
      keep_rows[stringr::str_detect(system_level_measures$measure_labels, "^Degree Centralization")] <- TRUE



      system_level_measures <- system_level_measures[keep_rows, ]

    }


    # Removing node-level and system-level data objects for clarity
    suppressWarnings(rm(measure_labels, measure_descriptions, num_clusters, proportion_largest, degree_assortativity,
                        reciprocity_rate, global_clustering_coefficient, average_path_length,
                        measures, singular, singular_df))

    # suppressWarnings(rm(bicomponent_summary, largest_bicomponent_memberships, envir = .GlobalEnv))

  }

  # system_level_time2 <- Sys.time()

  # System & Node-Level Visualizations

  if ("system_measure_plot" %in% output) {

    # browser()

    # Density Plot
    density_plot <- function(){
      # Defining degree distribution coordinates
      y_axis <- stats::density(nodes$total_degree)$y
      x_axis <- stats::density(nodes$total_degree)$x
      coordinates <- cbind(as.data.frame(x_axis), y_axis)
      coordinates <- coordinates[(coordinates$x_axis >= 0), ]
      x_axis <- pretty(coordinates$x_axis)
      y_axis <- pretty(coordinates$y_axis)
      x_spacer <- x_axis[c(length(x_axis))] - x_axis[c(length(x_axis)-1)]
      x_spacer <- x_spacer*0.5
      y_spacer <- y_axis[c(length(y_axis))] - y_axis[c(length(y_axis)-1)]
      y_spacer <- y_spacer*0.5


      # Defining Base Degree Plot
      # graphics::par(mar = c(5,6,2,2),  family='HersheySerif')
      plot(0, type='n', xlab=' ', ylab=' ', xlim=c(min(x_axis), max(x_axis)),
           ylim=c(min(y_axis), max(y_axis)), cex.axis=1.3, family='HersheySerif',
           las=1, main=' ', bty='n')
      graphics::grid(lwd = 2)

      # Adding Margin Text
      graphics::mtext(side = 1, text = 'Total Degree', col = "black", line = 3, cex = 1.5, family='HersheySerif')
      graphics::mtext(side = 2, text = 'Density', col = "black", line = 4.5, cex = 1.5, family='HersheySerif')

      # Plotting Degree
      graphics::lines(coordinates$x_axis, coordinates$y_axis, col='brown', lwd=1.5)

      # Adding Skew and Kurtosis
      skewness <- moments::skewness(nodes$total_degree)
      kurtosis <- moments::kurtosis(nodes$total_degree)
      graphics::text(x = (max(x_axis)-x_spacer), y = (max(y_axis)-y_spacer), paste('Skewness',round(skewness, digits=2)), cex=1.3)
      graphics::text(x = (max(x_axis)-x_spacer), y = (max(y_axis)-(y_spacer*2)), paste('Kurtosis',round(kurtosis, digits=2)), cex=1.3)

      # Adding Title
      graphics::title(c("Total Degree Distribution"), family='serif', cex.main=2)
    }
    density_grob <- cowplot::as_grob(density_plot)

    #######################################


    # Populating Subplots
    system_plot_names <- c("Number of Weak Components",
                           "Proportion in the Largest Weak Component",
                           "Degree Assortativity (Total)",
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

    if (directed == TRUE) {
      system_plot_names[[9]] <- "Density (Directed)"
      system_plot_labels[[9]] <- "Density (Directed)"
    } else {
      system_plot_names[[9]] <- "Density (Undirected)"
      system_plot_labels[[9]] <- "Density (Undirected)"
    }


    plot_texts <- list()
    plot_texts$num_weak <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[1]],
                                                          system_level_measures[system_level_measures$measure_labels == system_plot_names[[1]], 3],
                                                          sep = "\n"))
    plot_texts$prop_weak <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[2]],
                                                          substr(system_level_measures[system_level_measures$measure_labels == system_plot_names[[2]], 3], 1, 4),
                                                          sep = "\n"))
    plot_texts$deg_assort <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[3]],
                                                          substr(system_level_measures[system_level_measures$measure_labels == system_plot_names[[3]], 3], 1, 4),
                                                          sep = "\n"))
    plot_texts$recip <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[4]],
                                                          substr(system_level_measures[system_level_measures$measure_labels == system_plot_names[[4]], 3], 1, 4),
                                                          sep = "\n"))
    plot_texts$trans_rate <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[5]],
                                                          substr(system_level_measures[system_level_measures$measure_labels == system_plot_names[[5]], 3], 1, 4),
                                                          sep = "\n"))
    plot_texts$gcc <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[6]],
                                                          substr(system_level_measures[system_level_measures$measure_labels == system_plot_names[[6]], 3], 1, 4),
                                                          sep = "\n"))
    plot_texts$avg_geo <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[7]],
                                                          substr(system_level_measures[system_level_measures$measure_labels == system_plot_names[[7]], 3], 1, 4),
                                                          sep = "\n"))

    if (system_level_measures[system_level_measures$measure_labels == system_plot_names[[8]], 3] == "Singleplex Network") {
      plot_texts$multi_corr <- ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[8]],
                                                            system_level_measures[system_level_measures$measure_labels == system_plot_names[[8]], 3],
                                                            sep = "\n"))
    } else {
      plot_texts$multi_corr <- ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[8]],
                                                            substr(system_level_measures[system_level_measures$measure_labels == system_plot_names[[8]], 3], 1, 4),
                                                            sep = "\n"))
    }

    plot_texts$density <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::geom_text(ggplot2::aes(0,0), label = paste(system_plot_labels[[9]],
                                                          substr(system_level_measures[system_level_measures$measure_labels == system_plot_names[[9]], 3], 1, 6),
                                                          sep = "\n"))


    mid_right <- cowplot::plot_grid(plot_texts$deg_assort,
                                    plot_texts$recip,
                                    plot_texts$trans_rate,
                                    ### NEED TO DECIDE WHAT TO DO ABOUT MULTIPLEX
                                    ### CORRELATION WHEN 3+ TYPES
                                    # plot_texts$multi_corr,

                                    ncol = 1)

    center <- cowplot::plot_grid(density_grob, mid_right, ncol = 2,
                                 rel_widths = c(2.5, 1))

    top_row <- cowplot::plot_grid(plot_texts$num_weak,
                                  plot_texts$prop_weak,
                                  plot_texts$density,
                                  ncol = 3)

    bottom_row <- cowplot::plot_grid(plot_texts$gcc,
                                     plot_texts$avg_geo,
                                     ncol = 2)

    p_1 <- cowplot::plot_grid(top_row, center,
                              bottom_row,
                              nrow = 3, ncol = 1,
                              rel_heights = c(1, 5, 1)) +
      ggplot2::labs(title = "System-Level Measures") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                        size = 28,
                                                        face = "bold"))

    if (shiny == FALSE) {
      print(p_1)
    }


  }

  # system_plot <- Sys.time()


  if ("node_measure_plot" %in% output) {

    # browser()

    # Make list to store ggplots
    node_gg_list <- list()

    #####################

    # Weighted Degree
    columns <- colnames(nodes)
    columns <- columns[columns != "id" & columns != "attr" ]
    weighted_degree_measures <- columns[grepl("^weighted", columns)]
    weighted_degree_measures <- cbind(rep("weighted_degree", length(weighted_degree_measures)), weighted_degree_measures)
    weighted_degree_measures <- cbind(weighted_degree_measures, stringr::str_to_title(stringr::str_replace_all(weighted_degree_measures[,2], "^weighted_", "")))
    colnames(weighted_degree_measures) <- c('type', 'measure', 'label')

    wdeg_setup <- node_gg_setup(index_df = weighted_degree_measures, nodes = nodes)

    if (nrow(weighted_degree_measures) == 1) {
      node_gg_list[[1]] <- wdeg_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = wdeg_setup$coords$x_axis,
                                     y = wdeg_setup$coords$y_axis)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = wdeg_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = wdeg_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Weighted Degree\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    } else {
      node_gg_list[[1]] <- wdeg_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = wdeg_setup$coords$x_axis,
                                     y = wdeg_setup$coords$y_axis,
                                     color = wdeg_setup$coords$measure)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = wdeg_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = wdeg_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Weighted Degree\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    }


    # Degree
    degree_measures <- columns[grepl("_degree", columns)]
    degree_measures <- degree_measures[!grepl("weighted", degree_measures)]
    degree_measures <- cbind(rep("degree", length(degree_measures)), degree_measures)
    degree_measures <- cbind(degree_measures, stringr::str_to_title(stringr::str_replace_all(degree_measures[,2], "_degree$", "")))
    colnames(degree_measures) <- c('type', 'measure', 'label')

    deg_setup <- node_gg_setup(index_df = degree_measures, nodes = nodes)

    if (nrow(degree_measures) == 1) {
      node_gg_list[[2]] <- deg_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = deg_setup$coords$x_axis,
                                     y = deg_setup$coords$y_axis)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = deg_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = deg_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Degree\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    } else {
      node_gg_list[[2]] <- deg_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = deg_setup$coords$x_axis,
                                     y = deg_setup$coords$y_axis,
                                     color = deg_setup$coords$measure)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = deg_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = deg_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Degree\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    }

    # Closeness
    closeness_measures <- columns[grepl("closeness", columns)]
    closeness_measures <- cbind(rep("closeness", length(closeness_measures)), closeness_measures)
    closeness_measures <- cbind(closeness_measures, stringr::str_to_title(stringr::str_replace_all(closeness_measures[,2], "^closeness_", "")))
    colnames(closeness_measures) <- c('type', 'measure', 'label')

    closeness_setup <- node_gg_setup(index_df = closeness_measures, nodes = nodes)

    if (nrow(closeness_measures) == 1) {
      node_gg_list[[3]] <- closeness_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = closeness_setup$coords$x_axis,
                                     y = closeness_setup$coords$y_axis)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = closeness_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = closeness_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Closeness\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    } else {
      node_gg_list[[3]] <- closeness_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = closeness_setup$coords$x_axis,
                                     y = closeness_setup$coords$y_axis,
                                     color = closeness_setup$coords$measure)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = closeness_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = closeness_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Closeness\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    }



    # Betweenness
    betweenness_measures <- columns[grepl("betweenness", columns)]
    betweenness_measures <- cbind(rep("betweenness", length(betweenness_measures)), betweenness_measures)
    if (nrow(betweenness_measures) == 1) {
      betweenness_measures <- cbind(betweenness_measures, NA)
    } else {
      betweenness_measures <- cbind(betweenness_measures,
                                    stringr::str_to_title(stringr::str_replace_all(betweenness_measures[,2], "_", " ")))
    }
    colnames(betweenness_measures) <- c('type', 'measure', 'label')

    bet_setup <- node_gg_setup(index_df = betweenness_measures, nodes = nodes)

    if (nrow(betweenness_measures) == 1) {
      node_gg_list[[4]] <- bet_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = bet_setup$coords$x_axis,
                                     y = bet_setup$coords$y_axis)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = bet_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = bet_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Betweenness\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    } else {
      node_gg_list[[4]] <- bet_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = bet_setup$coords$x_axis,
                                     y = bet_setup$coords$y_axis,
                                     color = bet_setup$coords$measure)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = bet_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = bet_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Betweenness\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    }


    # Bonacich
    bon_measures <- columns[grepl("bon", columns)]
    bon_measures <- cbind(rep("bonacich", length(bon_measures)), bon_measures)
    bon_measures <- cbind(bon_measures, stringr::str_to_title(stringr::str_replace_all(bon_measures[,2], "_", " ")))
    bon_measures[,3] <- stringr::str_replace_all(bon_measures[,3], "^Bonpow ", "")
    bon_measures[,3] <- stringr::str_replace_all(bon_measures[,3], "^Bonpow", "Beta 0.75")
    bon_measures[,3] <- stringr::str_replace_all(bon_measures[,3], "^Negative", "Beta -0.75")
    colnames(bon_measures) <- c('type', 'measure', 'label')

    bon_setup <- node_gg_setup(index_df = bon_measures, nodes = nodes)

    node_gg_list[[5]] <- bon_setup$coords %>%
      ggplot2::ggplot(ggplot2::aes(x = bon_setup$coords$x_axis,
                                   y = bon_setup$coords$y_axis,
                                   color = bon_setup$coords$measure)) +
      ggplot2::geom_line() +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::scale_x_continuous(breaks = bon_setup$x_breaks) +
      ggplot2::scale_y_continuous(breaks = bon_setup$y_breaks) +
      ggplot2::labs(y = "\nDensity\n",
                    x = NULL,
                    color = NULL,
                    caption = "Bonacich\n") +
      ggplot2::theme(legend.position = c(.8, .8),
                     legend.background = ggplot2::element_rect(colour="white", fill="white"),
                     plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))

    # Eigenvector
    eigen_measures <- columns[grepl("eigen", columns)]
    eigen_measures <- cbind(rep("eigen", length(eigen_measures)), eigen_measures)
    if (nrow(eigen_measures) == 1) {
      eigen_measures <- cbind(eigen_measures, NA)
    } else {
      eigen_measures <- cbind(eigen_measures,
                              stringr::str_to_title(stringr::str_replace_all(eigen_measures[,2], "^eigen_", "")))
    }
    colnames(eigen_measures) <- c('type', 'measure', 'label')

    eigen_setup <- node_gg_setup(index_df = eigen_measures, nodes = nodes)

    if (nrow(eigen_measures) == 1) {
      node_gg_list[[6]] <- eigen_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = eigen_setup$coords$x_axis,
                                     y = eigen_setup$coords$y_axis)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = eigen_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = eigen_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Eigenvector\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    } else {
      node_gg_list[[6]] <- eigen_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = eigen_setup$coords$x_axis,
                                     y = eigen_setup$coords$y_axis,
                                     color = eigen_setup$coords$measure)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = eigen_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = eigen_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Eigenvector\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    }

   # browser()

    # Burt Measures
    burt_measures <- columns[grepl("burt", columns)]
    burt_measures <- cbind(rep("burt", length(burt_measures)), burt_measures)
    burt_measures <- cbind(burt_measures, stringr::str_to_title(stringr::str_replace_all(burt_measures[,2], "burt_", "")))
    colnames(burt_measures) <- c('type', 'measure', 'label')

    burt_setup <- node_gg_setup(index_df = burt_measures, nodes = nodes)

    node_gg_list[[7]] <- burt_setup$coords %>%
      ggplot2::ggplot(ggplot2::aes(x = burt_setup$coords$x_axis,
                                   y = burt_setup$coords$y_axis,
                                   color = burt_setup$coords$measure)) +
      ggplot2::geom_line() +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
      ggplot2::scale_x_continuous(breaks = burt_setup$x_breaks) +
      ggplot2::scale_y_continuous(breaks = burt_setup$y_breaks) +
      ggplot2::labs(y = "\nDensity\n",
                    x = NULL,
                    color = NULL,
                    caption = "Burt Measures\n") +
      ggplot2::theme(legend.position = c(.8, .8),
                     legend.background = ggplot2::element_rect(colour="white", fill="white"),
                     plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))


    # Reachability Measures
    reachable_measures <- columns[grepl("reach", columns)]
    reachable_measures <- cbind(rep("reachable", length(reachable_measures)), reachable_measures)
    if (nrow(reachable_measures) == 1) {
      reachable_measures <- cbind(reachable_measures, NA)
    } else {
      reachable_measures <- cbind(reachable_measures,
                                  stringr::str_to_title(stringr::str_replace_all(reachable_measures[,2], "^proportion_reachable_", "")))
    }
    colnames(reachable_measures) <- c('type', 'measure', 'label')

    reach_setup <- node_gg_setup(index_df = reachable_measures, nodes = nodes)

    if (nrow(reachable_measures) == 1) {
      node_gg_list[[8]] <- reach_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = reach_setup$coords$x_axis,
                                     y = reach_setup$coords$y_axis)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = reach_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = reach_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Reachability\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    } else {
      node_gg_list[[8]] <- reach_setup$coords %>%
        ggplot2::ggplot(ggplot2::aes(x = reach_setup$coords$x_axis,
                                     y = reach_setup$coords$y_axis,
                                     color = reach_setup$coords$measure)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::scale_x_continuous(breaks = reach_setup$x_breaks) +
        ggplot2::scale_y_continuous(breaks = reach_setup$y_breaks) +
        ggplot2::labs(y = "\nDensity\n",
                      x = NULL,
                      color = NULL,
                      caption = "Reachability\n") +
        ggplot2::theme(legend.position = c(.8, .8),
                       legend.background = ggplot2::element_rect(colour="white", fill="white"),
                       plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))
    }

    p_2 <- cowplot::plot_grid(plotlist = node_gg_list, nrow = 3, ncol = 3) +
      ggplot2::labs(title = "\nNode-Level Measures\n") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                        size = 24,
                                                        face = "bold"))

    if (shiny == FALSE) {
      print(p_2)
    }

  }

  # node_plot <- Sys.time()

  # Assigning Report Elements to the Global Environment
  if ("system_measure_plot" %in% output) {
    basic_output$system_measure_plot <- p_1
  }
  if ("node_measure_plot" %in% output) {
    basic_output$node_measure_plot <- p_2
  }
  if ("system_level_measures" %in% output) {
    basic_output$system_level_measures <- system_level_measures
  }

  # Remove largest component/bicomponent if not specified
  if ("largest_component" %in% output) {
    basic_output$largest_component <- weak_component$largest_component
  }

  if ("largest_bi_component" %in% output) {
    basic_output$largest_bi_component <- bicomponent$largest_bi_component
  }

  # finish_time <- Sys.time()
  #
  # time_vec <- c(processing-basic_start, node_level_time-processing,
  #               system_level_time1-node_level_time, system_level_time2-system_level_time1,
  #               system_plot-system_level_time2, node_plot-system_plot,
  #               finish_time-node_plot)
  #
  # basic_output$time_df <- data.frame(stage = c("processing", "node_measures",
  #                                              "system_measures1", "system_measures2",
  #                                              "system plot", "node plot", "finish"),
  #                                    duration = time_vec)


  return(basic_output)



}

# Function for preparing node-level measure plots

node_gg_setup <- function(index_df, nodes) {

  # browser()

  measures <- index_df[,2]

  # Eliminating NA Values
  sub_measures <- vector('list', length(measures))
  for(j in seq_along(measures)){
    plot_measure <- nodes[,measures[[j]]]
    plot_measure <- plot_measure[!is.na(plot_measure)]
        # If all values are NAs, just store a vector of `-9999` so the plotting
        # doesn't crash
        if (length(plot_measure) == 0) {
          plot_measure <- rep(-9999, length(nodes[,measures[[j]]]))
        }
    sub_measures[[j]] <- plot_measure
  }

  measure_ranges <- numeric(length = length(sub_measures))

  if (length(measure_ranges) > 1) {

    for (j in 1:length(measure_ranges)){
      this_range <- range(sub_measures[[j]])
      measure_ranges[[j]] <- this_range[[2]]-this_range[[1]]
    }

    measure_ranges <- as.data.frame(cbind(index_df[,3], measure_ranges))
    colnames(measure_ranges)[[1]] <- c("name")
    #          measure_ranges <- measure_ranges[!duplicated(measure_ranges[[2]]), ]
    measure_ranges <- measure_ranges[!duplicated(measure_ranges[[1]]), ]

  }

  # Defining degree distribution coordinates
  y_axis <- NULL
  x_axis <- NULL
  for(j in seq_along(sub_measures)){
    plot_measure <- sub_measures[[j]]
    y <- stats::density(plot_measure)$y
    x <- stats::density(plot_measure)$x

    coordinates <- cbind(as.data.frame(x), y)
    coordinates <- coordinates[(coordinates[,1] >= min(plot_measure)), ]
    coordinates <- coordinates[(coordinates[,1] <= max(plot_measure)), ]
    x <- c(min(plot_measure), coordinates$x, max(plot_measure))
    ymax <- max(coordinates$y)
    y <- c(0, coordinates$y, ymax)

    y_axis <- c(y_axis, y)
    x_axis <- c(x_axis, x)
  }

  y_breaks <- pretty(y_axis)
  x_breaks <- pretty(x_axis)

  # Plotting Degree
  for(j in seq_along(sub_measures)){
    plot_measure <- sub_measures[[j]]
        # If all values in `plot_measure` are set the de facto `NA` value
        # of `-9999`, do not plot this measure and present users with a warning
        if (all(plot_measure == -9999)) {
          base::warning(paste("Measure ",
                              measures[[j]],
                              " consists only of NA values. This measure will not be displayed in the node-level summary visualization.",
                              sep = ""))
          next
        }

    y_axis <- stats::density(plot_measure)$y
    x_axis <- stats::density(plot_measure)$x
    coordinates <- cbind(as.data.frame(x_axis), y_axis)
    coordinates <- coordinates[(coordinates$x_axis >= min(plot_measure)), ]

    # this_measure <- measure_index[measure_index$type == measures[[i]], ]
    # coordinates$measure <- this_measure[j,2]
    coordinates$measure <- index_df[j,3]

    if (j == 1) {
      full_coord <- coordinates
    } else {
      full_coord <- rbind(full_coord, coordinates)
    }


    # x_axis <- coordinates$x_axis
    # y_axis <- coordinates$y_axis
    # graphics::lines(x_axis, y_axis, col=colors[[j]], lwd=1.5)

  }

  full_coord <- full_coord[2:nrow(full_coord), ]

  output_list <- list(coords = full_coord,
                      y_breaks = y_breaks,
                      x_breaks = x_breaks)

  return(output_list)

}
