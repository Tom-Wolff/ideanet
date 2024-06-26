% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netwrite.R
\name{netwrite}
\alias{netwrite}
\title{Network Cleaning and Variable Calculation (\code{netwrite})}
\usage{
netwrite(
  data_type = c("edgelist"),
  adjacency_matrix = FALSE,
  adjacency_list = FALSE,
  nodelist = FALSE,
  node_id = NULL,
  i_elements = FALSE,
  j_elements = FALSE,
  fix_nodelist = TRUE,
  weights = NULL,
  type = NULL,
  remove_loops = FALSE,
  missing_code = 99999,
  weight_type = "frequency",
  directed = FALSE,
  net_name = "network",
  shiny = FALSE,
  output = c("graph", "largest_bi_component", "largest_component", "node_measure_plot",
    "nodelist", "edgelist", "system_level_measures", "system_measure_plot"),
  message = TRUE
)
}
\arguments{
\item{data_type}{A character value indicating the type of relational data being entered into \code{netwrite}. Available options are \code{edgelist}, \code{adjacency_matrix}, and \code{adjacency_list}.}

\item{adjacency_matrix}{If \code{data_type} is set to \code{adjacency_matrix}, a matrix object containing the adjacency matrix for the network being processed.}

\item{adjacency_list}{If \code{data_type} is set to \code{adjacency_list}, a data frame containing the adjacency list for the network being processed.}

\item{nodelist}{Either a vector of values indicating unique node/vertex IDs, or a data frame including all information about nodes in the network. If the latter, a value for \code{node_id} must be specified.}

\item{node_id}{If a data frame is entered for the \code{nodelist} arugment, \code{node_id} should be a character value indicating the name of the column in the node-level data frame containing unique node identifiers.}

\item{i_elements}{If \code{data_type} is set to \code{"edgelist"}, a numeric or character vector indicating the sender of ties in the edgelist.}

\item{j_elements}{If \code{data_type} is set to \code{"edgelist"}, a numeric or character vector indicating the receiver of ties in the edgelist.}

\item{fix_nodelist}{If \code{data_type} is set to \code{"edgelist"} and user inputs a vector or data frame into \code{nodelist}, a logical value indicating whether to include node IDs that do not appear in the nodelist but do appear in the edgelist in the nodelist used when processing network data. By default, \code{fix_nodelist} is set to \code{FALSE} to identify potential inconsistencies between the nodelist and edgelist to the user.}

\item{weights}{A numeric vector indicating the weight of ties in the edgelist. \code{netwrite} requires that all edge weights be positive values.}

\item{type}{A numeric or character vector indicating the types of relationships represented in the edgelist. If \code{type} contains this vector, \code{netwrite} will treat the data as a multi-relational network and produce additional outputs reflecting the different types of ties occurring in the network.}

\item{remove_loops}{A logical value indicating whether "self-loops" (ties directed toward oneself) should be considered valid ties in the network being processed.}

\item{missing_code}{A numeric value indicating "missing" values in an edgelist. Such "missing" values are sometimes included to identify the presence of isolated nodes in an edgelist when a corresponding nodelist is unavailable.}

\item{weight_type}{A character value indicating whether edge weights should be treated as frequencies or distances. Available options are \code{"frequency"}, indicating that higher values represent stronger ties, and \code{"distance"}, indicating that higher values represent weaker ties.}

\item{directed}{A logical value indicating whether edges should be treated as a directed or undirected when constructing the network.}

\item{net_name}{A character value indicating the name to which network/igraph objects should be given.}

\item{shiny}{A logical value indicating whether \code{netwrite} is being used in conjunction with IDEANet's Shiny-based visualization app. \code{shiny} should also be set to \code{TRUE} when using \code{ideanet} in an R Markdown file that users expect to knit into a document.}

\item{output}{A character vector indicating the kinds of objects \code{netwrite} should assign to the global environment. \code{netwrite} produces several outputs that may not all be necessary to a user's needs. Users can specify which outputs they specifically want in order to minimize the number of objects appearing in the global environment. Potential outputs include igraph object(s) (\code{"graph"}), subgraph(s) of only nodes that appear in the largest component and/or bicomponent of the network (\code{"largest_component"}, \code{"largest_bi_component"}), data frame(s) containing node-level measures (\code{"node_measure_plot"}), a processed edgelist of the network (\code{"edgelist"}), a data frame indicating network-level summaries (\code{"system_level_measures"}), and summary visualizations for node- and network-level measures (\code{"node_measure_plot"}, \code{"system_measure_plot"}).}

\item{message}{A logical value indicating whether warning messages should be displayed in the R console during processing.}
}
\value{
\code{netwrite} returns a list containing several output objects. Users may find it easier to access and work with outputs by applying \link{list2env} to this list, which will separate outputs and store them in the R Global Environment. Depending on the values assigned to the \code{output} argument, \code{netwrite} will produce any or all of the following:

If \code{output} contains \code{graph}, \code{netwrite} will return an igraph object of the network represented in the original data.
If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing igraph objects for each unique relation type as well as the overall network. These output objects are named according to the value specified in the \code{net_name} argument.

If \code{output} contains \code{"nodelist"}, \code{netwrite} will return a dataframe containing individual-level information for each node in the network. This dataframe contains a set of frequently used node-level measures for each node in the network. If a vector is entered into the \code{type} argument, \code{netwrite} will produce these node-level measures for each unique relation type.

If \code{output} contains \code{"edgelist"}, \code{netwrite} will return a formatted edgelist for the network represented in the original data. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing edgelists for each unique relation type as well as the overall network.

If \code{output} contains \code{"system_level_measures"}, \code{netwrite} will return a data frame providing network-level summary information.

If \code{output} contains \code{"node_measure_plot"}, \code{netwrite} will return a plot summarizing the distribution of frequently used node-level measures across all nodes in the network. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing node-level summary plots for each unique relation type as well as the overall network.

If \code{output} contains \code{"system_measure_plot"}, \code{netwrite} will return a plot summarizing the distribution of frequently used network-level measures. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing network-level summary plots for each unique relation type as well as the overall network.

If \code{output} contains \code{"largest_bi_component"}, \code{netwrite} will return an igraph object of the largest bicomponent in the network represented in the original data. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing the largest bicomponent for each unique relation type as well as the overall network.

If \code{output} contains \code{"largest_bi_component"}, \code{netwrite} will return an igraph object of the largest main component in the network represented in the original data. If a vector is entered into the \code{type} argument, \code{netwrite} also produces a list containing the largest main component for each unique relation type as well as the overall network.
}
\description{
The \code{netwrite} function reads in relational data of several formats and processes them into a set of standardized outputs. These outputs include sets of commonly calculated measures at the individual node and network-wide levels.
}
\examples{
# Use netwrite on an edgelist
nw_fauxmesa <- netwrite(nodelist = fauxmesa_nodes,
                      node_id = "id",
                      i_elements = fauxmesa_edges$from,
                      j_elements = fauxmesa_edges$to,
                      directed = TRUE,
                      net_name = "faux_mesa")

# For easier access of output objects, call `list2env` on output
list2env(nw_fauxmesa, .GlobalEnv)

### Inspect updated edgelist
head(edgelist)

### Inspect data frame of node-level measures
head(node_measures)

### Inspect system-level summary
head(system_level_measures)

### Plot sociogram of network
plot(faux_mesa)

### View node-level summary visualization
node_measure_plot

### View system-level summary visualization
system_measure_plot



# Run netwrite on an adjacency matrix

nw_triad <- netwrite(data_type = "adjacency_matrix",
                     adjacency_matrix = triad,
                     directed = TRUE,
                     net_name = "triad_igraph")


# Run netwrite on a multirelational network
flor <- netwrite(nodelist = florentine_nodes,
                 node_id = "id",
                 i_elements = florentine_edges$source,
                 j_elements = florentine_edges$target,
                 type = florentine_edges$type,
                 directed = FALSE,
                 net_name = "florentine")

# View system level summary
flor$system_level_measures

# View system level summary for network of marriage relations
flor$system_measure_plot$marriage
}
