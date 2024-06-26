% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/community_detection.R
\name{comm_detect}
\alias{comm_detect}
\title{Community Detection Across Multiple Routines (\code{comm_detect})}
\usage{
comm_detect(g, modres = 1, shiny = FALSE)
}
\arguments{
\item{g}{An igraph object. If the igraph object contains a directed network, the function will treat the network as undirected before running community detection routines.}

\item{modres}{A modularity resolution parameter used when performing community detection using the Leiden method.}

\item{shiny}{An argument indicating whether the output from the \code{comm_detect} function will be fed into the IDEANet visualization app.}
}
\value{
\code{comm_detect} returns three data frames. \code{comm_members} indicates each node's assigned community membership from each community detection routine. \code{comm_summaries} indicates the number of communities inferred from each routine as well as the modularity score arising from community assignments. \code{comp_scores} contains a matrix indicating the similarity of community assignments between each pair of community detection routines, measured using adjusted rand scores. If \code{shiny == FALSE}, this function will also plot a series of network visualizations in which nodes are colored by their assigned community memberships from each routine.
}
\description{
The \code{comm_detect} function runs a set of several commonly-used community detection routines on a network and provides community assignments from these routines. Need to mention that only supports undirected nets and that for some routines the median community value is used.
}
\examples{
# Run netwrite
nw_fauxmesa <- netwrite(nodelist = fauxmesa_nodes,
                      node_id = "id",
                      i_elements = fauxmesa_edges$from,
                      j_elements = fauxmesa_edges$to,
                      directed = TRUE,
                      net_name = "faux_mesa")

# Run community detection function
faux_communities <- comm_detect(g = nw_fauxmesa$faux_mesa)
}
