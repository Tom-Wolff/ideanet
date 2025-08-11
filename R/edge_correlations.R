#' Extracting Multi-Level Edge Correlations (\code{edge_correlations})
#'
#' @description Given a network with multiple unique edge types, \code{netwrite} calculates the multi-level edge correlations for each unique pair of edge types and stores them in the \code{system_level_measures} dataframe. The \code{edge_correlations} function extracts these values from \code{netwrite}'s output so users can interact with them more directly.
#'
#' @param netwrite_output A list object created by \code{netwrite} for a network with multiple unique edge types. This list must contain the \code{system_level_measures} dataframe. If the network stored in \code{netwrite_output} contains only a single edge type, \code{edge_correlations} will return an error message.
#'
#' @return \code{edge_correlations} returns a data frame containing each unique pair of edge types in the network and their respective correlation value.
#'
#' @export
#'
#' @examples
#' # Use netwrite on an edgelist
#' nw_flor <- netwrite(nodelist = florentine_nodes,
#'                     node_id = "id",
#'                     i_elements = florentine_edges$source,
#'                     j_elements = florentine_edges$target,
#'                     type = florentine_edges$type,
#'                     directed = FALSE,
#'                     net_name = "florentine")
#'
#' edge_correlations(nw_flor)

edge_correlations <- function(netwrite_output) {

  # browser()

  # Check if multirelational network
  if (!("system_level_measures" %in% names(netwrite_output))) {
    stop("List does not contain system_level_measure data frame. Please rerun netwrite to include this data frame in its output.")
  }

  if (is.na(netwrite_output$system_level_measures[netwrite_output$system_level_measures[,1] == "Number of Tie Types", 3])) {
    stop("Network does not contain multiple edge types")
  }

  # Get string from the system-level summary dataframe
  cor_string <- netwrite_output$system_level_measures[netwrite_output$system_level_measures[,1] == "Multi-Level Edge Correlation", "summary_graph"]

  # Split `cor_string` as needed
  cor_split <- stringr::str_split(cor_string, pattern = "; ")[[1]]
  # Extract variable names
  cor_split_names <- stringr::str_extract_all(cor_split, "\\b\\w+_weight\\b")
  # Extract correlation values
  cor_split_vals <- as.numeric(unlist(stringr::str_extract_all(cor_split, "\\d+(\\.\\d+)?$")))

  # Begin constructing dataframe
  cor_df <- dplyr::bind_rows(lapply(cor_split_names, function(x){data.frame(type1 = x[1], type2 = x[2])}))
  cor_df$type1 <- stringr::str_replace(cor_df$type1, "_weight$", "")
  cor_df$type2 <- stringr::str_replace(cor_df$type2, "_weight$", "")
  cor_df$correlation <- cor_split_vals

  # Return `cor_df`
  return(cor_df)
}
