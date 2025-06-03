#' Selecting Individual Networks from \code{ego_netwrite} Output (\code{get_egograph})
#'
#' @description The \code{get_egograph} function extracts one or more specific ego networks from a list object created by \code{ego_netwrite}. Specifically, it extracts the \code{igraph} objects associated with the ego networks selected by the user. This can be useful for close inspection and/or comparison of individual ego networks in your data.
#'
#' @param egonw A list created by \code{ego_netwrite}.
#' @param ego_id A single numeric value indicating the \code{ego_id} number associated with the ego network you want to extract, or a vector of numeric \code{ego_id} numbers.
#'
#' @return \code{get_egograph} returns a list containing the contents of the \code{igraph_objects} list found in \code{egonw} corresponding to the values specified in \code{ego_id}.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' # Run `ego_netwrite`
#' ngq_nw <- ego_netwrite(egos = ngq_egos,
#'                        ego_id = ngq_egos$ego_id,
#'
#'                        alters = ngq_alters,
#'                        alter_id = ngq_alters$alter_id,
#'                        alter_ego = ngq_alters$ego_id,
#'
#'                        max_alters = 10,
#'                        alter_alter = ngq_aa,
#'                        aa_ego = ngq_aa$ego_id,
#'                        i_elements = ngq_aa$alter1,
#'                        j_elements = ngq_aa$alter2,
#'                        directed = FALSE)
#'
#'
#' # Select `igraph` objects associated with `ego_id` 3.
#' ego3 <- get_egograph(ngq_nw, 3)

get_egograph <- function(egonw = NULL,
                         ego_id = NULL) {

  # Extract just igraph object list
  igraph_list <- egonw$igraph_objects

  # Confirm which item in `igraph_list` corresponds to given `ego_id`
  list_position <- which(unlist(lapply(igraph_list, function(x){x$ego})) == ego_id)

  # Return data for the given `ego_id`
  return(igraph_list[ego_id])

}

