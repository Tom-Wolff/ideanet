#' Measuring Homophily in Ego Networks (\code{ego_homophily})
#'
#' @description The \code{ego_homophily} function identifies how similar ego is from their alters on a given attribute.
#'
#' @param ego_id A vector of unique ego identifiers located in an ego dataframe. If using data objects created by \code{\link{ego_netwrite}}, this should be the data frame entitled \code{egos}.
#' @param ego_measure A vector of attributes corresponding to each ego.
#' @param alter_ego A vector of ego identifiers located in an alter dataframe. If using data objects created by \code{\link{ego_netwrite}}, this should be the data frame entitled \code{alters}.
#' @param alter_measure A vector of attributes corresponding to each alter
#' @param prefix A character value indicating the desired prefix for the calculated homophily measure.
#' @param suffix A character value indicating the desired suffix for the calculated homophily measure.
#' @param prop A logical value indicating whether homophily should be represented as a count or as a proportion.
#'
#' @return \code{ego_homophily} returns a dataframe of vectors that include the ego identifier and the number or proportion of alters with the same selected attribute
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#'# Run `ego_netwrite`
#'ngq_nw <- ego_netwrite(egos = ngq_egos,
#'                       ego_id = ngq_egos$ego_id,
#'
#'                       alters = ngq_alters,
#'                       alter_id = ngq_alters$alter_id,
#'                       alter_ego = ngq_alters$ego_id,
#'
#'                       max_alters = 10,
#'                       alter_alter = ngq_aa,
#'                       aa_ego = ngq_aa$ego_id,
#'                       i_elements = ngq_aa$alter1,
#'                       j_elements = ngq_aa$alter2,
#'                       directed = FALSE)
#'
#'
#'# Homophily as a Count
#'race_homophily_count <- ego_homophily(ego_id = ngq_nw$egos$ego_id,
#'                                      ego_measure = ngq_nw$egos$race,
#'                                      alter_ego = ngq_nw$alters$ego_id,
#'                                      alter_measure = ngq_nw$alters$race,
#'                                      suffix = "race")
#'
#'race_homophily_count
#'
#'# Homophily as a Proportion
#'race_homophily_prop <- ego_homophily(ego_id = ngq_nw$egos$ego_id,
#'                                     ego_measure = ngq_nw$egos$race,
#'                                     alter_ego = ngq_nw$alters$ego_id,
#'                                     alter_measure = ngq_nw$alters$race,
#'                                     prop = TRUE,
#'                                     suffix = "race")
#'race_homophily_prop


ego_homophily <- function(ego_id,
                          ego_measure,
                          alter_ego,
                          alter_measure,
                          prefix = NULL,
                          suffix = NULL,
                          prop = FALSE) {

  ego_df <- data.frame(ego_id = ego_id,
                       ego_val = ego_measure)
  alter_df <- data.frame(ego_id = alter_ego,
                         alter_val = alter_measure)

  var_df <- dplyr::left_join(alter_df, ego_df, by = "ego_id")

  hom_counts <- var_df %>%
    dplyr::group_by(ego_id) %>%
    dplyr::summarize(num_sim = sum(as.character(.data$alter_val) == as.character(.data$ego_val), na.rm = TRUE),
                     prop_sim = .data$num_sim/dplyr::n()) %>%
    dplyr::ungroup()

  if (prop == TRUE) {
    hom_counts <- hom_counts %>% dplyr::select(ego_id, prop_same = .data$prop_sim)
  } else {
    hom_counts <- hom_counts %>% dplyr::select(ego_id, num_same = .data$num_sim)
  }

  if (!is.null(prefix)) {
    colnames(hom_counts) <- paste(prefix, colnames(hom_counts), sep = "_")
    colnames(hom_counts)[[1]] <- "ego_id"
  }

  if (!is.null(suffix)) {
    colnames(hom_counts) <- paste(colnames(hom_counts), suffix, sep = "_")
    colnames(hom_counts)[[1]] <- "ego_id"
  }

  return(hom_counts)

}
