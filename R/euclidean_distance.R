#' Euclidean Distance (\code{euclidean_distance})
#'
#' @description Typical difference between between ego and their alters for a given continuous attribute (Perry et al. 2018)
#'
#' @param ego_id A vector of unique ego identifiers located in an ego dataframe. If using data objects created by \code{\link{ego_netwrite}}, this should be the data frame entitled \code{egos}.
#' @param ego_measure A vector of attributes corresponding to each ego.
#' @param alter_ego A vector of ego identifiers located in an alter dataframe. If using data objects created by \code{\link{ego_netwrite}}, this should be the data frame entitled \code{alters}.
#' @param alter_measure A vector of attributes corresponding to each alter.
#' @param prefix A character value indicating the desired prefix for the calculated homophily measure.
#' @param suffix A character value indicating the desired suffix for the calculated homophily measure.
#'
#' @return \code{euclidean_distance} returns a dataframe of vectors that include the ego identifier and euclidean distance for the desired continuous attribute
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
#'# Split items into Global Environment
#'list2env(ngq_nw, .GlobalEnv)
#'
#'# Calculate Euclidean Distance
#'pol_euc <- euclidean_distance(ego_id = egos$ego_id, ego_measure = egos$pol,
#'                              alter_ego = alters$ego_id, alter_measure = alters$pol,
#'                              prefix = "pol")
#'pol_euc

euclidean_distance <- function(ego_id,
                               ego_measure,
                               alter_ego,
                               alter_measure,
                               prefix = NULL,
                               suffix = NULL) {

  ego_df <- data.frame(ego_id = ego_id,
                       ego_val = ego_measure)
  alter_df <- data.frame(ego_id = alter_ego,
                         alter_val = alter_measure)

  var_df <- dplyr::left_join(alter_df, ego_df, by = "ego_id")

  euc_df <- var_df %>%
    # Setup for eucidean distance
    dplyr::mutate(diff = (.data$alter_val - .data$ego_val)^2) %>%
    # Summarize
    dplyr::group_by(ego_id) %>%
    dplyr::summarize(length = dplyr::n(),
                     euc_num = sqrt(sum(diff, na.rm = T))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(euclidean_distance = .data$euc_num/length) %>%
    dplyr::select(-.data$euc_num, -length)

  if (!is.null(prefix)) {
    colnames(euc_df) <- paste(prefix, colnames(euc_df), sep = "_")
    colnames(euc_df)[[1]] <- "ego_id"
  }

  if (!is.null(suffix)) {
    colnames(euc_df) <- paste(colnames(euc_df), suffix, sep = "_")
    colnames(euc_df)[[1]] <- "ego_id"
  }

  return(euc_df)

}
