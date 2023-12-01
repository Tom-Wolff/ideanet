#' Euclidean Distance (`euclidean_distance`)
#'
#' @description Typical difference between between ego and their alters for a given continuous attribute (Perry et al. 2018)
#'
#' @param ego_id A vector of unique ego identifiers located in an ego dataframe. If using data objects created by `ego_netwrite`, this should be the data frame entitled `egos`.
#' @param ego_measure A vector of attributes corresponding to each ego.
#' @param alter_ego A vector of ego identifiers located in an alter dataframe. If using data objects created by `ego_netwrite`, this should be the data frame entitled `alters`.
#' @param alter_measure A vector of attributes corresponding to each alter.
#' @param prefix A character value indicating the desired prefix for the calculated homophily measure.
#' @param suffix A character value indicating the desired suffix for the calculated homophily measure.
#'
#' @return `euclidean_distance` returns a dataframe of vectors that include the ego identifier and euclidean distance for the desired continuous attribute
#'
#' @export
#'
#' @examples
#'
#' data(package = "egor", "egos32")
#' data(package = "egor", "alters32")
#'
#' euclidean_distance(ego_id = egos32$.EGOID, ego_measure = egos32$income,
#' alter_ego = alters32$.EGOID, alter_measure = alters32$income)

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
  dplyr::mutate(diff = (alter_val - ego_val)^2) %>%
  # Summarize
  dplyr::group_by(ego_id) %>%
  dplyr::summarize(length = dplyr::n(),
                   euc_num = sqrt(sum(diff, na.rm = T))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(euclidean_distance = euc_num/length) %>%
  dplyr::select(-euc_num, -length)

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
