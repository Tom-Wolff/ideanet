
#' E-I Index (`ei_index`)
#'
#' @description
#'
#' @param ego_id A vector of unique ego identifiers located in an ego dataframe. If using data objects created by `ego_netwrite`, this should be the data frame entitled `egos`.
#' @param ego_measure A vector of attributes corresponding to each ego.
#' @param alter_ego A vector of ego identifiers located in an alter dataframe. If using data objects created by `ego_netwrite`, this should be the data frame entitled `alters`.
#' @param alter_measure A vector of attributes corresponding to each alter
#'
#' @return
#'
#' @export
#'
#' @examples
#'


ei_index <- function(ego_id,
                     ego_measure,
                     alter_ego,
                     alter_measure,
                     prefix = NULL,
                     suffix = NULL) {

  ego_df <- data.frame(ego_id = ego_id,
                       ego_val = ego_measure)
  alter_df <- data.frame(alter_ego,
                         alter_val = alter_measure)

  var_df <- dplyr::left_join(alter_df, ego_df, by = "ego_id")

  ei_df <- var_df %>%
    dplyr::group_by(ego_id) %>%
    dplyr::summarize(length = dplyr::n(),
                     num_sim = sum(as.character(alter_val) == as.character(ego_val), na.rm = T),
                     prop_sim = num_sim/length,
                     num_diff = sum(as.character(alter_val) != as.character(ego_val), na.rm = T),
                     prop_diff = num_diff/length,
                     ei_index = (prop_diff - prop_sim)/length) %>%
    dplyr::ungroup() %>%
    dplyr:: select(ego_id, ei_index)


  if (!is.null(prefix)) {
    colnames(ei_df) <- paste(prefix, colnames(ei_df), sep = "_")
    colnames(ei_df)[[1]] <- "ego_id"
  }

  if (!is.null(suffix)) {
    colnames(ei_df) <- paste(colnames(ei_df), suffix, sep = "_")
    colnames(ei_df)[[1]] <- "ego_id"
  }

  return(ei_df)

}

