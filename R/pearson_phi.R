#' Pearson's Phi (`pearson_phi`)
#'
#' @description
#'
#' @param ego_id A vector of unique ego identifiers located in an ego dataframe. If using data objects created by `ego_netwrite`, this should be the data frame entitled `egos`.
#' @param ego_measure A vector of attributes corresponding to each ego.
#' @param alter_ego A vector of ego identifiers located in an alter dataframe. If using data objects created by `ego_netwrite`, this should be the data frame entitled `alters`.
#' @param alter_measure A vector of attributes corresponding to each alter
#' @param prefix A character value indicating the desired prefix for the calculated homophily measure.
#' @param suffix A character value indicating the desired suffix for the calculated homophily measure.
#'
#' @return
#'
#' @export
#'
#' @examples
#'


pearson_phi <- function(ego_id,
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

  alter_tot <- data.frame(alter_val = var_df$alter_val) %>% dplyr::count(alter_val) %>%
    dplyr::mutate(n_tot = sum(n),
                  n_prop = n/n_tot) %>%
    dplyr::rename(n_alter_tot = n)

  alter_obs <- var_df %>% dplyr::group_by(ego_id) %>%
    dplyr::count(alter_val) %>%
    dplyr::rename(n_alter_obs = n)

  alter_tots <- alter_tot %>% dplyr::left_join(alter_obs, by = "alter_val") %>%
    dplyr::select(ego_id, dplyr::everything()) %>%
    dplyr::mutate(n_alter_obs = tidyr::replace_na(n_alter_obs, 0)) %>%
    dplyr::arrange(ego_id)

  p_phi <- alter_tots %>%
    dplyr::group_by(ego_id) %>%
    dplyr::mutate(exp_val = n_prop * sum(n_alter_obs),
                  diff = (n_alter_obs - exp_val)^2 / exp_val) %>%
    dplyr::summarize(chisq = sum(diff),
                     length = sum(n_alter_obs)) %>%
    dplyr::mutate(p_phi = sqrt(chisq/length)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-chisq, -length)

  if (!is.null(prefix)) {
    colnames(p_phi) <- paste(prefix, colnames(p_phi), sep = "_")
    colnames(p_phi)[[1]] <- "ego_id"
  }

  if (!is.null(suffix)) {
    colnames(p_phi) <- paste(colnames(p_phi), suffix, sep = "_")
    colnames(p_phi)[[1]] <- "ego_id"
  }

  return(p_phi)

}
