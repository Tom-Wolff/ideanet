#' Pearson's Phi (\code{pearson_phi})
#'
#' @description The \code{pearson_phi} function identifies the underlying homophilous preference of ego based on the distribution of alter attributes in the population (Perry et al. 2018)
#'
#' @param ego_id A vector of unique ego identifiers located in an ego dataframe. If using data objects created by \code{ego_netwrite}, this should be the data frame entitled \code{egos}.
#' @param ego_measure A vector of attributes corresponding to each ego.
#' @param alter_ego A vector of ego identifiers located in an alter dataframe. If using data objects created by \code{ego_netwrite}, this should be the data frame entitled \code{alters}.
#' @param alter_measure A vector of attributes corresponding to each alter
#' @param prefix A character value indicating the desired prefix for the calculated homophily measure.
#' @param suffix A character value indicating the desired suffix for the calculated homophily measure.
#'
#' @return \code{pearson_phi} returns a dataframe of vectors that include the ego identifier and phi value of homophilous preference.
#'
#' @export
#'
#' @examples
#'
#' data(package = "egor", "egos32")
#' data(package = "egor", "alters32")
#'
#' pearson_phi(ego_id = egos32$.EGOID, ego_measure = egos32$country,
#' alter_ego = alters32$.EGOID, alter_measure = alters32$country)


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
