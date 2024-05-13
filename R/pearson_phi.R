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
#'race_pphi <- pearson_phi(ego_id = egos$ego_id, ego_measure = egos$race,
#'                         alter_ego = alters$ego_id, alter_measure = alters$race,
#'                         suffix = "race")
#'
#'race_pphi


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

  alter_tot <- data.frame(alter_val = var_df$alter_val) %>% dplyr::count(.data$alter_val) %>%
    dplyr::mutate(n_tot = sum(.data$n),
                  n_prop = .data$n/.data$n_tot) %>%
    dplyr::rename(n_alter_tot = .data$n)

  alter_obs <- var_df %>% dplyr::group_by(.data$ego_id) %>%
    dplyr::count(.data$alter_val) %>%
    dplyr::rename(n_alter_obs = .data$n)

  alter_tots <- alter_tot %>% dplyr::left_join(alter_obs, by = "alter_val") %>%
    dplyr::select(.data$ego_id, dplyr::everything()) %>%
    dplyr::mutate(n_alter_obs = tidyr::replace_na(.data$n_alter_obs, 0)) %>%
    dplyr::arrange(.data$ego_id)

  p_phi <- alter_tots %>%
    dplyr::group_by(.data$ego_id) %>%
    dplyr::mutate(exp_val = .data$n_prop * sum(.data$n_alter_obs),
                  diff = (.data$n_alter_obs - .data$exp_val)^2 / .data$exp_val) %>%
    dplyr::summarize(chisq = sum(.data$diff),
                     length = sum(.data$n_alter_obs)) %>%
    dplyr::mutate(p_phi = sqrt(.data$chisq/.data$length)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$chisq, -.data$length)

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
