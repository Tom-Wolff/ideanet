#' H-Index (\code{h_index})
#'
#' @description Measure of ego network diversity for categorical attributes (Perry et al. 2018)
#'
#' @param ego_id A vector of ego identifiers located in an alter dataframe. If using data objects created by \code{\link{ego_netwrite}}, this should be the data frame entitled \code{alters}.
#' @param measure A vector of alter attributes for a given categorical measure.
#' @param prefix A character value indicating the desired prefix for the calculated homophily measure.
#' @param suffix A character value indicating the desired suffix for the calculated homophily measure.
#'
#' @return \code{h_index} returns a dataframe of vectors that include the ego identifier and h-index of diversity for the desired categorical attribute.
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
#'# Get H-index for race
#'race_hindex <- h_index(ego_id = ngq_nw$alters$ego_id,
#'                       measure = ngq_nw$alters$race,
#'                       prefix = "race")
#'
#'race_hindex


# User-facing function
h_index <- function(ego_id,
                    measure,
                    prefix = NULL,
                    suffix = NULL) {


  ego_df <- data.frame(ego_id = ego_id,
                       val = measure)

  h_df <- ego_df %>%
    dplyr::group_by(.data$ego_id) %>%
    dplyr::summarize(h_index = single_h_index(.data$val)) %>%
    dplyr::ungroup()

  if (!is.null(prefix)) {
    colnames(h_df) <- paste(prefix, colnames(h_df), sep = "_")
    colnames(h_df)[[1]] <- "ego_id"
  }

  if (!is.null(suffix)) {
    colnames(h_df) <- paste(colnames(h_df), suffix, sep = "_")
    colnames(h_df)[[1]] <- "ego_id"
  }

  return(h_df)

}


# H-index for ego network diversity of categorical measures
single_h_index <- function(x) {

  # Get total number of values in `x`
  num_vals <- length(x)

  # To deal with treating `NAs` as their own category, let's rely on `dplyr`
  h_df <- data.frame(value = x) %>%
    dplyr::group_by(.data$value) %>%
    dplyr::summarize(prop_sq = (dplyr::n()/num_vals)^2) %>%
    dplyr::ungroup()

  if (NA %in% h_df$value) {
    base::warning("NA values detected. NA will be treated as its own category when calculating H-index.")
  }

  h_val <- 1-sum(h_df$prop_sq)
  # Return output
  return(h_val)
}
