#' Index of Qualitative Variation (\code{iqv})
#'
#' @description A normalized value of the h-index for measuring the diversity of an ego's network for categorical attributes (Perry et al. 2018)
#'
#' @param ego_id A vector of ego identifiers located in an alter dataframe. If using data objects created by \code{ego_netwrite}, this should be the data frame entitled \code{alters}.
#' @param measure A vector of alter attributes for a given categorical measure.
#' @param prefix A character value indicating the desired prefix for the calculated homophily measure.
#' @param suffix A character value indicating the desired suffix for the calculated homophily measure.
#'
#' @return \code{iqv} returns a dataframe of vectors that include the ego identifier and iqv value of diversity for the desired categorical attribute.
#'
#' @export
#'
#' @examples
#'
#' data(package = "egor", "egos32")
#' data(package = "egor", "alters32")
#'
#' iqv(ego_id = alters32$.EGOID, measure = alters32$country)



# User-facing function
iqv <- function(ego_id,
                measure,
                prefix = NULL,
                suffix = NULL) {

  ego_df <- data.frame(ego_id = ego_id,
                       val = measure)

  iqv_df <- ego_df %>%
    dplyr::group_by(ego_id) %>%
    dplyr::summarize(iqv = single_iqv(val)) %>%
    dplyr::ungroup()

  if (!is.null(prefix)) {
    colnames(iqv_df) <- paste(prefix, colnames(iqv_df), sep = "_")
    colnames(iqv_df)[[1]] <- "ego_id"
  }

  if (!is.null(suffix)) {
    colnames(iqv_df) <- paste(colnames(iqv_df), suffix, sep = "_")
    colnames(iqv_df)[[1]] <- "ego_id"
  }

  return(iqv_df)


}

# Normalized H_index
single_iqv <- function(x) {

  num_vals <- length(x)
  num_distinct_vals <- length(unique(x))

  # To deal with treating `NAs` as their own category, let's rely on `dplyr`
  h_df <- data.frame(value = x) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(prop_sq = (dplyr::n()/num_vals)^2) %>%
    dplyr::ungroup()

  if (NA %in% h_df$value) {
    base::warning("NA values detected. NA will be treated as its own category when calculating IQV")
  }

  h_val <- 1-sum(h_df$prop_sq)
  iqv <- h_val / (1 - (1/num_distinct_vals))
  iqv <- ifelse(is.nan(iqv), 0, iqv)
  return(iqv)
}
