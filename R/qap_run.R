#' Quadratic Assignment Procedure (\code{qap_run}).
#'
#' @description The \code{qap_run} function is a wrapper around \code{sna}'s Quadratic Assignment Procedure models \code{\link[sna:netlm]{sna::netlm}} and \code{\link[sna:netlogit]{sna::netlogit}}. It expects a networks objects containing dependent and independent variables of interest (e.g, the output from \code{\link{qap_setup}}).
#'
#' @param net An \code{igraph} or \code{network} object.
#' @param dependent A string naming the dependent variable of interest.
#' @param variables A vector of strings naming the independent variables of interest.
#' @param directed A logical statement identifying if the network should be treated as directed. Defaults to False.
#' @param family A string identifying the functional form. Options are "linear" and "binomial". Defauts to "linear".
#' @return `qap_run` returns a list of elements \code{model_results} that include:
#'
#' - \code{covs_df}, a data frame containing term labels, estimates, standard errors and p-values
#'
#' - \code{mods_df}, a data frame containing model-level information including the number of observations, AIC and BIC statistics.
#' @export
#'
#' @examples
#' data("florentine", package = "ideanet")
#'
#'  ideanet::netwrite(i_elements = florentine$node,
#'                    j_elements = florentine$target,
#'                    directed = FALSE,
#'                    net_name = "florentine_graph")
#'
#' ideanet::qap_setup(florentine_graph, variables = c("total_degree"), methods = c("difference"))
#'
#' ideanet::qap_run(qap_results[[1]], variables = c("diff_total_degree"))

qap_run <- function(net, dependent = NULL, variables, directed = F, family = "linear") {

  # Make sure it's an igraph object
  if ("network" %in% class(net)) {
    net <- intergraph::asIgraph(net)
  }

  # Get DV matrix
  if (is.null(dependent)) {
    dv <- as.matrix(igraph::as_adjacency_matrix(net))
  } else {
    dv <- as.matrix(igraph::as_adjacency_matrix(net, attr = dependent))}

  # Get IV matrices
  ivs <- list()
  for (i in 1:length(variables)) {
    iv <- variables[[i]]
    iv <- as.matrix(igraph::as_adjacency_matrix(net, attr = iv))
    ivs[[i]] <- iv
  }

  # Run QAP
  if (directed == T) {mode = "digraph"} else {mode = "graph"}
  if (family == "binomial"){
    # if (all(dv %in% 0:1) == T){
    res <- sna::netlogit(dv, ivs, reps = 10, mode = mode)
  } else if (family == "linear") {
    res <- sna::netlm(dv, ivs, reps = 100, mode = mode)
  } else {print("Not an available family -- Try 'linear' or 'binomial'")}

  # Tidy results
  variables <- c("intercept", variables)
  if (class(res) == "sna::netlogit"){
    covs_df <- dplyr::tibble(covars = variables, estimate = res$coefficients,
                      `exp(estimate)` = exp(res$coefficients), se = res$se,
                      pvalue = res$pgreqabs)
    mods_df <- dplyr::tibble(num_obs = res$n, aic = res$aic, bic = res$bic)
  } else {
    covs_df <- dplyr::tibble(covars = variables, estimate = res$coefficients,
                      se = res$se, pvalue = res$pgreqabs)
    mods_df <- dplyr::tibble(num_obs = res$n, aic = res$aic, bic = res$bic)
  }

  model_results <<- list(covs_df, mods_df)
}
