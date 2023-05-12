#' Quadratic Assignment Procedure (`qap_run`).
#'
#' @description The `qap_run` function is a wrapper around `sna`'s Quadratic Assignment Procedure models `netlm()` and `netlogit()`. It expects a networks objects containing dependent and independent variables of interest (e.g, the output from `qap_setup`).
#'
#' @param net An `igraph` or `network` object.
#' @param dependent A string naming the dependent variable of interest.
#' @param variables A vector of strings naming the independent variables of interest.
#' @param directed A logical statement identifying if the network should be treated as directed. Defaults to False.
#' @param family A string identifying the functional form. Options are "linear" and "binomial". Defauts to "linear".
#' @return `qap_run` returns a list of elements `model_results` that include: 
#' 
#' - `covs_df`, a data frame containing term labels, estimates, standard errors and p-values
#' 
#' - `mods_df`, a data frame containing model-level information including the number of observations, AIC and BIC statistics.
#' @export
#'
#' @examples
#' qap_setup(net = igraph_object,
#'          variables = "attribute_1",
#'          methods = c("attribute_2", "attribute_3", "attribute_4"),
#'          directed = F,
#'          family = "linear")

qap_run <- function(net, dependent = NULL, variables, directed = F, family = "linear") {
  
  require(sna)
  require(igraph)
  require(intergraph)
  require(tidygraph)
  require(dplyr)
  
  # Make sure it's an igraph object
  if ("network" %in% class(net)) {
    net <- asIgraph(net)
  }
  
  # Get DV matrix
  if (is.null(dependent)) {
    dv <- as.matrix(as_adjacency_matrix(net))
  } else {
    dv <- as.matrix(as_adjacency_matrix(net, attr = dependent))}
  
  # Get IV matrices
  ivs <- list()
  for (i in 1:length(variables)) {
    iv <- variables[[i]]
    iv <- as.matrix(as_adjacency_matrix(net, attr = iv))
    ivs[[i]] <- iv
  }
  
  # Run QAP
  if (directed == T) {mode = "digraph"} else {mode = "graph"}
  if (family == "binomial"){
    # if (all(dv %in% 0:1) == T){
    res <- netlogit(dv, ivs, reps = 10, mode = mode)
  } else if (family == "linear") {
    res <- netlm(dv, ivs, reps = 100, mode = mode)
  } else {print("Not an available family -- Try 'linear' or 'binomial'")}
  
  # Tidy results
  variables <- c("intercept", variables)
  if (class(res) == "netlogit"){
    covs_df <- tibble(covars = variables, estimate = res$coefficients, 
                      `exp(estimate)` = exp(res$coefficients), se = res$se, 
                      pvalue = res$pgreqabs) 
    mods_df <- tibble(num_obs = res$n, aic = res$aic, bic = res$bic)
  } else {
    covs_df <- tibble(covars = variables, estimate = res$coefficients, 
                      se = res$se, pvalue = res$pgreqabs)
    mods_df <- tibble(num_obs = res$n, aic = res$aic, bic = res$bic)
  }
  
  model_results <<- list(covs_df, mods_df)
}