#' Quadratic Assignment Procedure (\code{qap_run}).
#'
#' @description The \code{qap_run} function is a wrapper around \code{sna}'s Quadratic Assignment Procedure models \code{\link[sna:netlm]{sna::netlm}} and \code{\link[sna:netlogit]{sna::netlogit}}. It expects a networks objects containing dependent and independent variables of interest. It is required to use the output from \code{\link{qap_setup}}.
#'
#' @param net An \code{igraph} or \code{network} object.
#' @param dependent A string naming the dependent variable of interest. By default, the probability of a tie. Can also be the output of \code{\link{qap_setup}} using prefixes "same_", "diff_" or "abs_diff_".
#' @param variables A vector of strings naming the independent variables of interest. Must be the output of \code{\link{qap_setup}} using prefixes "same_", "diff_" or "abs_diff_".
#' @param directed A logical statement identifying if the network should be treated as directed. Defaults to \code{FALSE}.
#' @param family A string identifying the functional form. Options are \code{"linear"} and \code{"binomial"}. Defauts to \code{"linear"}.
#' @param reps A numeric value indicating the number of draws. Defaults to 500.
#' @return `qap_run` returns a list of elements that include:
#'
#' - \code{covs_df}, a data frame containing term labels, estimates, standard errors and p-values
#'
#' - \code{mods_df}, a data frame containing model-level information including the number of observations, AIC and BIC statistics.
#' @export
#'
#' @examples
#'
#' flor <- netwrite(nodelist = florentine_nodes,
#'                  node_id = "id",
#'                  i_elements = florentine_edges$source,
#'                  j_elements = florentine_edges$target,
#'                  type = florentine_edges$type,
#'                  directed = FALSE,
#'                  net_name = "florentine_graph")
#'
#' flor_setup <- qap_setup(flor$florentine_graph,
#'                         variables = c("total_degree"),
#'                         methods = c("difference"))
#'
#' flor_qap <- qap_run(flor_setup$graph,
#'                     variables = c("diff_total_degree"))
#'
#' # Inspect results
#' flor_qap$covs_df

qap_run <- function(net, dependent = NULL, variables, directed = F, family = "linear", reps = 500) {

  # remove isolates and convert to network
  if ("igraph" %in% class(net)) {
    iso <- which(igraph::degree(net) == 0)
    net <- igraph::delete_vertices(net, iso)
    if (igraph::any_multiple(net) == T) {net <- igraph::simplify(net, edge.attr.comb = "sum")}
    edges <- igraph::as_data_frame(net, what = "edges")
    nodes <- igraph::as_data_frame(net, what = "vertices")
    net <- network::network(x = edges, directed = directed,
                            vertex.attr = nodes)
  }

  # Create new values in case "both" is utilized
  tempvars <- c(dependent, variables)
  for (i in 1:length(tempvars)){
    name <- tempvars[[i]]
    if (grepl("^both_", name, fixed = F)) {
      varname <- gsub("^both_(.*)_.*", "\\1", name)
      varvalue <- gsub("^both_.*_(.*)", "\\1", name)
      nodes[[name]] <- ifelse(nodes[[varname]] == varvalue, 1, 0)
    }
  }

  # Get DV matrix
  if (is.null(dependent)) {
    dv <- as.matrix(net)
  } else if (grepl("^both_", dependent, fixed = F)) {
    dv <- as.matrix(outer(nodes[[dependent]], nodes[[dependent]], "=="))
  } else if (grepl("^same_", dependent, fixed = F)) {
    dv_name <- gsub("^same_", "", dependent)
    dv <- as.matrix(outer(nodes[[dv_name]], nodes[[dv_name]], "=="))
  } else if (grepl("^diff_", dependent, fixed = F)) {
    dv_name <- gsub("^diff_", "", dependent)
    dv <- as.matrix(outer(nodes[[dv_name]], nodes[[dv_name]], "-"))
  } else if (grepl("^abs_diff_", dependent, fixed = F)) {
    dv_name <- gsub("^abs_diff_", "", dependent)
    dv <- as.matrix(abs(outer(nodes[[dv_name]], nodes[[dv_name]], "-")))
  } else {
    stop(paste0(dependent, " is not an output of qap_setup() with prefix `same`,`diff` or `abs_diff`"))
  }

  # Get IV matrices
  ivs <- list()
  rem <- list() # list of empty matrices
  for (i in 1:length(variables)) {
    iv_name <- variables[[i]]
    if (grepl("same_", iv_name, fixed = F)) {
      iv_name <- gsub("^same_", "", iv_name)
      iv <- as.matrix(outer(nodes[[iv_name]], nodes[[iv_name]], "=="))
    } else if (grepl("^both_", iv_name, fixed = F)) {
      iv <- as.matrix(outer(nodes[[iv_name]], nodes[[iv_name]], "=="))
    } else if (grepl("^diff_", iv_name, fixed = F)) {
      iv_name <- gsub("^diff_", "", iv_name)
      iv <- as.matrix(outer(nodes[[iv_name]], nodes[[iv_name]], "-"))
    } else if (grepl("^abs_diff_", iv_name, fixed = F)) {
      iv_name <- gsub("^abs_diff_", "", iv_name)
      iv <- as.matrix(abs(outer(nodes[[iv_name]], nodes[[iv_name]], "-")))
    } else {
      stop(paste0(iv_name, " is not an output of qap_setup() with prefix `same`,`diff` or `abs_diff`"))
    }

    if (sum(abs(iv)) == 0) {# check if variables is empty
      warning(paste0("The variable ", iv_name, " is empty. It is excluded from the model."))
      rem[[i]] <- iv_name
    } else {ivs[[i]] <- iv}
  }

  ivs <- ivs[lapply(ivs,length)>0]

  # Run QAP
  if (length(ivs) != 0) {
    if (directed == T) {mode = "digraph"} else {mode = "graph"}
    if (family == "binomial"){
      # if (all(dv %in% 0:1) == T){
      res <- sna::netlogit(dv, ivs, reps = reps, mode = mode)
    } else if (family == "linear") {
      res <- sna::netlm(dv, ivs, reps = reps, mode = mode)
    } else {print("Not an available family -- Try 'linear' or 'binomial'")}

    # Tidy results
    rem <- unlist(rem[lapply(rem,length)>0])
    if (!is.null(rem)) {variables <- variables[-which(variables %in% rem)]}
    variables <- c("intercept", variables)
    if (methods::is(res, "sna::netlogit")) {
      covs_df <- dplyr::tibble(covars = variables, estimate = res$coefficients,
                               `exp(estimate)` = exp(res$coefficients), se = res$se,
                               pvalue = res$pgreqabs)
      mods_df <- dplyr::tibble(num_obs = res$n, aic = res$aic, bic = res$bic)
    } else {
      covs_df <- dplyr::tibble(covars = variables, estimate = res$coefficients,
                               se = res$se, pvalue = res$pgreqabs)
      mods_df <- dplyr::tibble(num_obs = res$n, aic = res$aic, bic = res$bic)
    }

    output_list <- list(covs_df = covs_df, mods_df = mods_df)
    return(output_list)

  } else {warning("All IVs are empty. Try a different set of IVs")}
}



