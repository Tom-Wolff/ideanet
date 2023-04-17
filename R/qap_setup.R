
# Takes either an igraph or network object, a vector of variables and methods and merges on outside ego variables if needed.
# Variable and method rules:
# 1. Methods affect variables in order (e.g, variable 1 takes on method 1)
# 2. Possible methods:
#   2.a. "multibin" -- works on categorical variables; creates a variable for each value in input variable that displays if alter and ego are similar.
#   2.b. "bin" -- categorical variables; creates variables that determines if ego and alter share a value.
#   2.c. "both" -- computes both the "multibin" and "bin" methods.
#   2.d. "diff" -- computes the difference in value between ego and alter.

# Make output global

qap_setup <- function(net, variables, methods, directed = F, additional_vars = NULL) {
  
  require(sna)
  require(igraph)
  require(intergraph)
  require(tidygraph)
  require(dplyr)
  
  ### CONSTRUCTING NODE AND EDGE LISTS ###  
  
  # Make sure it's an igraph object
  if ("network" %in% class(net)) {
    net <- asIgraph(net)
  }
  
  # Create nodelist, checking for an "id" column
  if (!("id" %in% vertex_attr_names(net))) {
    nodes <- igraph::as_data_frame(net, what = "vertices") %>% 
      tibble::rownames_to_column(var = "id") %>% 
      mutate(id = as.numeric(id))
  } else {
    nodes <- igraph::as_data_frame(net, what = "vertices") %>% 
      mutate(id = as.numeric(id))
  }
  
  # Create edgelist  
  edges <- igraph::as_data_frame(net, what = "edges") %>% 
    mutate_at(vars(from, to), as.numeric)
  
  # Check if additional_vars was called
  if (!is.null(additional_vars)) {
    vec1 <- nodes$id
    
    # Check if the called ego df has an id column or a label column
    tryCatch(vec2 <- additional_vars$id,
             warning = function(e)
               tryCatch(vec2 <- additional_vars$label,
                        warning = function(e)
                          stop("Make sure your additional ego dataframe contains an id or label variable")))
    
    # Check if the two ids are identical
    if (identical(vec1, vec2) == F) {
      error_m <- paste0("Make sure the id or label of the additional ego dataframe match model ids exactly. 
     There are ", length(vec1), " ids in the model and ", length(vec2), " ids in the additional 
     dataframe, of which ", length(intersect(vec1, vec2)), " intersect.")
      stop(error_m)
    }
    
    # Merge if all tests are passed
    nodes <- nodes %>% left_join(additional_vars)
  }
  
  ### CONSTRUCTING DYAD MEASURES FROM EGO MEASURES ###
  
  # check if there are as many variables as methods
  if ((length(variables) == length(methods)) == F) {
    stop("Different number of variables and methods")
  }
  
  # loop over user defined variables
  for (i in 1:length(variables)) {
    variable <- variables[i]
    method <- methods[i]
    
    # Check (1) if variable in in nodelist (2) if variable NOT in edgelist (3) if transformed variable in edgelist. If False, skip.
    if (variable %in% names(nodes) & !(variable %in% names(edges)) & !(!!paste0(variable, "_ego") %in% names(edges))) {
    
      # Add each value with _ego or _alter suffix to edge dataframe
      edges <- edges %>% 
        left_join(nodes %>% select(id, all_of(variable)), by = c("from" = "id")) %>% 
        rename(!!paste0(variable, "_ego") := variable) %>% 
        left_join(nodes %>% select(id, all_of(variable)), by = c("to" = "id")) %>% 
        rename(!!paste0(variable, "_alter") := variable)
      
      # If method "reduced_category", create simple dichotomy
      if (method == "reduced_category") {
        edges <- edges %>% 
          mutate(!!sym((paste0("same_", variable))) := 
                   case_when(!!sym(paste0(variable, "_alter")) == 
                               !!sym(paste0(variable, "_ego")) ~ 1, 
                             is.na(!!sym(paste0(variable, "_ego"))) |
                               is.na(!!sym(paste0(variable, "_alter"))) ~ NA_real_, T ~ 0))
      }
      
      # If method "multi_category", create an all_of(variable) for each value and then dichotomize. 
      if (method == "multi_category") {
        opts <- nodes %>% select(all_of(variable)) %>% distinct() %>% tidyr::drop_na() %>% pull()
        
        for (n in 1:length(opts)) {
          edges <- edges %>% 
            mutate(!!sym((paste0("both_", variable, "_", opts[n]))) := 
                     case_when((!!sym(paste0(variable, "_alter")) == opts[n]) &
                                 (!!sym(paste0(variable, "_ego")) == opts[n]) ~ 1, 
                               is.na(!!sym(paste0(variable, "_ego"))) |
                                 is.na(!!sym(paste0(variable, "_alter"))) ~ NA_real_, T ~ 0))
        }
      }
      
      # If method "difference", take the difference between ego and alter
      if (method == "difference") {
        edges <- edges %>% 
          mutate(!!sym((paste0("diff_", variable))) := 
                   as.numeric(!!sym(paste0(variable, "_ego"))) -  
                   as.numeric(!!sym(paste0(variable, "_alter"))))          
      }
      
      # If diff is "both", run both reduced and multi categories.
      if (method == "both") {
        edges <- edges %>% 
          mutate(!!sym((paste0("same_", variable))) := 
                   case_when(!!sym(paste0(variable, "_alter")) == 
                               !!sym(paste0(variable, "_ego")) ~ 1, 
                             is.na(!!sym(paste0(variable, "_ego"))) |
                               is.na(!!sym(paste0(variable, "_alter"))) ~ NA_real_, T ~ 0))
        
        opts <- nodes %>% select(all_of(variable)) %>% distinct() %>% tidyr::drop_na() %>% pull()
        
        for (n in 1:length(opts)) {
          edges <- edges %>% 
            mutate(!!sym((paste0("both_", variable, "_", opts[n]))) := 
                     case_when((!!sym(paste0(variable, "_alter")) == opts[n]) &
                                 (!!sym(paste0(variable, "_ego")) == opts[n]) ~ 1, 
                               is.na(!!sym(paste0(variable, "_ego"))) |
                                 is.na(!!sym(paste0(variable, "_alter"))) ~ NA_real_, T ~ 0))
        }
      }
    }
  } 
  
  qap_graph <- graph_from_data_frame(edges, directed = directed, vertices = nodes)
  qap_results <<- list(qap_graph, nodes, edges)
}

# Takes a network of igraph object, a dependent variable and set of covariates. 
# Note that dependent variable default will default to binary tie prediction.

qap_run <- function(net, dependent = NULL, variables, directed = T, family = "linear") {
  
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
    res <- netlm(dv, ivs, reps = 10, mode = mode)
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
