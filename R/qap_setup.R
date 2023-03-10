#' Individual to Dyadic variable transformation (`qap_setup`).
#'
#' @description The `qap_setup` function transform an individual level attributes into dyadic comparisons following a set of methods. Output can be used to compute QAP measurements using sister functions in IDEAnet package.
#'
#' @param net An `igraph` or `network` object.
#' @param variables A vector of strings naming attributes to be transformed from individual-level to dyadic-level.
#' @param methods A vector of strings naming methods to be applied to the `variables` vector. The `methods` vector must be the same length as the `variables` vector. Methods are applied in order (e.g, first method is applied to the first named attribute in `variables`)
#' @param directed A logical statement identifying if the network should be treated as directed. Defaults to False.
#' @param directed A logical statement identifying if the network should be treated as directed. Defaults to False. 
#' @param additional_vars A data frame containing additional individual-level variables not contained in the primary network input. Additional dataframe must contain an `id` or `label` variables which matches network exactly.
#' @return `qap_setup` returns a list `qap_results` of elements that include: 
#' 
#' - `qap_graph`, an updated `igraph` object containing the newly constructed dyadic variables and additional individual-level variables.
#' 
#' - `nodes`, a nodelist reflecting additional variables if included.
#' 
#' - `edges`, a nodelist reflecting new dyadic variables.
#' @export
#'
#' @examples
#' qap_setup(net = igraph_object,
#'          variables = c("attribute_1", "attribute_2", "attribute_3"),
#'          methods = c("method_1", "method_2", "method_3"),
#'          directed = F,
#'          additional_vars = attribute_data)

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
  if (!("id" %in% list.vertex.attributes(net))) {
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