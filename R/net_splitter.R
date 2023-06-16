###########################################
#                                         #
#    N E T W O R K   S P L I T T I N G    #
#                                         #
###########################################

# This function is a prerequisite for using `netwrite` on multi-relational networks.
# It takes as arguments the standard vectors of ego IDs, alter IDs, edge weights, and
# relation types for edgelists.

# Net Splitting
net_splitter <- function(i_elements = NULL, j_elements = NULL, weights=NULL, type=NULL){
  # Creating DataFrame for the Purposes of Splitting
  if (!is.null(weights) == TRUE) {
    master_edgelist <- cbind(as.data.frame(i_elements), j_elements, weights, type)
  }else{
    master_edgelist <-  cbind(as.data.frame(i_elements), j_elements, type)
  }

  # Create two list for input edgelists and output igraph objects
  edges_list <- vector('list', length(unique(type))+1)
  names(edges_list) <- c(unique(type), 'summary_graph')

  # Looping through the types
  for(i in 1:length(edges_list)){
    # Isolating type
    iteration_type <- names(edges_list)[[i]]

    # Isolate edges by type
    iteration_df <- master_edgelist[(master_edgelist$type == iteration_type), ]
    iteration_df$type <- NULL

    # Populating edges_list
    edges_list[[i]] <- iteration_df
  }
  edges_list[[length(edges_list)]] <- master_edgelist

  # Return edges_list
  return(edges_list)
}
