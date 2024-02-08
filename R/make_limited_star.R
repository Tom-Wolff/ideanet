make_limited_star <- function(n, max_degree, mode) {

# Make a vector of node IDs
nodes <- data.frame(id = 1:n)

# Assign "Waves" of nodes to add to edgelist based on network size and maximum degree
if (n > max_degree + 1) {
  wave_seq <- c(1, rep(2, max_degree), rep(NA, (n-(max_degree+1))))

  while (NA %in% wave_seq) {
    wave <- max(wave_seq, na.rm = TRUE) + 1
    # How many people are in the next wave?
    len_this_wave <- sum(wave_seq == (wave-1), na.rm = TRUE)*(max_degree-1)
    first_na <- min(which(is.na(wave_seq)))
    if ((first_na + len_this_wave - 1) >= length(wave_seq)) {
      wave_seq[is.na(wave_seq)] <- wave
    } else {
      wave_seq[first_na:(first_na+len_this_wave-1)] <- wave
    }
  }

} else {
  wave_seq <- c(1, rep(2, n-1))
}

# Update nodelist to include wave information
nodes$wave <- wave_seq
max_wave <- max(wave_seq)

# Generate Edgelist
  for (i in 1:(max_wave-1)) {

    this_wave <- i
    next_wave <- this_wave + 1

    these_nodes <- nodes$id[nodes$wave == this_wave]
    next_nodes <- nodes$id[nodes$wave == next_wave]

    this_el <- data.frame(ego = next_nodes,
               alter = rep(these_nodes, length.out = length(next_nodes)))

    if (i == 1) {
      edgelist <- this_el
    } else {
      edgelist <- rbind(edgelist, this_el)
    }
  }

if (mode == "undirected") {
  directed <-  FALSE
} else if (mode == "in") {
  directed <-  TRUE
} else if (mode == "out") {
  edgelist <- data.frame(ego = edgelist$alter,
                         alter = edgelist$ego)
  directed <- TRUE
} else {
  stop("Incorrect mode specified")
}

# Create igraph object
star_graph <- igraph::graph_from_data_frame(edgelist, directed = directed,
                                            vertices = nodes)
# Return igraph object
return(star_graph)

}

# test <- make_limited_star(n = 200, max_degree = 4, mode = "undirected")
#
# plot(test, vertex.color = igraph::V(test)$wave, layout = igraph::layout_as_tree(test))
#
# igraph::degree(test)
#
# data.frame(id = igraph::V(test)$name,
#            wave = igraph::V(test)$wave,
#            degree = igraph::degree(test))
