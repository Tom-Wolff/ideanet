#' Gather a collection of community detection partitions (\code{get_partitions})
#'
#' @description The \code{get_partitions} function is a wrapper to gather a collection of community detection partitions using igraph's \code{cluster_leiden} for maximizing modularity at various resolution parameter values, along with the routines called by the \code{comm_detect} function, to gather different partitions for subsequent input to the \code{CHAMP} code for post-processing partitions to identify domains of modularity optimization.
#'
#' @param network The network, as igraph object, to be clustered into communities. Only undirected networks are currently supported. If the object has a 'weight' edge attribute, then that attribute will be used.
#' @param gamma_range The range of the resolution parameter gamma (default from 0 to 4).
#' @param n_runs The number of \code{cluster_leiden} runs to be attempted (default = 100).
#' @param n_iterations Parameter to be passed to cluster_leiden (default = 2).
#' @param seed Optional random seed for reproducing pseudo-random results.
#' @param add_comm_detect Boolean to decide whether to also call the clustering algorithms included in \code{comm_detect} (default = T). Alternatively, the output of \code{comm_detect} can be provided directly here.
#'
#' @returns \code{get_partitions} returns a list of unique partitions appropriate for subsequent input to \code{CHAMP}.
#' @import igraphdata
#'
#' @author Peter J. Mucha (\email{peter.j.mucha@dartmouth.edu}), Alex Craig, Rachel Matthew, Sydney Rosenbaum and Ava Scharfstein
#'
#' @export
#'
#' @examples
#' # Use get_partitions to generate multiple partitions of the
#' # Zachary karate club at different resolution parameters
#' data(karate, package = "igraphdata")
#' partitions <- get_partitions(karate, n_runs = 2500)

###################################
#   G E T  P A R T I T I O N S    #
###################################

#PJM: 7.15.2024. Gathered bits of code from Alex Craig, Rachel Matthew, Sydney Rosenbaum and Ava Scharfstein into add_champ branch
#PJM: 7.30.2024. Adding check for undirected network.
#PJM: 8.26.2024. Changed gamma_range default to improve behavior.
#PJM: 3.07.2025. Added comm_detect() to methods considered.

get_partitions <- function( network,
                            gamma_range=c(0,3),
                            n_runs=100,
                            n_iterations=2, #parameter for cluster_leiden
                            seed=NULL,
                            add_comm_detect=TRUE) {

  # browser()

  # Check input network is undirected
  if (igraph::is_directed(network)) {stop("Input is directed. Only undirected networks are currently supported.")}

  # If seed defined, use it to set the random seed for reproducibility
  if (!is.null(seed)) {set.seed(seed)}

  # If gamma_range has more than two values, treat as a list of desired gamma
  # resolution parameter values. Otherwise, generate uniform random collection
  # of gamma values between the min (>=0) and max (>=1) value. If there is only one value in
  # gamma_range, treat it as the max value and set the min to zero.
  if (length(gamma_range)==1) {
    gamma = stats::runif(n_runs,0,gamma_range)
  } else if (length(gamma_range)==2) {
    gamma = stats::runif(n_runs,max(min(gamma_range),0),max(gamma_range,1))
  } else {gamma = gamma_range}
  #print(gamma)

  # Call cluster_leiden multiple times, saving results to partitions
  nc <- vector("numeric",length(gamma))
  partitions <- list()
  for (i in 1:length(gamma)) {
    gc <- igraph::cluster_leiden(network, objective_function = "modularity",
                                 resolution = gamma[i],
                                 n_iterations = n_iterations)
    gc$gamma <- gamma[i]
    # cluster number
    cn <- length(gc)
    cur <- partitions[cn]
    # if there are no current partitions for this cn
    if (is.na(cur) || is.null(cur[[1]])) {
      partitions[[cn]] <- list(gc)
    } else {
      cur <- partitions[[cn]]
      partitions[[cn]][[length(cur)+1]] <- gc
    }
    nc[i] <- cn
    #partitions[[cn]]
  }

  # Now we fold in the comm_detect() results, running comm_detect() if
  # add_comm_detect is True, or directly using provided comm_detect() output
  if (is.logical(add_comm_detect)) {
    if (add_comm_detect == T) {
      grDevices::pdf(file = NULL) #Redirecting the graphical output of comm_detect()
      add_comm_detect <- comm_detect(network)
      grDevices::dev.off()
    }
  }
  if (min(c("memberships", "summaries", "score_comparison") %in%
          names(add_comm_detect)) == 1) {
    membs <- add_comm_detect$memberships #We only need
    for (i in 2:ncol(membs)) {
      memb <- membs[,i]
      #if (min(memb)==0) {memb <- memb + 1} #Some comm_detect() memberships count from 0?
      names(memb) <- igraph::V(network)$name
      gc <- igraph::make_clusters(network, membership=memb,
                                  algorithm=colnames(membs)[i], modularity = F)
      gc$gamma <- 0 #I don't think this gets used later, but setting for consistency
      # cluster number
      cn <- length(gc)
      cur <- partitions[cn]
      # if there are no current partitions for this cn
      if (is.na(cur) || is.null(cur[[1]])) {
        partitions[[cn]] <- list(gc)
      } else {
        cur <- partitions[[cn]]
        partitions[[cn]][[length(cur)+1]] <- gc
      }
      nc[length(gamma)-1+i] <- cn #This is bad form to be increasing the length of this vector
    }
  }

  # unique_partitions filters out duplicate partitions
  partitions <- unique_partitions(partitions)

  # best_partitions processes the output of unique_partitions to format it
  # appropriately for the following steps. The function can also remove
  # partitions that were only generated less than min times (but we take min=1,
  # effectively removing this functionality)
  partitions <- best_partitions(partitions$count, partitions$partitions, min=1)

  # Save gamma min & max used so they can be used in CHAMP.R
  partitions$gamma_min <- min(gamma_range)
  partitions$gamma_max <- max(gamma_range)

  cat('\n')
  if (is.logical(add_comm_detect)) { #At this point, if it is a logical, it's False
    print(paste(length(partitions$partitions),
                "unique partitions generated by cluster_leiden()"))
  } else {
    print(paste(length(partitions$partitions),
                "unique partitions generated between cluster_leiden() and comm_detect()"))
  }

  return(partitions)

}

###################################

# Given a list of partitions, finds the unique ones
# and the number of times each unique partition appeared.
unique_partitions <- function(partitions) {
  # number of distinct partitions
  partition_count <- vector("list", length = length(partitions))
  # which partitions are distinct
  distinct_partitions <- vector("list", length = length(partitions))
  for (i in 1:length(partitions)) { #The first index is the number of communities
    clustersi <- partitions[[i]]
    if (!is.null(clustersi)) { #Only proceed if found a partition with this number
      partition_count[i] <- list(rep(1,length(clustersi)))
      distinct_partitions[[i]] <- clustersi
      # only want to iterate if length of clustersi is greater than 1 (that is, found more than once)
      if (length(clustersi)>1) {
        for (j in 1:(length(clustersi)-1)) {
          # if cluster j is not a duplicate
          if (partition_count[[i]][j]!=0) {
            for (k in length(clustersi):(j+1)) {
              # if cluster k is not a duplicate
              if (partition_count[[i]][k]!=0) {
                cmp <- igraph::compare(clustersi[[j]],clustersi[[k]])
                if (cmp==0) {
                  partition_count[[i]][k] <- 0
                  partition_count[[i]][j] <- partition_count[[i]][j]+1
                  #distinct_partitions[[i]][k] <- NULL
                }
              }
            }
          }
        }
      }
      else {
        partition_count[[i]][1] <- 1
      }
    }
  }

  # clean up distinct_partitions and partition_count (remove 0s and NAs)
  for (i in 1:length(distinct_partitions)) {
    if (!is.null(distinct_partitions[[i]])) {
      distinct_partitions[[i]] <- distinct_partitions[[i]][c(partition_count[[i]]!=0)]
      partition_count[[i]] <- partition_count[[i]][partition_count[[i]]!=0] # eliminate zeros
    }
  }

  return(list("count" = partition_count, "partitions" = distinct_partitions))
}

###################################

# c = partition count, p = distinct partitions, min = minimum count value
# Eliminates partitions that have a partition count below the min
best_partitions <- function(c, p, min){
  final_count <- list()
  final_partitions <- list()
  for (i in 1:length(c)) {
    countsi <- c[[i]]
    if (!is.null(countsi)) {
      for (j in 1:length(countsi)) {
        if (countsi[[j]] >= min) {
          final_count <- append(final_count, countsi[[j]])
          final_partitions <- append(final_partitions, p[[i]][j])
        }
      }
    }
  }
  return(list("count" = final_count, "partitions" = final_partitions))
}
