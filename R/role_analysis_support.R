#' @importFrom rlang .data

# Support functions for role analysis tool

####################################################################
# Converting igraph objects into adjacency matrices
####################################################################

to_adjmats <- function(graph_list) {

  mat_list <- list()

  # For each igraph object in `graph_list`
  for (i in 1:length(graph_list)) {
    # Convert to adjancency matrix
    mat <- as.matrix(igraph::as_adjacency_matrix(graph_list[[i]], attr = "weight"))
    # Store in `mat_list`
    mat_list[[i]] <- mat
  }

  # Keep names in `mat_list`
  names(mat_list) <- names(graph_list)

  return(mat_list)

}

####################################################################
# Basic triad/position census counter
####################################################################

f.rc <- function(mat=NULL,REGE=FALSE,HUSO=FALSE,MES=FALSE, CHECK=TRUE)
{
  # Calculates a rolecensus/node-level triadcensus according to H.J. Hummell/W. Sodeur (1987)
  # and R. Burt (1990) in the sequence of Burt; cf.
  # http://www.uni-duisburg-essen.de/hummell/rolecensus/
  # Graphical description of 36 "triadic positional types" of rolecensus
  # in the notation von R. Burt:
  # http://www.uni-duisburg-essen.de/hummell/pdf/RoleCensus.pdf
  #
  # Input: Binary quadrat. matrix with diagonal entries to 0
  # <REGE=T> transforms rolecensus into binary values (checks only for
  # presence/absence of rolecensus types a la REGE)
  # Author: H.J. Hummell
  # 30.06.2013 14:51:12

  TEXT0 <- 'Calculates a triad census on the level of nodes\n'
  TEXT1 <- 'according to: H.J.Hummell & W.Sodeur(1987) "Positionenzensus" and Ronald Burt(1990) "Rolecensus"\n'
  TEXT2 <- 'Info: http://www.uni-duisburg-essen.de/hummell/rolecensus/ \n'
  TEXT3 <- 'Ordering of the 36 different "triadic positions" according to R.Burt\n'
  TEXT4 <- 'Info: http://www.uni-duisburg-essen.de/hummell/pdf/RoleCensus.pdf\n'
  TEXT5 <- 'Parameters: <mat=NULL>, <REGE=F>, <HUSO=F>, <MES=F>, <CHECK=T>\n'
  TEXT6 <- '<REGE=T> transforms rolecensus into binary values (checks only for presence/absence of role types a la REGE)\n'
  TEXT7 <- '<HUSO=T> gives Hummell/Sodeurs original sequence of "triadic positions"\n'
  TEXT8 <- 'Info: http://www.uni-duisburg-essen.de/hummell/rolecensus/PositionenZensus.jpg\n'
  TEXT9 <- '<MES=T> gives the sequence according to Solomon Messing from the <triads> package\n'
  TEXT10 <- 'Rather slow routine!\n'
  AUT <- 'H.J.Hummell; 30.06.2013.\n'

  if (length(mat)==0)
  {

    message(paste0(TEXT0,
                   TEXT2,
                   TEXT3,
                   TEXT4,
                   TEXT5,
                   TEXT6,
                   TEXT7,
                   TEXT8,
                   TEXT9,
                   TEXT10,
                   AUT,
                   collapse = "\n"
                   ))
    return(NULL)
  }

  if (HUSO==TRUE & MES==TRUE) {stop("<HUSO> and <MES> cannot both be true!\n\n\a")}

  # CHECK Soziomatrix
  if (CHECK==TRUE)
  {
    if (!is.matrix(mat)) {stop("Not a matrix!\n\n\a")}
    g1 <- dim(mat)[1]
    g2 <- dim(mat)[2]
    if ( g1 != g2 ) {stop("Matrix is not quadratic!\n\n\a")}
    l0 <- length(which(mat==0))
    l1 <- length(which(mat==1))
    l <- l0+l1
    if(l != g1*g2) {stop("Not a binary matrix!\n\n\a")}
    D <- sum(abs(diag(mat)))
    if (D != 0) {stop("Diagonal values are not zero!\n\n\a")}
  }
  # END CHECK


  x <- mat
  g <- dim(x)[1]
  z <- matrix(0,g,36)
  for (i in(1:g))
  {
    for (j in(1:g))
    {
      for (k in(1:g))
      {
        #################################################################################################
        #
        # Mapping of 64 (labeled) triads to 36 different "triadic postions" of triad-members i,j,k
        # according to H.J.Hummell & W. Sodeur, 1987, p. 186 (Abbildung 3)
        # http://www.uni-duisburg-essen.de/hummell/rolecensus/Triads2TriadicPositions.pdf
        # For numbering of 36 "triadic positions" see Hummell/Sodeur, 1987, p. 188 (Abbildung 5)
        # http://www.uni-duisburg-essen.de/hummell/rolecensus/PositionenZensus.jpg
        #
        if ((i!=j) & (i!=k) & (j!=k))
        {
          TRIAD <- (x[i,j] + 2*x[i,k] +  4*x[j,i] +  8*x[j,k] + 16*x[k,i] + 32*x[k,j] + 1)
          if (TRIAD < 1 | TRIAD > 64) {stop('Error in calculating TRIAD numbers!\n\n\a')}
          ################## triads no. 1 to 16
          if (TRIAD ==  1) {z[i, 1]<-z[i, 1]+1;z[j, 1]<-z[j, 1]+1;z[k, 1]<-z[k, 1]+1;next() }
          if (TRIAD ==  2) {z[i, 2]<-z[i, 2]+1;z[j, 4]<-z[j, 4]+1;z[k, 8]<-z[k, 8]+1;next() }
          if (TRIAD ==  3) {z[i, 2]<-z[i, 2]+1;z[j, 8]<-z[j, 8]+1;z[k, 4]<-z[k, 4]+1;next() }
          if (TRIAD ==  4) {z[i, 3]<-z[i, 3]+1;z[j,12]<-z[j,12]+1;z[k,12]<-z[k,12]+1;next() }

          if (TRIAD ==  5) {z[i, 4]<-z[i, 4]+1;z[j, 2]<-z[j, 2]+1;z[k, 8]<-z[k, 8]+1;next() }
          if (TRIAD ==  6) {z[i, 5]<-z[i, 5]+1;z[j, 5]<-z[j, 5]+1;z[k,27]<-z[k,27]+1;next() }
          if (TRIAD ==  7) {z[i, 6]<-z[i, 6]+1;z[j, 9]<-z[j, 9]+1;z[k,19]<-z[k,19]+1;next() }
          if (TRIAD ==  8) {z[i, 7]<-z[i, 7]+1;z[j,13]<-z[j,13]+1;z[k,30]<-z[k,30]+1;next() }

          if (TRIAD ==  9) {z[i, 8]<-z[i, 8]+1;z[j, 2]<-z[j, 2]+1;z[k, 4]<-z[k, 4]+1;next() }
          if (TRIAD == 10) {z[i, 9]<-z[i, 9]+1;z[j, 6]<-z[j, 6]+1;z[k,19]<-z[k,19]+1;next() }
          if (TRIAD == 11) {z[i,10]<-z[i,10]+1;z[j,10]<-z[j,10]+1;z[k,16]<-z[k,16]+1;next() }
          if (TRIAD == 12) {z[i,11]<-z[i,11]+1;z[j,14]<-z[j,14]+1;z[k,23]<-z[k,23]+1;next() }

          if (TRIAD == 13) {z[i,12]<-z[i,12]+1;z[j, 3]<-z[j, 3]+1;z[k,12]<-z[k,12]+1;next() }
          if (TRIAD == 14) {z[i,13]<-z[i,13]+1;z[j, 7]<-z[j, 7]+1;z[k,30]<-z[k,30]+1;next() }
          if (TRIAD == 15) {z[i,14]<-z[i,14]+1;z[j,11]<-z[j,11]+1;z[k,23]<-z[k,23]+1;next() }
          if (TRIAD == 16) {z[i,15]<-z[i,15]+1;z[j,15]<-z[j,15]+1;z[k,34]<-z[k,34]+1;next() }
          ################# triads no. 17 to 32
          if (TRIAD == 17) {z[i, 4]<-z[i, 4]+1;z[j, 8]<-z[j, 8]+1;z[k, 2]<-z[k, 2]+1;next() }
          if (TRIAD == 18) {z[i, 6]<-z[i, 6]+1;z[j,19]<-z[j,19]+1;z[k, 9]<-z[k, 9]+1;next() }
          if (TRIAD == 19) {z[i, 5]<-z[i, 5]+1;z[j,27]<-z[j,27]+1;z[k, 5]<-z[k, 5]+1;next() }
          if (TRIAD == 20) {z[i, 7]<-z[i, 7]+1;z[j,30]<-z[j,30]+1;z[k,13]<-z[k,13]+1;next() }

          if (TRIAD == 21) {z[i,16]<-z[i,16]+1;z[j,10]<-z[j,10]+1;z[k,10]<-z[k,10]+1;next() }
          if (TRIAD == 22) {z[i,17]<-z[i,17]+1;z[j,21]<-z[j,21]+1;z[k,28]<-z[k,28]+1;next() }
          if (TRIAD == 23) {z[i,17]<-z[i,17]+1;z[j,28]<-z[j,28]+1;z[k,21]<-z[k,21]+1;next() }
          if (TRIAD == 24) {z[i,18]<-z[i,18]+1;z[j,31]<-z[j,31]+1;z[k,31]<-z[k,31]+1;next() }

          if (TRIAD == 25) {z[i,19]<-z[i,19]+1;z[j, 9]<-z[j, 9]+1;z[k, 6]<-z[k, 6]+1;next() }
          if (TRIAD == 26) {z[i,20]<-z[i,20]+1;z[j,20]<-z[j,20]+1;z[k,20]<-z[k,20]+1;next() }
          if (TRIAD == 27) {z[i,21]<-z[i,21]+1;z[j,28]<-z[j,28]+1;z[k,17]<-z[k,17]+1;next() }
          if (TRIAD == 28) {z[i,22]<-z[i,22]+1;z[j,32]<-z[j,32]+1;z[k,24]<-z[k,24]+1;next() }

          if (TRIAD == 29) {z[i,23]<-z[i,23]+1;z[j,11]<-z[j,11]+1;z[k,14]<-z[k,14]+1;next() }
          if (TRIAD == 30) {z[i,24]<-z[i,24]+1;z[j,22]<-z[j,22]+1;z[k,32]<-z[k,32]+1;next() }
          if (TRIAD == 31) {z[i,25]<-z[i,25]+1;z[j,29]<-z[j,29]+1;z[k,25]<-z[k,25]+1;next() }
          if (TRIAD == 32) {z[i,26]<-z[i,26]+1;z[j,33]<-z[j,33]+1;z[k,35]<-z[k,35]+1;next() }
          ################# triads no. 33 to 48
          if (TRIAD == 33) {z[i, 8]<-z[i, 8]+1;z[j, 4]<-z[j, 4]+1;z[k, 2]<-z[k, 2]+1;next() }
          if (TRIAD == 34) {z[i,10]<-z[i,10]+1;z[j,16]<-z[j,16]+1;z[k,10]<-z[k,10]+1;next() }
          if (TRIAD == 35) {z[i, 9]<-z[i, 9]+1;z[j,19]<-z[j,19]+1;z[k, 6]<-z[k, 6]+1;next() }
          if (TRIAD == 36) {z[i,11]<-z[i,11]+1;z[j,23]<-z[j,23]+1;z[k,14]<-z[k,14]+1;next() }

          if (TRIAD == 37) {z[i,19]<-z[i,19]+1;z[j, 6]<-z[j, 6]+1;z[k, 9]<-z[k, 9]+1;next() }
          if (TRIAD == 38) {z[i,21]<-z[i,21]+1;z[j,17]<-z[j,17]+1;z[k,28]<-z[k,28]+1;next() }
          if (TRIAD == 39) {z[i,20]<-z[i,20]+1;z[j,20]<-z[j,20]+1;z[k,20]<-z[k,20]+1;next() }
          if (TRIAD == 40) {z[i,22]<-z[i,22]+1;z[j,24]<-z[j,24]+1;z[k,32]<-z[k,32]+1;next() }

          if (TRIAD == 41) {z[i,27]<-z[i,27]+1;z[j, 5]<-z[j, 5]+1;z[k, 5]<-z[k, 5]+1;next() }
          if (TRIAD == 42) {z[i,28]<-z[i,28]+1;z[j,17]<-z[j,17]+1;z[k,21]<-z[k,21]+1;next() }
          if (TRIAD == 43) {z[i,28]<-z[i,28]+1;z[j,21]<-z[j,21]+1;z[k,17]<-z[k,17]+1;next() }
          if (TRIAD == 44) {z[i,29]<-z[i,29]+1;z[j,25]<-z[j,25]+1;z[k,25]<-z[k,25]+1;next() }

          if (TRIAD == 45) {z[i,30]<-z[i,30]+1;z[j, 7]<-z[j, 7]+1;z[k,13]<-z[k,13]+1;next() }
          if (TRIAD == 46) {z[i,31]<-z[i,31]+1;z[j,18]<-z[j,18]+1;z[k,31]<-z[k,31]+1;next() }
          if (TRIAD == 47) {z[i,32]<-z[i,32]+1;z[j,22]<-z[j,22]+1;z[k,24]<-z[k,24]+1;next() }
          if (TRIAD == 48) {z[i,33]<-z[i,33]+1;z[j,26]<-z[j,26]+1;z[k,35]<-z[k,35]+1;next() }
          ################# triads no. 49 to 64
          if (TRIAD == 49) {z[i,12]<-z[i,12]+1;z[j,12]<-z[j,12]+1;z[k, 3]<-z[k, 3]+1;next() }
          if (TRIAD == 50) {z[i,14]<-z[i,14]+1;z[j,23]<-z[j,23]+1;z[k,11]<-z[k,11]+1;next() }
          if (TRIAD == 51) {z[i,13]<-z[i,13]+1;z[j,30]<-z[j,30]+1;z[k, 7]<-z[k, 7]+1;next() }
          if (TRIAD == 52) {z[i,15]<-z[i,15]+1;z[j,34]<-z[j,34]+1;z[k,15]<-z[k,15]+1;next() }

          if (TRIAD == 53) {z[i,23]<-z[i,23]+1;z[j,14]<-z[j,14]+1;z[k,11]<-z[k,11]+1;next() }
          if (TRIAD == 54) {z[i,25]<-z[i,25]+1;z[j,25]<-z[j,25]+1;z[k,29]<-z[k,29]+1;next() }
          if (TRIAD == 55) {z[i,24]<-z[i,24]+1;z[j,32]<-z[j,32]+1;z[k,22]<-z[k,22]+1;next() }
          if (TRIAD == 56) {z[i,26]<-z[i,26]+1;z[j,35]<-z[j,35]+1;z[k,33]<-z[k,33]+1;next() }

          if (TRIAD == 57) {z[i,30]<-z[i,30]+1;z[j,13]<-z[j,13]+1;z[k, 7]<-z[k, 7]+1;next() }
          if (TRIAD == 58) {z[i,32]<-z[i,32]+1;z[j,24]<-z[j,24]+1;z[k,22]<-z[k,22]+1;next() }
          if (TRIAD == 59) {z[i,31]<-z[i,31]+1;z[j,31]<-z[j,31]+1;z[k,18]<-z[k,18]+1;next() }
          if (TRIAD == 60) {z[i,33]<-z[i,33]+1;z[j,35]<-z[j,35]+1;z[k,26]<-z[k,26]+1;next() }

          if (TRIAD == 61) {z[i,34]<-z[i,34]+1;z[j,15]<-z[j,15]+1;z[k,15]<-z[k,15]+1;next() }
          if (TRIAD == 62) {z[i,35]<-z[i,35]+1;z[j,26]<-z[j,26]+1;z[k,33]<-z[k,33]+1;next() }
          if (TRIAD == 63) {z[i,35]<-z[i,35]+1;z[j,33]<-z[j,33]+1;z[k,26]<-z[k,26]+1;next() }
          if (TRIAD == 64) {z[i,36]<-z[i,36]+1;z[j,36]<-z[j,36]+1;z[k,36]<-z[k,36]+1;next() }
        }
      }
    }
  }

  # All "triadic positions" are counted 6 times
  z <- z/6

  if (HUSO==F)
  {
    # Reordering of 36 "triadic positions" in the sequence of Hummell/Sodeur
    # into the sequence of R. Burt, 1990
    # http://www.uni-duisburg-essen.de/hummell/pdf/RoleCensus.pdf
    v1 <- c(1, 2, 3, 4, 6, 8, 9,21,22,31,23,24)
    v2 <- c(26,28,29, 5,10, 7,32,34,33,35,25,30)
    v3 <- c(36,27,11,12,13,14,16,18,19,15,20,17)
    v<- cbind(v1,v2,v3)
    zz <- matrix(0,g,36)
    for (i in (1:36))
    {
      zz[,v[i]] <- z[,i]
    }
    #

    z <- zz}

  if(MES==TRUE)
  {
    v1 <- c( 1, 2, 7, 3,10, 5,25,12,18,15, 6,14)
    v2 <- c(26,19,28,24,36,31,33,34, 4,11,20, 8)
    v3 <- c(22,17,35,21,29,32, 9,13,16,23,30,27)
    v<- cbind(v1,v2,v3)
    zz <- matrix(0,g,36)
    for (i in (1:36))
    {
      zz[,v[i]] <- z[,i]
    }
    #
    z <- zz
  }

  if (REGE==TRUE)
  {
    z[z>0] <- 1
  }

  return(z)
}

####################################################################
# Expedited triad/position census counter (custom, dplyr based)
####################################################################

positions_dplyr <- function(nodes = NULL, edges, directed = FALSE) {

  # If nodelist isn't specified, infer from edgelist
  if (is.null(nodes)) {
    nodes <- unique(c(edges[,1], edges[,2]))
  }

  # 1. Create every possible triad in the network
  # all_triads <- tidyr::expand_grid(i = nodes, j = nodes, k = nodes) %>%
  #   filter(i != j) %>%
  #   filter(i != k) %>%
  #   filter(j != k)

  # POSITIONAL COUNTS FOR DIRECTED NETWORKS
  if (directed == TRUE) {

    # Make first edgelist for merging
    ij <- edges[,1:2]
    ij$tie <- 1
    colnames(ij) <- c("i", "j", "tie_ij")

    # Make other edgelists for merging
    ji <- ij
    colnames(ji) <- c("j", "i", "tie_ji")

    jk <- ij
    colnames(jk) <- c("j", "k", "tie_jk")

    kj <- ij
    colnames(kj) <- c("k", "j", "tie_kj")

    ik <- ij
    colnames(ik) <- c("i", "k", "tie_ik")

    ki <- ij
    colnames(ki) <- c("k", "i", "tie_ki")

    # Merge pairs together to get ij,ji edges, etc.
    el <- dplyr::full_join(ij, ji, by = c("i", "j"))
    el2 <- dplyr::full_join(jk, kj, by = c("j", "k"))
    el3 <- dplyr::full_join(ik, ki, by = c("i", "k"))

    # To make sure triad identification is exhausted, we
    # merge edgelists into triad lists in all possible orders.
    # This avoids the possibility of missing triads in which
    # node i isn't connected to either j or k

    el_123 <- el %>% dplyr::full_join(el2, by = 'j') %>%
      dplyr::full_join(el3, by = c("i", "k")) %>%
      dplyr::select(.data$i, .data$j, .data$k, dplyr::everything())

    el_132 <- el %>% dplyr::full_join(el3, by = "i") %>%
      dplyr::full_join(el2, by = c("j", "k"))

    el_213 <- el2 %>% dplyr::full_join(el, by = "j") %>%
      dplyr::full_join(el3, by = c("i", "k"))

    el_231 <- el2 %>% dplyr::full_join(el3, by = "k") %>%
      dplyr::full_join(el, by = c("i", "j"))

    el_312 <- el3 %>% dplyr::full_join(el, by = "i") %>%
      dplyr::full_join(el2, by = c("j", "k"))

    el_321 <- el3 %>% dplyr::full_join(el2, by = "k") %>%
      dplyr::full_join(el, by = c("i", "j"))

    el_together <- dplyr::bind_rows(el_123,
                                    el_132,
                                    el_231,
                                    el_213,
                                    el_312,
                                    el_321)

    el_together[is.na(el_together)] <- 0

    el_together <- el_together %>%
      dplyr::mutate(num_ties = .data$tie_ij + .data$tie_ji + .data$tie_jk + .data$tie_kj
                    + .data$tie_ik + .data$tie_ki) %>%
      dplyr::group_by(.data$i, .data$j, .data$k) %>%
      dplyr::mutate(triad_id = dplyr::cur_group_id()) %>%
      dplyr::arrange(.data$triad_id, dplyr::desc(.data$num_ties)) %>%
      dplyr::slice(1) %>%
      dplyr::filter(.data$i != .data$j) %>%
      dplyr::filter(.data$j != .data$k) %>%
      dplyr::filter(.data$i != .data$k) %>%
      dplyr::select(-.data$triad_id, -.data$num_ties)




    # all_triads <- all_triads %>%
    #   left_join(el1, by = c("i", "j")) %>%
    #   left_join(el2, by = c("j", "i")) %>%
    #   left_join(el3, by = c("j", "k")) %>%
    #   left_join(el4, by = c('k', 'j')) %>%
    #   left_join(el5, by = c("i", "k")) %>%
    #   left_join(el6, by = c("k", "i"))
    #
    # all_triads[is.na(all_triads)] <- 0


    # Create every possible directed tie combination for triads
    all_combos <- tidyr::expand_grid(tie_ij = 0:1,
                                     tie_ji = 0:1,
                                     tie_jk = 0:1,
                                     tie_kj = 0:1,
                                     tie_ik = 0:1,
                                     tie_ki = 0:1)

    # Make numeric identifier of triad type
    all_combos$triad_type <- c("003",
                               "012_E",
                               "012_S",
                               "102_D",
                               "012_I",
                               "021D_E",
                               "021C_S",
                               "111U_S",
                               "012_I",
                               "021C_E",
                               "021U_S",
                               "111D_E",
                               "102_I",
                               "111U_E",
                               "111D_S",
                               "201_S",
                               "012_E",
                               "021U_E",
                               "021C_B",
                               "111D_B",
                               "021C_E",
                               "030T_E",
                               "030C",
                               "120C_E",
                               "021D_E",
                               "030T_E",
                               "030T_B",
                               "120D_E",
                               "111U_E",
                               "120U_E",
                               "120C_B",
                               "210_B",
                               "012_S",
                               "021C_B",
                               "021D_S",
                               "111U_B",
                               "021U_S",
                               "030T_B",
                               "030T_S",
                               "120U_S",
                               "021C_S",
                               "030C",
                               "030T_S",
                               "120C_S",
                               "111D_S",
                               "120C_B",
                               "120D_S",
                               "210_S",
                               "102_D",
                               "111D_B",
                               "111U_B",
                               "201_B",
                               "111D_E",
                               "120D_E",
                               "120C_S",
                               "210_E",
                               "111U_S",
                               "120C_E",
                               "120U_S",
                               "210_E",
                               "201_S",
                               "210_B",
                               "210_S",
                               "300")

    # Merge `all_combos` into complete triad list
    all_triads <- el_together %>%
      dplyr::left_join(all_combos, by = c("tie_ij",
                                          "tie_ji",
                                          "tie_jk",
                                          "tie_kj",
                                          "tie_ik",
                                          "tie_ki"))

    # POSITIONAL COUNTS FOR UNDIRECTED NETWORKS
  } else {

    # Make first edgelist for merging
    el <- edges[,1:2]
    el$tie <- 1
    colnames(el) <- c("i", "j", "tie_ij")

    # Make other edgelists
    el2 <- el
    colnames(el2) <- c("i", "k", "tie_ik")

    el3 <- el
    colnames(el3) <- c("j", "k", "tie_jk")


    # To make sure triad identification is exhausted, we
    # merge edgelists into triad lists in all possible orders.
    # This avoids the possibility of missing triads in which
    # node i isn't connected to either j or k

    el_123 <- el %>% dplyr::full_join(el2, by = 'i') %>%
      dplyr::full_join(el3, by = c("j", "k")) %>%
      dplyr::select(.data$i, .data$j, .data$k, dplyr::everything())

    el_132 <- el %>% dplyr::full_join(el3, by = "j") %>%
      dplyr::full_join(el2, by = c("i", "k"))

    el_213 <- el2 %>% dplyr::full_join(el, by = "i") %>%
      dplyr::full_join(el3, by = c("j", "k"))

    el_231 <- el2 %>% dplyr::full_join(el3, by = "k") %>%
      dplyr::full_join(el, by = c("i", "j"))

    el_312 <- el3 %>% dplyr::full_join(el, by = "j") %>%
      dplyr::full_join(el2, by = c("i", "k"))

    el_321 <- el3 %>% dplyr::full_join(el2, by = "k") %>%
      dplyr::full_join(el, by = c("i", "j"))

    el_together <- dplyr::bind_rows(el_123,
                                    el_132,
                                    el_231,
                                    el_213,
                                    el_312,
                                    el_321)

    el_together[is.na(el_together)] <- 0

    el_together <- el_together %>%
      dplyr::mutate(num_ties = .data$tie_ij + .data$tie_jk + .data$tie_ik) %>%
      dplyr::group_by(.data$i, .data$j, .data$k) %>%
      dplyr::mutate(triad_id = dplyr::cur_group_id()) %>%
      dplyr::arrange(.data$triad_id, dplyr::desc(.data$num_ties)) %>%
      dplyr::slice(1) %>%
      dplyr::filter(.data$i != .data$j) %>%
      dplyr::filter(.data$j != .data$k) %>%
      dplyr::filter(.data$i != .data$k) %>%
      dplyr::select(-.data$triad_id, -.data$num_ties)


    # all_triads <- all_triads %>%
    #   left_join(el1, by = c("i", "j")) %>%
    #   left_join(el2, by = c("i", "k")) %>%
    #   left_join(el3, by = c("j", "k"))
    #
    # all_triads[is.na(all_triads)] <- 0


    # Create every possible directed tie combination for triads
    all_combos <- tidyr::expand_grid(tie_ij = 0:1,
                                     tie_ik = 0:1,
                                     tie_jk = 0:1)

    all_combos$triad_type <- c("003",
                               "102_i",
                               "102_d",
                               "201_s",
                               "102_d",
                               "201_s",
                               "201_b",
                               "300")

    # Merge `all_combos` into complete triad list
    all_triads <- el_together %>%
      dplyr::left_join(all_combos, by = c("tie_ij",
                                          "tie_ik",
                                          "tie_jk"))


  }


  # Now we count how many of each triad type each ego has
  if (directed == TRUE) {

    triad_count <- all_triads %>%
      dplyr::group_by(.data$i, .data$triad_type) %>%
      dplyr::summarize(count = dplyr::n()/2) %>%
      tidyr::pivot_wider(names_from = .data$triad_type,
                         values_from = .data$count) %>%
      dplyr::ungroup()

  } else {

    triad_count <- all_triads %>%
      dplyr::group_by(.data$i, .data$triad_type) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      tidyr::pivot_wider(names_from = .data$triad_type,
                         values_from = .data$count) %>%
      dplyr::ungroup()

  }

  triad_count[is.na(triad_count)] <- 0

  colnames(triad_count) <- c("id", colnames(triad_count)[2:ncol(triad_count)])

  return(triad_count)

}

####################################################################
# Correlation between relation types
####################################################################

relation_cors <- function(role_centrality,
                          adjmats) {

  cor_df <- data.frame()

  for (i in 1:nrow(role_centrality)) {

    this_mat <- matrix(nrow = 2*nrow(role_centrality),
                       ncol = length(adjmats))

    for (j in 1:length(adjmats)) {

      this_vec <- c(adjmats[[j]][i,], adjmats[[j]][,i])
      this_mat[,j] <- this_vec



    }

    # Get correlation matrix
    this_cor <- stats::cor(this_mat)
    # Make a matrix of all pair combos for naming
    #label_mat <- matrix(paste("cor", rep(1:length(graph)), rep(1:length(graph), each = length(graph)), sep = "_"),
    #  nrow = length(graph), ncol = length(graph))

    label_mat <- matrix(paste("cor",
                              rep(names(adjmats), each = length(adjmats)),
                              rep(names(adjmats), length(adjmats)),
                              sep = "_"),
                        nrow = length(adjmats), ncol = length(adjmats))


    # Get upper right triangles
    node_cors <- t(as.data.frame(this_cor[upper.tri(this_cor)]))
    colnames(node_cors) <- label_mat[upper.tri(label_mat)]

    cor_df <- rbind(cor_df, node_cors)

    # I'm pretty sure a lot of these NA values have to do with nodes being isolates in one of the
    # two relation types for which a correlation is calculated. Do we default to replacing NAs
    # with zeros here?
    # UPDATE: Jim said replacing with 0 is fine
    cor_df[is.na(cor_df)] <- 0



  }

  rownames(cor_df) <- NULL

  return(cor_df)

}

####################################################################
# Collapsing smaller clusters into parent clusters
####################################################################

cluster_collapse <- function(min_partition_size,
                             max_mod,
                             cut_df) {


  max_mod_val <- max_mod[,3] + 1

  this_cut <- cut_df[,1:max_mod_val]

  for (i in 2:ncol(this_cut)) {

    this_cut[,i] <- paste(colnames(this_cut)[[i]], this_cut[,i], sep = "_")

  }

  test <- data.frame(cluster = this_cut[,ncol(this_cut)]) %>%
    dplyr::group_by(.data$cluster) %>%
    dplyr::mutate(cluster_size = dplyr::n())

  for (i in (max_mod_val-1):3) {


    test$alt_cluster <- this_cut[,i]
    test <- test %>%
      dplyr::group_by(.data$alt_cluster) %>%
      dplyr::mutate(new_partition_size = dplyr::n(),
                    new_partition_check = min(.data$cluster_size)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cluster = ifelse(.data$new_partition_check < min_partition_size,
                                     .data$alt_cluster,
                                     .data$cluster)) %>%
      dplyr::group_by(.data$cluster) %>%
      dplyr::mutate(cluster_size = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$cluster, .data$cluster_size)

  }

  test <- test %>%
    dplyr::group_by(.data$cluster) %>%
    dplyr::mutate(final_cluster = dplyr::cur_group_id())

  return(test$final_cluster)

}




####################################################################
# VISUALIZATION FUNCTIONS
####################################################################
# Cluster summary plots (for HC method only)
####################################################################

cluster_summary_plots <- function(graph_list,
                                  summary_data) {

  # Create list for storing output
  output_list <- list()

  # These are vectors with the names of variables that can appear
  # in the summary plots for the overall graph

  cent_names <- c("degree", "total_degree", "weighted_degree",
                  "in_degree", "out_degree",
                  "weighted_indegree", "weighted_outdegree",
                  "betweenness", "binarized_betweenness",
                  "betweenness_scores",
                  "bonpow", "bonpow_negative",
                  "bonpow_in", "bonpow_out", "bonpow_sym",
                  "bonpow_negative_in", "bonpow_negative_out",
                  "bonpow_negative_sym",
                  "eigen_centrality",
                  "eigen_in", "eigen_out", "eigen_sym",
                  "closeness",
                  "isolate")


  # Make lists for storing individual plots
  cluster_summaries_cent <- list()
  cluster_summaries_triad <- list()


  for (i in 1:length(graph_list)) {

    if (i == 1) {

      centralities <- summary_data[(summary_data$var %in% cent_names), ] %>%
        dplyr::arrange(.data$order_id)

      triad_regex <- paste("^", names(graph_list)[[i]], "_[0-9]",
                           sep = "")

      triad_positions <- summary_data[stringr::str_detect(summary_data$var, triad_regex), ] %>%
        dplyr::arrange(.data$order_id)
      triad_positions$order_id <- triad_positions$order_id - min(triad_positions$order_id) + 1


    } else {

      cent_regex <- paste("^", names(graph_list)[[i]], "_[a-z]",
                          sep = "")
      triad_regex <- paste("^", names(graph_list)[[i]], "_[0-9]",
                           sep = "")

      centralities <- summary_data[stringr::str_detect(summary_data$var, cent_regex), ] %>%
        dplyr::arrange(.data$order_id)
      centralities$order_id <- centralities$order_id - min(centralities$order_id) + 1
      centralities <- centralities[!stringr::str_detect(centralities$var, "^cor"),]

      triad_positions <- summary_data[stringr::str_detect(summary_data$var, triad_regex), ] %>%
        dplyr::arrange(.data$order_id)
      triad_positions$order_id <- triad_positions$order_id - min(triad_positions$order_id) + 1

    }


    cent_plot <-  centralities %>%
      dplyr::group_by(.data$order_id) %>%
      dplyr::mutate(new_id = dplyr::cur_group_id()) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$new_id,
                                   y = .data$value,
                                   color = as.factor(.data$cluster))) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(breaks = 1:length(unique(centralities$var)),
                                  labels = unique(centralities$var)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                     panel.grid.minor.x = ggplot2::element_blank()) +
      ggplot2::ylab("Cluster Mean Value (Standardized)") +
      ggplot2::xlab("Variable") +
      ggplot2::labs(color = "Cluster")

    # In some cases, particularly with small networks or sparse relation types,
    # we might have situations where no triads are counted for a particular
    # relation type. We need to handle this case.

    if (nrow(triad_positions) != 0) {

      triad_plot <- triad_positions %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$order_id,
                                     y = .data$value,
                                     color = as.factor(.data$cluster))) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(breaks = 1:length(unique(triad_positions$var)),
                                    labels = unique(triad_positions$var)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                       panel.grid.minor.x = ggplot2::element_blank()) +
        ggplot2::ylab("Cluster Mean Value (Standardized)") +
        ggplot2::xlab("Variable") +
        ggplot2::labs(color = "Cluster")

    } else {

      triad_plot <- triad_positions %>%
        dplyr::mutate(cluster = 1, var = "No relevant triads", value = 0,
                      sd = 0, order_id = 1, var_id = 1) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$order_id,
                                     y = .data$value,
                                     color = as.factor(.data$cluster))) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        # ggplot2::scale_x_continuous(breaks = 1:length(unique(triad_positions$var)),
        #                             labels = unique(triad_positions$var)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                       panel.grid.minor.x = ggplot2::element_blank()) +
        # ggplot2::ylab("Cluster Mean Value (Standardized)") +
        # ggplot2::xlab("Variable") +
        ggplot2::ggtitle("No relevant triads") +
        ggplot2::labs(color = "Cluster")


    }

    cluster_summaries_cent[[i]] <- cent_plot
    cluster_summaries_triad[[i]] <- triad_plot

    # assign(x = paste("cluster_summaries_cent_", names(graph_list)[[i]], sep = ""),
    #        value = cent_plot,
    #        .GlobalEnv)
    #
    # assign(x = paste("cluster_summaries_triad_", names(graph_list)[[i]], sep = ""),
    #        value = triad_plot,
    #        .GlobalEnv)
  }

  names(cluster_summaries_cent) <- names(graph_list)
  names(cluster_summaries_triad) <- names(graph_list)

  # Store in `output_list`

  output_list$cluster_summaries_cent <- cluster_summaries_cent
  # assign(x = "cluster_summaries_cent",
  #        value = cluster_summaries_cent,
  #        .GlobalEnv)

  output_list$cluster_summaries_triad <- cluster_summaries_triad
  # assign(x = "cluster_summaries_triad",
  #        value = cluster_summaries_triad,
  #        .GlobalEnv)

  return(output_list)

}

cluster_summary_cor <- function(summary_data) {

  cor_regex <- "^cor_"

  cors <- summary_data[stringr::str_detect(summary_data$var, cor_regex), ] %>%
    dplyr::arrange(.data$order_id)

  cors$order_id <- cors$order_id - min(cors$order_id) + 1

  cor_plot <- cors %>%
    dplyr::group_by(.data$order_id) %>%
    dplyr::mutate(new_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$new_id,
                                 y = .data$value,
                                 color = as.factor(.data$cluster))) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = 1:length(unique(cors$var)),
                                labels = unique(cors$var)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
                   panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::ylab("Cluster Mean Value (Standardized)") +
    ggplot2::xlab("Variable") +
    ggplot2::labs(color = "Cluster")

  return(cor_plot)
  # assign(x = "cluster_summaries_correlations",
  #        value = cor_plot,
  #        .GlobalEnv)
}

####################################################################
# Cluster heatmaps
####################################################################

cluster_heatmaps <- function(node_data,
                             graph_list,
                             version){

  # Need cluster IDs for merging
  cluster_ego <- data.frame(ego = as.character(node_data$id),
                            ego_cluster = node_data$best_fit)
  cluster_alter <- data.frame(alter = as.character(node_data$id),
                              alter_cluster = node_data$best_fit)


  ### For density calculations, we need to get population of
  ### each cluster
  cluster_sizes <- node_data %>%
    dplyr::rename(cluster = .data$best_fit) %>%
    dplyr::group_by(.data$cluster) %>%
    dplyr::summarize(size = dplyr::n()) %>%
    dplyr::ungroup()

  i_size <- matrix(rep(cluster_sizes$size, nrow(cluster_sizes)),
                   nrow = nrow(cluster_sizes))
  j_size <- matrix(rep(cluster_sizes$size, each = nrow(cluster_sizes)),
                   nrow = nrow(cluster_sizes))

  # REVISIT: DO WE NEED TO CHANGE FOR UNDIRECTED GRAPHS?
  diag(j_size) <- diag(j_size)-1
  possible_cells <- i_size * j_size


  # CHI-SQUARED HEATMAPS / DENSITY
  for (i in 1:length(graph_list)) {

    # Get edgelist from each igraph object
    this_el <- as.data.frame(igraph::get.edgelist(graph_list[[i]]))
    this_el$weight <- igraph::E(graph_list[[i]])$weight
    colnames(this_el) <- c("ego", "alter", "weight")

    this_el <- this_el %>%
      dplyr::left_join(cluster_ego, by = "ego") %>%
      dplyr::left_join(cluster_alter, by = "alter") %>%
      dplyr::group_by(.data$ego_cluster, .data$alter_cluster) %>%
      dplyr::summarize(weight = sum(.data$weight)) %>%
      dplyr::ungroup() #%>%
    # mutate(weight_scl = scale(weight)[,1],
    #        weight_scl = weight_scl + -1*min(weight_scl) + 1,
    #          rel = i)
    #

    # Need to make full edgelist
    full_el <- data.frame(ego_cluster = rep(cluster_sizes$cluster, each = length(cluster_sizes$cluster)),
                          alter_cluster = rep(cluster_sizes$cluster, length(cluster_sizes$cluster))) %>%
      dplyr::left_join(this_el, by = c("ego_cluster", "alter_cluster"))

    this_mat <- full_el %>% tidyr::pivot_wider(id_cols = .data$ego_cluster,
                                               names_from = .data$alter_cluster,
                                               values_from = .data$weight)

    this_mat[is.na(this_mat)] <- 0
    this_mat <- as.matrix(this_mat[,2:ncol(this_mat)])

    # Incorrectly sorting if column names are numeric. Otherwise will be
    # sorted as "1, 10, 11, 2, 3, ..."
    if (sum(is.na(as.numeric(colnames(this_mat)))) == 0) {
      this_mat <- this_mat[,sort(as.numeric(colnames(this_mat)))]
    } else {
      this_mat <- this_mat[,sort(colnames(this_mat))]
    }




    # # Need to handle if one or more clusters doesn't have any
    # # outgoing ties
    #
    # if (ncol(this_mat) != ncol(possible_cells)) {
    #
    #   colnames(possible_cells) <- 1:ncol(possible_cells)
    #   columns_to_add <- colnames(possible_cells)[!(colnames(possible_cells) %in% colnames(this_mat))]
    #
    #   this_mat <- as.data.frame(this_mat)
    #
    #   for (j in 1:length(columns_to_add)) {
    #     this_mat$fill <- 0
    #     colnames(this_mat) <- c(colnames(this_mat)[1:(length(colnames(this_mat))-1)],
    #                             columns_to_add[[j]])
    #   }
    #
    #   this_mat <- this_mat[,sort(colnames(this_mat))]
    #   this_mat <- as.matrix(this_mat)
    #
    # }


    # CHI-SQUARED
    # So my instinct is to make chi-square the default, but allow density as a choice people can select if they want to override the default.
    chisq_mat <- stats::chisq.test(this_mat)
    chisq_mat2 <- chisq_mat$observed/chisq_mat$expected
    chisq_mat2[is.nan(chisq_mat2)] <- 0



    # DENSITY
    density_mat <- this_mat/possible_cells
    # If zero possible ties in a cell, will create `NaN`
    # values. Replace with zero
    density_mat[is.nan(density_mat)] <- 0

    # Standardize
    density_std <- matrix(scale(as.vector(density_mat))[,1],
                          nrow = nrow(cluster_sizes))
    # Add minimum value (+1?)
    density_std2 <- density_std+(-1*min(density_std))
    density_std2[this_mat > 0] <- density_std2[this_mat > 0] + 1

    # ASK JIM: WHEN WE HAVE A CLUSTER OF A VERY SMALL SIZE,
    # IT CAN CREATE OUTLIER VALUES WITH DENSITY THAT MAKE
    # THRESHOLD SELECTION POTENTIALLY DIFFICULT. NEED GUIDANCE
    # ON HOW TO HANDLE THESE CASES, AS SOMETIMES SUBSTANTIVE
    # SETTING PRODUCE ROLES WITH SMALL POPULATIONS (E.G. LEADERS)


    # Make cluster-to-cluster edgelists from matrices
    # Chi-squared
    chisq_df <- as.data.frame(chisq_mat2)
    chisq_df$cluster <- as.numeric(rownames(chisq_df))

    chisq_df <- chisq_df %>%
      tidyr::pivot_longer(cols = -.data$cluster,
                          names_to = "alter",
                          values_to = "chisq") %>%
      dplyr::mutate(alter = as.numeric(.data$alter)) %>%
      dplyr::arrange(.data$cluster, .data$alter) %>%
      dplyr::mutate(relation = names(graph_list)[[i]])

    # Density
    ### Basic
    density_df <- as.data.frame(density_mat)
    density_df$cluster <- as.numeric(rownames(density_df))

    density_df <- density_df %>%
      tidyr::pivot_longer(cols = -.data$cluster,
                          names_to = "alter",
                          values_to = "density") %>%
      dplyr::mutate(alter = as.numeric(.data$alter)) %>%
      dplyr::arrange(.data$cluster, .data$alter) %>%
      dplyr::mutate(relation = names(graph_list)[[i]])

    ### Standardized
    density_std_df <- as.data.frame(density_std)
    colnames(density_std_df) <- 1:ncol(density_std_df)
    density_std_df$cluster <- as.numeric(rownames(density_std_df))

    density_std_df <- density_std_df %>%
      tidyr::pivot_longer(cols = -.data$cluster,
                          names_to = "alter",
                          values_to = "density_std") %>%
      dplyr::mutate(alter = as.numeric(.data$alter)) %>%
      dplyr::arrange(.data$cluster, .data$alter) %>%
      dplyr::mutate(relation = names(graph_list)[[i]])

    ### Standardized and centered
    density_std_df2 <- as.data.frame(density_std2)
    colnames(density_std_df2) <- 1:ncol(density_std_df2)
    density_std_df2$cluster <- as.numeric(rownames(density_std_df2))

    density_std_df2 <- density_std_df2 %>%
      tidyr::pivot_longer(cols = -.data$cluster,
                          names_to = "alter",
                          values_to = "density_std2") %>%
      dplyr::mutate(alter = as.numeric(.data$alter)) %>%
      dplyr::arrange(.data$cluster, .data$alter) %>%
      dplyr::mutate(relation = names(graph_list)[[i]])

    # Merge edgelists together
    this_cluster_edgelist <- chisq_df %>%
      dplyr::left_join(density_df, by = c("relation", "cluster", "alter")) %>%
      dplyr::left_join(density_std_df, by = c("relation", "cluster", "alter")) %>%
      dplyr::left_join(density_std_df2, by = c("relation", "cluster", "alter"))


    # If this is the first relation type, make a dataframe
    # for storing value
    if (i == 1) {
      cluster_edgelist <- this_cluster_edgelist
      # Otherwise add this relation's values to `chisq_values`
    } else {
      cluster_edgelist <- dplyr::bind_rows(cluster_edgelist, this_cluster_edgelist)
    }

  }

  cluster_edgelist$cluster <- as.ordered(cluster_edgelist$cluster)
  cluster_edgelist$alter <- as.ordered(cluster_edgelist$alter)

  # Need to make the y-axis an ordered factor and reverse the
  # order of the factor to properly get the y-axis displayed



  # We need to reverse the factor order of the y-axis
  # to make these heatmaps look the way we want
  # cluster_edgelist <- cluster_edgelist %>%
  #   mutate(alter2 = factor(alter, levels = c("4", "3", "2", "1")))
  #
  # CHI-SQUARED HEATMAP

  # Question: do we want to scale the shading based on distribution
  # of values across all relation types? That's what I'm doing here
  # Get mean Chi-squared values for heatmap shading
  range_chisq <- range(cluster_edgelist$chisq)
  range_chisq[1] <- ceiling(range_chisq[1])
  range_chisq[2] <- ceiling(range_chisq[2])
  mean_chisq <- mean(range_chisq)

  chisq_heat <- cluster_edgelist %>%
    # filter(relation == i) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$alter,
                                 y = forcats::fct_rev(.data$cluster),
                                 fill = .data$chisq)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$chisq, digits = 2)), color = "white", size = 4) +
    ggplot2::theme_minimal() +
    # scale_x_continuous(breaks = 1:max(cluster_edgelist$cluster)) +
    #   scale_y_continuous(breaks = 1:max(cluster_edgelist$cluster)) +
    # scale_y_reverse() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient2(low="#102033", mid = "#2C6093", high="#47a0f3", #colors in the scale
                                  midpoint=mean_chisq,    #same midpoint for plots (mean of the range)
                                  breaks=seq(0,max(range_chisq),1), #breaks in the scale bar
                                  limits=c(floor(range_chisq[1]), ceiling(range_chisq[2]))) +
    ggplot2::facet_wrap(~relation, ncol = 3, scales = "free")

  # Store chi-squared heatmap to global environment
  # if (version == "cluster") {
  #     assign(x = "cluster_relations_chisq", value = chisq_heat, .GlobalEnv)
  # } else {
  #     assign(x = "concor_relations_chisq", value = chisq_heat, .GlobalEnv)
  # }


  # DENSITY HEATMAP
  range_density <- range(cluster_edgelist$density)
  range_density[1] <- ceiling(range_density[1])
  range_density[2] <- round(range_density[2], digits = 2)
  mean_density <- mean(range_density)

  density_heat <- cluster_edgelist %>%
    # filter(relation == i) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$alter,
                                 y = forcats::fct_rev(.data$cluster),
                                 fill = .data$density)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$density, digits = 2)), color = "white", size = 4) +
    ggplot2::theme_minimal() +
    #  scale_x_continuous(breaks = 1:max(cluster_edgelist$cluster)) +
    # scale_y_continuous(breaks = 1:max(cluster_edgelist$cluster)) +
    #  scale_y_reverse() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient2(low="#102033", mid = "#2C6093", high="#47a0f3", #colors in the scale
                                  midpoint=mean_density,    #same midpoint for plots (mean of the range)
                                  breaks=seq(0,max(range_density),.1), #breaks in the scale bar
                                  limits=c(floor(range_density[1]), range_density[2])) +
    ggplot2::facet_wrap(~relation, ncol = 3, scales = "free")

  # Store density heatmap to global environment
  # if (version == "cluster") {
  #     assign(x = "cluster_relations_density", value = density_heat, .GlobalEnv)
  # } else {
  #     assign(x = "concor_relations_density", value = density_heat, .GlobalEnv)
  # }

  # DENSITY HEATMAP (STANDARDIZED)
  range_density_std <- range(cluster_edgelist$density_std)
  range_density_std[1] <- floor(range_density_std[1])
  range_density_std[2] <- ceiling(range_density_std[2])

  density_std_heat <- cluster_edgelist %>%
    dplyr::mutate(limits = dplyr::case_when(.data$density_std > 3 ~ 3,
                                            .data$density_std < -3 ~ -3,
                                            TRUE ~ .data$density_std)) %>%
    # filter(relation == i) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$alter,
                                 y = forcats::fct_rev(.data$cluster),
                                 fill = .data$limits)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$density_std, digits = 2)), color = "white", size = 4) +
    ggplot2::theme_minimal() +
    # scale_x_continuous(breaks = 1:max(cluster_edgelist$cluster)) +
    # scale_y_continuous(breaks = 1:max(cluster_edgelist$cluster)) +
    # scale_y_reverse() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient2(low="#ad0206", mid = "grey", high="#47a0f3", #colors in the scale
                                  midpoint=0,    #same midpoint for plots (mean of the range)
                                  breaks=seq(-3,
                                             3, 1), #breaks in the scale bar
                                  limits=c(-3,
                                           3)) +
    ggplot2::facet_wrap(~relation, ncol = 3, scales = "free")

  # Store standardized density heatmap to global environment
  # if (version == "cluster") {
  #     assign(x = "cluster_relations_density_std", value = density_std_heat, .GlobalEnv)
  # } else {
  #     assign(x = "concor_relations_density_std", value = density_std_heat, .GlobalEnv)
  # }

  # DENSITY HEATMAP (STANDARDIZED AND CENTERED)
  range_density_std2 <- range(cluster_edgelist$density_std2)
  range_density_std2[1] <- floor(range_density_std2[1])
  range_density_std2[2] <- ceiling(range_density_std2[2])
  mean_density_std2 <- mean(range_density_std2)

  density_std2_heat <- cluster_edgelist %>%
    # filter(relation == i) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$alter,
                                 y = forcats::fct_rev(.data$cluster),
                                 fill = .data$density_std2)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$density_std2, digits = 2)), color = "white", size = 4) +
    ggplot2::theme_minimal() +
    # scale_x_continuous(breaks = 1:max(cluster_edgelist$cluster)) +
    # scale_y_continuous(breaks = 1:max(cluster_edgelist$cluster)) +
    # scale_y_reverse() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_gradient2(low="#102033", mid = "#2C6093", high="#47a0f3", #colors in the scale
                                  midpoint=mean_density_std2,    #same midpoint for plots (mean of the range)
                                  breaks=seq(0,max(range_density_std2),1), #breaks in the scale bar
                                  limits=c(floor(range_density_std2[1]), range_density_std2[2])) +
    ggplot2::facet_wrap(~relation, ncol = 3, scales = "free")

  # Store standardized + centered density heatmap to global environment
  # if (version == "cluster") {
  #     assign(x = "cluster_relations_density_centered", value = density_std2_heat, .GlobalEnv)
  # } else {
  #   assign(x = "concor_relations_density_centered", value = density_std2_heat, .GlobalEnv)
  # }

  # Alternate version of output: Store all four plots in a named list
  cluster_relation_heatmaps <- list(chisq = chisq_heat,
                                    density = density_heat,
                                    density_std = density_std_heat,
                                    density_centered = density_std2_heat)

  return(cluster_relation_heatmaps)

  # if (version == "cluster") {
  #   assign(x = "cluster_relations_heatmaps", value = cluster_relation_heatmaps, .GlobalEnv)
  # } else {
  #   assign(x = "concor_relations_heatmaps", value = cluster_relation_heatmaps, .GlobalEnv)
  # }

}

####################################################################
# Sociograms with nodes colored by cluster membership (cluster)
####################################################################

cluster_sociogram <- function(graph_list,
                              version,
                              color2) {

  # CLUSTERING VERSION
  if (version == "cluster") {

    # If there's only one graph, you just plot the one graph
    if (length(graph_list) == 1) {

      graphics::plot.new()
      plot(graph_list[[1]], vertex.color = igraph::V(graph_list[[1]])$color,
           vertex.label = NA,
           vertex.frame.color = NA,
           vertex.size = 2,
           edge.width = .1,
           edge.arrow.size = .1)

      graphics::legend(
        "bottomright",
        legend = color2$cluster,
        pt.bg  = color2$color,
        col  = color2$color,
        pch    = 21,
        cex    = 1,
        bty    = "n",
        title  = "cluster"
      )


      sociogram <- grDevices::recordPlot()
      return(sociogram)
      # assign(x = 'cluster_sociogram', value = sociogram, .GlobalEnv)

    } else {

      sociogram_list <- list()

      for (i in 1:length(graph_list)) {

        graphics::plot.new()

        plot(graph_list[[i]], vertex.color = igraph::V(graph_list[[i]])$color,
             vertex.label = NA,
             vertex.frame.color = NA,
             vertex.size = 2,
             edge.width = .1,
             edge.arrow.size = .1)
        graphics::title(names(graph_list)[[i]])

        graphics::legend(
          "bottomright",
          legend = color2$cluster,
          pt.bg  = color2$color,
          col  = color2$color,
          pch    = 21,
          cex    = 1,
          bty    = "n",
          title  = "cluster"
        )

        sociogram <- grDevices::recordPlot()

        sociogram_list[[i]] <- sociogram

      }

      names(sociogram_list) <- names(graph_list)

      return(sociogram_list)
      # assign(x = 'cluster_sociogram', value = sociogram_list, .GlobalEnv)

    }

    # CONCOR VERSION
  } else {

    # If there's only one graph, you just plot the one graph
    if (length(graph_list) == 1) {

      graphics::plot.new()
      plot(graph_list[[1]], vertex.color = igraph::V(graph_list[[1]])$color,
           vertex.label = NA,
           vertex.frame.color = NA,
           vertex.size = 2,
           edge.width = .1,
           edge.arrow.size = .1)

      graphics::legend(
        "bottomright",
        legend = color2$cluster,
        pt.bg  = color2$color,
        col  = color2$color,
        pch    = 21,
        cex    = 1,
        bty    = "n",
        title  = "block"
      )

      sociogram <- grDevices::recordPlot()
      return(sociogram)
      # assign(x = 'concor_sociogram', value = sociogram, .GlobalEnv)

    } else {

      sociogram_list <- list()

      for (i in 1:length(graph_list)) {

        graphics::plot.new()

        plot(graph_list[[i]], vertex.color = igraph::V(graph_list[[i]])$color,
             vertex.label = NA,
             vertex.frame.color = NA,
             vertex.size = 2,
             edge.width = .1,
             edge.arrow.size = .1)
        graphics::title(names(graph_list)[[i]])

        graphics::legend(
          "bottomright",
          legend = color2$cluster,
          pt.bg  = color2$color,
          col  = color2$color,
          pch    = 21,
          cex    = 1,
          bty    = "n",
          title  = "block"
        )

        sociogram <- grDevices::recordPlot()

        sociogram_list[[i]] <- sociogram

      }

      names(sociogram_list) <- names(graph_list)

      return(sociogram_list)
      # assign(x = 'concor_sociogram', value = sociogram_list, .GlobalEnv)

    }

  }
}

####################################################################
# CONCOR parent tree
####################################################################

concor_tree <- function(df) {

  block_assigns <- df[,2:(ncol(df)-2)]

  # Handling if only one partitioning level was selected
  if (!("data.frame" %in% class(block_assigns))) {
    # Record plot and assign to environment
    graphics::plot.new()
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste("Only one partitioning level selected.\n",
                                 "Partitioning tree not available.\n"),
         cex = 1, col = "black")
    tree_plot <- grDevices::recordPlot()
    grDevices::dev.off()
    return(tree_plot)
  } else {

  # Relabel Nodes

  for (i in 1:ncol(block_assigns)) {

    block_assigns[,i] <- paste(i, block_assigns[,i], sep = "_")

  }

  # Make edgelist of block assignments
  # Needs edges for full population and first cut

  block_el <- data.frame(ego = "full", alter = block_assigns[,1])

  for (i in 1:(ncol(block_assigns)-1)) {

    this_el <- block_assigns[,i:(i+1)]
    colnames(this_el) <- c("ego", "alter")
    block_el <- rbind(block_el, this_el)


  }

  block_el <- unique(block_el)

  # Make nodelist to indicate block sizes for each partition step
  block_counts <- rep("full", nrow(block_assigns))
  for (i in 1:ncol(block_assigns)) {
    block_counts <- c(block_counts, block_assigns[,i])
  }
  count_df <- data.frame(block = block_counts) %>%
    dplyr::group_by(.data$block) %>%
    dplyr::summarize(orig_size = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(size = ((.data$orig_size/max(.data$orig_size)) * 50))

  # Make igraph object of block assignments
  tree_igraph <- igraph::graph_from_data_frame(block_el,
                                               directed = TRUE,
                                               vertices = count_df)

  # Make tree layout
  tree_layout <- igraph::layout_as_tree(tree_igraph)


  # Record plot and assign to environment
  graphics::plot.new()
  plot(tree_igraph, layout = tree_layout,
       edge.arrow.size = .25)
  tree_plot <- grDevices::recordPlot()
  # assign(x = "concor_block_tree", value = tree_plot, .GlobalEnv)
  grDevices::dev.off()
  return(tree_plot)

  }

}

####################################################################
# Cluster relation sociograms
####################################################################

role_sociogram <- function(graph, version, color2) {

  # Make aggregated nodelist
  if (version == "cluster") {
    supernodes <- data.frame(id = igraph::V(graph[[1]])$name,
                             cluster = igraph::V(graph[[1]])$cluster) %>%
      dplyr::group_by(.data$cluster) %>%
      dplyr::summarize(original_size = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(size = log(.data$original_size)) %>%
      dplyr::left_join(color2, by = "cluster")

  } else {

    supernodes <- data.frame(id = igraph::V(graph[[1]])$name,
                             cluster = igraph::V(graph[[1]])$block) %>%
      dplyr::group_by(.data$cluster) %>%
      dplyr::summarize(original_size = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(size = log(.data$original_size)) %>%
      dplyr::left_join(color2, by = "cluster")

  }


  i_size <- matrix(rep(supernodes$original_size, nrow(supernodes)),
                   nrow = nrow(supernodes))
  j_size <- t(matrix(rep(supernodes$original_size, nrow(supernodes)),
                     nrow = nrow(supernodes)))

  # REVISIT: DO WE NEED TO CHANGE FOR UNDIRECTED GRAPHS?
  diag(j_size) <- diag(j_size)-1
  possible_cells <- as.data.frame(i_size * j_size)
  colnames(possible_cells) <- supernodes$cluster
  rownames(possible_cells) <- supernodes$cluster

  # Make full edgelist of potential tie weights
  weight_el <- tidyr::pivot_longer(possible_cells, "1":as.character(max(supernodes$cluster)), names_to = "cluster_ego", values_to = "max_possible")
  weight_el$cluster_ego <- as.numeric(weight_el$cluster_ego)
  weight_el$cluster_alter <- rep(supernodes$cluster, each=nrow(supernodes))
  weight_el$max_possible_summary <- weight_el$max_possible*(length(graph)-1)








  # Make list for storing sociograms
  super_list <- list()


  for (i in 1:length(graph)) {

    # Get edgelist
    this_el <- as.data.frame(igraph::get.edgelist(graph[[i]]))
    colnames(this_el) <- c("ego", "alter")

    if (version == "cluster") {
      ego_label <- data.frame(ego = igraph::V(graph[[i]])$name,
                              cluster_ego = igraph::V(graph[[i]])$cluster)
      alter_label <- data.frame(alter = igraph::V(graph[[i]])$name,
                                cluster_alter = igraph::V(graph[[i]])$cluster)
    } else {
      ego_label <- data.frame(ego = igraph::V(graph[[i]])$name,
                              cluster_ego = igraph::V(graph[[i]])$block)
      alter_label <- data.frame(alter = igraph::V(graph[[i]])$name,
                                cluster_alter = igraph::V(graph[[i]])$block)
    }

    this_el <- this_el %>%
      dplyr::left_join(ego_label, by = "ego") %>%
      dplyr::left_join(alter_label, by = "alter") %>%
      dplyr::group_by(.data$cluster_ego, .data$cluster_alter) %>%
      dplyr::summarize(weight = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(weight_el, by = c("cluster_ego", "cluster_alter"))

    # When visualizing the overall summary graph, we can have multiple ties for
    # specific directed dyads. If we treat the demoninator for density as we usually would,
    # there a chance that we'd get edge density scores that exceed 1. To fix this,
    # we'll need to multiply the number of possible ties by number of relation types
    # to get a more accurate denominator
    if (i == 1) {
      this_el$density <- this_el$weight/this_el$max_possible_summary
    } else {
      this_el$density <- this_el$weight/this_el$max_possible_summary
    }

    # We're only going to keep "edges" between "supernodes" if the density of ties
    # between them exceed the median
    median_dens <- stats::median(this_el$density)
    this_el <- dplyr::filter(this_el, .data$density > median_dens)

    # Add a base value and multiplier to scale edge weights based on density
    this_el$density2 <- this_el$density*5 + 1

    # Make igraph object
    this_igraph <- igraph::graph_from_data_frame(this_el, directed = TRUE,
                                                 vertices = supernodes)

    # # Color nodes (this makes adding a legend easier)
    # igraph::V(this_igraph)$color <- color_assign(input = igraph::V(this_igraph)$name)


    this_layout <- igraph::layout.fruchterman.reingold(this_igraph)

    # Record plot and assign to environment
    graphics::plot.new()
    plot(this_igraph,
         vertex.size = igraph::V(this_igraph)$size + 5,
         vertex.color = igraph::V(this_igraph)$color,
         vertex.label = NA,
         vertex.frame.color = NA,
         edge.width = igraph::E(this_igraph)$density2,
         edge.arrow.size = .5,
         layout = this_layout)
    graphics::title(main = names(graph)[[i]],
                    sub = paste("Median edge density: ", median_dens, sep = ""))

    graphics::legend(
      "bottomright",
      legend = color2$cluster,
      pt.bg  = color2$color,
      col  = color2$color,
      pch    = 21,
      cex    = 1,
      bty    = "n",
      title  = ifelse(version == "cluster", "cluster", "block")
    )


    this_plot <- grDevices::recordPlot()
    super_list[[i]] <- this_plot
    grDevices::dev.off()


  }

  names(super_list) <- names(graph)

  # plot_name <- paste(version, "relations_sociograms", sep = "_")
  #
  # assign(x = plot_name, value = super_list, envir = .GlobalEnv)

  return(super_list)


}

####################################################################
# Coloring nodes
####################################################################

color_assign <- function(input) {

  # Get unique values of `input`
  unique_vals <- unique(input)

  # Use `colorspace` to get your colors
  color_df <- data.frame(val = unique_vals,
                         color = colorspace::qualitative_hcl(n = length(unique_vals)))

  # Now create another dataframe containing input
  assign_df <- data.frame(val = input)
  assign_df <- dplyr::left_join(assign_df, color_df, by = "val")

  return(assign_df$color)

}
