#' Multiplex network with 2 edge types representing marriage alliances and business relationships between Florentine families during the Italian Renaissance.
#'
#' @docType data
#'
#' @usage data(florentine)
#'
#' @format An object of class \code{"csv"}. Contains 16 different families, and  35 relationships
#' between those families. Relationships can take on two modes: 1 represents marital ties, 2 represents business ties.
#'
#' @keywords datasets
#'
#' @references JF Padgett and CK Ansell, "Robust Action and the Rise of the Medici, 1400-1434". American journal of sociology, 1259-1319 (1993)., https://doi.org/10.1086/230190
#'
#' @source John Padgett (\href{http://www.casos.cs.cmu.edu/computational_tools/datasets/external/padgett/index2.html}{Website})
#'
#' @examples
#' edges <- ideanet::florentine
#' ideanet::netwrite(i_elements = edges$node,
#'                  j_elements = edges$target,
#'                  directed = F, type = edges$layer)
"florentine"
