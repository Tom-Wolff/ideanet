#' Multiplex network of 3 edge types representing relationships (advice, friendship, and “reports to”) between managers of a high-tech company.
#'
#' @docType data
#'
#' @usage data(hightech)
#'
#' @format An object of class \code{"csv"}. Contains 21 different managers, and  312 directed relationships
#' between those managers. Relationships can take on three modes: 1 represents advice relationships, 2 represents friendship relationships,
#' 3 represents chain of command (e.g., "reporting-to").
#'
#' @keywords datasets
#'
#' @references D. Krackhardt, "Cognitive social structures". Social Networks (1987), 9, 104-134., https://doi.org/10.1016/0378-8733(87)90009-8
#'
#' @source Carnegie Mellon University (\href{http://casos.cs.cmu.edu/computational_tools/datasets/external/Hi-tech/index2.html}{Website})
#'
#' @examples
#' ideanet::netwrite(i_elements = edges$node,
#'                  j_elements = edges$target,
#'                  directed = T, type = edges$layer)
"hightech"
