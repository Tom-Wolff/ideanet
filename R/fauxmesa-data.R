#'  Simulation of an in-school friendship network based on a high-school community in rural Western US, with a student body that is largely Hispanic and Native American.
#'
#' @docType data
#'
#' @usage data(fauxmesa)
#'
#' @format Two objects of class \code{"data.frame"}. Contains 205 vertices (students) and 203 undirected edges (mutual friendships).
#' The vertex attributes are Grade, Sex, and Race. The Grade attribute has values 7 through 12, indicating each student's grade in school.
#' The Race attribute is based on the answers to two questions, one on Hispanic identity and one on race, and takes six possible values: White (non-Hisp.), Black (non-Hisp.), Hispanic, Asian (non-Hisp.), Native American, and Other (non-Hisp.)
#'
#' @keywords datasets
#'
#' @references Mark S. Handcock, David R. Hunter, Carter T. Butts, Steven M. Goodreau, and Martina Morris. 2003 statnet: Software tools for the Statistical Modeling of Network Data
#'
#' @source ERGM package (\href{https://search.r-project.org/CRAN/refmans/ergm/html/faux.mesa.high.html}{Website})
#'
#' @examples
#' data("fauxmesa", package = "ideanet")
#'
#' ideanet::netwrite(i_elements = edges$from,
#'                  j_elements = edges$to,
#'                  nodelist = nodes, node_id = "id",
#'                  directed = F)
"fauxmesa"
