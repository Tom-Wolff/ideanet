#' Character relationships among Marvel characters
#'
#' @docType data
#'
#' @usage data(marvel)
#'
#' @format An object of class \code{"csv"}. Contains 107 different characters, and  353 weighted relationships
#' between those characters, which were calculated based on how many times two characters' names appeared
#' within 15 words of one another in the novel.
#'
#' @keywords datasets
#'
#' @references
#'
#' @source Melanie Walsh (\href{https://github.com/melaniewalsh/sample-social-network-datasets/tree/master/sample-datasets/marvel}{Github}),
#' adapted from data originally compiled by Cesc Rosselló, Ricardo Alberich,
#' and Joe Miro from Russ Chappell (\href{https://www.chronologyproject.com}{Website})
#'
#' @examples
#' edges <- ideanet::marvel
#' ideanet::netwrite(i_elements = edges$Source, j_elements = edges$Target,
#' weights = edges$Weight)
"marvel"
