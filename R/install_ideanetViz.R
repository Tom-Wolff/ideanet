#' Install IDEANet Visualization GUI (`install_ideanetViz`)
#'
#' @description The IDEANet suite includes a Shiny-based GUI designed to help people process their data and perform basic analyses. `ideanetViz` also allows users to easily customize network visualizations and access more advanced analysis modules within its GUI.
#' @export
#'
#' @examples
#'\dontrun{
#' # Run `install_ideanetViz`
#' install_ideanetViz()
#'
#' # Open the ideanetViz GUI
#' library(ideanetViz)
#' ideanet_viz()
#' }


install_ideanetViz <- function() {
  devtools::install_github("Tom-Wolff/ideanetViz")
}
