#' Interactive GUI for Working with Sociocentric Networks (\code{ideanetViz})
#'
#' @description \code{ideanetViz} is a Shiny app that presents the output of \code{ideanet}'s workflow for sociocentric data (i.e. \code{\link{netwrite}}) in a clear and accessible GUI. This GUI is convenient for users with limited R experience and is useful for classrooms, workshops, and other educational spaces. It is also useful for experienced users interested in quick exploration of network data. Moreover, \code{ideanetViz} streamlines customization of network visualizations and provides quick access into \code{ideanet}'s more advanced analytic tools for sociocentric networks.
#'
#' \code{ideanetViz}'s design is centered around a series of tabs lining the top of the app, which are ordered according to a typical workflow for acquiring, processing, exploring, and modeling data.
#'
#' @export

ideanetViz <- function() {

  rlang::check_installed("shinythemes")
  rlang::check_installed("DT")

  name = "ideanetViz"
  appDir <- system.file(paste0("apps/", name), package = "ideanet")
  if (appDir == "") stop("The shiny app ", name, " does not exist")
  shiny::runApp(appDir#,
                #display.mode = 'showcase',
                #...
  )
}
