#' Goodreau's Faux Mesa High School (Nodelist)
#'
#' This data set (originally found in as a \code{network} object in the \code{\link[ergm:faux.mesa.high]{ergm}} package)
#' represents a simulation of an in-school friendship network. The network is named "Faux Mesa High" because the school
#' community on which it is based is in the rural western US, with a student body that is largely Hispanic and Native American.
#'
#' @format
#' A data frame with 205 rows and 4 columns:
#' \describe{
#'   \item{id}{Node ID}
#'   \item{grade}{Student grade year}
#'   \item{race}{Student race}
#'   \item{sex}{Student sex}
#'   ...
#' }
#' @source The data set is based upon a model fit to data from one school
#' community from the AddHealth Study, Wave I (Resnick et al., 1997). It was
#' constructed as follows:
#'
#' A vector representing the sex of each student in the school was randomly
#' re-ordered.  The same was done with the students' response to questions on
#' race and grade.  These three attribute vectors were permuted independently.
#' Missing values for each were randomly assigned with weights determined by
#' the size of the attribute classes in the school.
#'
#' The following \code{\link{ergm}} formula was used to fit a model to the
#' original data:
#'
#' \preformatted{ ~ edges + nodefactor("Grade") + nodefactor("Race") +
#' nodefactor("Sex") + nodematch("Grade",diff=TRUE) +
#' nodematch("Race",diff=TRUE) + nodematch("Sex",diff=FALSE) +
#' gwdegree(1.0,fixed=TRUE) + gwesp(1.0,fixed=TRUE) + gwdsp(1.0,fixed=TRUE) }
#'
#' The resulting model fit was then applied to a network with actors possessing
#' the permuted attributes and with the same number of edges as in the original
#' data.
#'
#' The processes for handling missing data and defining the race attribute are
#' described in Hunter, Goodreau & Handcock (2008).
"fauxmesa_nodes"


#' Goodreau's Faux Mesa High School (Edgelist)
#'
#' This data set (originally found in as a \code{network} object in the \code{\link[ergm:faux.mesa.high]{ergm}} package)
#' represents a simulation of an in-school friendship network. The network is named "Faux Mesa High" because the school
#' community on which it is based is in the rural western US, with a student body that is largely Hispanic and Native American.
#'
#' @format
#' A data frame with 203 rows and 2 columns:
#' \describe{
#'   \item{from}{Outgoing node}
#'   \item{to}{Receiving node}
#'   ...
#' }
#' @source The data set is based upon a model fit to data from one school
#' community from the AddHealth Study, Wave I (Resnick et al., 1997). It was
#' constructed as follows:
#'
#' A vector representing the sex of each student in the school was randomly
#' re-ordered.  The same was done with the students' response to questions on
#' race and grade.  These three attribute vectors were permuted independently.
#' Missing values for each were randomly assigned with weights determined by
#' the size of the attribute classes in the school.
#'
#' The following \code{\link{ergm}} formula was used to fit a model to the
#' original data:
#'
#' \preformatted{ ~ edges + nodefactor("Grade") + nodefactor("Race") +
#' nodefactor("Sex") + nodematch("Grade",diff=TRUE) +
#' nodematch("Race",diff=TRUE) + nodematch("Sex",diff=FALSE) +
#' gwdegree(1.0,fixed=TRUE) + gwesp(1.0,fixed=TRUE) + gwdsp(1.0,fixed=TRUE) }
#'
#' The resulting model fit was then applied to a network with actors possessing
#' the permuted attributes and with the same number of edges as in the original
#' data.
#'
#' The processes for handling missing data and defining the race attribute are
#' described in Hunter, Goodreau & Handcock (2008).
"fauxmesa_edges"


#' Marriage alliances and business relationships between Florentine families during the Italian Renaissance
#'
#' Breiger & Pattison (1986), in their discussion of local role analysis, use a subset of data on
#' the social relations among Renaissance Florentine families collected by John Padgett
#' from historical documents. The two relations are business ties (recorded financial ties such as loans,
#' credits and joint partnerships) and marriage alliances. This dataset has since become a standard for illustrating
#' role analysis methods and working with networks featuring multiple types of relations.
#'
#' @format
#' A data frame with 35 rows and 4 columns:
#' \describe{
#'   \item{node}{Outgoing node}
#'   \item{target}{Receiving node}
#'   \item{weight}{A placeholder variable for tie/edge weights, set to 1}
#'   \item{layer}{Relation type}
#'   ...
#' }
#' @source John Padgett (\href{http://www.casos.cs.cmu.edu/computational_tools/datasets/external/padgett/index2.html}{Website})
#' @references Ronald L. Breiger and Philippa E Pattison. 1986. "Cumulated social roles: The duality of persons and their algebras." \emph{Social Networks} 8(13):215-256.
"florentine"


#' Multiplex Network of Relationships Between Managers of a High-Tech Company
#'
#' A network of a small hi-tech computer firm that sold, installed, and maintained computer
#' systems, represented as an edgelist. Relationships in the network can take on three modes:
#' 1 represents advice relationships, 2 represents friendship relationships, and
#' 3 represents chain of command (e.g., "reporting-to").
#'
#' @format
#' A data frame with 312 rows and 4 columns:
#' \describe{
#'   \item{node}{Outgoing node}
#'   \item{target}{Receiving node}
#'   \item{weight}{A placeholder variable for tie/edge weights, set to 1}
#'   \item{layer}{Relation type}
#'   ...
#' }
#' @source Carnegie Mellon University (\href{http://casos.cs.cmu.edu/computational_tools/datasets/external/Hi-tech/index2.html}{Website})
#' @references David Krackhardt. 1987. "Cognitive social structures". \emph{Social Networks} 9(2):104-134. https://doi.org/10.1016/0378-8733(87)90009-8
"hightech"


#' Character Relations in Marvel Comics
#'
#' A network, represented as edgelist, containing weighted edges between Marvel Comics characters.
#' Edge weights were calculated based on how many times two characters' names appeared within 15 words
#' of one another in a comic.
#'
#' @format
#' A data frame with 9891 rows and 3 columns:
#' \describe{
#'   \item{Source}{Outgoing node}
#'   \item{Target}{Receiving node}
#'   \item{Weight}{Edge weight}
#'   ...
#' }
#' @source Melanie Walsh (\href{https://github.com/melaniewalsh/sample-social-network-datasets/tree/master/sample-datasets/marvel}{Github}),
#' adapted from data originally compiled by Cesc Rosselló, Ricardo Alberich,
#' and Joe Miro from Russ Chappell (\href{https://www.chronologyproject.com}{Website})
"marvel"

