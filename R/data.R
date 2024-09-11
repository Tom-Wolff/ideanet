#' Goodreau's Faux Mesa High School (Nodelist)
#'
#' This data set (originally found in as a \code{network} object in the \code{ergm} package)
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
#' The following \code{ergm} formula was used to fit a model to the
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
#' This data set (originally found in as a \code{network} object in the \code{ergm} package)
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
#' The following \code{ergm} formula was used to fit a model to the
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


#' Nodelist of marriage alliances and business relationships between Florentine families during the Italian Renaissance
#'
#' Breiger & Pattison (1986), in their discussion of local role analysis, use a subset of data on
#' the social relations among Renaissance Florentine families collected by John Padgett
#' from historical documents. The two relations are business ties (recorded financial ties such as loans,
#' credits and joint partnerships) and marriage alliances. This dataset has since become a standard for illustrating
#' role analysis methods and working with networks featuring multiple types of relations.
#'
#' @format
#' A data frame with 16 rows and 2 columns:
#' \describe{
#'   \item{id}{Unique node ID number}
#'   \item{family}{Name of family corresponding to node}
#'   ...
#' }
#' @source John Padgett (\href{http://www.casos.cs.cmu.edu/computational_tools/datasets/external/padgett/index2.html}{Website})
#' @references Ronald L. Breiger and Philippa E Pattison. 1986. "Cumulated social roles: The duality of persons and their algebras." \emph{Social Networks} 8(13):215-256.
"florentine_nodes"


#' Edgelist of marriage alliances and business relationships between Florentine families during the Italian Renaissance
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
#'   \item{source}{Outgoing node}
#'   \item{target}{Receiving node}
#'   \item{weight}{A placeholder variable for tie/edge weights, set to 1}
#'   \item{type}{Relation type}
#'   ...
#' }
#' @source John Padgett (\href{http://www.casos.cs.cmu.edu/computational_tools/datasets/external/padgett/index2.html}{Website})
#' @references Ronald L. Breiger and Philippa E Pattison. 1986. "Cumulated social roles: The duality of persons and their algebras." \emph{Social Networks} 8(13):215-256.
"florentine_edges"


#' A Small Network Containing all Triads and Motifs
#'
#' An adjacency matrix representing a network of 9 nodes, the ties between which form all possible
#' triads and 3-node motifs that can appear in a directed network.
#'
#' @format
#' A matrix with 9 rows and 9 columns
#'
"triad"


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


#' Ego Networks Elicited from the "Important Matters" Name Generator Question (Nodelist)
#'
#' This dataset contains a simplified subset of 60 ego networks elicited using the "important matters" name generator question (NGQ),
#' which is frequently used to capture an individual's close personal ties. These networks were collected as part
#' of an experiment illustrating how networks generated by this question may vary depending on the topics covered in
#' preceding survey items. Networks were collected using an online survey deployed via Amazon Mechanical Turk.
#'
#'
#' @format
#' A data frame with 60 rows and 9 columns:
#' \describe{
#'   \item{ego_id}{Unique identifier for ego providing network}
#'   \item{age}{A numeric indicating ego's self-reported age}
#'   \item{sex}{A numeric indicating ego's self-reported sex. \code{1} indicates male, \code{2} female.}
#'   \item{race}{A character indicating a simplification of ego's self-reported race/ethnicity. Values include \code{"White"}, \code{"Black"}, and \code{"Other"}.}
#'   \item{black}{A logical indicating ego's self-identification as "Black" or "African-American."}
#'   \item{white}{A logical indicating ego's self-identification as "White."}
#'   \item{other_race}{A logical indicating ego's self-identification with a race or ethnicity other than "Black," "African-American," or "White."}
#'   \item{edu}{A numeric indicating ego's highest level of educational attainment. \code{1} indicates less than a high school diploma, \code{4} indicates a high school diploma or GED, \code{5} some college, \code{6} a college degree, and \code{7} a graduate or professional degree.}
#'   \item{pol}{A numeric indicating ego's self-identified political orientation on a seven-point scale. \code{1} indicates "Extremely Liberal," \code{4} "Moderate," and \code{7} "Extremely Conservative."}
#'   ...
#' }
#' @source Original Data, Collected by Danielle Montagne, Joseph Quinn, Liann Tucker, and Tom Wolff.
"ngq_egos"


#' Ego Networks Elicited from the "Important Matters" Name Generator Question (Alter List)
#'
#' This dataset contains a simplifed subset of 60 ego networks elicited using the "important matters" name generator question (NGQ),
#' which is frequently used to capture an individual's close personal ties. These networks were collected as part
#' of an experiment illustrating how networks generated by this question may vary depending on the topics covered in
#' preceding survey items. Networks were collected using an online survey deployed via Amazon Mechanical Turk.
#'
#'
#' @format
#' A data frame with 184 rows and 14 columns:
#' \describe{
#'   \item{ego_id}{Unique identifier for ego providing network}
#'   \item{alter_id}{Within-network unique identifier for person nominated by ego (alter).}
#'   \item{sex}{A numeric indicating alter's sex as reported by ego. \code{1} indicates male, \code{2} female.}
#'   \item{race}{A character indicating a simplified characterization of alter's race/ethnicity as reported by ego. Values include \code{"White"}, \code{"Black"}, and \code{"Other"}.}
#'   \item{black}{A logical indicating ego's perception of alter as "Black" or "African-American."}
#'   \item{white}{A logical indicating ego's perception of alter as "White."}
#'   \item{other_race}{A logical indicating ego's perception of alter as belonging to a racial/ethnic group other than "Black," "African-American," or "White."}
#'   \item{pol}{A numeric indicating political orientation on a seven-point scale, as perceived by ego. \code{1} indicates "Extremely Liberal," \code{4} "Moderate," and \code{7} "Extremely Conservative."}
#'   \item{family}{A logical indicating alter as ego's family member.}
#'   \item{friend}{A logical indicating alter as ego's friend.}
#'   \item{other_rel}{A logical indicating alter as have a relationship to ego other than one of the types of relationships listed above.}
#'   \item{face}{A numeric indicating how frequently ego and alter interact in person. \code{1} indicates "Never," \code{2} "Less than once a month," \code{3} "1-3 times a month," \code{4} "1-3 times a week," \code{5} "Daily or almost daily."}
#'   \item{phone}{A numeric indicating how frequently ego and alter talk on the phone or via video chat. \code{1} indicates "Never," \code{2} "Less than once a month," \code{3} "1-3 times a month," \code{4} "1-3 times a week," \code{5} "Daily or almost daily."}
#'   \item{text}{A numeric indicating how frequently ego and alter interact via electronic messaging (e.g. texting, email, social media). \code{1} indicates "Never," \code{2} "Less than once a month," \code{3} "1-3 times a month," \code{4} "1-3 times a week," \code{5} "Daily or almost daily."}
#'   ...
#' }
#' @source Original Data, Collected by Danielle Montagne, Joseph Quinn, Liann Tucker, and Tom Wolff.
"ngq_alters"


#' Ego Networks Elicited from the "Important Matters" Name Generator Question (Alter-Alter Edgelist)
#'
#' This dataset contains a simplified subset of 60 ego networks elicited using the "important matters" name generator question (NGQ),
#' which is frequently used to capture an individual's close personal ties. These networks were collected as part
#' of an experiment illustrating how networks generated by this question may vary depending on the topics covered in
#' preceding survey items. Networks were collected using an online survey deployed via Amazon Mechanical Turk.
#'
#'
#' @format
#' A data frame with 226 rows and 5 columns:
#' \describe{
#'   \item{ego_id}{Unique identifier for ego providing network}
#'   \item{alter1}{Within-network unique identifier for Alter 1 in alter-alter edgelist.}
#'   \item{alter2}{Within-network unique identifier for Alter 2 in alter-alter edgelist.}
#'   \item{type}{A character indicating the type of relationship that Alter 1 and Alter 2 have with one another. Note that each dyad-type combination has its own unique row in this dataset, so more than one row may correspond to a single dyad if the dyad involves multiple types of relationships.}
#'   \item{freqtalk}{A numeric indicating how frequently ego believes Alter 1 and Alter 2 talk with one another. \code{1} indicates "Never," \code{2} "Less than once a month," \code{3} "1-3 times a month," \code{4} "1-3 times a week," \code{5} "Daily or almost daily."}
#'   ...
#' }
#' @source Original Data, Collected by Danielle Montagne, Joseph Quinn, Liann Tucker, and Tom Wolff.
"ngq_aa"


#' Division I-A College Football, Fall 2000
#' 
#' This dataset is a copy of the data in the football.gml file from Mark Newman's Network data website, as compiled by M. Girvan and M. Newman and used here with their permission. As described in their football.txt file, the data contains the network of American football games between Division I-A colleges during the regular season of Fall 2000. The nodes have values that indicate to which conferences they belong. If you make use of these data, please cite M. Girvan and M. E. J. Newman, Community structure in social and biological networks, Proc. Natl. Acad. Sci. USA 99, 7821-7826 (2002). Note also their correction that two edges were erroneously duplicated in this data set, and have been removed (21 SEP 2014).
#' 
#' @format 
#' An igraph object with 115 nodes and 613 edges, including vertex attributes:
#' \describe{
#'   \item{id}{Numerical node index, 0 to 114.}
#'   \item{label}{College name of each node.}
#'   \item{value}{Numerical indicator of conference affiliation: \code{0} Atlantic Coast Conference, \code{1} Big East, \code{2} Big Ten, \code{3} Big 12, \code{4} Conference USA, \code{5} Independents, \code{6} Mid-American, \code{7} Mountain West, \code{8} Pacific-10, \code{9} Southeastern, \code{10} Sun Belt, \code{11} Western Athletic.}
#'   ...
#' }
#' @source M. Girvan and M. E. J. Newman, Community structure in social and biological networks, Proc. Natl. Acad. Sci. USA 99, 7821-7826 (2002). Data downloaded from https://websites.umich.edu/~mejn/netdata/.
"football"
  
 