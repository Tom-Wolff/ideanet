% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/euclidean_distance.R
\name{euclidean_distance}
\alias{euclidean_distance}
\title{Euclidean Distance (\code{euclidean_distance})}
\usage{
euclidean_distance(
  ego_id,
  ego_measure,
  alter_ego,
  alter_measure,
  prefix = NULL,
  suffix = NULL
)
}
\arguments{
\item{ego_id}{A vector of unique ego identifiers located in an ego dataframe. If using data objects created by \code{\link{ego_netwrite}}, this should be the data frame entitled \code{egos}.}

\item{ego_measure}{A vector of attributes corresponding to each ego.}

\item{alter_ego}{A vector of ego identifiers located in an alter dataframe. If using data objects created by \code{\link{ego_netwrite}}, this should be the data frame entitled \code{alters}.}

\item{alter_measure}{A vector of attributes corresponding to each alter.}

\item{prefix}{A character value indicating the desired prefix for the calculated homophily measure.}

\item{suffix}{A character value indicating the desired suffix for the calculated homophily measure.}
}
\value{
\code{euclidean_distance} returns a dataframe of vectors that include the ego identifier and euclidean distance for the desired continuous attribute
}
\description{
Typical difference between between ego and their alters for a given continuous attribute (Perry et al. 2018)
}
\examples{

data(package = "egor", "egos32")
data(package = "egor", "alters32")

euclidean_distance(ego_id = egos32$.EGOID, ego_measure = egos32$income,
alter_ego = alters32$.EGOID, alter_measure = alters32$income)
}
