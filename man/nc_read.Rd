% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nc_reader.R
\name{nc_read}
\alias{nc_read}
\title{Reading and Reshaping Network Canvas Data (\code{nc_read})}
\usage{
nc_read(path, protocol = NULL, cat.to.factor = TRUE)
}
\arguments{
\item{path}{A character value indicating the directory in which Network Canvas CSVs are located. \code{nc_read} will read in all CSV files located in this directory and process them.}

\item{protocol}{A character value indicating the pathname of the Network Canvas protocol file corresponding to the data being read. Reading in the protocol is optional but recommended for accurate encoding of categorical variables.}

\item{cat.to.factor}{A logical value indicating whether categorical variables, originally stored as a series of TRUE/FALSE columns, should be converted into a single factor column.}
}
\value{
\code{nc_read} returns a list containing three items: an ego list, an ego-alter edgelist, and an alter-alter edgelist. If multiple edge types exist for ego-alter and/or alter-alter ties, edgelists for each type of tie will be stored as individual data frames as elements in a list. All data frames are optimized for use with \code{\link{ego_netwrite}}.  \cr \cr
Note that in the \code{alters} data frame(s), column \code{node_type} reflects the "node type" assigned to a given alter as specified in a Network Canvas protocol. Values in \code{node_type} are not necessarily those which should be fed into the \code{alter_types} argument in \code{\link{ego_netwrite}}.
}
\description{
The \code{nc_read} function reads in and processes CSV files produced by \href{https://networkcanvas.com/}{Network Canvas}, a popular tool for egocentric data capture. \code{nc_read} produces three dataframes optimized for use with \code{\link{ego_netwrite}}.
}
