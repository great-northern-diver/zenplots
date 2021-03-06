% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getpath.R
\name{move}
\alias{move}
\title{Determine the new position when moving from the current position
       in a given direction}
\usage{
move(curpos, dir, method = c("in.occupancy", "in.plane"))
}
\arguments{
\item{curpos}{current position (i, j) in the occupancy matrix}

\item{dir}{direction in which we move ("d", "u", "r" or "l")}

\item{method}{choice of method ("in.occupancy" means the (current/new)
position is given in terms of (row, column) indices in the
occupancy matrix; "in.plane" means the directions are
interpreted as in the (x,y)-plane).}
}
\value{
new position in the occupancy matrix
}
\description{
Determine the new position when moving from the current position
       in a given direction
}
\author{
Marius Hofert and Wayne Oldford
}
