\name{na_omit_loon}
\alias{na_omit_loon}
\title{Function for Dealing with Missing Data}
\description{
  Auxiliary function for dealing with missing data (\code{NA}) in \pkg{loon}.
}
\usage{
na_omit_loon(x, y = NULL, linkingKey = NULL, itemlabel = NULL)
}
\arguments{
  \item{x}{vector of x data.}
  \item{y}{vector of y data (optional) of the same length as
    \code{x}.}
  \item{linkingKey}{list of IDs to link on.}
  \item{itemlabel}{vector of strings to serve as the itemlabel.}
}
\value{
  A \code{\link{list}} with components \code{x}, \code{y},
  \code{linkingKey} and \code{itemlabel}.
}
\details{
  This is an auxiliary function used by the provided \pkg{loon}-related
  1d and 2d plots.
}
\author{Marius Hofert}
\seealso{
  \link{plots_loon}
}
\examples{
na_omit_loon
}
\keyword{dplot}