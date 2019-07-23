\name{l_ispace_config}
\alias{l_ispace_config}
\title{Configuring a Loon Plot to Accommodate the Inner Space}
\description{
  Auxiliary function for using \code{ispace} with \pkg{loon}.
}
\usage{
l_ispace_config(baseplot, ispace = NULL,
                x = NULL, y = NULL, xlim = NULL, ylim = NULL, ...)
}
\arguments{
  \item{baseplot}{plot to be modified.}
  \item{ispace}{inner space (around each plot); defaults to
    \code{rep(0.2, 4)} if \code{NULL}.}
  \item{x}{x data.}
  \item{y}{y data.}
  \item{xlim}{x-axis limits; if \code{NULL}, the data limits are used.}
  \item{ylim}{y-axis limits; if \code{NULL}, the data limits are used.}
  \item{\dots}{additional arguments passed to \code{l_configure()}.}
}
\value{
  A \code{\link{loon}} object.
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
l_ispace_config
}
\keyword{dplot}