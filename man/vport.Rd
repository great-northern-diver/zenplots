\name{vport}
\alias{vport}
\title{Viewport Constructing Function for Grid Functions}
\description{
  Auxiliary function for constructing viewports for 1d and 2d
  (default) plots.
}
\usage{
vport(ispace, xlim = NULL, ylim = NULL, x = NULL, y = NULL, ...)
}
\arguments{
  \item{ispace}{inner space (in \eqn{[0,1]}).}
  \item{xlim}{x-axis limits; if \code{NULL}, the data limits are used.}
  \item{ylim}{y-axis limits; if \code{NULL}, the data limits are used.}
  \item{x}{x data (only used if \code{is.null(xlim)});
    if \code{NULL}, \code{0:1} is used.}
  \item{y}{y data (only used if \code{is.null(ylim)});
    if \code{NULL}, \code{0:1} is used.}
  \item{\dots}{additional arguments passed to the underlying \code{\link{viewport}()}.}
}
\value{
  A \code{\link{viewport}}.
}
\details{
  This is an auxiliary function used by the provided \pkg{grid}-related 1d and 2d plots.
}
\author{Marius Hofert}
\seealso{
  \link{plots_grid}
}
\examples{
vport
}
\keyword{dplot}