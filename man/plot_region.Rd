\name{plot_region}
\alias{plot_region}
\title{Setting up Plot Region for Graphics Functions}
\description{
  Auxiliary function for setting up the plot region of 1d and 2d
  graphics plots.
}
\usage{
plot_region(xlim, ylim, plot... = NULL)
}
\arguments{
  \item{xlim}{x-axis limits.}
  \item{ylim}{y-axis limits.}
  \item{plot...}{arguments passed to \code{\link{plot}()}.}
}
\value{
  \code{\link{invisible}()}.
}
\details{
  This is an auxiliary function used by the provided \pkg{graphics}-related 1d and 2d plots.
}
\author{Marius Hofert}
\seealso{
  \link{plots_graphics}
}
\examples{
plot_region
}
\keyword{dplot}