\name{plot_indices}
\alias{plot_indices}
\title{Plot Indices of the Current Plot}
\description{
  Determining the indices of the x and y variables of the current plot.
}
\usage{
plot_indices(zargs)
}
\arguments{
  \item{zargs}{argument list as passed from \code{\link{zenplot}()}.
    This must at least contain \code{vars} and \code{num}; see
    \code{\link{zenplot}()} for an explanation of these variables.}
}
\value{
  A \code{numeric(2)} containing the indices of the x and y variables to
  be plotted in the current plot (the plot with number \code{num}). If
  the current plot is a 2d plot, the same variable is used twice.
}
\details{
  This is an auxiliary function useful, for example, when writing
  user-provided 1d and 2d plot functions.
}
\author{Marius Hofert}
\examples{
plot_indices # its definition
}
\keyword{datagen}