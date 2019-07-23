\name{scaling}
\alias{scale01}
\title{Scaling Data}
\description{
  Auxiliary function to scale data.
}
\usage{
scale01(x, method = c("columnwise", "all", "pobs"), ...)
}
\arguments{
  \item{x}{\code{\link{matrix}}, \code{\link{data.frame}} or
    \code{\link{list}} of vectors containing the data to be scaled
    to \eqn{[0,1]}.}
  \item{method}{\code{\link{character}} string indicating the
    scaling method to be used. Available are:
    \describe{
      \item{\code{"columnwise"}:}{scales \code{x} columnwise.}
      \item{\code{"all"}:}{scales all components of \code{x} simultaneously.}
      \item{\code{"pobs"}:}{applies columnwise the respective empirical
	distribution function to \code{x}; see the \R\ package
	``copula'' for more details.}
    }}
  \item{...}{additional arguments passed to \code{\link{rank}()} (for
    \code{method = "pobs"}) and \code{\link{range}()} (for all other methods).}
}
\value{
  \code{scale01()} returns an object of the same type as \code{x},
  but scaled to lie in \eqn{[0,1]}. Note that \code{\link{NA}}
  values are passed through.
}
\author{Marius Hofert and Wayne Oldford}
\examples{
## Implementation
scale01
}
\keyword{datagen}