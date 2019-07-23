\name{extreme_pairs}
\alias{extreme_pairs}
\alias{extreme_pairs_graph}
\title{Find So-Many Pairs with Largest/Smallest Values in a Symmetric Matrix}
\description{
  Find those n pairs with the largest/smallest/both values (entries) in a symmetric matrix.
}
\usage{
extreme_pairs(x, n = 6, method = c("largest", "smallest", "both", "all"),
              decreasing = TRUE, use.names = FALSE)
extreme_pairs_graph(x, n = 6, method = c("largest", "smallest", "both", "all"),
                    decreasing = TRUE, use.names = FALSE)
}
\arguments{
  \item{x}{symmetric \code{\link{numeric}} matrix.}
  \item{n}{number of pairs with extreme values in \code{x} to be
    considered; note that \code{n} is ignored if \code{method = "all"}.}
  \item{method}{\code{\link{character}} string indicating the method
    to be used (with \code{"largest"} to compute the \code{n} pairs with
    largest entries in \code{x} (sorted in decreasing order); with
    \code{"smallest"} to compute the \code{n} pairs with smallest entries
    in \code{x} (sorted in increasing order); with \code{"both"} to
    compute the 2\code{n} pairs with \code{n} largest entries and
    \code{n} smallest entries (sorted in decreasing order); and with
    \code{"all"} simply all pairs (sorted in decreasing order)).}
  \item{decreasing}{\code{\link{logical}} indicating whether
    sorting is done in decreasing order (the default).}
  \item{use.names}{\code{\link{logical}} indicating whether
    \code{colnames(x)} are used as labels (if \code{!is.null(colnames(x))}).}
}
\value{
  \code{extreme_pairs()} returns a \code{\link{data.frame}}
  consisting of three columns (\code{row} (index or name), \code{col}
  (index or name) and \code{value}).

  \code{extreme_pairs_graph()} returns a \code{graphNEL} object
  representing the output of \code{extreme_pairs()}; this can be plotted.
}
\author{Marius Hofert}
\examples{
set.seed(271)
n <- 1000
d <- 10
set.seed(271)
X <- matrix(rnorm(n*d), ncol = d)
P <- cor(X)
colnames(P) <- paste("Var", 1:d)
extreme_pairs(P, n = 5, method = "both")
if(FALSE) {
    require(graph) # requires 'graph' from Bioconductor
    plot(extreme_pairs_graph(P, n = 5))
}
}
\keyword{utilities}