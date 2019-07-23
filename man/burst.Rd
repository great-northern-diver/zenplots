\name{burst}
\alias{burst}
\title{Splitting an Input Object into a List of Columns}
\description{
  Split a (numeric/logical/character) vector, matrix, data.frame or a list of
  such into a list of columns, with corresponding group and variable
  information and labels.
}
\usage{
burst(x, labs = list())
}
\arguments{
  \item{x}{\code{\link{numeric}} \code{\link{vector}}, \code{\link{matrix}},
    \code{\link{data.frame}} or, for \code{burst()}, a
    \code{\link{list}} of such.}
  \item{labs}{either \code{\link{NULL}} (in which case
    neither group nor variable labels are computed) or a list
    containing the components \code{group} (either
    \code{\link{NULL}}, the group label basename or labels for the groups),
    \code{var} (either \code{\link{NULL}}, the variable label
    basename or labels for the variables), \code{sep} (the
    separator between group and variable labels) and \code{group2d} (a
    \code{\link{logical}} indicating whether labels of
    \code{group_2d_*()} plots are affected by \code{group = NULL} or
    still printed). If any of these components is
    not given, it is set to the defaults as can be found in
    \code{\link{zenplot}()}. Note that if at least one (group or
    variable) label is given in \code{x}, then those (original)
    labels will be used.}
}
\value{
  \code{burst()} returns a \code{\link{list}} of length five,
  containing all columns of \code{x} (possibly with constructed group
  and variable names), the group and variable numbers (indices), and the group and
  variable labels.
}
\author{Marius Hofert}
\examples{
## Unnamed list of (some named, some unnamed) valid components
A <- matrix(1:12, ncol = 3)
x <- list(A, 1:4, as.data.frame(A))
burst(x, labs = list(group = "G", var = "V", sep = ", "))
burst(x) # the same defaults as above
burst(x, labs = list(sep = " ")) # only changing the separator
## Note: - No group labels are given in 'x' and thus they are constructed
##         in the above call
##       - The variable names are only constructed if not given
burst(x, labs = list(group = ""))
burst(x, labs = list(group = NULL, group2d = TRUE)) # no group labels
## Note: There's no effect of 'group2d = TRUE' visible here as
##       'x' doesn't contain group labels
burst(x, labs = list(group = NULL)) # no group labels unless groups change
burst(x, labs = list(var = NULL)) # no variable labels
burst(x, labs = list(group = NULL, var = NULL)) # neither one
burst(x, labs = NULL) # similarly, without any labels at all

## Named list
x <- list(mat = A, vec = 1:4, df = as.data.frame(A))
burst(x)
## Note: - The given group labels are used
##       - The variable names are only constructed if not given
burst(x, labs = list(group = NULL, group2d = TRUE)) # no group labels
burst(x, labs = list(group = NULL)) # no group labels unless groups change
## Note: Now the effect of 'group2d' is visible.

## Partially named list
x <- list(mat = A, vec = 1:4, as.data.frame(A))
burst(x)
burst(x, labs = list(group = NULL, group2d = TRUE)) # no group labels
burst(x, labs = list(group = NULL)) # no group labels unless groups change
burst(x, labs = list(var = NULL)) # no variable labels
burst(x, labs = list(group = NULL, var = NULL)) # only group labels and only if groups change
burst(x, labs = NULL) # neither group nor variable labels
}
\keyword{datagen}