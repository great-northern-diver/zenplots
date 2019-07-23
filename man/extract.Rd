\name{extract}
\alias{extract_1d}
\alias{extract_2d}
\title{Extracting Information from Zen Arguments}
\description{
  Auxiliary functions to extract information from zargs for 1d and 2d
  (default) plots.
}
\usage{
extract_1d(zargs)
extract_2d(zargs)
}
\arguments{
  \item{zargs}{argument list as passed from \code{\link{zenplot}()}.
    This must at least contain \code{x}, \code{orientations},
    \code{vars}, \code{num}, \code{lim} and \code{labs} (for
    \code{extract_1d()})
    and \code{x}, \code{vars}, \code{num}, \code{lim} and
    \code{labs} (for \code{extract_2d()}); see \code{\link{zenplot}()}
    for an explanation of these variables.}
}
\value{
  \describe{
    \item{for \code{extract_1d()}:}{\code{\link{list}} with the data to be plotted
      in the 1d plot (\code{x}), a list with all columns of \code{x}
      (\code{xcols}), the group numbers for each column of \code{x} (\code{groups}),
      the variable numbers for each column of \code{x} (\code{vars}), the group
      labels for each column of \code{x} (\code{glabs}), the variable labels for
      each column of \code{x} (\code{vlabs}), a \code{\link{logical}} indicating
      whether the plot is horizontal or vertical (\code{horizontal}) and
      the axis limits (\code{xlim}).}
    \item{for \code{extract_2d()}:}{\code{\link{list}} with the data to be plotted
      in the 2d plot (\code{x} and \code{y}), a list with all columns of \code{x}
      (\code{xcols}), the group numbers for each column of \code{x} (\code{groups}),
      the variable numbers for each column of \code{x} (\code{vars}), the group
      labels for each column of \code{x} (\code{glabs}), the variable labels for
      each column of \code{x} (\code{vlabs}), the x-axis and y-axis
      limits (\code{xlim} and \code{ylim}) and a \code{\link{logical}} indicating
      whether the x and y variables belong to the same group (\code{same.group}).}
  }
}
\details{
  This is an auxiliary function used by the provided 1d and 2d plots.
  For performance reasons, no checking of the input object is done.
}
\author{Marius Hofert}
\seealso{
  \code{\link{plots_graphics}},
  \code{\link{plots_grid}},
  \code{\link{plots_loon}}
}
\examples{
## Dummy example (mimicking how zargs are built internally)
set.seed(271)
n <- 100
x <- list(matrix(rnorm(n*2), ncol = 2), matrix(rnorm(n*3), ncol = 3))
n2dplots <- 5 - 1
pathLayout <- unfold(n2dplots)
path <- pathLayout$path
Layout <- pathLayout$layout
zargs2d <- list(x = x,
                turns = path$turns,
                orientations = Layout$orientations,
                vars = Layout$vars,
                lim = "individual",
                labs = list(group = "G", var = "V", sep = " "),
                width1d = 1,
                width2d = 10,
                num = 2,
                ispace = 0)

## Calling extract_2d()
str(extract_2d(zargs2d))
}
\keyword{datagen}