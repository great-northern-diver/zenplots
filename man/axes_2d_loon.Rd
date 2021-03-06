% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot2dloon.R
\name{axes_2d_loon}
\alias{axes_2d_loon}
\title{Axes arrows in 2d using the interactive loon package}
\usage{
axes_2d_loon(
  zargs,
  angle = 30,
  length = 0.05,
  eps = 0.02,
  linkingGroup = NULL,
  color = NULL,
  showLabels = FALSE,
  showScales = FALSE,
  showGuides = FALSE,
  baseplot = NULL,
  parent = NULL,
  group... = NULL,
  ...
)
}
\arguments{
\item{zargs}{The argument list as passed from \code{\link{zenplot}()}}

\item{angle}{The angle of the arrow head}

\item{length}{The length of the arrow head}

\item{eps}{The distance by which the axes are moved away from the plot region}

\item{linkingGroup}{The initial linking group}

\item{color}{Colour used fill if ccol is NULL, a grey palette is used otherwise.}

\item{showLabels}{Logical determining whether axis labels are displayed}

\item{showScales}{Logical determining whether scales are displayed}

\item{showGuides}{Logical determining whether the background guidelines are displayed}

\item{baseplot}{If non-null the base plot on which the plot should be layered}

\item{parent}{The tk parent for this loon plot widget}

\item{group...}{A list of arguments passed to group_2d_loon (or NULL)}

\item{...}{Additional arguments passed to loon::l_plot()}
}
\value{
the loon plot
}
\description{
Axes arrows in 2d using the interactive loon package
}
\note{
Inspired by https://stat.ethz.ch/pipermail/r-help/2004-October/059525.html
}
\seealso{
Other default 2d plot functions using the interactive loon package: 
\code{\link{arrow_2d_loon}()},
\code{\link{density_2d_loon}()},
\code{\link{group_2d_loon}()},
\code{\link{label_2d_loon}()},
\code{\link{points_2d_loon}()},
\code{\link{rect_2d_loon}()}

Other default 2d plot functions: 
\code{\link{arrow_2d_graphics}()},
\code{\link{arrow_2d_grid}()},
\code{\link{arrow_2d_loon}()},
\code{\link{axes_2d_graphics}()},
\code{\link{axes_2d_grid}()},
\code{\link{density_2d_graphics}()},
\code{\link{density_2d_grid}()},
\code{\link{density_2d_loon}()},
\code{\link{extract_2d}()},
\code{\link{group_2d_graphics}()},
\code{\link{group_2d_grid}()},
\code{\link{group_2d_loon}()},
\code{\link{label_2d_graphics}()},
\code{\link{label_2d_grid}()},
\code{\link{label_2d_loon}()},
\code{\link{points_2d_graphics}()},
\code{\link{points_2d_grid}()},
\code{\link{points_2d_loon}()},
\code{\link{qq_2d_graphics}()},
\code{\link{qq_2d_grid}()},
\code{\link{rect_2d_graphics}()},
\code{\link{rect_2d_grid}()},
\code{\link{rect_2d_loon}()}
}
\author{
Marius Hofert and Wayne Oldford
}
\concept{default 2d plot functions}
\concept{default 2d plot functions using the interactive loon package}
