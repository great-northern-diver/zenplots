% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot1dgrid.R
\name{boxplot_1d_grid}
\alias{boxplot_1d_grid}
\title{Boxplot in 1d using the grid package}
\usage{
boxplot_1d_grid(
  zargs,
  pch = 21,
  size = 0.02,
  col = NULL,
  lwd = 2,
  bpwidth = 0.5,
  range = NULL,
  draw = FALSE,
  ...
)
}
\arguments{
\item{zargs}{argument list as passed from \code{\link{zenplot}()}}

\item{pch}{plot symbol}

\item{size}{size of the plot symbol}

\item{col}{color}

\item{lwd}{graphical parameter line width for whiskers and median}

\item{bpwidth}{width of boxplot on scale of default.units}

\item{range}{numerical value used to determine how far the plot whiskers extend. If
NULL, the whiskers (range) grows with sample size.}

\item{draw}{logical indicating whether drawing should take place}

\item{...}{additional arguments passed to gpar()}
}
\value{
gTree grob containing the boxplot components as grobs
}
\description{
Boxplot in 1d using the grid package
}
\seealso{
Other default 1d plot functions using the grid package: 
\code{\link{arrow_1d_grid}()},
\code{\link{density_1d_grid}()},
\code{\link{hist_1d_grid}()},
\code{\link{jitter_1d_grid}()},
\code{\link{label_1d_grid}()},
\code{\link{lines_1d_grid}()},
\code{\link{points_1d_grid}()},
\code{\link{rect_1d_grid}()},
\code{\link{rug_1d_grid}()}

Other default 1d plot functions: 
\code{\link{arrow_1d_graphics}()},
\code{\link{arrow_1d_grid}()},
\code{\link{arrow_1d_loon}()},
\code{\link{boxplot_1d_graphics}()},
\code{\link{boxplot_1d_loon}()},
\code{\link{density_1d_graphics}()},
\code{\link{density_1d_grid}()},
\code{\link{density_1d_loon}()},
\code{\link{extract_1d}()},
\code{\link{hist_1d_graphics}()},
\code{\link{hist_1d_grid}()},
\code{\link{hist_1d_loon}()},
\code{\link{jitter_1d_graphics}()},
\code{\link{jitter_1d_grid}()},
\code{\link{jitter_1d_loon}()},
\code{\link{label_1d_graphics}()},
\code{\link{label_1d_grid}()},
\code{\link{label_1d_loon}()},
\code{\link{lines_1d_graphics}()},
\code{\link{lines_1d_grid}()},
\code{\link{lines_1d_loon}()},
\code{\link{points_1d_graphics}()},
\code{\link{points_1d_grid}()},
\code{\link{points_1d_loon}()},
\code{\link{rect_1d_graphics}()},
\code{\link{rect_1d_grid}()},
\code{\link{rect_1d_loon}()},
\code{\link{rug_1d_graphics}()},
\code{\link{rug_1d_grid}()},
\code{\link{rug_1d_loon}()}
}
\author{
Marius Hofert and Wayne Oldford
}
\concept{default 1d plot functions}
\concept{default 1d plot functions using the grid package}
