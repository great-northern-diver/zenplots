\name{plots_grid}
\alias{plots_grid}
\alias{rug_1d_grid}
\alias{points_1d_grid}
\alias{jitter_1d_grid}
\alias{density_1d_grid}
\alias{boxplot_1d_grid}
\alias{hist_1d_grid}
\alias{arrow_1d_grid}
\alias{rect_1d_grid}
\alias{lines_1d_grid}
\alias{label_1d_grid}
\alias{layout_1d_grid}
\alias{group_2d_grid}
\alias{points_2d_grid}
\alias{qq_2d_grid}
\alias{density_2d_grid}
\alias{axes_2d_grid}
\alias{arrow_2d_grid}
\alias{rect_2d_grid}
\alias{label_2d_grid}
\alias{layout_2d_grid}
\title{Grid-Based Plotting Functions}
\description{
    The 1d and 2d plotting functions based on the \R package \pkg{grid}.
}
\usage{
rug_1d_grid(zargs,
            loc = 0.5, length = 0.5, width = 1e-3, col = par("fg"),
            draw = FALSE, ...)
points_1d_grid(zargs,
               loc = 0.5, pch = 21, size = 0.02,
               draw = FALSE, ...)
jitter_1d_grid(zargs,
               loc = 0.5, offset = 0.25, pch = 21, size = 0.02,
               draw = FALSE, ...)
hist_1d_grid(zargs,
             breaks = NULL, length.out = 21, col = NULL, fill = NULL,
             draw = FALSE, ...)
density_1d_grid(zargs,
                density... = NULL, offset = 0.08,
                draw = FALSE, ...)
boxplot_1d_grid(zargs,
                pch = 21, size = 0.02,
                col = NULL, lwd = 2, bpwidth = 0.5, range = NULL,
                draw = FALSE, ...)
arrow_1d_grid(zargs,
              loc = c(0.5, 0.5), angle = 60, length = 0.6,
              draw = FALSE, ...)
rect_1d_grid(zargs,
             loc = c(0.5, 0.5), width = 1, height = 1,
             draw = FALSE, ...)
lines_1d_grid(zargs,
              loc = c(0.5, 0.5), length = 1, arrow = NULL,
              draw = FALSE, ...)
label_1d_grid(zargs,
              loc = c(0.5, 0.5), label = NULL, cex = 0.66,
              box = FALSE, box.width = 1, box.height = 1,
              draw = FALSE, ...)
layout_1d_grid(zargs, ...)


group_2d_grid(zargs,
              glabs = NULL, sep = "\n", loc = c(0.5, 0.5),
              draw = FALSE, ...)
points_2d_grid(zargs,
               type = c("p", "l", "o"), pch = NULL, size = 0.02,
               box = FALSE, box.width = 1, box.height = 1,
               group... = list(cex = 0.66), draw = FALSE, ...)
qq_2d_grid(zargs,
           do.line = TRUE, lines... = NULL, pch = NULL, size = 0.02,
           box = FALSE, box.width = 1, box.height = 1,
           group... = list(cex = 0.66), draw = FALSE, ...)
density_2d_grid(zargs,
                ngrids = 25, ccol = NULL, clwd = 1, clty = 1,
                box = FALSE, box.width = 1, box.height = 1,
                group... = list(cex = 0.66), draw = FALSE, ...)
axes_2d_grid(zargs,
             angle = 30, length = unit(0.05, "npc"), type = "open", eps = 0.02,
             group... = list(cex = 0.66), draw = FALSE, ...)
arrow_2d_grid(zargs,
              loc = c(0.5, 0.5), angle = 60, length = 0.2,
              group... = list(cex = 0.66), draw = FALSE, ...)
rect_2d_grid(zargs,
             loc = c(0.5, 0.5), width = 1, height = 1,
             group... = list(cex = 0.66), draw = FALSE, ...)
label_2d_grid(zargs,
              loc = c(0.98, 0.05), label = NULL, cex = 0.66,
              just = c("right", "bottom"), rot = 0,
              box = FALSE, box.width = 1, box.height = 1,
              group... = list(cex = cex), draw = FALSE, ...)
layout_2d_grid(zargs, ...)
}
\arguments{
  \item{zargs}{argument list as passed from \code{\link{zenplot}()}.}
  \item{width}{width (passed on to the underlying \pkg{grid} functions).}
  \item{height}{height (passed on to the underlying \pkg{grid} functions).}
  \item{just}{justification (see \code{\link{rectGrob}()} and
    \code{\link{textGrob}()}).}
  \item{col}{for
    \describe{
      \item{\code{rug_1d_grid}:}{color and fill color of the
	rectangels forming the rugs.}
      \item{\code{boxplot_1d_grid}:}{color of the box, whiskers
        and points.}
      \item{\code{hist_1d_grid}:}{color of the bins.}
    }
  }
  \item{draw}{\code{\link{logical}} indicating whether graphics output
    is produced.}
  \item{pch}{plot symbol.}
  \item{size}{plot symbol size as passed to
    \code{\link{pointsGrob}()}.}
  \item{offset}{number in \eqn{[0, 0.5]} determining how far away the
    plot stays from the plot margins (for creating space between the two).}
  \item{lwd}{line width.}
  \item{bpwidth}{width of the boxplot (in \code{default.units}).}
  \item{range}{determines how far the plot whiskers extend out of the box. If
    \code{range = NULL}, this will be automatically determined depending
    on the sample size.}
  \item{breaks}{break points for the histogram as passed to the
    underlying \code{\link{hist}()}. If \code{NULL}, the default is to
    use 20 equi-width bins covering the range of the data.}
  \item{length.out}{number of break points if \code{is.null(breaks)}.}
  \item{fill}{fill color of the bins.}
  \item{loc}{(x,y)-location of the center of the arrow.}
  \item{arrow}{see \code{\link{linesGrob}()}.}
  \item{label}{label to be used (with default being the column names
    of the data if \code{\link{NULL}}).}
  \item{rot}{rotation of the label in degrees.}
  \item{box}{\code{\link{logical}} indicating whether a box is drawn
    around the plot region.}
  \item{box.width}{width of the box (if drawn).}
  \item{box.height}{height of the box (if drawn).}
  \item{cex}{character expansion (aims for a useful default for grid
    but might not always be suitable -- for that one would need to know
    both the number of rows and columns in the plot layout and yet this
    would still be affected by the size of the plot window).}
  \item{glabs}{group labels being indexed by the plot variables
    (thus need to be of length as the number of plot variables);
    if \code{NULL}, they are determined with \code{\link{extract_2d}()}
    and the underlying \code{\link{burst}()}.}
  \item{sep}{a separator \code{\link{character}} for the group labels.}
  \item{group...}{list of arguments passed to
    \code{\link{group_2d_grid}()} (or \code{NULL})}.
  \item{do.line}{\code{\link{logical}} indicating whether a Q-Q line is
    plotted.}
  \item{lines...}{\code{\link{list}} of additional arguments passed to
    the underlying \code{\link{lines}()} function for drawing the Q-Q
    line through the empirical 0.25- and 0.75-quantiles.}
  \item{ngrids}{number of grid points in each dimension (a scalar or
    an integer vector of length two).}
  \item{ccol, clwd, clty}{colors (\code{col}), line widths
    (\code{lwd}) andline types (\code{lty}) of the contour lines.
    These can be single values or vectors (which are then recycled).}
  \item{angle}{angle between the two edges of the arrow head.}
  \item{length}{length of the arrow in [0,1] from tip to base.}
  \item{type}{axis type.}
  \item{eps}{distance by which the axes are moved away from the plot region.}
  \item{density...}{\code{\link{list}()} of arguments for
    the underlying \code{\link{density}()}.}
  \item{\dots}{additional (graphical) parameters passed to
    \code{\link{gpar}()}.}
}
\value{
    (Mostly) the underlying \code{\link[grid]{grob}} via \code{\link{invisible}()}.
}
\details{
    These functions based on the \R package \pkg{grid} are provided as
    useful defaults for the arguments \code{plot1d} and \code{plot2d} of
    \code{\link{zenplot}()}, respectively. See
    \code{\link{zenplot}()} for how to use them,
    their source code for how to adjust them or how to write your own
    \code{plot1d} or \code{plot2d}. The main idea is that \code{\link{zenplot}()}
    passes on the \code{zargs} arguments to the \code{plot1d} or
    \code{plot2d} functions and the ellipsis argument is used to pass
    down all other (mostly graphical) parameters (to both \code{plot1d} or
    \code{plot2d}; via \code{\link{gpar}()}).
}
\author{Marius Hofert and Wayne Oldford}
%% \references{
%%   Hofert, M., Oldford, W. (2015). Zigzag Expanded Navigation Plots.
%%   \emph{} \bold{}(), --.
%% }
\seealso{
  \code{\link{zenplot}()} for how to use these functions.
}
\examples{
## Implementation of 1d functions (for plot1d of zenplot())
rug_1d_grid
points_1d_grid
jitter_1d_grid
density_1d_grid
boxplot_1d_grid
hist_1d_grid
arrow_1d_grid
rect_1d_grid
lines_1d_grid
label_1d_grid
layout_1d_grid

## Implementation of 2d functions (for plot2d of zenplot())
group_2d_grid
points_2d_grid
qq_2d_grid
density_2d_grid
axes_2d_grid
arrow_2d_grid
rect_2d_grid
label_2d_grid
layout_2d_grid
}
\keyword{dplot}