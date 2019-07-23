\name{plots_loon}
\alias{plots_loon}
\alias{rug_1d_loon}
\alias{points_1d_loon}
\alias{jitter_1d_loon}
\alias{density_1d_loon}
\alias{boxplot_1d_loon}
\alias{hist_1d_loon}
\alias{arrow_1d_loon}
\alias{rect_1d_loon}
\alias{lines_1d_loon}
\alias{label_1d_loon}
\alias{layout_1d_loon}
\alias{group_2d_loon}
\alias{points_2d_loon}
\alias{density_2d_loon}
\alias{axes_2d_loon}
\alias{arrow_2d_loon}
\alias{rect_2d_loon}
\alias{label_2d_loon}
\alias{layout_2d_loon}
\title{Loon-Based Plotting Functions}
\description{
    The 1d and 2d plotting functions based on the \R package loon.
}
\usage{
rug_1d_loon(zargs, ...)
points_1d_loon(zargs,
               linkingGroup = NULL, linkingKey = NULL,
               showLabels = FALSE, showScales = FALSE,
               showGuides = FALSE, glyph = "ocircle",
               itemLabel = NULL, showItemLabels = TRUE,
               parent = NULL, ...)
jitter_1d_loon(zargs,
               linkingGroup = NULL, showLabels = FALSE,
               showScales = FALSE, showGuides = FALSE,
               glyph = "ocircle", itemLabel = NULL,
               showItemLabels = TRUE, parent = NULL, ...)
hist_1d_loon(zargs,
             breaks = NULL, color = NULL, fill = NULL,
             showStackedColors = TRUE,
             showBinHandle = FALSE, showLabels = FALSE,
             linkingGroup = NULL, showScales = FALSE,
             showGuides = FALSE, parent = NULL, ...)
density_1d_loon(zargs,
                density.args = list(), method = c("single", "double"),
                lwd = NULL, linewidth = NULL, color = NULL,
                fill = NULL, linecolor = NULL, linkingGroup = NULL,
                showLabels = FALSE, showScales = FALSE,
                showGuides = FALSE, baseplot = NULL, parent = NULL, ...)
boxplot_1d_loon(zargs,
                color = NULL, linecolor = NULL, lwd = 2,
                range = NULL, showLabels = FALSE, showScales = FALSE,
                showGuides = FALSE, linkingGroup = NULL,
                baseplot = NULL, parent, ...)
arrow_1d_loon(zargs,
              loc = c(0.5, 0.5), length = 0.6, angle = NULL,
              linkingGroup = NULL, showLabels = FALSE,
              showScales = FALSE, showGuides = FALSE,
              baseplot = NULL, parent = NULL, ...)
rect_1d_loon(zargs,
             loc.x = NULL, loc.y = NULL, color = NULL,
             fill = NULL, lwd = 1,
             linkingGroup = NULL, showLabels = FALSE,
             showScales = FALSE, showGuides = FALSE,
             baseplot = NULL, parent = NULL, ...)
lines_1d_loon(zargs,
              loc.x = NULL, loc.y = NULL,
              color = NULL, lwd = 1,
              linkingGroup = NULL,
              showLabels = FALSE, showScales = FALSE,
              showGuides = FALSE, baseplot = NULL,
              parent = NULL, ...)
label_1d_loon(zargs,
              loc.x = NULL, loc.y = NULL, label = NULL,
              rot = NULL, size = 8, box = FALSE, color = NULL,
              linkingGroup = NULL, showLabels = FALSE,
              showScales = FALSE, showGuides = FALSE,
              baseplot = NULL, parent = NULL, ...)
layout_1d_loon(zargs, ...)


group_2d_loon(zargs,
              glabs = NULL, sep = "\n", size = 8, rot = 0,
              baseplot = NULL, parent = NULL, ...)
points_2d_loon(zargs,
               showLabels = FALSE, showScales = FALSE,
               showGuides = FALSE, linkingGroup = NULL,
               linkingKey = NULL, glyph = "ocircle",
               itemLabel = NULL, showItemLabels = TRUE,
               parent = NULL, group... = NULL, ...)
density_2d_loon(zargs, ngrids = 25,
                ccol = NULL, color = NULL, clwd = NULL, lwd = NULL,
                linewidth = 1, showLabels = FALSE,
                showScales = FALSE, showGuides = FALSE,
                linkingGroup = NULL,
                baseplot = NULL, parent = NULL, group... = NULL, ...)
axes_2d_loon(zargs,
             angle = 30, length = 0.05, eps = 0.02,
             linkingGroup = NULL, color = NULL, showLabels = FALSE,
             showScales = FALSE, showGuides = FALSE,
             baseplot = NULL, parent = NULL,
             group... = NULL, ...)
arrow_2d_loon(zargs,
              loc = rep(0.5, 2), length = 0.2, angle = 30,
              linkingGroup = NULL, color = NULL,
              showLabels = FALSE, showScales = FALSE,
              showGuides = FALSE, baseplot = NULL, parent = NULL,
              group... = NULL, ...)
rect_2d_loon(zargs, loc.x = NULL, loc.y = NULL, color = NULL,
             fill = NULL, lwd = 1, linkingGroup = NULL,
             showLabels = FALSE, showScales = FALSE,
             showGuides = FALSE, baseplot = NULL,
             parent = NULL, group... = NULL, ...)
label_2d_loon(zargs,
              loc = NULL, label = NULL, rot = 0, size = 8,
              box = FALSE, color = NULL,
              linkingGroup = NULL, showLabels = FALSE,
              showScales = FALSE, showGuides = FALSE,
              baseplot = NULL, parent = NULL,
              group... = NULL, ...)
layout_2d_loon(zargs, ...)
}
\arguments{
  \item{zargs}{argument list as passed from \code{\link{zenplot}()}.}
  \item{linkingGroup}{string specifying the initial group of plots to
    be linked to this plot.}
  \item{linkingKey}{list of IDs to link on.}
  \item{showLabels}{logical determining whether axis labels are displayed.}
  \item{showScales}{logical determining whether scales are displayed.}
  \item{showGuides}{logical determining whether the background
    guidelines are displayed.}
  \item{glyph}{string determining the glyph type to be displayed for
    points, default is an open circle: "ocircle".}
  \item{itemLabel}{vector of strings to serve as the item label.}
  \item{showItemLabels}{logical determing whether item labels display on
    mouse hover.}
  \item{parent}{tk parent for this loon plot widget.}
  \item{density.args}{\code{\link{list}()} of arguments for
    \code{\link{density}()}.}
  \item{method}{type of density plot used (\code{single} or \code{double};
    the latter being reflected).}
  \item{lwd}{line width.}
  \item{linewidth}{for
    \describe{
      \item{\code{density_1d_loon}:}{line width of outline for density
	polygons (highest priority).}
      \item{\code{density_2d_loon}:}{line width used when both
	\code{clwd} and \code{lwd} are \code{NULL}, value of 1 used otherwise.}
    }
  }
  \item{linecolor}{for
    \describe{
      \item{\code{density_1d_loon}:}{colour used for the outline of the
	density.}
      \item{\code{boxplot_1d_loon}:}{colour used for the lines to draw
	the boxplot.}
    }
  }
  \item{color}{color.}
  \item{fill}{fill color.}
  \item{baseplot}{if non-null the base plot on which the plot should be
    layered.}
  \item{range}{determines how far the plot whiskers extend out of the box.
    If \code{range = NULL}, this will be automatically determined depending
    on the sample size.}
  \item{breaks}{break points for the histogram as passed to the
    underlying \code{\link{hist}()}. If \code{NULL}, the default is to
    use 20 equi-width bins covering the range of the data.}
  \item{showStackedColors}{logical determining whether
    to show the individual point colours stacked in the histogram.}
  \item{showBinHandle}{logical to show a handle to adjust bins.}
  \item{loc}{location of the center of the arrow.}
  \item{length}{length of the arrow head.}
  \item{angle}{angle between the shaft and one of the edges of the
    arrow head.}
  \item{loc.x, loc.y}{x- and y-location.}
  \item{label}{label to be used (with a useful default if
    \code{\link{NULL}}).}
  \item{rot}{rotation of the label in degrees.}
  \item{size}{plot size.}
  \item{box}{\code{\link{logical}} indicating whether a box is drawn
    around the plot.}
  \item{glabs}{group labels being indexed by the plot variables
    (thus need to be of length as the number of plot variables);
    if \code{NULL}, they are determined with \code{\link{extract_2d}()}
    and the underlying \code{\link{burst}()}.}
  \item{sep}{a separator \code{\link{character}} for the group labels.}
  \item{group...}{list of arguments passed to
    \code{\link{group_2d_loon}()} (or \code{NULL})}.
  \item{ngrids}{number of grid points in each dimension (a scalar or
    an integer vector of length two).}
  \item{ccol, clwd}{colors (\code{ccol}) and line widths (\code{clwd})
    of the contour lines. These can be single values or
    vectors (which are then recycled).}
  \item{eps}{distance by which the axes are moved away from the plot region.}
  \item{\dots}{additional arguments passed to the underlying loon functions.}
}
\value{
  The path, layout and a loon object (returned invisibly).
}
\details{
    These functions based on the \R package loon are provided as
    useful choices for the arguments \code{plot1d} and \code{plot2d} of
    \code{\link{zenplot}()}. See \code{\link{zenplot}()} how to use
    them, their source code for how to adjust them, their source code
    for how to adjust them or how towrite your own
    \code{plot1d} or \code{plot2d}. The main idea is that \code{\link{zenplot}()}
    passes on the \code{zargs} arguments to the \code{plot1d} or
    \code{plot2d} functions and the ellipsis argument is used to pass
    down all other (mostly graphical) parameters (to both \code{plot1d} or
    \code{plot2d}).
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
rug_1d_loon
points_1d_loon
jitter_1d_loon
density_1d_loon
boxplot_1d_loon
hist_1d_loon
arrow_1d_loon
rect_1d_loon
lines_1d_loon
label_1d_loon
layout_1d_loon

## Implementation of 2d functions (for plot2d of zenplot())
group_2d_loon
points_2d_loon
density_2d_loon
axes_2d_loon
arrow_2d_loon
rect_2d_loon
label_2d_loon
layout_2d_loon
}
\keyword{dplot}