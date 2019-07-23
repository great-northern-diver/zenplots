\name{plots_graphics}
\alias{plots_graphics}
\alias{rug_1d_graphics}
\alias{points_1d_graphics}
\alias{jitter_1d_graphics}
\alias{density_1d_graphics}
\alias{boxplot_1d_graphics}
\alias{hist_1d_graphics}
\alias{arrow_1d_graphics}
\alias{rect_1d_graphics}
\alias{lines_1d_graphics}
\alias{label_1d_graphics}
\alias{layout_1d_graphics}
\alias{group_2d_graphics}
\alias{points_2d_graphics}
\alias{qq_2d_graphics}
\alias{density_2d_graphics}
\alias{axes_2d_graphics}
\alias{arrow_2d_graphics}
\alias{rect_2d_graphics}
\alias{label_2d_graphics}
\alias{layout_2d_graphics}
\title{Graphics-Based Plotting Functions}
\description{
    The 1d and 2d plotting functions based on the \R package \pkg{graphics}.
}
\usage{
rug_1d_graphics(zargs,
                loc = 0.5, length = 0.5, width = 1, col = par("fg"),
                add = FALSE, plot... = NULL, ...)
points_1d_graphics(zargs,
                   loc = 0.5, cex = 0.4,
                   add = FALSE, plot... = NULL, ...)
jitter_1d_graphics(zargs,
                   loc = 0.5, offset = 0.25, cex = 0.4,
                   add = FALSE, plot... = NULL, ...)
hist_1d_graphics(zargs,
                 breaks = NULL, length.out = 21, col = NULL,
                 plot... = NULL, ...)
density_1d_graphics(zargs,
                    density... = NULL, offset = 0.08,
                    add = FALSE, plot... = NULL, ...)
boxplot_1d_graphics(zargs,
                    cex = 0.4, range = NULL, axes = FALSE,
                    add = FALSE, ...)
arrow_1d_graphics(zargs,
                  loc = c(0.5, 0.5), angle = 60, length = 0.6,
                  add = FALSE, plot... = NULL, ...)
rect_1d_graphics(zargs,
                 loc = c(0.5, 0.5), width = 1, height = 1,
                 add = FALSE, plot... = NULL, ...)
lines_1d_graphics(zargs,
                  loc = c(0.5, 0.5), length = 1,
                  add = FALSE, plot... = NULL, ...)
label_1d_graphics(zargs,
                  loc = c(0.5, 0.5), label = NULL, box = FALSE,
                  add = FALSE, plot... = NULL, ...)
layout_1d_graphics(zargs, ...)


group_2d_graphics(zargs,
                  glabs = NULL, sep = "\n", loc = c(0.5, 0.5),
                  add = FALSE, plot... = NULL, ...)
points_2d_graphics(zargs,
                   cex = 0.4, box = FALSE,
                   add = FALSE, group... = NULL, plot... = NULL, ...)
qq_2d_graphics(zargs,
               do.line = TRUE, lines... = NULL, cex = 0.4, box = FALSE,
               add = FALSE, group... = NULL, plot... = NULL, ...)
density_2d_graphics(zargs,
                    ngrids = 25, drawlabels = FALSE,
                    axes = FALSE, box = FALSE,
                    add = FALSE, group... = NULL, ...)
axes_2d_graphics(zargs,
                 length = 0.1, eps = 0.04, code = 2, xpd = NA,
                 add = FALSE, group... = NULL, plot... = NULL, ...)
arrow_2d_graphics(zargs,
                  loc = c(0.5, 0.5), angle = 60, length = 0.2,
                  add = FALSE, group... = NULL, plot... = NULL, ...)
rect_2d_graphics(zargs,
                 loc = c(0.5, 0.5), width = 1, height = 1,
                 add = FALSE, group... = NULL, plot... = NULL, ...)
label_2d_graphics(zargs,
                  loc = c(0.98, 0.05), label = NULL, adj = 1:0, box = FALSE,
                  add = FALSE, group... = NULL, plot... = NULL, ...)
layout_2d_graphics(zargs, ...)
}
\arguments{
  \item{zargs}{argument list as passed from \code{\link{zenplot}()}.}
  \item{width}{width of the rugs/rectangle.}
  \item{height}{height of the rugs/rectangle.}
  \item{col}{color (of the rugs) or vector of colors (for the bars
    and bar components; see \code{\link{barplot}()}).}
  \item{add}{\code{\link{logical}} indicating whether the current plot
    should be added to (or on top of) the previous one.}
  \item{axes}{A \code{\link{logical}} indicating whether axes should be drawn.}
  \item{cex}{character expansion factor.}
  \item{offset}{number in \eqn{[0, 0.5]} determining how far away the
    plot stays from the plot margins (for creating space between the two).}
  \item{range}{argument \code{range} of the underlying \code{\link{boxplot}()}
    (determines how far the plot whiskers extend out of the box). If
    \code{range = NULL}, this will be automatically determined depending
    on the sample size.}
  \item{breaks}{break points for the histogram as passed to the
    underlying \code{\link{hist}()}. If \code{NULL}, the default is to
    use 20 equi-width bins covering the range of the data.}
  \item{length.out}{number of break points if \code{is.null(breaks)}.}
  \item{loc}{x-location or (x,y)-location (for 1d plots when viewed in
    the direction of the path; for 2d plots when viewed in normal
    viewing direction) of the center of the respective geometric shape
    or plot.}
  \item{angle}{angle between the two edges of the arrow head.}
  \item{length}{length of the arrow in \eqn{[0,1]} from tip to base.}
  \item{label}{label to be used (with default being the column names
    of the data if \code{\link{NULL}}).}
  \item{box}{\code{\link{logical}} indicating whether a box is drawn
    around the plot region.}
  \item{glabs}{group labels being indexed by the plot variables
    (thus need to be of length as the number of plot variables);
    if \code{NULL}, they are determined with \code{\link{extract_2d}()}
    and the underlying \code{\link{burst}()}.}
  \item{sep}{a separator \code{\link{character}} for the group labels.}
  \item{do.line}{\code{\link{logical}} indicating whether a Q-Q line is
    plotted.}
  \item{lines...}{\code{\link{list}} of additional arguments passed to
    the underlying \code{\link{lines}()} function for drawing the Q-Q
    line through the empirical 0.25- and 0.75-quantiles.}
  \item{ngrids}{number of grid points in each dimension (a scalar or
    an integer vector of length two).}
  \item{drawlabels}{\code{\link{logical}} indicating whether
    the contours should be labeled.}
  \item{eps}{distance by which the axes are moved away from the plot region.}
  \item{code}{integer code determining the kind of arrows to be drawn;
    see \code{\link{arrows}}.}
  \item{xpd}{\code{\link{logical}} or \code{NA}, determining the
    region with respect to which clipping takes place; see
    \code{\link{par}()}.}
  \item{adj}{x (and optionally y) adjustment of the label.}
  \item{density...}{\code{\link{list}} of additional arguments passed to
    the underlying \code{\link{density}()}.}
  \item{group...}{\code{\link{list}()} of additional arguments passed to
    \code{\link{group_2d_graphics}()}.}
  \item{plot...}{\code{\link{list}} of additional arguments passed to
    the underlying \code{\link{plot}()}.}
  \item{\dots}{additional arguments passed to the underlying
    \pkg{graphics} functions.}
}
\value{
  (Mostly) \code{\link{invisible}()}.
}
\details{
    These functions based on the \R package \pkg{graphics} are provided
    as useful defaults for the arguments \code{plot1d} and \code{plot2d}
    of \code{\link{zenplot}()}, respectively. See
    \code{\link{zenplot}()} for how to use them,
    their source code for how to adjust them or how to write your own
    \code{plot1d} or \code{plot2d}. The main idea is that \code{\link{zenplot}()}
    passes on the \code{zargs} arguments to the \code{plot1d} or
    \code{plot2d} functions and the ellipsis argument is used to pass
    down all other (mostly graphical) parameters (to both \code{plot1d} or
    \code{plot2d}).

    Overlaying of different \pkg{graphics} functions might not
    always turn out nicely (e.g. arrows over a boxplot; the latter
    creates problems concerning the spacing). For such tasks, it is
    recommended to work with \pkg{grid} via \code{pkg = "grid"} in
    \code{\link{zenplot}()}.
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
rug_1d_graphics
points_1d_graphics
jitter_1d_graphics
density_1d_graphics
boxplot_1d_graphics
hist_1d_graphics
arrow_1d_graphics
rect_1d_graphics
lines_1d_graphics
label_1d_graphics
layout_1d_graphics

## Implementation of 2d functions (for plot2d of zenplot())
group_2d_graphics
points_2d_graphics
qq_2d_graphics
density_2d_graphics
axes_2d_graphics
arrow_2d_graphics
rect_2d_graphics
label_2d_graphics
layout_2d_graphics
}
\keyword{dplot}