% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot2dgraphics.R
\name{group_2d_graphics}
\alias{group_2d_graphics}
\title{Plot of labels indicating adjacent groups using R's base graphics}
\usage{
group_2d_graphics(
  zargs,
  glabs = NULL,
  sep = "\\n",
  loc = c(0.5, 0.5),
  add = FALSE,
  plot... = NULL,
  ...
)
}
\arguments{
\item{zargs}{argument list as passed from \code{\link{zenplot}()}}

\item{glabs}{group labels being indexed by the plot variables
(and thus of length as the number of variables);
if NULL then they are determined with extract_2d()}

\item{sep}{group label separator}

\item{loc}{(x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
the direction of the path)}

\item{add}{logical indicating whether this plot should be added to the last one}

\item{plot...}{additional arguments passed to plot_region()}

\item{...}{additional arguments passed to text()}
}
\value{
invisible()
}
\description{
Plot of labels indicating adjacent groups using R's base graphics
}
\note{
For performance reasons (avoiding having to call extract_2d() twice),
      'glabs' is an extra argument
}
\seealso{
Other default 2d plot functions using R's base graphics: 
\code{\link{arrow_2d_graphics}()},
\code{\link{axes_2d_graphics}()},
\code{\link{density_2d_graphics}()},
\code{\link{label_2d_graphics}()},
\code{\link{points_2d_graphics}()},
\code{\link{qq_2d_graphics}()},
\code{\link{rect_2d_graphics}()}

Other default 2d plot functions: 
\code{\link{arrow_2d_graphics}()},
\code{\link{arrow_2d_grid}()},
\code{\link{arrow_2d_loon}()},
\code{\link{axes_2d_graphics}()},
\code{\link{axes_2d_grid}()},
\code{\link{axes_2d_loon}()},
\code{\link{density_2d_graphics}()},
\code{\link{density_2d_grid}()},
\code{\link{density_2d_loon}()},
\code{\link{extract_2d}()},
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
\concept{default 2d plot functions using R's base graphics}
