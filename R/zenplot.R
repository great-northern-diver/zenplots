## Unfolding and zenplots
##' @title Unfold the hypercube and produce all information concerning the zenpath
##'        and zenplot layout
##' @family creating zenplots
##' @name unfold
##' @aliases unfold
##' @description The \code{unfold()} function imagines each pair of variables/dimensions
##' as a "face" of a high dimensional cube. These faces are "unfolded" from one 2d space
##' or "face" to the next about the 1d face or "edge" they share.  The \code{unfold()}
##' function  takes, as first argument, \code{nfaces},
##' the number of 2d plots/spaces to be "unfolded" and produces the zenpath and
##' zenplot layout required for the function zenplot().  Laying out these pairs
##' with a zenplot is what is alluded to as an "unfolding" of (at least a part of)
##' the high dimensional space.
##' @usage
##' unfold(nfaces, turns = NULL,
##'        n2dcols = c("letter", "square", "A4", "golden", "legal"),
##'        method = c("tidy", "double.zigzag", "single.zigzag", "rectangular"),
##'        first1d = TRUE, last1d = TRUE, width1d = 1, width2d = 10)
##' @param nfaces The number of faces of the hypercube to unfold
##' @param turns A \code{\link{character}} vector (of length two times the
##' number of variables to be plotted minus 1) consisting of \code{"d"},
##' \code{"u"}, \code{"r"} or \code{"l"} indicating the turns out of the
##' current plot position; if \code{NULL}, the \code{turns} are
##' constructed.
##' @param n2dcols number of columns of 2d plots (\eqn{\ge 1}{>= 1})
##' or one of \code{"letter"}, \code{"square"}, \code{"A4"},
##' \code{"golden"} or \code{"legal"} in which case a similar layout is constructed.
##' Note that \code{n2dcols} is ignored if \code{!is.null(turns)}.
##' @param method The type of zigzag plot (a \code{\link{character}}).
##'
##' Available are:
##' \describe{
##'     \item{\code{tidy}:}{more tidied-up \code{double.zigzag}
##'          (slightly more compact placement of plots towards the end).}
##'     \item{\code{double.zigzag}:}{zigzag plot in the form of a
##'         flipped \dQuote{S}. Along this path, the plots
##'         are placed in the form of an \dQuote{S} which is rotated
##'         counterclockwise by 90 degrees.}
##'     \item{\code{single.zigzag}:}{zigzag plot in the form of a
##'         flipped \dQuote{S}.}
##'     \item{\code{rectangular}:}{plots that fill the page from
##'         left to right and top to bottom. This is useful (and most compact)
##'         for plots that do not share an axis.}
##' }
##' Note that \code{method} is ignored if \code{turns} are provided.
##' @param first1d A \code{\link{logical}} indicating whether the first one-dimensional (1d)
##'  plot should be plotted.
##' @param last1d A \code{\link{logical}} indicating whether the last one-dimensional (1d)
##'  plot should be plotted
##' @param width1d A graphical parameter > 0 giving the width of 1d plots.
##' @param width2d A graphical parameter > 0 giving the width of 2d plots.
##' @return A \code{\link{list}} describing the unfolded path and its layout
##'  as a list of named components:
##' \describe{
##'     \item{\code{path}:}{the path of the unfolding, itself given
##'     as a structured \code{\link{list}} having components
##'     \describe{
##'     \item{\code{turns}:}{the sequence of turns
##'      -- each being one of \dQuote{l} (for left), \dQuote{r} (for right),
##'     \dQuote{d} (for down), and \dQuote{u}  (for up) --
##'     required to move from the current plot location in the display to the next along
##'     the unfolded path.}
##'     \item{\code{positions}:}{the path as a matrix of \code{(x, y)} positions giving
##'      the indices in the \code{occupancy} matrix of each plot in the path.}
##'     \item{\code{occupancy}:}{A rectangular array whose cells indicate the positions
##'     of the plots on the page.}
##'     }
##'     }
##'     \item{\code{layout}:}{the details of the visual layout of the plots and given
##'     as a structured \code{\link{list}} having components
##'     \describe{
##'     \item{\code{orientations}:}{a vector indicating the orientation of each of the
##'     displays in order -- \dQuote{h} for horizontal, \dQuote{v} for vertical, and
##'     \dQuote{s} for square.}
##'     \item{\code{dimensions}:}{a vector giving the dimensionality of each
##'     plot in order.}
##'     \item{\code{vars}:}{A matrix of the variable indices to be used in each plot -- \code{x}
##'     being the horizontal variable and \code{y} the vertical.}
##'     \item{\code{layoutWidth}:}{A positive integer giving the display width of
##'     a 2d plot.}
##'     \item{\code{layoutHeight}:}{A positive integer giving the display height of
##'     a 2d plot.}
##'     \item{\code{boundingBoxes}:}{A matrix of 4 columns giving locations (\code{left},
##'     \code{right}, \code{bottom}, and \code{top}) of the box which bound each of the
##'     plots in order.}
##'     }
##'     }
##'     }
##' @author Marius Hofert and Wayne Oldford
##' @export
##' @note Although \code{unfold()} is probably rather rarely used directly by a user,
##'  it provides insight into how zenplots are constructed.
##' @examples
##' dim <- 20
##' unfolding <- unfold(nfaces = dim -1)
##' names(unfolding)
unfold <- function(nfaces, turns = NULL,
                   n2dcols = c("letter", "square", "A4", "golden", "legal"),
                   method = c("tidy", "double.zigzag", "single.zigzag", "rectangular"),
                   first1d = TRUE, last1d = TRUE, width1d = 1, width2d = 10)
{
    ## Checking
    stopifnot(nfaces >= 0, is.logical(first1d), is.logical(last1d), length(width1d) == 1,
              length(width2d) == 1, width1d >= 0, width2d >= 0)
    if(nfaces == 0 && (!first1d || !last1d))
        stop("'first1d' or 'last1d' can only be FALSE if 'nfaces' is >= 1.")
    if(is.character(n2dcols)) n2dcols <- n2dcols_aux(nfaces, method = n2dcols)
    if(is.null(turns)) { # turns not provided => use n2dcols and method
        stopifnot(length(n2dcols) == 1, n2dcols >= 1)
        if(nfaces >= 2 && n2dcols < 2)
            stop("If nfaces >= 2, n2dcols must be >= 2.")
        method <- match.arg(method)
    } else { # turns provided
        ## If the turns are provided, we should check them *before* calling
        ## get_path(). Otherwise (see below), we check them after they
        ## have been constructed by get_path()
        turn_checker(turns, n2dplots = nfaces, first1d = first1d, last1d = last1d)
    }

    ## 1) Construct the path (= turns, positions in the occupancy matrix
    ##    and the occupancy matrix)
    path <- get_path(turns, n2dplots = nfaces, n2dcols = n2dcols,
                     method = method, first1d = first1d, last1d = last1d)
    ##    If 'turns' is not provided, extract them now (for checking and computing
    ##    the layout via get_layout())
    if(is.null(turns)) {
        turns <- path$turns
        turn_checker(turns, n2dplots = nfaces, first1d = first1d, last1d = last1d)
    }

    ## 2) Determine the layout
    layout <- get_layout(turns, n2dplots = nfaces, first1d = first1d, last1d = last1d,
                         width1d = width1d, width2d = width2d)

    ## Return
    list(path = path, layout = layout)
}

## Set up hidden (not found on ls()) environment in R_GlobalEnv for burst object 'x'
.zenplots_burst_envir <- new.env(hash = FALSE, parent = emptyenv()) # define the environment to cache the burst x

##' @title Main function to create a zenplot
##' @family creating zenplots
##' @name zenplot
##' @aliases zenplot
##' @description Constructs and draws a zigzag expanded navigation plot for a
##' graphical exploratory analysis of a path of variables.  The result is an
##' alternating sequence of one-dimensional (1d) and two-dimensional (2d) plots
##' laid out in a zigzag-like structure so that each consecutive pair of 2d plots has one of its
##' variates (or coordinates) in common with that of the 1d plot appearing between them.
##' @usage
##' zenplot(x, turns = NULL,
##'         first1d = TRUE, last1d = TRUE,
##'         n2dcols = c("letter", "square", "A4", "golden", "legal"),
##'         n2dplots = NULL,
##'         plot1d = c("label", "points", "jitter", "density", "boxplot", "hist",
##'                    "rug", "arrow", "rect", "lines", "layout"),
##'         plot2d = c("points", "density", "axes", "label", "arrow", "rect", "layout"),
##'         zargs = c(x = TRUE, turns = TRUE, orientations = TRUE,
##'                   vars = TRUE, num = TRUE, lim = TRUE, labs = TRUE,
##'                   width1d = TRUE, width2d = TRUE,
##'                   ispace = match.arg(pkg) != "graphics"),
##'         lim = c("individual", "groupwise", "global"),
##'         labs = list(group = "G", var = "V", sep = ", ", group2d = FALSE),
##'         pkg = c("graphics", "grid", "loon"),
##'         method = c("tidy", "double.zigzag", "single.zigzag", "rectangular"),
##'         width1d = if(is.null(plot1d)) 0.5 else 1,
##'         width2d = 10,
##'         ospace = if(pkg == "loon") 0 else 0.02,
##'         ispace = if(pkg == "graphics") 0 else 0.037,
##'         draw = TRUE,
##'         ...)
##' @param x A data object of "standard forms", being a \code{\link{vector}}, or a \code{\link{matrix}},
##'        or a \code{\link{data.frame}}, or a \code{\link{list}} of any of these.
##'        In the case of a list, the components of \code{x} are interpreted as
##'        groups of data which are visually separated by a two-dimensional
##'        (group) plot.
##' @param turns A \code{\link{character}} vector (of length two times the
##'        number of variables to be plotted minus 1) consisting of \code{"d"},
##'        \code{"u"}, \code{"r"} or \code{"l"} indicating the turns out of the
##'        current plot position; if \code{NULL}, the \code{turns} are
##'        constructed (if \code{x} is of the "standard form" described above).
##' @param first1d A \code{\link{logical}} indicating whether the first
##'        one-dimensional plot is included.
##' @param last1d A \code{\link{logical}} indicating whether the last
##'        one-dimensional plot is included.
##' @param n2dcols number of columns of 2d plots (\eqn{\ge 1}{>= 1})
##'        or one of \code{"letter"}, \code{"square"}, \code{"A4"},
##'        \code{"golden"} or \code{"legal"}
##'        in which case a similar layout is constructed.
##'        Note that \code{n2dcols} is ignored if \code{!is.null(turns)}.
##' @param n2dplots The number of 2d plots.
##' @param plot1d A \code{\link{function}} to use to return a
##'        one-dimensional plot constructed with package \code{pkg}.
##'        Alternatively, a \code{\link{character}} string of an existing
##'        function.
##'        For the defaults provided, the corresponding functions
##'        are obtained when appending \code{_1d_graphics}, \code{_1d_grid}
##'        or \code{_1d_loon} depending on which \code{pkg} is used.
##'
##'        If \code{plot1d = NULL}, then no 1d plot is produced in the \code{zenplot}.
##' @param plot2d A \code{\link{function}} returning a two-dimensional plot
##'        constructed with package \code{pkg}.
##'        Alternatively, a \code{\link{character}} string of an existing
##'        function. For the defaults provided, the corresponding functions
##'        are obtained when appending \code{_2d_graphics}, \code{_2d_grid}
##'        or \code{_2d_loon} depending on which \code{pkg} is used.
##'
##'        As for \code{plot1d}, \code{plot2d} omits 2d plots if \code{plot2d = NULL}.
##' @param zargs A fully named \code{\link{logical}} \code{\link{vector}}
##'        indicating whether the respective arguments are (possibly) passed to
##'        \code{plot1d()} and \code{plot2d()} (if the latter contain the
##'        formal argument \code{zargs}, which they typically do/should, but
##'        see below for an example in which they do not).
##'
##'        \code{zargs} can maximally contain all variables as given in the default.
##'        If one of those variables does not appear in \code{zargs}, it is
##'        treated as \code{TRUE} and the corresponding arguments are passed
##'        on to \code{plot1d} and \code{plot2d}. If one of them is set to
##'        \code{FALSE}, the argument is not passed on.
##' @param lim (x-/y-)axis limits. This can be a \code{\link{character}} string
##'        or a \code{numeric(2)}.
##'
##'        If \code{lim = "groupwise"} and \code{x} does not contain groups,
##'        the behaviour is equivalent to \code{lim = "global"}.
##'
##' @param labs The plot labels to be used; see the argument \code{labs} of
##'        \code{\link{burst}()} for the exact specification.
##'        \code{labs} can, in general, be anything as long as \code{plot1d}
##'        and \code{plot2d} know how to deal with it.
##' @param pkg The R package used for plotting (depends on how the
##'        functions \code{plot1d} and \code{plot2d} were constructed;
##'        the user is responsible for choosing the appropriate package
##'        among the supported ones).
##' @param method The type of zigzag plot (a \code{\link{character}}).
##'
##'        Available are:
##'        \describe{
##'            \item{\code{tidy}:}{more tidied-up \code{double.zigzag}
##'                 (slightly more compact placement of plots towards the end).}
##'            \item{\code{double.zigzag}:}{zigzag plot in the form of a
##'                flipped \dQuote{S}. Along this path, the plots
##'                are placed in the form of an \dQuote{S} which is rotated
##'                counterclockwise by 90 degrees.}
##'            \item{\code{single.zigzag}:}{zigzag plot in the form of a
##'                flipped \dQuote{S}.}
##'            \item{\code{rectangular}:}{plots that fill the page from
##'                left to right and top to bottom. This is useful (and most compact)
##'                for plots that do not share an axis.}
##'        }
##'        Note that \code{method} is ignored if \code{turns} are provided.
##' @param width1d A graphical parameter > 0 giving the width of 1d plots.
##' @param width2d A graphical parameter > 0 giving the height of 2d plots.
##' @param ospace The outer space around the zenplot.   A vector
##'        of length four (bottom, left, top, right),
##'        or one whose values are repeated to be of length four,
##'        which gives the outer space between the device region and
##'        the inner plot region around the zenplot.
##'
##'        Values should be in \eqn{[0,1]}  when \code{pkg} is \code{"graphics"} or
##'        \code{"grid"}, and as number of pixels when\code{pkg} is \code{"loon"}.
##' @param ispace The inner space in \eqn{[0,1]} between the each figure region
##'        and the region of the (1d/2d) plot it contains.
##'        Again, a vector of length four (bottom, left, top, right) or a shorter one
##'        whose values are repeated to produce a vector of length four.
##' @param draw A \code{\link{logical}} indicating whether a the \code{zenplot}
##'        is immediately displayed (the default) or not.
##' @param ... arguments passed to the drawing functions for both \code{plot1d} and
##'        \code{plot2d}. If you need to pass certain arguments only to one
##'        of them, say, \code{plot2d}, consider providing your own
##'        \code{plot2d}; see the examples below.
##' @return (besides plotting) invisibly returns a list having additional classnames
##'        marking it as a zenplot and a zenPkg object (with Pkg being one of Graphics,
##'        Grid, or Loon, so as to identify the
##'        package used to construct the plot).  
##'        
##'        As a list it contains at least
##'        the path and layout (see \code{\link{unfold}} for details).
##'
##'        Depending on the graphics package \code{pkg} used, the returned list
##'        includes additional components.  For \code{pkg = "grid"},
##'        this will be the whole plot as a \code{\link[grid]{grob}} (grid object).
##'        For \code{pkg = "loon"},  this will be the whole plot as a
##'        \code{loon} plot object as
##'        well as the toplevel \code{tk} object in which the plot appears.
##'
##' @author Marius Hofert and Wayne Oldford
##' @seealso  All provided default \code{plot1d} and \code{plot2d} functions.
##'
##' \code{\link{extract_1d}()} and \code{\link{extract_2d}()}
##' for how \code{zargs} can be split up into a list of columns and corresponding
##' group and variable information.
##'
##' \code{\link{burst}()} for how \code{x} can be split up into all sorts of
##' information useful for plotting (see our default \code{plot1d} and \code{plot2d}).
##' \code{\link{vport}()} for how to construct a viewport for
##' (our default) \pkg{grid} (\code{plot1d} and \code{plot2d}) functions.
##'
##' \code{\link{extract_pairs}()}, \code{\link{connect_pairs}()},
##' \code{\link{group}()} and \code{\link{zenpath}()} for
##' (zen)path-related functions.
##'
##' The various vignettes for additional examples.
##' @keywords hplot
##' @export
##' @examples
##' ### Basics #####################################################################
##'
##' ## Generate some data
##' n <- 1000 # sample size
##' d <- 20 # dimension
##' set.seed(271) # set seed (for reproducibility)
##' x <- matrix(rnorm(n * d), ncol = d) # i.i.d. N(0,1) data
##'
##' ## A basic zenplot
##' res <- zenplot(x)
##' uf <- unfold(nfaces = d - 1)
##' ## `res` and `uf` is not identical as `res` has specific
##' ## class attributes.
##' for(name in names(uf)) {
##'   stopifnot(identical(res[[name]], uf[[name]]))
##' }
##'
##' ## => The return value of zenplot() is the underlying unfold()
##'
##' ## Some missing data
##' z <- x
##' z[seq_len(n-10), 5] <- NA # all NA except 10 points
##' zenplot(z)
##'
##' ## Another column with fully missing data (use arrows)
##' ## Note: This could be more 'compactified', but is technically
##' ##       more involved
##' z[, 6] <- NA # all NA
##' zenplot(z)
##'
##' ## Lists of vectors, matrices and data frames as arguments (=> groups of data)
##' ## Only two vectors
##' z <- list(x[,1], x[,2])
##' zenplot(z)
##'
##' ## A matrix and a vector
##' z <- list(x[,1:2], x[,3])
##' zenplot(z)
##'
##' ## A matrix, NA column and a vector
##' z <- list(x[,1:2], NA, x[,3])
##' zenplot(z)
##' z <- list(x[,1:2], cbind(NA, NA), x[,3])
##' zenplot(z)
##' z <- list(x[,1:2], 1:10, x[,3])
##' zenplot(z)
##'
##' ## Without labels or with different labels
##' z <- list(A = x[,1:2], B = cbind(NA, NA), C = x[,3])
##' zenplot(z, labs = NULL) # without any labels
##' zenplot(z, labs = list(group = NULL, group2d = TRUE)) # without group labels
##' zenplot(z, labs = list(group = NULL)) # without group labels unless groups change
##' zenplot(z, labs = list(var = NULL)) # without variable labels
##' zenplot(z, labs = list(var = "Variable ", sep = " - ")) # change default labels
##'
##' ## Example with a factor
##' zenplot(iris)
##' zenplot(iris, lim = "global") # global scaling of axis
##' zenplot(iris, lim = "groupwise") # acts as 'global' here (no groups in the data)
##'
##'
##' ### More sophisticated examples ################################################
##'
##' ## Note: The third component (data.frame) naturally has default labels.
##' ##       zenplot() uses these labels and prepends a default group label.
##' z <- list(x[,1:5], x[1:10, 6:7], NA,
##'           data.frame(x[seq_len(round(n/5)), 8:19]), cbind(NA, NA), x[1:10, 20])
##' zenplot(z, labs = list(group = "Group ")) # change the group label (var and sep are defaults)
##' ## Alternatively, give z labels
##' names(z) <- paste("Group", LETTERS[seq_len(length(z))]) # give group names
##' zenplot(z) # uses given group names
##' ## Now let's change the variable labels
##' z. <- lapply(z, function(z.) {
##'                      if(!is.matrix(z.)) z. <- as.matrix(z.)
##'                      colnames(z.) <- paste("Var.", seq_len(ncol(z.)))
##'                      z.
##'                      }
##'             )
##' zenplot(z.)
##'
##'
##' ### A dynamic plot based on 'loon' (if installed and R compiled with tcl support)
##'
##' \dontrun{
##'     if(requireNamespace("loon", quietly = TRUE))
##'         zenplot(x, pkg = "loon")
##' }
##'
##'
##' ### Providing your own turns ###################################################
##'
##' ## A basic example
##' turns <- c("l","d","d","r","r","d","d","r","r","u","u","r","r","u","u","l","l",
##'            "u","u","l","l","u","u","l","l","d","d","l","l","d","d","l","l",
##'            "d","d","r","r","d","d")
##' zenplot(x, plot1d = "layout", plot2d = "layout", turns = turns) # layout of plot regions
##' ## => The tiles stick together as ispace = 0.
##' zenplot(x, plot1d = "layout", plot2d = "layout", turns = turns,
##'         pkg = "grid") # layout of plot regions with grid
##' ## => Here the tiles show the small (default) ispace
##'
##' ## Another example (with own turns and groups)
##' zenplot(list(x[,1:3], x[,4:7]), plot1d = "arrow", plot2d = "rect",
##'         turns = c("d", "r", "r", "r", "r", "d",
##'                   "d", "l", "l", "l", "l", "l"), last1d = FALSE)
##'
##'
##' ### Providing your own plot1d() or plot2d() ####################################
##'
##' ## Creating a box
##' zenplot(x, plot1d = "label", plot2d = function(zargs)
##'     density_2d_graphics(zargs, box = TRUE))
##'
##' ## With grid
##' \donttest{
##'     zenplot(x, plot1d = "label", plot2d = function(zargs)
##'         density_2d_grid(zargs, box = TRUE), pkg = "grid")
##' }
##'
##' ## An example with width1d = width2d and where no zargs are passed on.
##' ## Note: This could have also been done with 'rect_2d_graphics(zargs, col = ...)'
##' ##       as plot1d and plot2d.
##' myrect <- function(...) {
##'     plot(NA, type = "n", ann = FALSE, axes = FALSE, xlim = 0:1, ylim = 0:1)
##'     rect(xleft = 0, ybottom = 0, xright = 1, ytop = 1, ...)
##' }
##' zenplot(matrix(0, ncol = 15),
##'         n2dcol = "square", width1d = 10, width2d = 10,
##'         plot1d = function(...) myrect(col = "royalblue3"),
##'         plot2d = function(...) myrect(col = "maroon3"))
##'
##' ## Colorized rugs as plot1d()
##' basecol <- c("royalblue3", "darkorange2", "maroon3")
##' palette <- colorRampPalette(basecol, space = "Lab")
##' cols <- palette(d) # different color for each 1d plot
##' zenplot(x, plot1d = function(zargs) {
##'               rug_1d_graphics(zargs, col = cols[(zargs$num+1)/2])
##'               }
##'        )
##'
##' ## With grid
##' library(grid) # for gTree() and gList()
##' \donttest{
##'     zenplot(x, pkg = "grid", # you are responsible for choosing the right pkg (cannot be tested!)
##'             plot1d = function(zargs)
##'                 rug_1d_grid(zargs, col = cols[(zargs$num+1)/2]))
##' }
##'
##' ## Rectangles with labels as plot2d() (shows how to overlay plots)
##' ## With graphics
##' ## Note: myplot2d() could be written directly in a simpler way, but is
##' ##       based on the two functions here to show how they can be combined.
##' zenplot(x, plot1d = "arrow", plot2d = function(zargs) {
##'     rect_2d_graphics(zargs)
##'     label_2d_graphics(zargs, add = TRUE)
##' })
##'
##' ## With grid
##' \donttest{
##'     zenplot(x, pkg = "grid", plot1d = "arrow", plot2d = function(zargs)
##'         gTree(children = gList(rect_2d_grid(zargs),
##'                                label_2d_grid(zargs))))
##' }
##'
##' ## Rectangles with labels outside the 2d plotting region as plot2d()
##' ## With graphics
##' zenplot(x, plot1d = "arrow", plot2d = function(zargs) {
##'     rect_2d_graphics(zargs)
##'     label_2d_graphics(zargs, add = TRUE, xpd = NA, srt = 90,
##'                       loc = c(1.04, 0), adj = c(0,1), cex = 0.7)
##' })
##'
##' ## With grid
##' \donttest{
##'     zenplot(x, pkg = "grid", plot1d = "arrow", plot2d = function(zargs)
##'         gTree(children = gList(rect_2d_grid(zargs),
##'                                label_2d_grid(zargs, loc = c(1.04, 0),
##'                                              just = c("left", "top"),
##'                                              rot = 90, cex = 0.45))))
##' }
##'
##' ## 2d density with points, 1d arrows and labels
##' zenplot(x, plot1d = function(zargs) {
##'     rect_1d_graphics(zargs)
##'     arrow_1d_graphics(zargs, add = TRUE, loc = c(0.2, 0.5))
##'     label_1d_graphics(zargs, add = TRUE, loc = c(0.8, 0.5))
##' }, plot2d = function(zargs) {
##'     points_2d_graphics(zargs, col = adjustcolor("black", alpha.f = 0.4))
##'     density_2d_graphics(zargs, add = TRUE)
##' })
##'
##' ## 2d density with labels, 1d histogram with density and label
##' ## Note: The 1d plots are *improper* overlays here as the density
##' ##       plot does not know the heights of the histogram. In other
##' ##       words, both histograms and densities use the whole 1d plot
##' ##       region but are not correct relative to each other in the
##' ##       sense of covering the same are. For a *proper* overlay
##' ##       see below.
##' zenplot(x,
##'     plot1d = function(zargs) {
##'                     hist_1d_graphics(zargs)
##'                     density_1d_graphics(zargs, add = TRUE,
##'                                         border = "royalblue3",
##'                                         lwd = 1.4)
##'                     label_1d_graphics(zargs, add = TRUE,
##'                                       loc = c(0.2, 0.8),
##'                                       cex = 0.6, font = 2,
##'                                       col = "darkorange2")
##'                     },
##'     plot2d = function(zargs) {
##'                     density_2d_graphics(zargs)
##'                     points_2d_graphics(zargs, add = TRUE,
##'                                        col = adjustcolor("black", alpha.f = 0.3))
##'                                 }
##'             )
##'
##'
##' ### More sophisticated examples ################################################
##'
##' ### Example: Overlaying histograms with densities (the *proper* way)
##' \donttest{
##' ## Define proper 1d plot for overlaying histograms with densities
##' hist_with_density_1d <- function(zargs)
##' {
##' ## Extract information and data
##' num <- zargs$num # plot number (among all 1d and 2d plots)
##' turn.out <- zargs$turns[num] # turn out of current position
##' horizontal <- turn.out == "d" || turn.out == "u"
##' # the indices of the 'x' variable to be displayed in the current plot
##' ii <- plot_indices(zargs)
##' label <- paste0("V", ii[1]) # label
##' srt <- if(horizontal) 0 else if(turn.out == "r") -90 else 90 # label rotation
##' x <- zargs$x[,ii[1]] # data
##' lim <- range(x) # data limits
##' ## Compute histogram information
##' breaks <- seq(from = lim[1], to = lim[2], length.out = 21)
##' binInfo <- hist(x, breaks = breaks, plot = FALSE)
##' binBoundaries <- binInfo$breaks
##' widths <- diff(binBoundaries)
##' heights <- binInfo$density
##' ## Compute density information
##' dens <- density(x)
##' xvals <- dens$x
##' keepers <- (min(x) <= xvals) & (xvals <= max(x)) # keep those within the range of the data
##' x. <- xvals[keepers]
##' y. <- dens$y[keepers]
##' ## Determine plot limits and data
##' if(turn.out == "d" || turn.out == "l") { # flip density/histogram
##'     heights <- -heights
##'     y. <- -y.
##' }
##' if(horizontal) {
##'     xlim <- lim
##'     xlim.bp <- xlim - xlim[1] # special for barplot(); need to shift the bars
##'     ylim <- range(0, heights, y.)
##'     ylim.bp <- ylim
##'     x <- c(xlim[1], x., xlim[2]) - xlim[1] # shift due to plot region set up by barplot()
##'     y <- c(0, y., 0)
##' } else {
##'     xlim <- range(0, heights, y.)
##'     xlim.bp <- xlim
##'     ylim <- lim
##'     ylim.bp <- ylim - ylim[1] # special for barplot(); need to shift the bars
##'     x <-  c(0, y., 0)
##'     y <- c(xlim[1], x., xlim[2]) - ylim[1] # shift due to plot region set up by barplot()
##' }
##' ## Determining label position relative to the zenpath
##' loc <- c(0.1, 0.6)
##'
##' # when walking downwards, change both left/right and up/down
##' if(turn.out == "d") loc <- 1-loc
##'
##' # when walking to the right, coordinates change and 2nd is flipped
##' if(turn.out == "r") {
##'     loc <- rev(loc)
##'     loc[2] <- 1-loc[2]
##' }
##'
##' # when walking to the left, coordinates change and 1st is flipped
##' if(turn.out == "l") {
##'     loc <- rev(loc)
##'     loc[1] <- 1-loc[1]
##' }
##' ## Plotting
##' barplot(heights, width = widths, xlim = xlim.bp, ylim = ylim.bp,
##'         space = 0, horiz = !horizontal, main = "", xlab = "", axes = FALSE) # histogram
##' polygon(x = x, y = y, border = "royalblue3", lwd = 1.4) # density
##' opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates for text
##' on.exit(par(opar))
##' text(x = loc[1], y = loc[2], labels = label, cex = 0.7, srt = srt, font = 2,
##'      col = "darkorange2") # label
##'     }
##'
##' ## Zenplot
##' zenplot(x,
##' plot1d = "hist_with_density_1d",
##' plot2d = function(zargs) {
##'        density_2d_graphics(zargs)
##'        points_2d_graphics(zargs,
##'                           add = TRUE,
##'                           col = adjustcolor("black", alpha.f = 0.3))
##' }
##' )
##' }
##'
##' ### Example: A path through pairs of a grouped t copula sample
##'
##' \donttest{
##' ## 1) Build a random sample from a 17-dimensional grouped t copula
##' d. <- c(8, 5, 4) # sector dimensions
##' d <- sum(d.) # total dimension
##' nu <- rep(c(12, 1, 0.25), times = d.) # d.o.f. for each dimension
##' n <- 500 # sample size
##' set.seed(271)
##' Z <- matrix(rnorm(n * d), ncol = n) # (d,n)-matrix
##' P <- matrix(0.5, nrow = d, ncol = d)
##' diag(P) <- 1
##' L <- t(chol(P))   # L: LL^T = P
##' Y <- t(L %*% Z) # (n,d)-matrix containing n d-vectors following N(0,P)
##' U. <- runif(n)
##' W <- sapply(nu, function(nu.) 1/qgamma(U., shape = nu./2, rate = nu./2)) # (n,d)-matrix
##' X <- sqrt(W) * Y # (n,d)-matrix
##' U <- sapply(1:d, function(j) pt(X[,j], df = nu[j])) # (n,d)-matrix
##'
##' ## 2) Plot the data with a pairs plot, colorizing the groups
##' cols <- matrix("black", nrow = d, ncol = d) # colors
##' start <- c(1, cumsum(head(d., n = -1))+1) # block start indices
##' end <- cumsum(d.) # block end indices
##' for(j in seq_along(d.)) cols[start[j]:end[j], start[j]:end[j]] <- basecol[j] # colors
##' diag(cols) <- NA # remove colors corresponding to diagonal entries
##' cols <- as.vector(cols) # convert to a vector
##' cols <- cols[!is.na(cols)] # remove NA entries corresponding to diagonal
##' count <- 0 # panel number
##' my_panel <- function(x, y, ...) # panel function for colorizing groups
##'              { count <<- count + 1; points(x, y, pch = ".", col = cols[count]) }
##' pairs(U, panel = my_panel, gap = 0,
##'      labels = as.expression( sapply(1:d, function(j) bquote(italic(U[.(j)]))) ))
##'
##' ## 3) Zenplot of a random path through all pairs, colorizing the respective group
##' ## Define our own points_2d_grid() for colorizing the groups
##' my_points_2d_grid <- function(zargs, basecol, d.) {
##'        r <- extract_2d(zargs) # extract information from zargs
##'        x <- r$x
##'        y <- r$y
##'        xlim <- r$xlim
##'        ylim <- r$ylim
##'        num2d <- zargs$num/2
##'        vars <- as.numeric(r$vlabs[num2d:(num2d+1)]) # two variables to be plotted
##'        ## Alternatively, we could have used ord[r$vars[num2d:(num2d+1)]] with
##'        ## the order 'ord' (see below) being passed to my_points_2d_grid()
##'        col <- if(all(1 <= vars & vars <= d.[1])) { basecol[1] } else {
##'            if(all(d.[1]+1 <= vars & vars <= d.[1]+d.[2])) { basecol[2] } else {
##'                if(all(d.[1]+d.[2]+1 <= vars & vars <= d)) basecol[3] else "black"
##'            }
##'        } # determine the colors
##'        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim, x = x, y = y) # viewport
##'        pointsGrob(x = x[[1]], y = y[[1]], pch = 21, size = unit(0.02, units = "npc"),
##'                   name = "points_2d", gp = gpar(col = col), vp = vp)
##'    }
##' ## Plot a random permutation of columns via a zenplot
##' ## Note: We set column labels here, as otherwise the labels can only
##' ##       show *indices* of the variables to be plotted, i.e., the column
##' ##       number in U[,ord], and not the original column number in U (which
##' ##       is what we want to see in order to see how our 'path' through
##' ##       the pairs of variables looks like).
##' colnames(U) <- 1:d
##' set.seed(1)
##' (ord <- sample(1:d, size = d)) # path; 1:d would walk parallel to the secondary diagonal
##' zenplot(U[,ord], plot1d = "layout", plot2d = "layout", pkg = "grid") # layout
##' zenplot(U[,ord], # has correct variable names as column names
##'         pkg = "grid",
##'         plot1d = function(zargs) arrow_1d_grid(zargs, col = "grey50"),
##'         plot2d = function(zargs)
##'                gTree(children = gList(
##'                    my_points_2d_grid(zargs, basecol = basecol, d. = d.),
##'                    rect_2d_grid(zargs, width = 1.05, height = 1.05,
##'                                 col = "grey50", lty = 3),
##'                    label_2d_grid(zargs, loc = c(1.06, -0.03),
##'                                  just = c("left", "top"), rot = 90, cex = 0.45,
##'                                  fontface = "bold") )))
##' ## => The points are colorized correctly (compare with the pairs plot).
##' }
##'
##'
##' ### Using ggplot2 ##############################################################
##'
##' ## Although not thoroughly tested, in principle ggplot2 can also be used via
##' ## pkg = "grid" as follows.
##' \donttest{
##' library(ggplot2)
##'
##' ## Define our own 2d plot
##' my_points_2d_ggplot <- function(zargs, extract2d = TRUE)
##'    {
##'        if(extract2d) {
##'            r <- extract_2d(zargs) # extract results from zargs
##'            df <- data.frame(r$x, r$y) # data frame
##'            names(df) <- c("x", "y")
##'            cols <- zargs$x[,"Species"]
##'        } else {
##'            ii <- plot_indices(zargs) # the indices of the variables to be plotted
##'            irs <- zargs$x # iris data
##'            df <- data.frame(x = irs[,ii[1]], y = irs[,ii[2]]) # data frame
##'            cols <- irs[,"Species"]
##'        }
##'        num2d <- zargs$num/2 # plot number among all 2d plots
##'        p <- ggplot() + geom_point(data = df, aes(x = x, y = y, colour = cols),
##'                                   show.legend = num2d == 3) +
##'            labs(x = "", y = "") # 2d plot
##'        if(num2d == 3) p <- p + theme(legend.position = "bottom", # legend for last 2d plot
##'                                      legend.title = element_blank())
##'        ggplot_gtable(ggplot_build(p)) # 2d plot as grob
##'    }
##'
##' ## Plotting
##' iris. <- iris
##' colnames(iris.) <- gsub("\\\\.", " ", x = colnames(iris)) # => nicer 1d labels
##' zenplot(iris., n2dplots = 3, plot2d = "my_points_2d_ggplot", pkg = "grid")
##' zenplot(iris., n2dplots = 3,
##'         plot2d = function(zargs) my_points_2d_ggplot(zargs, extract2d = FALSE),
##'         pkg = "grid")
##' }
##'
##'
##' ### Providing your own data structure ##########################################
##'
##' \donttest{
##' ## Danger zone: An example with a new data structure (here: a list of *lists*)
##' ## Note: - In this case, we most likely need to provide both plot1d and plot2d
##' ##         (but not in this case here since arrow_1d_graphics() does not depend
##' ##         on the data structure)
##' ##       - Note that we still make use of zargs here.
##' ##       - Also note that the variables are not correctly aligned anymore:
##' ##         In the ggplot2 examples we guaranteed this by plot_indices(),
##' ##         but here we don't. This then still produces our layout but the
##' ##         x/y axis of adjacent plots might not be the same anymore. This is
##' ##         fine if only a certain order of the plots is of interest, but
##' ##         not a comparison between adjacent plots.
##' z <- list(list(1:5, 2:1, 1:3), list(1:5, 1:2))
##' zenplot(z, n2dplots = 4, plot1d = "arrow", last1d = FALSE,
##'         plot2d = function(zargs, ...) {
##'             r <- unlist(zargs$x, recursive = FALSE)
##'             num2d <- zargs$num/2 # plot number among 2d plots
##'             x <- r[[num2d]]
##'             y <- r[[num2d + 1]]
##'             if(length(x) < length(y)) x <- rep(x, length.out = length(y))
##'             else if(length(y) < length(x)) y <- rep(y, length.out = length(x))
##'             plot(x, y, type = "b", xlab = "", ylab = "")
##'         }, ispace = c(0.2, 0.2, 0.1, 0.1))
##' }
##'
##'
##' ### Zenplots based on 3d lattice plots #########################################
##'
##' \donttest{
##' library(lattice)
##' library(grid)
##' library(gridExtra)
##'
##' ## Build a list of cloud() plots (trellis objects)
##' ## Note:
##' ## - 'grid' problem: Without print(), the below zenplot() may fail (e.g.,
##' ##   in fresh R sessions) with: 'Error in UseMethod("depth") :
##' ##   no applicable method for 'depth' applied to an object of class "NULL"'
##' ## - col = "black" inside scales is needed to make the ticks show
##' mycloud <- function(x, num) {
##'        lim <- extendrange(0:1, f = 0.04)
##'        print(cloud(x[, 3] ~ x[, 1] * x[, 2], xlim = lim, ylim = lim, zlim = lim,
##'                    xlab = substitute(U[i.], list(i. = num)),
##'                    ylab = substitute(U[i.], list(i. = num + 1)),
##'                    zlab = substitute(U[i.], list(i. = num + 2)),
##'                    zoom = 1, scales = list(arrows = FALSE, col = "black"),
##'                    col = "black",
##'                    par.settings = list(standard.theme(color = FALSE),
##'                                        axis.line = list(col = "transparent"),
##'                                        clip = list(panel = "off"))))
##'    }
##' plst.3d <- lapply(1:4, function(i)
##'        mycloud(x[,i:(i+2)], num = i)) # list of trellis objects
##'
##' ## Preparing the zenplot
##' num <- length(plst.3d)
##' ncols <- 2
##' turns <- c(rep("r", 2*(ncols-1)), "d", "d",
##'            rep("l", 2*(ncols-1)), "d")
##' plot2d <- function(zargs) {
##'        num2d <- (zargs$num+1)/2
##'        vp <- vport(zargs$ispace, xlim = 0:1, ylim = 0:1)
##'        grob(p = zargs$x[[num2d]], vp = vp, cl = "lattice") # convert trellis to grid object
##'        ## Note: For further plots, Work with
##'        ##       gTree(children = gList(grob(zargs$x[[num2d]], vp = vp,
##'        ##                                   cl = "lattice")))
##'    }
##'
##' ## Zenplot
##' ## Note: We use a list of *plots* here already (not data)
##' zenplot(plst.3d, turns = turns, n2dplots = num, pkg = "grid", first1d = FALSE,
##'         last1d = FALSE, plot1d = "arrow_1d_grid", plot2d = plot2d)
##' }
zenplot <- function(x, turns = NULL, first1d = TRUE, last1d = TRUE,
                    n2dcols = c("letter", "square", "A4", "golden", "legal"),
                    n2dplots = NULL,
                    plot1d = c("label", "points", "jitter", "density", "boxplot",
                               "hist", "rug", "arrow", "rect", "lines", "layout"),
                    plot2d = c("points", "density", "axes", "label", "arrow",
                               "rect", "layout"),
                    zargs = c(x = TRUE, turns = TRUE, orientations = TRUE,
                              vars = TRUE, num = TRUE, lim = TRUE, labs = TRUE,
                              width1d = TRUE, width2d = TRUE,
                              ispace = match.arg(pkg) != "graphics"),
                    lim = c("individual", "groupwise", "global"),
                    labs = list(group = "G", var = "V", sep = ", ", group2d = FALSE),
                    pkg = c("graphics", "grid", "loon"),
                    method = c("tidy", "double.zigzag", "single.zigzag", "rectangular"),
                    width1d = if(is.null(plot1d)) 0.5 else 1, width2d = 10,
                    ospace = if(pkg == "loon") 0 else 0.02,
                    ispace = if(pkg == "graphics") 0 else 0.037,
                    draw = TRUE, ...)
{

### Check and define basic variables ###########################################

    ## Check whether 'x' is of standard form and check 'n2dplots'
    if(is.standard(x) && is.null(n2dplots)) {
        n2dplots <- num_cols(x) - 1
    } else {
        if(is.null(n2dplots))
            stop("'n2dplots' must be provided if 'x' is not a vector, matrix, data.frame or list of such.")
    }
    if(!is.numeric(n2dplots) || n2dplots < 0 || (n2dplots %% 1 != 0))
        stop("'n2dplots' must be a nonnegative number.")

    ## Check zargs
    nms <- names(zargs)
    if(!is.logical(zargs) || is.null(nms) || any(nms == ""))
        stop("'zargs' has to be a (fully) named, logical vector.")
    if(!all(nms %in% c("x", "turns", "orientations", "vars", "num", "lim", "labs", "width1d", "width2d", "ispace")))
        stop("The only valid components of 'zargs' are \"x\", \"turns\", \"orientations\", \"vars\", \"num\", \"lim\", \"labs\", \"width1d\", \"width2d\" or \"ispace\".")

    ## Check lim
    if(is.character(lim)) {
        lim <- match.arg(lim)
    } else {
        if(!(is.numeric(lim) && length(lim) == 2))
            stop("'lim' must be a character string or numeric(2).")
    }

    ## Default for n2dcols
    if(is.character(n2dcols))
        n2dcols <- n2dcols_aux(n2dplots, method = n2dcols)

    ## Check logicals and turns
    stopifnot(is.logical(first1d), is.logical(last1d), is.logical(draw))
    if(n2dplots == 0 && (!first1d || !last1d))
        stop("'first1d' or 'last1d' can only be FALSE if 'n2dplots' is >= 1.")
    if(is.null(turns)) { # turns not provided => use n2dcols and method
        stopifnot(length(n2dcols) == 1, n2dcols >= 1)
        if(n2dplots >= 2 && n2dcols < 2)
            stop("If the number of 2d plots is >= 2, n2dcols must be >= 2.")
        method <- match.arg(method)
    } else { # turns provided
        ## Check length of 'turns'
        turn_checker(turns, n2dplots = n2dplots, first1d = first1d, last1d = last1d)
    }

    ## Check width1d, width2d
    ## Note: Use the same defaults in the respective *_1d/2d_graphics/grid functions
    stopifnot(length(width1d) == 1, width1d > 0, length(width2d) == 1, width2d > 0)

    ## Check pkg
    ## Note: If you provide your own function, you have to choose 'pkg' accordingly
    pkg <- match.arg(pkg)
    if(pkg == "grid" && !requireNamespace("grid", quietly = TRUE))
        stop("Package 'grid' is not available.")
    if(pkg == "loon" && !requireNamespace("loon", quietly = TRUE))
        stop("Package 'loon' is not available.")

    ## Check plot1d
    if(missing(plot1d)) plot1d <- match.arg(plot1d)
    if(!is.null(plot1d)) {
        if(is.character(plot1d)) {
            if(plot1d %in% eval(formals(zenplot)$plot1d)) # we don't use partial matching here as this could conflict with a user's provided string
                plot1d <- paste(plot1d, "1d", pkg, sep = "_")
            ## Note: see below for plot2d
        } else {
            if(!is.function(plot1d))
                stop("'plot1d' has to be either a character string or a function.")
        }
        if(!plot_exists(plot1d))
            stop("Function provided as argument 'plot1d' does not exist.")
    } # => plot1d either NULL, "<defaults>_<pkg>", a string of an existing function or an existing function

    ## Check plot2d
    if(missing(plot2d)) plot2d <- match.arg(plot2d)
    if(!is.null(plot2d)) {
        if(is.character(plot2d)) {
            if(plot2d %in% eval(formals(zenplot)$plot2d)) # we don't use partial matching here as this could conflict with a user's provided string
                plot2d <- paste(plot2d, "2d", pkg, sep = "_")
            ## Note: We don't throw an error in the 'else' case as the user can provide
            ##       a string of an existing or self-defined function as well. This may
            ##       lead to problems. For example, if plot2d = "lines" (does not exist
            ##       as one of the provided options), R's 1d lines() function is used
            ##       (which of course fails).
        } else {
            if(!is.function(plot2d))
                stop("'plot2d' has to be either a character string or a function")
        }
        if(!plot_exists(plot2d))
            stop("Function provided as argument 'plot2d' does not exist.")
    } # => plot2d either NULL, "<defaults>_<pkg>", a string of an existing function or an existing function

    ## Check ospace and ispace
    if(length(ospace) != 4) ospace <- rep(ospace, length.out = 4)
    if(length(ispace) != 4) ispace <- rep(ispace, length.out = 4)


### 1) Get arguments, variable names etc., call unfold(), determine layout #####

    ## Get '...' arguments
    .args <- list(...)

    ## Call unfold() to compute the path and corresponding layout
    ## Note: This is *independent* of the data
    pathLayout <- unfold(n2dplots, turns = turns, n2dcols = n2dcols, method = method,
                         first1d = first1d, last1d = last1d,
                         width1d = width1d, width2d = width2d)
    path <- pathLayout$path
    layout <- pathLayout$layout
    bbs <- layout$boundingBoxes
    vars <- layout$vars # 2-column matrix of plot variables (= indices)
    dims <- layout$dimensions
    orientations <- layout$orientations
    layoutWidth <- layout$layoutWidth
    layoutHeight <- layout$layoutHeight
    turns <- path$turns
    nPlots <- nrow(bbs)
    stopifnot(nPlots == nrow(vars)) # fail-safe programming

    ## Determine layout
    fg.rows <- unique(bbs[,c("bottom", "top"), drop = FALSE])
    fg.rows <- fg.rows[order(fg.rows[,1], decreasing = TRUE),, drop = FALSE]
    fg.cols <- unique(bbs[,c("left", "right"), drop = FALSE])
    fg.cols <- fg.cols[order(fg.cols[,1], decreasing = FALSE),, drop = FALSE]
    fg.nrow <- nrow(fg.rows)
    fg.ncol <- nrow(fg.cols)
    heights <- (fg.rows[,  "top"] - fg.rows[,"bottom"]) / layoutHeight
    widths  <- (fg.cols[,"right"] - fg.cols[,  "left"]) / layoutWidth


### 2) Determine formal arguments of plot1d() and plot2d() to be passed ########

    ## Decide whether to add the object named arg to the argument list zargs
    add_to_zargs <- function(arg) {
        exists.arg <- arg %in% names(zargs)
        !exists.arg || (exists.arg && zargs[[arg]]) # if not appearing in zargs or appearing and TRUE, add it to the argument list zargs (only if set to FALSE, they are omitted)
    }

    ## Determine whether the formal argument 'zargs' needs to be constructed,
    ## filled and passed on to plot1d() and plot2d()

    ## 1d plots
    zargs1d <- list()
    if(!is.null(plot1d) && "zargs" %in% names(eval(formals(plot1d)))) { # if 'zargs' is a formal argument of plot1d()
        if(add_to_zargs("x")) zargs1d <- c(zargs1d, list(x = x)) # the original data object
        if(add_to_zargs("turns")) zargs1d <- c(zargs1d, list(turns = turns)) # the vector of turns
        if(add_to_zargs("orientations")) zargs1d <- c(zargs1d, list(orientations = orientations)) # the vector of orientations
        if(add_to_zargs("vars")) zargs1d <- c(zargs1d, list(vars = vars)) # the 2-column matrix of plot variables
        if(add_to_zargs("lim")) zargs1d <- c(zargs1d, list(lim = lim)) # character string containing the plot limits or plot limits themselves
        if(add_to_zargs("labs")) zargs1d <- c(zargs1d, list(labs = labs)) # the argument 'labs' of zenplot()
        if(add_to_zargs("width1d")) zargs1d <- c(zargs1d, list(width1d = width1d)) # the width of the 1d plots
        if(add_to_zargs("width2d")) zargs1d <- c(zargs1d, list(width2d = width2d)) # the width of the 2d plots
        if(add_to_zargs("num")) zargs1d <- c(zargs1d, list(num = NULL)) # current plot number
        if(add_to_zargs("ispace")) zargs1d <- c(zargs1d, list(ispace = ispace)) # the inner space
    }

    ## 2d plots
    zargs2d <- list()
    if(!is.null(plot2d) && "zargs" %in% names(eval(formals(plot2d)))) { # if 'zargs' is a formal argument of plot2d()
        if(add_to_zargs("x")) zargs2d <- c(zargs2d, list(x = x)) # the original data object
        if(add_to_zargs("turns")) zargs2d <- c(zargs2d, list(turns = turns)) # the vector of turns
        if(add_to_zargs("orientations")) zargs2d <- c(zargs2d, list(orientations = orientations)) # the vector of orientations
        if(add_to_zargs("vars")) zargs2d <- c(zargs2d, list(vars = vars)) # the 2-column matrix of plot variables
        if(add_to_zargs("lim")) zargs2d <- c(zargs2d, list(lim = lim)) # character string containing the plot limits or plot limits themselves
        if(add_to_zargs("labs")) zargs2d <- c(zargs2d, list(labs = labs)) # the argument 'labs' of zenplot()
        if(add_to_zargs("width1d")) zargs2d <- c(zargs2d, list(width1d = width1d)) # the width of the 1d plots
        if(add_to_zargs("width2d")) zargs2d <- c(zargs2d, list(width2d = width2d)) # the width of the 2d plots
        if(add_to_zargs("num")) zargs2d <- c(zargs2d, list(num = NULL)) # current plot number
        if(add_to_zargs("ispace")) zargs2d <- c(zargs2d, list(ispace = ispace)) # the inner space
    }


### Plot #######################################################################

    ## plot1d = NULL or plot2d = NULL (=> plot nothing)
    plot.NULL <- apply(vars, 1, function(vars.) {
        ((vars.[1] == vars.[2]) && is.null(plot1d)) || # plot1d = NULL
            ((vars.[1] != vars.[2]) && is.null(plot2d))})  # plot2d = NULL

    ## Big switch
    positions <- path$positions
    switch(pkg,
    "graphics" = { # graphics ##################################################

        if(draw) {

            ## 3) Determine default spacing around zenplot and around 1d/2d plots
            stopifnot(0 <= ospace, ospace <= 1, 0 <= ispace, ispace <= 1)
            opar <- par(no.readonly = TRUE) # get plotting parameter list
            par(omd = c(0+ospace[2], 1-ospace[4], 0+ospace[1], 1-ospace[3]), # left, right, bottom, top space in [0,1]; more convenient than 'oma'
                plt = c(0+ispace[2], 1-ispace[4], 0+ispace[1], 1-ispace[3])) # left, right, bottom, top space in [0,1]; more convenient than 'mar'
            on.exit(par(opar)) # set back after function call

            ##    Layout
            lay <- matrix(0, nrow = fg.nrow, ncol = fg.ncol, byrow = TRUE)
            for(k in 1:nPlots)
                lay[positions[k,1], positions[k,2]] <- k
            indices <- sort(unique(as.numeric(lay))) # get all assigned numbers >= 0
            indices <- indices[indices > 0] # omit 0s in matrix
            if(length(indices) > nPlots || !all(indices %in% 0:nPlots))
                stop("That's a bug, please report.")
            if(length(indices) < nPlots) {
                mssng <- which(!(1:nPlots %in% indices)) # missing indices
                stop("layout() failed due to missing plot numbers (",paste(mssng, collapse = ", "),") in its first argument.\nThese were most likely overwritten by later 'turns', please check.")
            }
            layout(lay, widths = widths, heights = heights) # layout
            ## => Use, e.g., layout.show(nPlots) to display the layout

            ## 4) Iterate over plots
            for(i in seq_len(nPlots))
            {
                ## Possibly add the plot number
                if(exists("num", where = zargs1d)) zargs1d[["num"]] <- i
                if(exists("num", where = zargs2d)) zargs2d[["num"]] <- i

                ## Plot
                if(plot.NULL[i]) { # no plot
                    plot(NA, type = "n", ann = FALSE, axes = FALSE, xlim = 0:1, ylim = 0:1)
                } else { # plot
                    if(dims[i] == 1) {
                        do.call(plot1d, args = c(list(zargs = zargs1d), .args))
                    } else {
                        do.call(plot2d, args = c(list(zargs = zargs2d), .args))
                    }
                }
            }

        }

        ## Delete 'burst.x' from .zenplots_burst_envir (need to do that
        ## here as extract_*d() might not be called by all 1d/2d plots
        ## (e.g., if plot1d is user-provided and does not call extract_*d() or
        ## if last1d = FALSE etc.)
        if(exists("burst.x", envir = .zenplots_burst_envir))
            rm("burst.x", envir = .zenplots_burst_envir) # remove 'burst.x'

        ## Return (the return value of unfold())
        zen <- list(path = path, layout = layout)
        attr(zen, "class") <- c("zenGraphics", "zenplot", "list")
        invisible(zen)

    },
    "grid" = { # grid ###################################################

        ## 3) Layout
        lay <- grid.layout(nrow = fg.nrow, ncol = fg.ncol,
                           widths = unit(widths, "npc"),
                           heights = unit(heights, "npc"), just = "centre")

        ##    Determine default spacing around zenplot
        stopifnot(0 <= ospace, ospace <= 1, 0 <= ispace, ispace <= 1)
        vp <- viewport(x = unit(ospace[2], "npc"),
                       y = unit(ospace[1], "npc"),
                       just = c("left", "bottom"),
                       width  = unit(1-sum(ospace[c(2,4)]), "npc"),
                       height = unit(1-sum(ospace[c(1,3)]), "npc"))

        ## 4) Iterate over plots (time-consuming part)
        if(draw) grid.newpage()
        fg <- frameGrob(layout = lay, vp = vp) # major result (a frame grob)
        for(i in seq_len(nPlots))
        {
            ## Possibly add the plot number
            if(exists("num", where = zargs1d)) zargs1d[["num"]] <- i
            if(exists("num", where = zargs2d)) zargs2d[["num"]] <- i

            ## Plot
            plotGrob <- if(plot.NULL[i]) { # no plot
                nullGrob()
            } else { # plot
                if(dims[i] == 1) {
                    do.call(plot1d, args = c(list(zargs = zargs1d), .args))
                } else {
                    do.call(plot2d, args = c(list(zargs = zargs2d), .args))
                }
            }

            ## Placing the plot grob in the frame grob
            fg <- placeGrob(fg, grob = plotGrob, row = positions[i,1], col = positions[i,2])
        }

        ## Plot the actual plot object (another time-consuming part)
        if(draw) grid.draw(fg)

        ## Delete 'burst.x' from .zenplots_burst_envir (need to do that
        ## here as extract_*d() might not be called by all 1d/2d plots
        ## (e.g., if plot1d is user-provided and does not call extract_*d() or
        ## if last1d = FALSE etc.)
        if(exists("burst.x", envir = .zenplots_burst_envir))
            rm("burst.x", envir = .zenplots_burst_envir) # remove 'burst.x'

        zen <- list(path = path, layout = layout, grob = fg)
        attr(zen, "class") <- c("zenGrid", "zenplot", "list")
        ## Return (the return value of unfold() and the frame grob)
        invisible(zen)
    },
    "loon" = { # loon ###################################################

        if(!draw) # return the return value of unfold()
           return(invisible(list(path = path, layout = layout)))

        ## 3) Determine default spacing around zenplot
        stopifnot(0 <= ospace, 0 == ospace %% 1, 0 <= ispace, ispace <= 1)
        tt <- tktoplevel()
        parent <- tkframe(tt)
        tkpack(parent, fill="both", expand=TRUE,
               padx=ospace[1:2],
               pady=ospace[3:4])

        ## tktitle(parent) <- "Zenplot layout in Loon"

        ##    Save the plots in a list for later configuration
        loonPlots <- vector(mode="list", length = nPlots)

        ## 4) Iterate over plots
        for(i in seq_len(nPlots))
        {
            ## Possibly add the plot number
            if(exists("num", where = zargs1d)) zargs1d[["num"]] <- i
            if(exists("num", where = zargs2d)) zargs2d[["num"]] <- i

            ## Add additional args
            ## args1d <- if(!hasArg("linkingGroup")) {
            ##  c(zargs1d,
            ##    list(linkingGroup = paste0("Zenplots: loon ID =", parent$ID)),
            ##    .args)
            ##} else c(zargs1d, .args)
            ##args2d <- if(!hasArg("linkingGroup")) {
            ##  c(zargs2d,
            ##    list(linkingGroup = paste0("Zenplots: loon ID =", parent$ID)),
            ##    .args)
            ##} else c(zargs2d, .args)

            ## Plot
            newPlot <- if(plot.NULL[i]) { # no plot
                loon::l_plot(showLabels = FALSE, showScales = FALSE)
            } else { # plot
                if(dims[i] == 1) {
                    do.call(plot1d, args = c(list(zargs = zargs1d, parent=parent), .args))
                } else {
                    do.call(plot2d, args = c(list(zargs = zargs2d, parent=parent), .args))
                }
            }

            ## Placing of plots in the parent tkgrid
            ##
            tkgrid(newPlot,  row = positions[i,1] - 1,  column = positions[i,2] - 1, sticky="nesw" )# tk is zero based
            ## Tuck the plot away for later configuration
            loonPlots[[i]] <- newPlot
        }

        ## Resize the parent so that all plots fit
        loon::l_resize(tt, 600, 600)
        ## Resize all plots
        for(p in loonPlots)
            tkconfigure(paste0(p,'.canvas'), width=1, height=1)
        ## loon::l_configure(p, showLabels=FALSE, showScales=FALSE)

        ## Set the row widths/weights
        rowWeights <- round(heights * layoutHeight)
        for(rowNo in 1:length(rowWeights))
            tkgrid.rowconfigure(parent,
                                rowNo - 1, # zero based indexing
                                weight=rowWeights[rowNo])

        ## Set the column widths/weights
        colWeights <- round(widths * layoutWidth)
        for(colNo in 1:length(colWeights))
            tkgrid.columnconfigure(parent,
                                   colNo - 1, # zero based indexing
                                   weight=colWeights[colNo])

        ## Delete 'burst.x' from .zenplots_burst_envir (need to do that
        ## here as extract_*d() might not be called by all 1d/2d plots
        ## (e.g., if plot1d is user-provided and does not call extract_*d() or
        ## if last1d = FALSE etc.)
        if(exists("burst.x", envir = .zenplots_burst_envir))
            rm("burst.x", envir = .zenplots_burst_envir) # remove 'burst.x'

        ## Return (the return value of unfold() and more)
        zen <- list(path = path, layout = layout, loon = parent, toplevel = tt)
        attr(zen, "class") <- c("zenLoon", "zenplot", "list")
        ## Return (the return value of unfold() and the frame grob)
        invisible(zen)

    },
    stop("Wrong 'pkg'"))
}
