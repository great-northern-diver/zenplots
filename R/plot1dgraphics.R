## Default 1d plot functions based on graphics
##' @title Rug plot in 1d using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name rug_1d_graphics
##' @aliases rug_1d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc location in [0,1]; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param length length of the rugs
##' @param width line width of the rugs
##' @param col color of the rugs
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to segments()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
rug_1d_graphics <- function(zargs,
                            loc = 0.5, length = 0.5, width = 1, col = par("fg"),
                            add = FALSE, plot... = NULL, ...)
{
    r <- extract_1d(zargs)
    x <- as.matrix(r$x) # for segments()
    horizontal <- r$horizontal
    lim <- r$xlim
    check_zargs(zargs, "num", "turns")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    if(turn.out == "d" || turn.out == "l") loc <- 1-loc # when walking downwards, change both left/right and up/down
    if(add) usr <- par("usr")
    if(horizontal) {
        xlim <- lim
        ylim <- 0:1
        x0 <- x
        y0 <- loc - length/2
        x1 <- x
        y1 <- loc + length/2
        if(add) opar <- par(usr = c(usr[1:2], 0, 1)) # force y-coordinates to be [0,1]
    } else {
        xlim <- 0:1
        ylim <- lim
        x0 <- loc - length/2
        y0 <- x
        x1 <- loc + length/2
        y1 <- x
        if(add) opar <- par(usr = c(0, 1, usr[3:4])) # force x-coordinates to be [0,1]
    }
    ## Plotting
    if(add) {
        on.exit(par(opar))
    } else {
        plot_region(xlim = xlim, ylim = ylim, plot... = plot...) # plot region; uses xlim, ylim
    }
    if(length(x) > 0)
        segments(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                 col = col, lwd = width, ...)
}

##' @title Dot plot in 1d using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name points_1d_graphics
##' @aliases points_1d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc location in [0,1]; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param cex character expansion factor
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to points()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
points_1d_graphics <- function(zargs,
                               loc = 0.5, cex = 0.4,
                               add = FALSE, plot... = NULL, ...)
{
    r <- extract_1d(zargs)
    x <- as.matrix(r$x) # for points()
    horizontal <- r$horizontal
    lim <- r$xlim
    check_zargs(zargs, "num", "turns")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    if(turn.out == "d" || turn.out == "l") loc <- 1-loc # when walking downwards, change both left/right and up/down
    if(length(loc) == 1) loc <- rep(loc, length(x))
    if(add) usr <- par("usr")
    if(horizontal) {
        xlim <- lim
        ylim <- 0:1
        x <- x
        y <- loc
        if(add) opar <- par(usr = c(usr[1:2], 0, 1)) # force y-coordinates to be [0,1]
    } else {
        ylim <- lim
        xlim <- 0:1
        y <- x
        x <- loc
        if(add) opar <- par(usr = c(0, 1, usr[3:4])) # force x-coordinates to be [0,1]
    }
    ## Plotting
    if(add) {
        on.exit(par(opar))
    } else {
        plot_region(xlim = xlim, ylim = ylim, plot... = plot...) # plot region; uses xlim, ylim
    }
    points(x = x, y = y, cex = cex, ...)
}

##' @title Jittered dot plot in 1d using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name jitter_1d_graphics
##' @aliases jitter_1d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc location in [0,1]; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param offset number in [0,0.5] determining how far off the center
##'        the jittered points reach maximally
##' @param cex character expansion factor
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to points()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
jitter_1d_graphics <- function(zargs,
                               loc = 0.5, offset = 0.25, cex = 0.4,
                               add = FALSE, plot... = NULL, ...)
{
    r <- extract_1d(zargs)
    x <- r$x
    stopifnot(0 <= offset, offset <= 0.5, 0 <= loc, loc <= 1, offset <= min(loc, 1-loc))
    loc. <- loc + runif(length(x), min = -offset, max = offset)
    points_1d_graphics(zargs, loc = loc., cex = cex, add = add, plot... = plot..., ...)
}

##' @title Histogram as 1d plot using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name hist_1d_graphics
##' @aliases hist_1d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param breaks see ?hist; the default is 20 equi-width bins covering the data range
##' @param length.out number of break points if breaks = NULL
##' @param col vector of colors for the bars or bar components; see ?barplot
##' @param axes logicial indicating whether axes should be drawn
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to barplot()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
hist_1d_graphics <- function(zargs,
                             breaks = NULL, length.out = 21, col = NULL,
                             axes = FALSE, add = TRUE,
                             plot... = NULL, ...)
{
    r <- extract_1d(zargs)
    x <- as.matrix(r$x)
    horizontal <- r$horizontal
    check_zargs(zargs, "num", "turns")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    lim <- r$xlim
    if(all(is.na(x))) {
        ## Empty plot
        opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot)
        on.exit(par(opar))
        plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
    } else {
        if(is.null(breaks))
            breaks <- seq(from = lim[1], to = lim[2], length.out = length.out)
        binInfo <- hist(x, breaks = breaks, plot = FALSE)
        binBoundaries <- binInfo$breaks
        widths <- diff(binBoundaries)
        heights <- binInfo$density
        if(turn.out == "d" || turn.out == "l") heights <- -heights
        if(horizontal) {
            xlim <- lim - lim[1] # c(0, sum(widths))
            ylim <- range(0, heights)
        } else {
            xlim <- range(0, heights)
            ylim <- lim - lim[1] # c(0, sum(widths))
        }
        ## Plotting
        plot_region(xlim = xlim, ylim = ylim, plot... = plot...) # plot region; uses xlim, ylim
        barplot(heights, width = widths, space = 0, horiz = !horizontal,
                main = "", xlab = "", col = col, border = col, add = add, axes = axes, ...)
    }
}

##' @title Density plot in 1d using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name density_1d_graphics
##' @aliases density_1d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param density... list of arguments for density()
##' @param offset number in [0, 0.5] determining how far away the density stays
##'        from the plot margins (for creating space between the two)
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to polygon()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
density_1d_graphics <- function(zargs,
                                density... = NULL, offset = 0.08,
                                add = FALSE, plot... = NULL, ...)
{
    r <- extract_1d(zargs)
    x <- r$x[!is.na(r$x)] # omit missing data
    horizontal <- r$horizontal
    check_zargs(zargs, "num", "turns")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    if(length(x) == 0) {
        if(!add) {
            ## Empty plot
            opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot)
            on.exit(par(opar))
            plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
        }
    } else {
        ## Determine density values
        stopifnot(0 <= offset, offset <= 0.5)
        dens <- do.call(density, args = c(list(x), density...))
        xvals <- dens$x
        keepers <- (min(x) <= xvals) & (xvals <= max(x)) # keep those within the range of the data
        x. <- xvals[keepers]
        y. <- dens$y[keepers]
        if(turn.out == "d" || turn.out == "l") y. <- -y.
        if(horizontal) {
            xlim <- range(x.)
            ylim <- range(0, y.)
            x <- c(xlim[1], x., xlim[2])
            y <- c(0, y., 0)
            ## Scaling (f(y) = a * y + b with f(0) = b = offset * ylim[2] and
            ## f(ylim[2]) = a * ylim[2] + b = (1-offset) * ylim[2])
            y <- (1-2*offset) * y + offset * if(turn.out == "d") ylim[1] else ylim[2] # scale to [offset, 1-offset] * ylim[2]
        } else {
            xlim <- range(0, y.)
            ylim <- range(x.)
            x <-  c(0, y., 0)
            y <- c(ylim[1], x., ylim[2])
            ## Scaling
            x <- (1-2*offset) * x + offset * if(turn.out == "l") xlim[1] else xlim[2] # scale to [offset, 1-offset] * xlim[2]
        }
        ## Plotting
        if(add) {
            usr <- par("usr")
            opar <- par(usr = c(xlim, ylim)) # switch to relative coordinates (easier when adding to a plot)
            on.exit(par(opar))
        } else {
            plot_region(xlim = xlim, ylim = ylim, plot... = plot...) # plot region; uses xlim, ylim
        }
        polygon(x = x, y = y, ...)
    }
}

##' @title Box plot in 1d using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name boxplot_1d_graphics
##' @aliases boxplot_1d_graphics
##' @param zargs The argument list as passed from \code{\link{zenplot}()}
##' @param cex The character expansion factor
##' @param range A numerical value which determines how far the plot whiskers extend.
##'        If NULL, the whiskers (range) grows with sample size.
##' @param axes A logicial indicating whether axes should be drawn
##' @param add A logical indicating whether this plot should be added to the last one
##' @param ... Additional arguments passed to boxplot()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
boxplot_1d_graphics <- function(zargs,
                                cex = 0.4, range = NULL, axes = FALSE,
                                add = FALSE, ...)
{
    r <- extract_1d(zargs)
    x <- r$x
    horizontal <- r$horizontal
    if(is.null(range)) { # choose 'range' depending on sample size
        n <- length(x)
        q25 <- qnorm(0.25)
        iqr <- qnorm(0.75) - q25
        range <- (q25 - qnorm(0.35/(2*n)))/iqr
    }
    boxplot(x, horizontal = horizontal, range = range, cex = cex,
            add = add, axes = axes, ...)
}

##' @title Arrow plot in 1d using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name arrow_1d_graphics
##' @aliases arrow_1d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param angle angle in [0, 180]
##' @param length length of the arrow in [0,1] from tip to base
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to segments()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
arrow_1d_graphics <- function(zargs,
                              loc = c(0.5, 0.5), angle = 60, length = 0.6,
                              add = FALSE, plot... = NULL, ...)
{
    check_zargs(zargs, "num", "turns", "width1d", "width2d")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    horizontal <- turn.out %in% c("d", "u") # ... quicker than via extract_1d()
    if(turn.out == "d") loc <- 1-loc # when walking downwards, change both left/right and up/down
    if(turn.out == "r") { # when walking to the right, coordinates change and 2nd is flipped
        loc <- rev(loc)
        loc[2] <- 1-loc[2]
    }
    if(turn.out == "l") { # when walking to the left, coordinates change and 1st is flipped
        loc <- rev(loc)
        loc[1] <- 1-loc[1]
    }
    width1d <- zargs$width1d
    width2d <- zargs$width2d
    arrow <- zenarrow(turn.out, angle = angle, length = length,
                      coord.scale = width1d / width2d) # scaling according to aspect ratio
    ## Note: To see why coord.scale is like this, notice that we need
    ##       width1d/width2d for data in [0,1]^2, for example (=> unit for 1d
    ##       plots is by the factor width2d/width1d smaller)
    arr <- loc + arrow
    ## Plotting
    opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot)
    on.exit(par(opar))
    if(!add) plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
    segments(x0 = rep(arr[1,2], 2),      y0 = rep(arr[2,2], 2),
             x1 = c(arr[1,1], arr[1,3]), y1 = c(arr[2,1], arr[2,3]), ...)
}

##' @title Rectangle plot in 1d using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name rect_1d_graphics
##' @aliases rect_1d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param width width of the rectangle (when viewed in walking direction)
##' @param height height of the rectangle (when viewed in walking direction)
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to lines()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
rect_1d_graphics <- function(zargs,
                             loc = c(0.5, 0.5), width = 1, height = 1,
                             add = FALSE, plot... = NULL, ...)
{
    check_zargs(zargs, "num", "turns")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    horizontal <- turn.out %in% c("d", "u") # ... quicker than via extract_1d()
    if(turn.out == "d") loc <- 1-loc # when walking downwards, change both left/right and up/down
    if(turn.out == "r") { # when walking to the right, coordinates change and 2nd is flipped
        loc <- rev(loc)
        loc[2] <- 1-loc[2]
    }
    if(turn.out == "l") { # when walking to the left, coordinates change and 1st is flipped
        loc <- rev(loc)
        loc[1] <- 1-loc[1]
    }
    if(!horizontal) {
        tmp <- width
        width <- height
        height <- tmp
    }
    x <- c(loc[1] - width/2, loc[1] + width/2)
    y <- c(loc[2] - height/2, loc[2] + height/2)
    ## Plotting
    opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot)
    on.exit(par(opar))
    if(!add) plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
    rect(xleft = x[1], ybottom = y[1], xright = x[2], ytop = y[2], ...)
}

##' @title Line plot in 1d using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name lines_1d_graphics
##' @aliases lines_1d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param length length of the line (in [0,1])
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to lines()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
lines_1d_graphics <- function(zargs,
                              loc = c(0.5, 0.5), length = 1,
                              add = FALSE, plot... = NULL, ...)
{
    check_zargs(zargs, "num", "turns")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    horizontal <- turn.out %in% c("d", "u") # ... quicker than via extract_1d()
    if(turn.out == "d") loc <- 1-loc # when walking downwards, change both left/right and up/down
    if(turn.out == "r") { # when walking to the right, coordinates change and 2nd is flipped
        loc <- rev(loc)
        loc[2] <- 1-loc[2]
    }
    if(turn.out == "l") { # when walking to the left, coordinates change and 1st is flipped
        loc <- rev(loc)
        loc[1] <- 1-loc[1]
    }
    if(horizontal) {
        x <- c(loc[1] - length/2, loc[1] + length/2)
        y <- rep(loc[2], 2)
    } else {
        x <- rep(loc[1], 2)
        y <- c(loc[2] - length/2, loc[2] + length/2)
    }
    ## Plotting
    opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot)
    on.exit(par(opar))
    if(!add) plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
    lines(x, y = y, ...) # uses x, y
}

##' @title Label plot in 1d using R's base graphics
##' @family default 1d plot functions using R's base graphics
##' @family default 1d plot functions
##' @name label_1d_graphics
##' @aliases label_1d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param label label to be used
##' @param box logical indicating whether a box is to be drawn.
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to text() and box()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
label_1d_graphics <- function(zargs,
                              loc = c(0.5, 0.5), label = NULL, box = FALSE,
                              add = FALSE, plot... = NULL, ...)
{
    r <- extract_1d(zargs)
    horizontal <- r$horizontal
    if(is.null(label)) label <- names(r$x) # combined group and variable label
    check_zargs(zargs, "num", "turns")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    if(turn.out == "d") loc <- 1-loc # when walking downwards, change both left/right and up/down
    if(turn.out == "r") { # when walking to the right, coordinates change and 2nd is flipped
        loc <- rev(loc)
        loc[2] <- 1-loc[2]
    }
    if(turn.out == "l") { # when walking to the left, coordinates change and 1st is flipped
        loc <- rev(loc)
        loc[1] <- 1-loc[1]
    }
    srt <- if(horizontal) {
        0 # note: we don't turn label upside down
    } else {
        if(turn.out == "r") -90 else 90
    }
    ## Plotting
    ## Note: par("usr") gives x1, x2, y1, y2; this *includes* some marginal space
    opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot; the same if not adding to a plot)
    on.exit(par(opar))
    if(!add) plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
    text(x = loc[1], y = loc[2], labels = label, srt = srt, ...)
    if(box) box(...) # plot the box
}

##' @title Layout plot in 1d
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param ... additional arguments passed to label_1d_graphics()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
layout_1d_graphics <- function(zargs, ...)
    label_1d_graphics(zargs, box = TRUE, ...)
