## Default 2d plot functions based on graphics
##' @title Plot of labels indicating adjacent groups using R's base graphics
##' @family default 2d plot functions using R's base graphics
##' @family default 2d plot functions
##' @name group_2d_graphics
##' @aliases group_2d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param glabs group labels being indexed by the plot variables
##'        (and thus of length as the number of variables);
##'        if NULL then they are determined with extract_2d()
##' @param sep group label separator
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param add logical indicating whether this plot should be added to the last one
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to text()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @note For performance reasons (avoiding having to call extract_2d() twice),
##'       'glabs' is an extra argument
##' @export
group_2d_graphics <- function(zargs,
                              glabs = NULL, sep = "\n", loc = c(0.5, 0.5),
                              add = FALSE, plot... = NULL, ...)
{
    check_zargs(zargs, "turns", "vars", "num")
    turns <- zargs$turns
    vars <- zargs$vars
    num <- zargs$num
    ii <- range(vars[num,]) # variable index
    ii <- if(turns[num-1] == "u" || turns[num] == "u") rev(ii) else ii
    if(is.null(glabs)) {
        glabs <- extract_2d(zargs)$glabs
    } else {
        len.groups <- length(unlist(zargs$x, recursive = FALSE))
        if(length(glabs) != len.groups)
            stop("length(glabs) has to equal the number ",len.groups," of variables in all groups together; consider rep()")
    }
    labs <- paste0(glabs[ii], collapse = sep) # labels (in the correct order for displaying the group change)
    ## Plotting
    opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot; the same if not adding to a plot)
    on.exit(par(opar))
    if(!add) plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
    text(x = loc[1], y = loc[2], labels = labs, ...)
}

##' @title Point plot in 2d using R's base graphics
##' @family default 2d plot functions using R's base graphics
##' @family default 2d plot functions
##' @name points_2d_graphics
##' @aliases points_2d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param cex character expansion factor
##' @param box logical indicating whether a box should be drawn
##' @param add logical indicating whether this plot should be added to the last one
##' @param group... list of arguments passed to group_2d_graphics (or NULL)
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to points()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
points_2d_graphics <- function(zargs,
                               cex = 0.4, box = FALSE,
                               add = FALSE, group... = NULL, plot... = NULL, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- as.matrix(r$x) # for points()
    y <- as.matrix(r$y)
    same.group <- r$same.group
    if(same.group) {
        if(!add) plot_region(xlim = xlim, ylim = ylim, plot... = plot...) # plot region; uses xlim, ylim
        points(x = x, y = y, cex = cex, ...)
        if(box) box(...) # plot the box
    } else {
        args <- c(list(zargs = zargs, add = add), group...)
        do.call(group_2d_graphics, args)
    }
}

##' @title Quantile-quantile plot in 2d using R's base graphics
##' @family default 2d plot functions using R's base graphics
##' @family default 2d plot functions
##' @name qq_2d_graphics
##' @aliases qq_2d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param do.line logical indicating whether a line is drawn (through both
##'        empirical c(0.25, 0.75)-quantiles)
##' @param lines... additional arguments passed to lines()
##' @param cex character expansion factor
##' @param box logical indicating whether a box should be drawn
##' @param add logical indicating whether this plot should be added to the last one
##' @param group... list of arguments passed to group_2d_graphics (or NULL)
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to qqplot()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @note line iff both margins are of the same *type*
##' @export
qq_2d_graphics <- function(zargs,
                           do.line = TRUE, lines... = NULL, cex = 0.4, box = FALSE,
                           add = FALSE, group... = NULL, plot... = NULL, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- as.matrix(r$x) # for points()
    y <- as.matrix(r$y)
    same.group <- r$same.group
    if(same.group) {
        if(!add) plot_region(xlim = xlim, ylim = ylim, plot... = plot...) # plot region; uses xlim, ylim
        ## Calculation (see qqplot())
        sx <- sort(x)
        sy <- sort(y)
        lenx <- length(sx)
        leny <- length(sy)
        if (leny < lenx)
            sx <- approx(1L:lenx, sx, n = leny)$y
        if (leny > lenx)
            sy <- approx(1L:leny, sy, n = lenx)$y
        ## Plot
        points(x = sx, y = sy, cex = cex, ...) # Q-Q plot
        if(do.line) {
            qx <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
            qy <- quantile(y, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
            slope <- diff(qy) / diff(qx)
            intercept <- qy[1] - qx[1] * slope
            do.call(abline, c(list(a = intercept, b = slope), lines...))
        }
        if(box) box(...) # plot the box
    } else {
        args <- c(list(zargs = zargs, add = add), group...)
        do.call(group_2d_graphics, args)
    }
}

##' @title Density plot in 2d using R's base graphics
##' @family default 2d plot functions using R's base graphics
##' @family default 2d plot functions
##' @name density_2d_graphics
##' @aliases density_2d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param ngrids number of grid points in each dimension.
##'        Can be scalar or a length-2 integer vector.
##' @param drawlabels logical indicating whether the contours should be labelled
##' @param axes logicial indicating whether axes should be drawn
##' @param box logical indicating whether a box should be drawn
##' @param add logical indicating whether this plot should be added to the last one
##' @param group... list of arguments passed to group_2d_graphics (or NULL)
##' @param ... additional arguments passed to points()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
density_2d_graphics <- function(zargs,
                                ngrids = 25, drawlabels = FALSE,
                                axes = FALSE, box = FALSE,
                                add = FALSE, group... = NULL, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- r$x
    y <- r$y
    same.group <- r$same.group
    if(same.group) {
        data <- na.omit(cbind(x, y))
        dens <- kde2d(data[,1], data[,2], n = ngrids, lims = c(xlim, ylim))
        contour(dens$x, dens$y, dens$z, drawlabels = drawlabels,
                axes = axes, add = add, ...)
        if(box) box(...) # plot the box
    } else {
        args <- c(list(zargs = zargs, add = add), group...)
        do.call(group_2d_graphics, args)
    }
}

##' @title Axes arrows in 2d using R's base graphics
##' @family default 2d plot functions using R's base graphics
##' @family default 2d plot functions
##' @name axes_2d_graphics
##' @aliases axes_2d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param length length of the arrow head
##' @param eps distance by which the axes are moved away from the plot region
##' @param code integer code determining the kind of arrows to be drawn; see ?arrows
##' @param xpd logical or NA, determining the region with respect to which clipping
##'        takes place; see ?par
##' @param add logical indicating whether this plot should be added to the last one
##' @param group... list of arguments passed to group_2d_graphics (or NULL)
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to points()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @note Inspired by https://stat.ethz.ch/pipermail/r-help/2004-October/059525.html
##' @export
axes_2d_graphics <- function(zargs,
                             length = 0.1, eps = 0.04, code = 2, xpd = NA,
                             add = FALSE, group... = NULL, plot... = NULL, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    same.group <- r$same.group
    if(same.group) {
        if(!add) plot_region(xlim = xlim, ylim = ylim, plot... = plot...)
        epsx <- eps * diff(xlim)
        epsy <- eps * diff(ylim)
        arrows(xlim[1]-epsx, ylim[1]-epsy, xlim[2]+epsx, ylim[1]-epsy,
               length = length, code = code, xpd = xpd, ...) # x axis
        arrows(xlim[1]-epsx, ylim[1]-epsy, xlim[1]-epsx, ylim[2]+epsy,
               length = length, code = code, xpd = xpd, ...) # y axis
    } else {
        args <- c(list(zargs = zargs, add = add), group...)
        do.call(group_2d_graphics, args)
    }
}

##' @title Arrow plot in 2d using R's base graphics
##' @family default 2d plot functions using R's base graphics
##' @family default 2d plot functions
##' @name arrow_2d_graphics
##' @aliases arrow_2d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location (in (0,1)^2) of the center of the arrow
##' @param angle angle from the shaft to the edge of the arrow head
##' @param length length of the arrow in [0,1] from tip to base
##' @param add logical indicating whether this plot should be added to the last one
##' @param group... list of arguments passed to group_2d_graphics (or NULL)
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to points()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
arrow_2d_graphics <- function(zargs,
                              loc = c(0.5, 0.5), angle = 60, length = 0.2,
                              add = FALSE, group... = NULL, plot... = NULL, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    check_zargs(zargs, "num", "turns")
    turn.out <- zargs$turns[zargs$num]
    if(same.group) {
        arrow <- zenarrow(turn.out, angle = angle, length = length,
                          coord.scale = 1) # scaling according to aspect ratio
        arr <- loc + arrow
        ## Plotting
        opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot)
        on.exit(par(opar))
        if(!add) plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
        segments(x0 = rep(arr[1,2], 2),      y0 = rep(arr[2,2], 2),
                 x1 = c(arr[1,1], arr[1,3]), y1 = c(arr[2,1], arr[2,3]), ...)
    } else {
        args <- c(list(zargs = zargs, add = add), group...)
        do.call(group_2d_graphics, args)
    }
}

##' @title Rectangle plot in 2d using R's base graphics
##' @family default 2d plot functions using R's base graphics
##' @family default 2d plot functions
##' @name rect_2d_graphics
##' @aliases rect_2d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location (in (0,1)^2) of the center of the rectangle
##' @param width width of the rectangle as a fraction of 1
##' @param height height of the rectangle as a fraction of 1
##' @param add logical indicating whether this plot should be added to the last one
##' @param group... list of arguments passed to group_2d_graphics (or NULL)
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to rect()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
rect_2d_graphics <- function(zargs,
                             loc = c(0.5, 0.5), width = 1, height = 1,
                             add = FALSE, group... = NULL, plot... = NULL, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    if(same.group) {
        x <- c(loc[1] - width/2, loc[1] + width/2)
        y <- c(loc[2] - height/2, loc[2] + height/2)
        ## Plotting
        opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot)
        on.exit(par(opar))
        if(!add) plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
        rect(xleft = x[1], ybottom = y[1], xright = x[2], ytop = y[2], ...)
    } else {
        args <- c(list(zargs = zargs, add = add), group...)
        do.call(group_2d_graphics, args)
    }
}

##' @title Label plot in 2d using R's base graphics
##' @family default 2d plot functions using R's base graphics
##' @family default 2d plot functions
##' @name label_2d_graphics
##' @aliases label_2d_graphics
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location (in (0,1)^2) of the center of the rectangle
##' @param label label to be used
##' @param adj x (and optionally y) adjustment of the label
##' @param box logical indicating whether a box should be drawn
##' @param add logical indicating whether this plot should be added to the last one
##' @param group... list of arguments passed to group_2d_graphics (or NULL)
##' @param plot... additional arguments passed to plot_region()
##' @param ... additional arguments passed to rect()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
label_2d_graphics <- function(zargs,
                              loc = c(0.98, 0.05), label = NULL, adj = 1:0, box = FALSE,
                              add = FALSE, group... = NULL, plot... = NULL, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    vlabs <- r$vlabs
    check_zargs(zargs, "vars", "num")
    vars <- zargs$vars
    num <- zargs$num
    if(same.group) {
        xlab <- vlabs[vars[num, 1]]
        ylab <- vlabs[vars[num, 2]]
        if(is.null(label)) label <- paste0("(",xlab,", ",ylab,")")
        ## Plotting
        opar <- par(usr = c(0, 1, 0, 1)) # switch to relative coordinates (easier when adding to a plot)
        on.exit(par(opar))
        if(!add) plot_region(xlim = 0:1, ylim = 0:1, plot... = plot...) # plot region; uses xlim, ylim
        text(x = loc[1], y = loc[2], labels = label, adj = adj, ...)
        if(box) box(...) # plot the box
    } else {
        args <- c(list(zargs = zargs, add = add), group...)
        do.call(group_2d_graphics, args)
    }
}

##' @title Layout plot in 2d
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param ... additional arguments passed to label_2d_graphics()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @note Here we also pass '...' to group_2d_grid() (to easily adjust
##'       font size etc.)
layout_2d_graphics <- function(zargs, ...)
    label_2d_graphics(zargs, loc = c(0.5, 0.5), adj = rep(0.5, 2), # centered
                      box = TRUE, group... = list(...), ...)
