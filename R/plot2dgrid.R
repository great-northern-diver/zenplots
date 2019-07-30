## Default 2d plot functions based on grid
library(grid)
library(MASS) # for kde2d

##' @title Plot of labels indicating adjacent groups using the grid package
##' @family default 2d plot functions using the grid package
##' @family default 2d plot functions
##' @name group_2d_grid
##' @aliases group_2d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param glabs group labels being indexed by the plot variables
##'        (and thus of length as the number of variables);
##'        if NULL then they are determined with extract_2d()
##' @param sep group label separator
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert
##' @note For performance reasons (avoiding having to call extract_2d() twice),
##'       'glabs' is an extra argument
##' @export
group_2d_grid <- function(zargs,
                          glabs = NULL, sep = "\n", loc = c(0.5, 0.5),
                          draw = FALSE, ...)
{
    check_zargs(zargs, "turns", "vars", "num", "ispace")
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
    vp <- vport(zargs$ispace)
    res <- textGrob(label = labs, x = loc[1], y = loc[2],
                    default.units = "npc",
                    name = "group_2d", gp = gpar(...), vp = vp)
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Point plot in 2d using the grid package
##' @family default 2d plot functions using the grid package
##' @family default 2d plot functions
##' @name points_2d_grid
##' @aliases points_2d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param type line type
##' @param pch plot symbol
##' @param size size of the plot symbol
##' @param box logical indicating whether a box should be drawn
##' @param box.width width of the box
##' @param box.height height of the box
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note - We use names depending on the 'type' here since otherwise, if one calls it
##'         once for 'p' and once for 'l', only one of them is plotted
##'       - The default point size was chosen to match the default of graphics
##' @export
points_2d_grid <- function(zargs,
                           type = c("p", "l", "o"), pch = NULL, size = 0.02,
                           box = FALSE, box.width = 1, box.height = 1,
                           group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- as.matrix(r$x) # for pointsGrob()
    y <- as.matrix(r$y)
    same.group <- r$same.group
    check_zargs(zargs, "ispace")
    res <- if(same.group) {
        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim)
        if(box)
            gBox <- rectGrob(x = 0.5, y = 0.5,
                             width = box.width, height = box.height, # => plotting outside of viewport (space has been reserved by default ispace)
                             just = "centre", default.units = "npc",
                             name = "box_2d", gp = gpar(...), vp = vp)
        type <- match.arg(type)
        switch(type,
        "p" = {
            if(is.null(pch)) pch <- 21
            gPoints <- pointsGrob(x = x, y = y, pch = pch,
                                  size = unit(size, units = "npc"),
                                  default.units = "native",
                                  name = "points_2d", gp = gpar(...), vp = vp)
            if(box) { # create a single grob
                gTree(children = gList(gBox, gPoints)) # note: first box
            } else {
                gTree(children = gList(gPoints))
            }
        },
        "l" = {
            gLines <- linesGrob(x = x, y = y,
                                default.units = "native",
                                name = "lines_2d", gp = gpar(...), vp = vp)
            if(box) { # create a single grob
                gTree(children = gList(gBox, gLines)) # note: first box
            } else {
                gTree(children = gList(gLines))
            }
        },
        "o" = {
            if(is.null(pch)) pch <- 20
            gLines <- linesGrob(x = x, y = y,
                                default.units = "native",
                                name = "lines_2d", gp = gpar(...), vp = vp)
            gPoints <- pointsGrob(x = x, y = y, pch = pch,
                                  size = unit(size, units = "npc"),
                                  default.units = "native",
                                  name = "points_2d", gp = gpar(...), vp = vp)
            if(box) { # create a single grob
                gTree(children = gList(gBox, gLines, gPoints)) # note: first box
            } else {
                gTree(children = gList(gLines, gPoints))
            }
        },
        stop("Wrong 'type'"))
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Quantile-quantile plot in 2d using the grid package
##' @family default 2d plot functions using the grid package
##' @family default 2d plot functions
##' @name qq_2d_grid
##' @aliases qq_2d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param do.line logical indicating whether a line is drawn (through both
##'        empirical c(0.25, 0.75)-quantiles)
##' @param lines... additional arguments passed to lines()
##' @param pch plot symbol
##' @param size size of the plot symbol
##' @param box logical indicating whether a box should be drawn
##' @param box.width width of the box
##' @param box.height height of the box
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note - line iff both margins are of the same *type*
##'       - The default point size was chosen to match the default of graphics
##' @export
qq_2d_grid <- function(zargs,
                       do.line = TRUE, lines... = NULL, pch = NULL, size = 0.02,
                       box = FALSE, box.width = 1, box.height = 1,
                       group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- r$x
    y <- r$y
    same.group <- r$same.group
    check_zargs(zargs, "ispace")
    res <- if(same.group) {
        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim)
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
        if(is.null(pch)) pch <- 21
        gPoints <- pointsGrob(x = sx, y = sy, pch = pch,
                              size = unit(size, units = "npc"),
                              default.units = "native",
                              name = "points_2d", gp = gpar(...), vp = vp) # Q-Q plot
        groblist <- list(gPoints)
        if(do.line) {
            qx <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
            qy <- quantile(y, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
            slope <- diff(qy) / diff(qx)
            intercept <- qy[1] - qx[1] * slope
            ## We can't just plot that as there is no abline() in grid. Evaluating
            ## the line at xlim and corresponding y-values slope * xvals + intercept
            ## could lie very well outside the plot region.
            ## We solve this here *brute force*
            xvals <- seq(xlim[1], xlim[2], length.out = 1024)
            yvals <- slope * xvals + intercept
            ok <- (xlim[1] <= xvals) & (xvals <= xlim[2]) &
                  (ylim[1] <= yvals) & (yvals <= ylim[2])
            vals <- cbind(xvals, yvals)[ok, ]
            x0.x1 <- c(vals[1,1], vals[nrow(vals),1])
            y0.y1 <- c(vals[1,2], vals[nrow(vals),2])
            gLines <- linesGrob(x = x0.x1, y = y0.y1,
                                default.units = "native", name = "lines_2d",
                                gp = gpar(...), vp = vp) # Q-Q line
            groblist <- c(list(gLines), groblist) # append to list
        }
        if(box) {
            gBox <- rectGrob(x = 0.5, y = 0.5,
                             width = box.width, height = box.height, # => plotting outside of viewport (space has been reserved by default ispace)
                             just = "centre", default.units = "npc",
                             name = "box_2d", gp = gpar(...), vp = vp)
            groblist <- c(list(gBox), groblist) # append to list
        }
        gTree(children = do.call(gList, groblist))
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Density plot in 2d using the grid package
##' @family default 2d plot functions using the grid package
##' @family default 2d plot functions
##' @name density_2d_grid
##' @aliases density_2d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param ngrids number of grid points in each direction. Can be scalar or
##'        a length-2 integer vector.
##' @param ccol vector (which is then recycled to the appropriate length)
##'        giving the color of the contours
##' @param clwd vector (which is then recycled to the appropriate length)
##'        giving the line widths of the contours
##' @param clty vector (which is then recycled to the appropriate length)
##'        giving the line types of the contours
##' @param box logical indicating whether a box should be drawn
##' @param box.width width of the box
##' @param box.height height of the box
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note - We use names depending on the 'type' here since otherwise, if one calls it
##'         once for 'p' and once for 'l', only one of them is plotted
##'       - The default point size was chosen to match the default of graphics
##' @author Marius Hofert and Wayne Oldford
##' @export
density_2d_grid <- function(zargs,
                            ngrids = 25, ccol = NULL, clwd = 1, clty = 1,
                            box = FALSE, box.width = 1, box.height = 1,
                            group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- r$x
    y <- r$y
    same.group <- r$same.group
    check_zargs(zargs, "ispace")
    res <- if(same.group) {
        data <- na.omit(data.frame(x, y))
        colnames(data) <- c("x", "y")
        dens <- kde2d(data$x, data$y, n = ngrids, lims = c(xlim, ylim))
        contours <- contourLines(dens$x, dens$y, dens$z)
        levels <- sapply(contours, function(contour) contour$level) # list of contour levels
        nLevels <- length(levels) # number of levels
        uniqueLevels <- unique(levels) # unique levels (there could be more than one level curve with the same level)
        nuLevels <- length(uniqueLevels)
        if(is.null(ccol)) { # default grey scale colors
            basecol <- c("grey80", "grey0")
            palette <- colorRampPalette(basecol, space = "Lab")
            ccol <- palette(nuLevels) # different color for each 1d plot
        }
        ccol <- rep_len(ccol, nuLevels)
        clwd <- rep_len(clwd, nuLevels)
        clty <- rep_len(clty, nuLevels)
        ## Match the levels in the unique levels
        ccol. <- numeric(nLevels)
        clwd. <- numeric(nLevels)
        clty. <- numeric(nLevels)
        for (i in 1:nuLevels) {
            idx <- (1:nLevels)[levels == uniqueLevels[i]]
            ccol.[idx] <- ccol[i]
            clwd.[idx] <- clwd[i]
            clty.[idx] <- clty[i]
        }
        ## Plotting
        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim, x = x, y = y)
        if(box)
            gBox <- rectGrob(x = 0.5, y = 0.5,
                             width = box.width, height = box.height, # => plotting outside of viewport (space has been reserved by default ispace)
                             just = "centre", default.units = "npc",
                             name = "box_2d", gp = gpar(...), vp = vp)
        contourGrobs <- lapply(1:length(contours), # go over all contours
                               function(i) {
            contour <- contours[[i]]
            linesGrob(x = contour$x, y = contour$y,
                      gp = gpar(col = ccol.[i],
                                lwd = clwd.[i], lty = clty.[i], ...),
                      default.units = "native",
                      name = paste0("contour_",i), # note: have to be different!
                      vp = vp)
        })
        if(box) { # create a single grob
            gTree(children = do.call(gList, args = c(contourGrobs, list(gBox))))
        } else {
            gTree(children = do.call(gList, args = contourGrobs))
        }
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Axes arrow using the grid package
##' @family default 2d plot functions using the grid package
##' @family default 2d plot functions
##' @name axes_2d_grid
##' @aliases axes_2d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param angle angle of the arrow head (see ?arrow)
##' @param length length of the arrow in [0,1] from tip to base
##' @param type type of the arrow head (see ?arrow)
##' @param eps distance by which the axes are moved away from the plot region
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note Inspired by https://stat.ethz.ch/pipermail/r-help/2004-October/059525.html
##' @export
axes_2d_grid <- function(zargs,
                         angle = 30, length = unit(0.05, "npc"), type = "open", eps = 0.02,
                         group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    x <- r$x
    y <- r$y
    same.group <- r$same.group
    check_zargs(zargs, "ispace")
    res <- if(same.group) {
        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim, x = x, y = y)
        x.grob <- linesGrob(x = unit(c(-eps, 1+eps), "npc"),
                            y = unit(c(-eps,  -eps), "npc"),
                            arrow = arrow(angle = angle, length = length,
                                          ends = "last", type = type),
                            name = "x_axis_2d",
                            gp = gpar(...), vp = vp) # x axis
        y.grob <- linesGrob(x = unit(c(-eps,  -eps), "npc"),
                            y = unit(c(-eps, 1+eps), "npc"),
                            arrow = arrow(angle = angle, length = length,
                                          ends = "last", type = type),
                            name = "y_axis_2d",
                            gp = gpar(...), vp = vp) # y axis
        gTree(children = gList(x.grob, y.grob)) # create a single grob
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Arrow plot in 2d using the grid package
##' @family default 2d plot functions using the grid package
##' @family default 2d plot functions
##' @name arrow_2d_grid
##' @aliases arrow_2d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location of the center of the arrow
##' @param angle angle from the shaft to the edge of the arrow head
##' @param length length of the arrow in [0,1] from tip to base
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
arrow_2d_grid <- function(zargs,
                          loc = c(0.5, 0.5), angle = 60, length = 0.2,
                          group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    check_zargs(zargs, "num", "turns", "ispace")
    turn.out <- zargs$turns[zargs$num]
    res <- if(same.group) {
        vp <- vport(zargs$ispace)
        arrow <- zenarrow(turn.out, angle = angle, length = length,
                          coord.scale = 1)
        arr <- loc + arrow
        ## Plotting
        linesGrob(x = arr[1,], y = arr[2,], default.units = "npc",
                  name = "arrow_2d", gp = gpar(...), vp = vp)
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Rectangle plot in 2d using the grid package
##' @family default 2d plot functions using the grid package
##' @family default 2d plot functions
##' @name rect_2d_grid
##' @aliases rect_2d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location of the rectangle
##' @param width rectangle width as a fraction of 1
##' @param height rectangle height as a fraction of 1
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
rect_2d_grid <- function(zargs,
                         loc = c(0.5, 0.5), width = 1, height = 1,
                         group... = list(cex = 0.66), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    check_zargs(zargs, "ispace")
    res <- if(same.group) {
        vp <- vport(zargs$ispace)
        rectGrob(x = loc[1], y = loc[2], width = width, height = height,
                 default.units = "npc", name = "rect_2d",
                 gp = gpar(...), vp = vp)
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Label plot in 2d using the grid package
##' @family default 2d plot functions using the grid package
##' @family default 2d plot functions
##' @name label_2d_grid
##' @aliases label_2d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param label label to be used
##' @param cex character expansion factor
##' @param just (x,y)-justification of the label
##' @param rot rotation of the label
##' @param box logical indicating whether a box should be drawn
##' @param box.width width of the box
##' @param box.height height of the box
##' @param group... list of arguments passed to group_2d_grid (or NULL)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
label_2d_grid <- function(zargs,
                          loc = c(0.98, 0.05), label = NULL, cex = 0.66,
                          just = c("right", "bottom"), rot = 0,
                          box = FALSE, box.width = 1, box.height = 1,
                          group... = list(cex = cex), draw = FALSE, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    vlabs <- r$vlabs
    check_zargs(zargs, "vars", "num", "ispace")
    vars <- zargs$vars
    num <- zargs$num
    res <- if(same.group) {
        xlab <- vlabs[vars[num, 1]]
        ylab <- vlabs[vars[num, 2]]
        if(is.null(label)) label <- paste0("(",xlab,", ",ylab,")")
        vp <- vport(zargs$ispace)
        gText <- textGrob(label = label,
                          x = loc[1], y = loc[2], just = just, rot = rot,
                          default.units = "npc",
                          name = "label_2d", gp = gpar(cex = cex, ...), vp = vp)
        if(box) {
            gBox <- rectGrob(x = 0.5, y = 0.5,
                             width = box.width, height = box.height, # => plotting outside of viewport (space has been reserved by default ispace)
                             default.units = "npc",
                             name = "box_2d", gp = gpar(...), vp = vp)
            gTree(children = gList(gBox, gText)) # note: first box
        } else {
            gTree(children = gList(gText))
        }
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_grid, args)
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Layout plot in 2d using the grid package
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param ... additional arguments passed to label_2d_grid()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note Here we also pass '...' to group_2d_grid() (to easily adjust
##'       font size etc.)
layout_2d_grid <- function(zargs, ...)
    label_2d_grid(zargs, loc = c(0.5, 0.5),
                  just = "centre", box = TRUE, group... = list(...),
                  ...)

