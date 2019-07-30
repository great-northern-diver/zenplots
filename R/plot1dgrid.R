## Default 1d plot functions based on grid
library(grid)
library(stats)

##' @title Rug plot in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name rug_1d_grid
##' @aliases rug_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc location in [0,1]; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param length length of the rugs
##' @param width line width of the rugs
##' @param col default color of the rectangles/rugs
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
##' @note The choice of width and height is to leave the rugs enough space to not
##'       touch points (so to avoid points and rugs overplotting).
rug_1d_grid <- function(zargs,
                        loc = 0.5, length = 0.5, width = 1e-3, col = par("fg"),
                        draw = FALSE, ...)
{
    r <- extract_1d(zargs)
    x <- as.matrix(r$x)
    horizontal <- r$horizontal
    lim <- r$xlim
    check_zargs(zargs, "num", "turns", "ispace")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    if(turn.out == "d" || turn.out == "l") loc <- 1-loc # when walking downwards, change both left/right and up/down
    if(horizontal) {
        xlim <- lim
        ylim <- 0:1
        x <- x
        y <- loc
        height <- length
        width <- width
    } else {
        xlim <- 0:1
        ylim <- lim
        y <- x
        x <- loc
        height <- width
        width <- length
    }
    ## Plotting
    vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim)
    res <- rectGrob(x = x, y = y, width = width, height = height,
                    default.units = "native",
                    name = "rug_1d", gp = gpar(fill = col, col = col, ...), vp = vp)
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Dot plot in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name points_1d_grid
##' @aliases points_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc location in [0,1]; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param pch plotting symbol
##' @param size size of the plotting symbol
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
##' @note The default point size was chosen to match the default of graphics
points_1d_grid <- function(zargs,
                           loc = 0.5, pch = 21, size = 0.02,
                           draw = FALSE, ...)
{
    r <- extract_1d(zargs)
    x <- as.matrix(r$x)
    horizontal <- r$horizontal
    lim <- r$xlim
    check_zargs(zargs, "num", "turns", "ispace", "width1d", "width2d")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    if(turn.out == "d" || turn.out == "l") loc <- 1-loc # when walking downwards, change both left/right and up/down
    width1d <- zargs$width1d
    width2d <- zargs$width2d
    if(length(loc) == 1) loc <- rep(loc, length(x))
    if(horizontal) {
        xlim <- lim
        ylim <- 0:1
        x <- x
        y <- loc
        size <- size
    } else {
        ylim <- lim
        xlim <- 0:1
        y <- x
        x <- loc
        size <- size * width2d/width1d
    }
    ## Plotting
    vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim)
    res <- pointsGrob(x = x, y = y, pch = pch, size = unit(size, units = "npc"),
                      default.units = "native",
                      name = "points_1d", gp = gpar(...), vp = vp)
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Jittered dot plot in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name jitter_1d_grid
##' @aliases jitter_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc location in [0,1]; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param offset number in [0,0.5] determining how far off the center
##'        the jittered points reach maximally
##' @param pch plotting symbol
##' @param size size of the plotting symbol
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @note The default point size was chosen to match the default of graphics
##' @export
jitter_1d_grid <- function(zargs,
                           loc = 0.5, offset = 0.25, pch = 21, size = 0.02,
                           draw = FALSE, ...)
{
    r <- extract_1d(zargs)
    x <- r$x
    stopifnot(0 <= offset, offset <= 0.5, 0 <= loc, loc <= 1, offset <= min(loc, 1-loc))
    loc. <- loc + runif(length(x), min = -offset, max = offset)
    points_1d_grid(zargs, loc = loc., pch = pch, size = size, draw = draw, ...)
}

##' @title Histogram in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name hist_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param breaks see ?hist; the default is 20 equi-width bins covering the data range
##' @param length.out number of break points if breaks = NULL
##' @param col colour of the histogram bar interiors, unless fill is specified, then
##'        this is the colour of the border
##' @param fill logical passed to the underlying rectGrob()
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
hist_1d_grid <- function(zargs,
                         breaks = NULL, length.out = 21, col = NULL, fill = NULL,
                         draw = FALSE, ...)
{
    r <- extract_1d(zargs)
    x <- as.matrix(r$x)
    horizontal <- r$horizontal
    check_zargs(zargs, "ispace")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    lim <- r$xlim
    res <- if(all(is.na(x))) {
        nullGrob()
    } else {
        ## Colors
        if(is.null(fill)) {
            fill <- if(is.null(col)) "grey" else "black"
        }
        if(is.null(col)) col <- "black"

        ## Range, counts, breaks
        if(is.null(breaks))
            breaks <- seq(from = lim[1], to = lim[2], length.out = length.out)
        binInfo <- hist(x, breaks = breaks, plot = FALSE)
        binBoundaries <- binInfo$breaks
        widths <- diff(binBoundaries)
        heights <- binInfo$density
        if(turn.out == "d" || turn.out == "l") heights <- -heights
        if(horizontal) {
            xlim <- lim
            ylim <- range(0, heights)
        } else {
            xlim <- range(0, heights)
            ylim <- lim
        }

        ## Build the bins
        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim)
        binGrobs <- lapply(1:length(heights), function(i) {
            left <- binBoundaries[i]
            right <- binBoundaries[i+1]
            height <- heights[i]
            rectGrob(x = if(horizontal) left else 0,
                     y = if(horizontal) 0 else left,
                     width = if(horizontal) (right-left) else height,
                     height = if(horizontal) height else (right-left),
                     just = c("left", "bottom"), default.units = "native",
                     name = paste("hist_1d", "bin",i, sep = "_"),
                     gp = gpar(fill = fill, col = col, ...), vp = vp)
        })
        gTree(children = do.call(gList, binGrobs))
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Density plot in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name density_1d_grid
##' @aliases density_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param density... list of arguments for density()
##' @param offset numerical value in \deqn{[0, 0.5]} used to offset
##'        the density within the height 1 box in which it appears
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
density_1d_grid <- function(zargs,
                            density... = NULL, offset = 0.08,
                            draw = FALSE, ...)
{
    r <- extract_1d(zargs)
    x <- r$x[!is.na(r$x)] # omit missing data
    horizontal <- r$horizontal
    check_zargs(zargs, "num", "turns", "ispace")
    turn.out <- zargs$turns[zargs$num] # turn out of current position
    lim <- r$xlim
    res <- if(length(x) == 0) {
        ## Empty plot
        nullGrob()
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
            y <- (1-2*offset) * y + offset * if(turn.out == "d") ylim[1] else ylim[2] 
            # scale to [offset, 1-offset] * ylim[2]
        } else {
            xlim <- range(0, y.)
            ylim <- range(x.)
            x <-  c(0, y., 0)
            y <- c(ylim[1], x., ylim[2])
            ## Scaling
            x <- (1-2*offset) * x + offset * if(turn.out == "l") xlim[1] else xlim[2] # scale to [offset, 1-offset] * xlim[2]
        }
        ## Plotting
        vp <- vport(zargs$ispace, xlim = xlim, ylim = ylim)
        polygonGrob(x = x, y = y, name = "density_1d",
                    default.units = "native",
                    gp = gpar(...), vp = vp)
    }
    ## Plotting
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Boxplot in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name boxplot_1d_grid
##' @aliases boxplot_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param pch plot symbol
##' @param size size of the plot symbol
##' @param col color
##' @param lwd graphical parameter line width for whiskers and median
##' @param bpwidth width of boxplot on scale of default.units
##' @param range numerical value used to determine how far the plot whiskers extend. If
##'        NULL, the whiskers (range) grows with sample size.
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return gTree grob containing the boxplot components as grobs
##' @author Marius Hofert and Wayne Oldford
##' @export
boxplot_1d_grid <- function(zargs,
                            pch = 21, size = 0.02,
                            col = NULL, lwd = 2, bpwidth = 0.5, range = NULL,
                            draw = FALSE, ...)
{
    r <- extract_1d(zargs)
    x <- as.matrix(r$x)
    horizontal <- r$horizontal
    lim <- r$xlim
    check_zargs(zargs, "width1d", "width2d", "ispace")
    width1d <- zargs$width1d
    width2d <- zargs$width2d
    res <- if(all(is.na(x))) {
        nullGrob()
    } else {
        ## Range and color
        if(is.null(range)) { # choose 'range' depending on sample size
            n <- length(x)
            q25 <- qnorm(0.25)
            iqr <- qnorm(0.75) - q25
            range <- (q25 - qnorm(0.35/(2*n)))/iqr
        }
        if(is.null(col)) col <- "grey" # hcl(h = 210, alpha = 0.5)
        medCol <- if(col == "black") "white" else "black"

        ## Summary statistics
        med <- median(x, na.rm = TRUE)
        Q1 <- quantile(x, 0.25, na.rm = TRUE)
        Q3 <- quantile(x, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        upper.fence <- Q3 + (range * IQR)
        lower.fence <- Q1 - (range * IQR)
        upper.adjacent.value <- max(x[x <= upper.fence])
        lower.adjacent.value <- min(x[x >= lower.fence])
        ## upper.outliers <- x[x>upper.adjacent.value]
        ## lower.outliers <- x[x <lower.adjacent.value]
        outliers <- x[(x < lower.adjacent.value) | (x > upper.adjacent.value)]
        existOutliers <- length(outliers) != 0

        ## Draw the boxplot
        if(horizontal) {

            ## Build the viewport
            vp <- vport(zargs$ispace, xlim = lim)

            ## Build the box
            highbox <-  rectGrob(x = med, width = Q3-med, height = bpwidth,
                                 default.units = "native",
                                 just = c("left", "center"),
                                 gp = gpar(fill = col, col = col, ...), vp = vp)
            medLine <- linesGrob(x = c(med, med), y = c(0.5-bpwidth/2, 0.5+bpwidth/2),
                                 default.units = "native", gp = gpar(fill = medCol,
                                                                     col = medCol, lwd = lwd, ...),
                                 vp = vp)
            lowbox <-  rectGrob(x = med, width = med-Q1, height = bpwidth,
                                default.units = "native", just = c("right", "center"),
                                gp = gpar(fill = col, col = col, ...), vp = vp)

            ## Build the whiskers
            highadjacent <- linesGrob(x = c(upper.adjacent.value,upper.adjacent.value),
                                      y = c(0.5 - bpwidth/5, 0.5 + bpwidth/5),
                                      default.units = "native",
                                      gp = gpar(fill = col, col = col, lwd = lwd, ...), vp = vp)
            highwhisker <- linesGrob(x = c(Q3,upper.adjacent.value),
                                     y = c(0.5, 0.5),
                                     default.units = "native",
                                     gp = gpar(fill = col, col = col, lwd = lwd, ...), vp = vp)
            lowadjacent <- linesGrob(x = c(lower.adjacent.value,lower.adjacent.value),
                                     y = c(0.5 - bpwidth/5, 0.5 + bpwidth/5),
                                     default.units = "native",
                                     gp = gpar(fill = col, col = col, lwd = lwd, ...), vp = vp)
            lowwhisker <- linesGrob(x = c(Q1,lower.adjacent.value),
                                    y = c(0.5, 0.5),
                                    default.units = "native",
                                    gp = gpar(fill = col, col = col, lwd = lwd, ...), vp = vp)

            ## Gather the outliers (if any)
            if (existOutliers)
                outlierpoints <- pointsGrob(x = outliers, y = rep(0.5, length(outliers)),
                                            pch = pch, size = unit(size, units = "npc"),
                                            default.units = "native",
                                            gp = gpar(fill = col, col = col, ...),
                                            vp = vp)

        } else { # !horizontal

            ## Build the viewport
            vp <- vport(zargs$ispace, ylim = lim)

            ## Build the box
            highbox <-  rectGrob(y = med, height = Q3-med, width = bpwidth,
                                 default.units = "native", just = c("center", "bottom"),
                                 gp = gpar(fill = col, col = col, ...), vp = vp)
            medLine <- linesGrob(x = c(0.5-bpwidth/2, 0.5+bpwidth/2),
                                 y = c(med, med), default.units = "native",
                                 gp = gpar(fill = medCol, col = medCol, lwd = lwd, ...), vp = vp)
            lowbox <-  rectGrob(y = med, height = med-Q1, width = bpwidth,
                                default.units = "native",
                                just = c("center", "top"), gp = gpar(fill = col, col = col, ...),
                                vp = vp)

            ## Build the whiskers
            highadjacent <- linesGrob(x = c(0.5 - bpwidth/5, 0.5 + bpwidth/5),
                                      y = c(upper.adjacent.value,upper.adjacent.value),
                                      default.units = "native",
                                      gp = gpar(fill = col, col = col,lwd = lwd, ...), vp = vp)
            highwhisker <- linesGrob(x = c(0.5, 0.5),
                                     y = c(Q3,upper.adjacent.value),
                                     default.units = "native",
                                     gp = gpar(fill = col, col = col,lwd = lwd, ...), vp = vp)
            lowadjacent <- linesGrob(x = c(0.5 - bpwidth/5, 0.5 + bpwidth/5),
                                     y = c(lower.adjacent.value,lower.adjacent.value),
                                     default.units = "native",
                                     gp = gpar(fill = col, col = col, lwd = lwd, ...), vp = vp)
            lowwhisker <- linesGrob(x = c(0.5, 0.5),
                                    y = c(Q1,lower.adjacent.value),
                                    default.units = "native",
                                    gp = gpar(fill = col, col = col, lwd = lwd, ...), vp = vp)

            ## Gather the outliers (if any)
            if(existOutliers)
                outlierpoints <- pointsGrob(x = rep(0.5, length(outliers)), y = outliers,
                                            pch = pch, size = unit(size * width2d/width1d, units = "npc"),
                                            default.units = "native",
                                            gp = gpar(fill = col, col = col, ...),
                                            vp = vp)
        }

        ## Put it all together
        boxplotGrobs <- if(existOutliers)
                            gList(lowadjacent, lowwhisker, lowbox, highbox,
                                  ## medPoint, # med must come after the boxes
                                  medLine, highwhisker, highadjacent, outlierpoints)
                        else
                            gList(lowadjacent, lowwhisker, lowbox, highbox,
                                  ## medPoint, # med must come after the boxes
                                  medLine, highwhisker, highadjacent)
        gTree(children = boxplotGrobs, name = "boxplot_1d")
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Arrow plot in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name arrow_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param angle angle from the shaft to the edge of the arrow head
##' @param length length of the arrow in [0,1] from tip to base
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
arrow_1d_grid <- function(zargs,
                          loc = c(0.5, 0.5), angle = 60, length = 0.6,
                          draw = FALSE, ...)
{
    check_zargs(zargs, "num", "turns", "width1d", "width2d", "ispace")
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
                      coord.scale = width1d / width2d)
    arr <- loc + arrow
    ## Plotting
    vp <- vport(zargs$ispace)
    res <- linesGrob(x = arr[1,], y = arr[2,], default.units = "npc",
                     name = "arrow_1d", gp = gpar(...), vp = vp)
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Rectangle plot in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name rect_1d_grid
##' @aliases rect_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location of the rectangle
##' @param width width of the rectangle (when viewed in walking direction)
##' @param height height of the rectangle (when viewed in walking direction)
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
rect_1d_grid <- function(zargs,
                         loc = c(0.5, 0.5), width = 1, height = 1,
                         draw = FALSE, ...)
{
    check_zargs(zargs, "num", "turns", "ispace")
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
    ## Plotting
    vp <- vport(zargs$ispace)
    res <- rectGrob(x = loc[1], y = loc[2], width = width, height = height,
                    default.units = "npc",
                    name = "rect_1d", gp = gpar(...), vp = vp)
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Lines plot in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name lines_1d_grid
##' @aliases lines_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param length length of the line (in [0,1])
##' @param arrow list describing the arrow head
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
lines_1d_grid <- function(zargs,
                          loc = c(0.5, 0.5), length = 1, arrow = NULL,
                          draw = FALSE, ...)
{
    check_zargs(zargs, "num", "turns", "ispace")
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
    vp <- vport(zargs$ispace)
    res <- linesGrob(x = x, y = y, arrow = arrow,
                     default.units = "npc",
                     name = "lines_1d", gp = gpar(...), vp = vp)
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Label plot in 1d using the grid package
##' @family default 1d plot functions using the grid package
##' @family default 1d plot functions
##' @name label_1d_grid
##' @aliases label_1d_grid
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param loc (x,y)-location in [0,1]^2; 0 corresponds to left, 1 to right (in
##'        the direction of the path)
##' @param label label to be used
##' @param cex character expansion factor
##' @param box logical indicating whether a box should be drawn around
##'        the text
##' @param box.width width of the box
##' @param box.height height of the box
##' @param draw logical indicating whether drawing should take place
##' @param ... additional arguments passed to gpar()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
label_1d_grid <- function(zargs,
                          loc = c(0.5, 0.5), label = NULL, cex = 0.66,
                          box = FALSE, box.width = 1, box.height = 1,
                          draw = FALSE, ...)
{
    r <- extract_1d(zargs)
    horizontal <- r$horizontal
    if(is.null(label)) label <- names(r$x) # combined group and variable label
    check_zargs(zargs, "num", "turns", "ispace")
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
    rot <- if(horizontal) {
        0 # note: we don't turn label upside down
    } else {
        if(turn.out == "r") -90 else 90
    }
    ## Plotting
    vp <- vport(zargs$ispace)
    gText <- textGrob(label = label,
                      x = loc[1], y = loc[2], rot = rot,
                      default.units = "npc",
                      name = "label_1d", gp = gpar(cex = cex, ...), vp = vp)
    res <- if(box) {
        gBox <- rectGrob(x = 0.5, y = 0.5,
                         width = box.width, height = box.height, # => plotting outside of viewport (space has been reserved by default ispace)
                         default.units = "npc",
                         name = "box_2d", gp = gpar(fill = 0, ...), vp = vp)
        gTree(children = gList(gBox, gText)) # note: first box
    } else {
        gTree(children = gList(gText))
    }
    if(draw) grid.draw(res)
    invisible(res)
}

##' @title Layout plot in 1d using the grid package
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param ... additional arguments passed to label_1d_grid()
##' @return grob (invisibly)
##' @author Marius Hofert and Wayne Oldford
layout_1d_grid <- function(zargs, ...)
    label_1d_grid(zargs, box = TRUE, ...)

