## Default 1d plot functions based on grid


##' @title Rug plot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param ... Additional parameters passed to l_plot()
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
##' @note Just calls points_1d_loon with glyph = "osquare" to preserve linking
rug_1d_loon <- function(zargs, ...)
    points_1d_loon(zargs, glyph = "square", ...)

##' @title Dot plot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param linkingGroup A string specifying the initial group of plots to be
##'        linked to this plot
##' @param linkingKey List of IDs to link on
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are
##'        displayed
##' @param glyph The plot glyph
##' @param itemlabel A vector of strings to serve as the itemlabels
##' @param showItemlabels Logical determing whether itemlabels display on mouse
##'        hover
##' @param parent The tk parent for this loon plot widget
##' @param ... Additional parameters passed to l_plot()
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
points_1d_loon <- function(zargs,
                           linkingGroup = NULL, linkingKey = NULL,
                           showLabels = FALSE, showScales = FALSE,
                           showGuides = FALSE, glyph = "ocircle",
                           itemlabel = NULL, showItemlabels = TRUE,
                           parent = NULL, ...)
{
    r <- extract_1d(zargs)
    x <- r$x
    xlim <- r$xlim
    horizontal <- r$horizontal

    ## Check for linkingGroup
    ## TODO: not sure we should do this here, or simply (as before) rely
    ##       on linkingGroup being passed by zenplot.
    ##       Alternatively linkingGroup could default to `none`
    if (is.null(linkingGroup))
        linkingGroup <- paste0("zenplot parent =", parent$ID)

    ## Remove NAs
    ldata <- na_omit_loon(x = x, linkingKey = linkingKey, itemlabel = itemlabel)

    ## Main
    x <- ldata$x
    linkingKey <- ldata$linkingKey
    itemlabel <- ldata$itemlabel
    check_zargs(zargs, "ispace")
    if(horizontal) {
        baseplot <- l_plot(x = x, y = rep(0, length(x)),
                           linkingGroup = linkingGroup,
                           linkingKey = linkingKey,
                           showLabels = FALSE,
                           showScales = showScales,
                           showGuides = showGuides,
                           glyph = glyph,
                           itemlabel = itemlabel,
                           showItemlabels = showItemlabels,
                           parent = parent,
                           ...)
        l_ispace_config(baseplot = baseplot,
                        ispace = zargs$ispace,
                        xlim = xlim)
    } else {
        baseplot <- l_plot(x = rep(0, length(x)), y = x,
                           linkingGroup = linkingGroup,
                           showLabels = FALSE,
                           showScales = showScales,
                           showGuides = showGuides,
                           glyph = glyph,
                           itemlabel = itemlabel,
                           showItemlabels = showItemlabels,
                           parent = parent,
                           ...)
        l_ispace_config(baseplot = baseplot,
                        ispace = zargs$ispace,
                        ylim = xlim)
    }
    baseplot
}

##' @title Jittered dot plot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param linkingGroup A string specifying the initial group of plots to be
##'        linked to this plot
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines
##'        are displayed
##' @param glyph Glyph to be used for points, default is the open circle:
##'        "ocircle"
##' @param itemlabel A vector of strings to serve as the itemlabels
##' @param showItemlabels Logical determing whether itemlabels display on mouse
##'        hover
##' @param parent The tk parent for this loon plot widget
##' @param ... Additional parameters passed to l_plot()
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
jitter_1d_loon <- function(zargs,
                           linkingGroup = NULL, showLabels = FALSE,
                           showScales = FALSE, showGuides = FALSE,
                           glyph = "ocircle", itemlabel = NULL,
                           showItemlabels = TRUE, parent = NULL, ...)
{
    r <- extract_1d(zargs)
    x <- r$x
    x <- na.omit(x)
    xlim <- r$xlim
    horizontal <- r$horizontal
    if(is.null(itemlabel)) {
        if(!is.null(rownames(x))) {
            itemlabel <- rownames(x)
        } else {
            itemlabel <- sapply(1:length(x),
                                function (i) {paste0("point", i)})
        }
    }
    if (is.null(linkingGroup))
        linkingGroup <- paste0("zenplot parent =", parent$ID)
    check_zargs(zargs, "ispace")
    if(horizontal) {
        baseplot <- l_plot(x = x,  y = runif(length(x)),
                           linkingGroup = linkingGroup,
                           showLabels = showLabels,
                           showScales = showScales,
                           showGuides = showGuides,
                           glyph = glyph,
                           itemlabel = itemlabel,
                           showItemlabels = showItemlabels,
                           parent = parent,
                           ...)
        l_ispace_config(baseplot = baseplot,
                        ispace = zargs$ispace,
                        xlim = xlim)
    } else {
        baseplot <- l_plot(x = runif(length(x)),  y = x,
                           linkingGroup = linkingGroup,
                           showLabels = showLabels,
                           showScales = showScales,
                           showGuides = showGuides,
                           glyph = glyph,
                           itemlabel = itemlabel,
                           showItemlabels = showItemlabels,
                           parent = parent,
                           ...)
        l_ispace_config(baseplot = baseplot,
                        ispace = zargs$ispace,
                        ylim = xlim)
    }
    baseplot
}

##' @title Histogram in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param breaks Argument passed to hist() to get information on bins. Default
##'        is 20 equi-width bins covering the range of x
##' @param color colour of the histogram bar interiors, unless fill is specified,
##'        then this is the colour of the border
##' @param fill colour of the histogram bar interior if given
##' @param showStackedColors Logical determining whether
##'        to show the individual point colours stacked in the histogram
##' @param showBinHandle Logical to show a handle to adjust bins
##' @param showLabels Logical determining whether axis labels are displayed
##' @param linkingGroup A string specifying the initial group of plots to be
##'        linked to this plot
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are
##'        displayed
##' @param parent The tk parent for this loon plot widget
##' @param ... Additional parameters passed to l_hist()
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
hist_1d_loon <- function(zargs,
                         breaks = NULL, color = NULL, fill = NULL,
                         showStackedColors = TRUE,
                         showBinHandle = FALSE, showLabels = FALSE,
                         linkingGroup = NULL, showScales = FALSE,
                         showGuides = FALSE, parent = NULL, ...)
{
    ## Extracting the information
    r <- extract_1d(zargs)
    x <- r$x
    xlim <- r$xlim
    horizontal <- r$horizontal
    loonInfo <- na_omit_loon(x = x)
    x <- loonInfo$x
    linkingKey <- loonInfo$linkingKey
    if (is.null(linkingGroup))
        linkingGroup <- paste0("zenplot parent =", parent$ID)

    ## Main
    if(all(is.na(x))) {
        h <- l_hist(linkingGroup = linkingGroup)
    } else {
        if(is.null(fill)) {
            if(is.null(color)) {
                colorFill <- "grey"
                colorOutline <- "black"
            } else {
                colorFill <- color
                colorOutline <- "black"
            }
        } else {
            colorFill <- fill
            if(is.null(color)) colourOutline <- "black"
        }

        xRange <- range(x)
        if(is.null(breaks))
            breaks <- seq(from = xRange[1], to = xRange[2], length.out = 21)
        binInfo <- hist(x, breaks = breaks, plot = FALSE)
        binBoundaries <- binInfo$breaks
        h <- l_hist(x = x,
                    yshows = 'density',
                    origin = binBoundaries[1],
                    binwidth = abs(diff(binBoundaries[1:2])),
                    linkingGroup = linkingGroup,
                    linkingKey = linkingKey,
                    swapAxes = !horizontal,
                    showBinHandle = showBinHandle,
                    showLabels = showLabels,
                    showScales = showScales,
                    showGuides = showGuides,
                    showStackedColors = showStackedColors,
                    colorFill = colorFill,
                    colorOutline = colorOutline,
                    parent = parent,
                    ...)
    }

    ## Scale
    check_zargs(zargs, "ispace")
    l_ispace_config(baseplot = h,
                    ispace = zargs$ispace,
                    xlim = xlim)

    ## Return
    h
}

##' @title Density plot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param density.args A list of arguments for density()
##' @param method A character specifying the type of density used
##' @param lwd Line width  used only when linewidth = NULL, value of 1 used
##'        otherwise.
##' @param linewidth Line width of outline for density polygons (highest
##'        priority)
##' @param color Colour used to fill the density when fill is NULL and to
##'        outline the density when linecolor is NULL, foreground colour used
##'        otherwise.
##' @param fill Colour used to fill the density polygon
##' @param linecolor Colour used for the outline of the density
##' @param linkingGroup A string specifying the initial group of plots to be
##'        linked to this plot
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are
##'        displayed
##' @param baseplot If non-null the base plot on which the plot should be
##'        layered
##' @param parent The tk parent for this loon plot widget
##' @param ... Additional parameters passed to l_layer()
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
density_1d_loon <- function(zargs,
                            density.args = list(), method = c("single", "double"),
                            lwd = NULL, linewidth = NULL, color = NULL,
                            fill = NULL, linecolor = NULL, linkingGroup = NULL,
                            showLabels = FALSE, showScales = FALSE,
                            showGuides = FALSE, baseplot = NULL, parent = NULL, ...)
{
    ## Extracting the information
    r <- extract_1d(zargs)
    x <- r$x
    x <- na_omit_loon(x)$x
    xlim <- r$xlim
    horizontal <- r$horizontal

    if (is.null(linkingGroup))
        linkingGroup <- paste0("zenplot parent =", parent$ID)

    ## Main
    if(all(is.na(x))) {
        if (is.null(baseplot)) baseplot <- l_plot(linkingGroup = linkingGroup)
    } else {
        dens <- do.call(density, args = c(list(x), density.args))
        xvals <- dens$x
        keepers <- xvals >= min(x) & xvals <= max(x)
        xvals <- xvals[keepers]

        xrange <- range(xvals)
        ##xvals <- (xvals - min(xrange))/diff(xrange)
        yvals <- dens$y[keepers]
        method <- match.arg(method)
        switch(method,
               "single" = {
            ##yvals <- yvals/max(yvals)
            x <- c(min(xvals), xvals, max(xvals))
            y <- c(0, yvals, 0)
        },
        "double" = {
            x <- rep(c(min(xvals), xvals, max(xvals)), 2)
            yvals <- c(c(0, -yvals, 0), c(0, yvals, 0))
            yrange <- range(yvals)
            y <- yvals # (yvals - min(yrange))/diff(yrange)
        },
        stop("Wrong 'method'"))

        ## Get the base plot if not supplied
        if(is.null(baseplot)) {
            baseplot <- l_plot(showLabels = showLabels,
                               showScales = showScales,
                               showGuides = showGuides,
                               linkingGroup = linkingGroup,
                               parent = parent)
        }

        ## Sort out colours
        if(is.null(fill)) {
            if(is.null(color)) fill <- "grey50" else fill <- color
        } # fill has a value, on to linecolor
        if(is.null(linecolor)) {
            if(is.null(color)) linecolor <- baseplot['foreground'] else linecolor <- color
        } # linecolor has a value

        ## Sort out line widths
        if(is.null(linewidth)) {
            if(is.null(lwd)) {
                ## use linewidth
                linewidth <- 1
            } else { # use lwd
                linewidth <- lwd
            }
        }

        densityPoly <- l_layer_polygon(baseplot,
                                       x = x,
                                       y = y,
                                       color = fill,
                                       linecolor = linecolor,
                                       linewidth = linewidth,
                                       ...)
        ## l_scaleto_layer(baseplot, densityPoly)
    }

    if (!horizontal)  baseplot['swapAxes'] <- TRUE
    check_zargs(zargs, "ispace")
    l_ispace_config(baseplot = baseplot,
                    ispace = zargs$ispace,
                    x = x, y = y,
                    xlim = xlim)
    baseplot
}

##' @title Boxplot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param color colour for boxplot
##' @param linecolor Colour used for the lines to draw the boxplot
##' @param lwd The parameter line width for whiskers and median and box
##'        boundaries
##' @param range numerical value used to determine how far the plot whiskers
##'        extend. If NULL, the whiskers (range) grows with sample size.
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines
##'        are displayed
##' @param linkingGroup A string specifying the initial group of plots to be
##'        linked to this plot
##' @param baseplot If non-null the base plot on which the plot should be layered
##' @param parent The tk parent for this loon plot widget
##' @param ... Additional parameters passed to gpar()
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
boxplot_1d_loon <- function(zargs,
                            color = NULL, linecolor = NULL, lwd = 2,
                            range = NULL, showLabels = FALSE, showScales = FALSE,
                            showGuides = FALSE, linkingGroup = NULL,
                            baseplot = NULL, parent, ...)
{
    ## Extracting the information
    r <- extract_1d(zargs)
    x <- r$x
    xlim <- r$xlim
    horizontal <- r$horizontal
    loonInfo <- na_omit_loon(x)
    x <- loonInfo$x
    linkingKey <- loonInfo$linkingKey
    itemlabel <- loonInfo$itemlabel

    if (is.null(linkingGroup))
        linkingGroup <- paste0("zenplot parent =", parent$ID)

    if(is.null(range)) { # choose 'range' depending on sample size
        n <- length(x)
        q25 <- qnorm(0.25)
        iqr <- qnorm(0.75) - q25
        range <- (q25 - qnorm(0.35/(2*n)))/iqr
    }

    if(is.null(color)) color <- "grey"
    medianCol <- if(color == "black") "grey90" else "black"

    if (is.null(linecolor)) linecolor <- color

    ## Summary statistics
    median <- median(x, na.rm = TRUE)
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    upper.fence <- Q3 + (range * IQR)
    lower.fence <- Q1 - (range * IQR)
    upper.adjacent.value <- max(x[x <= upper.fence])
    lower.adjacent.value <- min(x[x >= lower.fence])
    ## upper.outliers <- x[x > upper.adjacent.value]
    ## lower.outliers <- x[x < lower.adjacent.value]
    outlying <- (x < lower.adjacent.value) | (x > upper.adjacent.value)
    outlierLabels <- itemlabel[outlying]
    outlierLinkingKey <- linkingKey[outlying]
    outliers <- x[outlying]
    nOutliers <- sum(outlying)
    existOutliers <- nOutliers != 0

    ## Get the base plot if not supplied
    if(is.null(baseplot)) {
        baseplot <- l_plot(showLabels = showLabels,
                           showScales = showScales,
                           showGuides = showGuides,
                           parent = parent)
    }

    ## Build order matters to get the layering right.
    ## Build the whiskers
    highadjacent <- l_layer_line(baseplot,
                                 x = c(upper.adjacent.value, upper.adjacent.value),
                                 y = c(0.25, 0.75),
                                 label = "Upper adjacent value",
                                 color = linecolor,
                                 linewidth = lwd,
                                 ...)
    highwhisker <- l_layer_line(baseplot,
                                x = c(Q3,upper.adjacent.value),
                                y = c(0.5, 0.5),
                                label = "Upper whisker",
                                color = linecolor,
                                linewidth = lwd,
                                ...)
    lowadjacent <- l_layer_line(baseplot,
                                x = c(lower.adjacent.value,lower.adjacent.value),
                                y = c(0.25, 0.75),
                                label = "Lower adjacent value",
                                color = linecolor,
                                linewidth = lwd,
                                ...)
    lowwhisker <- l_layer_line(baseplot,
                               x = c(Q1,lower.adjacent.value),
                               y = c(0.5, 0.5),
                               label = "Lower whisker",
                               color = linecolor,
                               linewidth = lwd,
                               ...)

    ## Build the box
    highbox <- l_layer_rectangle(baseplot,
                                 x = c(median, Q3),
                                 y = c(0, 1),
                                 label = "upper half of middle 50%",
                                 color = color,
                                 linecolor = linecolor,
                                 linewidth = lwd,
                                 ...)
    lowbox <- l_layer_rectangle(baseplot,
                                x = c(median, Q1),
                                y = c(0, 1),
                                label = "lower half of middle 50%",
                                color = color,
                                linecolor = linecolor,
                                linewidth = lwd,
                                ...)
    medianLine <- l_layer_line(baseplot,
                               x = c(median, median),
                               y = c(0, 1),
                               label = "Median line",
                               color = medianCol,
                               linewidth = lwd,
                               ...)

    ## Gather the outliers (if any)
    if (existOutliers){
        if (is.null(itemlabel)){
            outlierIndices <- which (x %in% outliers)
            if (!is.null(rownames(x))) {
                outlierLabels <- rownames(x)[outlierIndices]
            } else
            {outlierLabels <- sapply(outlierIndices,
                                     function(i){
                 paste0("point",i)
             })
            }

        } else outlierLabels <- itemlabel

        outlierpoints <- l_layer_points(baseplot,
                                        x = outliers,
                                        y = rep(0.5, nOutliers),
                                        label = itemlabel,
                                        color = color,
                                        ...)
    }

    ## Scale
    check_zargs(zargs, "ispace")
    l_ispace_config(baseplot = baseplot,
                    ispace = zargs$ispace,
                    xlim = xlim)
    if (!horizontal) baseplot['swapAxes'] <- TRUE

    ## Return
    baseplot
}

##' @title Arrow plot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param loc The (x,y) location of the center of the arrow
##' @param length The length of the arrow
##' @param angle The angle from the shaft to the edge of the arrow head
##' @param linkingGroup A string specifying the initial group of plots to be
##'        linked to this plot
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are
##'        displayed
##' @param baseplot If non-null the base plot on which the plot should be layered
##' @param parent The tk parent for this loon plot widget
##' @param ... Additional parameters passed to l_layer_line(...)
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
arrow_1d_loon <- function(zargs,
                          loc = c(0.5, 0.5), length = 0.6, angle = NULL,
                          linkingGroup = NULL, showLabels = FALSE,
                          showScales = FALSE, showGuides = FALSE,
                          baseplot = NULL, parent = NULL, ...)
{
    check_zargs(zargs, "width1d", "width2d")
    width1d <- zargs$width1d
    width2d <- zargs$width2d
    if(is.null(angle)) angle <- 30 * width1d/width2d
    if (is.null(linkingGroup))
        linkingGroup <- paste0("zenplot parent =", parent$ID)
    arrow_2d_loon(zargs,
                  loc = loc, length = length, angle = angle,
                  linkingGroup = linkingGroup,
                  showLabels = showLabels,
                  showScales = showScales,
                  showGuides = showGuides,
                  baseplot = baseplot,
                  parent = parent,
                  ...)
}

##' @title Rectangle plot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param loc.x x-location of rectangle
##' @param loc.y y-location of rectangle
##' @param color Colour of the rectangle outline
##' @param fill Colour of the rectangle interior
##' @param lwd line width for rectangle outline
##' @param linkingGroup A string specifying the initial group of plots to be
##'        linked to this plot (ignored)
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines
##'        are displayed
##' @param baseplot If non-NULL the base plot on which the plot should be
##'        layered
##' @param parent The tk parent for this loon plot widget
##' @param ... Additional parameters passed to l_layer_text(...)
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
rect_1d_loon <- function(zargs,
                         loc.x = NULL, loc.y = NULL, color = NULL,
                         fill = NULL, lwd = 1,
                         linkingGroup = NULL, showLabels = FALSE,
                         showScales = FALSE, showGuides = FALSE,
                         baseplot = NULL, parent = NULL, ...)
{
    r <- extract_1d(zargs)
    horizontal <- r$horizontal
    xlim <- r$xlim
    x <- r$x

    if (is.null(linkingGroup))
        linkingGroup <- paste0("zenplot parent =", parent$ID)

    ## Get the base plot if not supplied
    if(is.null(baseplot)) {
        baseplot <- l_plot(showLabels = showLabels,
                           showScales = showScales,
                           showGuides = showGuides,
                           linkingGroup  = linkingGroup,
                           parent = parent)
    }
    if(is.null(color)) color <- baseplot['foreground']
    if(is.null(fill)) fill <- baseplot['background']

    ## Get rectangle info now
    label <- paste("Rectangle:", colnames(x))

    if (is.null(loc.x)) loc.x <- 0:1
                                        # if (is.null(loc.x)) loc.x <- c(baseplot['panX'] +
                                        #                                  (0.1) * baseplot['deltaX']/baseplot['zoomX'],
                                        #                                baseplot['panX'] +
                                        #                                  (0.9) * (baseplot['deltaX']/baseplot['zoomX']))
                                        # c(baseplot['panX'],
                                        #   baseplot['panX'] +
                                        #     (baseplot['deltaX']/baseplot['zoomX']))
                                        # #0:1
    if (is.null(loc.y)) loc.y <- 0:1
                                        # if (is.null(loc.y)) loc.y <- c(baseplot['panY'] +
                                        #                                  (0.2) * (baseplot['deltaY']/baseplot['zoomY']),
                                        #                                baseplot['panY'] +
                                        #                                  (0.8) * (baseplot['deltaY']/baseplot['zoomY']))
                                        # c(baseplot['panY'],
                                        #   baseplot['panY'] +
                                        #     (baseplot['deltaY']/baseplot['zoomY']))
                                        #0:1

    ## Build the box
    l_layer_rectangle(baseplot,
                      x = loc.x,
                      y = loc.y,
                      label = label,
                      color = fill,
                      linecolor = color,
                      linewidth = lwd,
                      ...)
    if (!horizontal) baseplot['swapAxes'] <- TRUE

    ## Scale
    check_zargs(zargs, "ispace")
    l_ispace_config(baseplot = baseplot,
                    ispace = zargs$ispace,
                    x = loc.x, y = loc.y,
                    xlim = xlim)

    ## Return
    baseplot
}
##' @title Lines plot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param loc.x x-coordinates of the points on the line
##' @param loc.y y-coordinates of the pointson the line
##' @param color Colour of the line
##' @param lwd line width
##' @param linkingGroup A string specifying the initial group of plots to be
##'        linked to this plot (ignored)
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are
##'        displayed
##' @param baseplot If non-null the base plot on which the plot should be
##'        layered
##' @param parent The tk parent for this loon plot widget
##' @param ... Additional parameters passed to l_layer_text(...)
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
lines_1d_loon <- function(zargs,
                          loc.x = NULL, loc.y = NULL,
                          color = NULL, lwd = 1,
                          linkingGroup = NULL,
                          showLabels = FALSE, showScales = FALSE,
                          showGuides = FALSE, baseplot = NULL,
                          parent = NULL, ...)
{
    r <- extract_1d(zargs)
    horizontal <- r$horizontal
    ldata <- na_omit_loon(r$x)
    xlim <- r$xlim

    x <- ldata$x
    if(length(x) == 0) x <- c(0,1)

    if (is.null(linkingGroup))
        linkingGroup <- paste0("zenplot parent =", parent$ID)
    if(is.null(loc.x)) loc.x <- range(x)
    if(is.null(loc.y)) loc.y <- c(0.5, 0.5)

    ## Get the base plot if not supplied
    if(is.null(baseplot)) {
        baseplot <- l_plot(showLabels = showLabels,
                           showScales = showScales,
                           showGuides = showGuides,
                           parent = parent)
    }
    if(is.null(color)) color <- baseplot['foreground']

    l_layer_line(widget = baseplot, x = loc.x, y = loc.y,
                 color = color, linewidth = lwd, ...)

    if (!horizontal) baseplot['swapAxes'] <- TRUE

    ## Scale
    check_zargs(zargs, "ispace")
    l_ispace_config(baseplot = baseplot,
                    ispace = zargs$ispace,
                    x = loc.x, y = loc.y,
                    xlim = xlim)

    ## Return
    baseplot
}

##' @title Label plot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param loc.x x-location of the label
##' @param loc.y y-location of the label
##' @param label The label to be used
##' @param rot The rotation of the label
##' @param size The font size
##' @param linkingGroup A string specifying the initial group of plots to be
##'        linked to this plot
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines
##'        are displayed
##' @param baseplot If non-null the base plot on which the plot should be
##'        layered
##' @param parent The tk parent for this loon plot widget
##' @param ... Additional parameters passed to l_layer_text(...)
##' @return A loon l_plot(...)
##' @author Marius Hofert and Wayne Oldford
label_1d_loon <- function(zargs,
                          loc.x = NULL, loc.y = NULL, label = NULL,
                          rot = NULL, size = 8, box = FALSE, color = NULL,
                          linkingGroup = NULL, showLabels = FALSE,
                          showScales = FALSE, showGuides = FALSE,
                          baseplot = NULL, parent = NULL, ...)
{
    r <- extract_1d(zargs)
    horizontal <- r$horizontal
    x <- r$x

    if(is.null(loc.y)) loc.y <- 0.5
    if(is.null(loc.x)) {loc.x <- 0.5}
    if(is.null(label)) label <- colnames(x)
    if(is.null(rot)) rot <- if (horizontal) 0 else 90

    if (is.null(linkingGroup))
        linkingGroup <- paste0("zenplot parent =", parent$ID)

    if(is.null(baseplot))
        baseplot <- l_plot(showLabels = showLabels,
                           showScales = showScales,
                           showGuides = showGuides,
                           parent = parent)

    if(is.null(color)) color <- baseplot['foreground']

    l_layer_text(baseplot, text = label,
                 x = loc.x,
                 y = loc.y,
                 angle = rot,
                 size = size,
                 color = color,
                 ...)
    if (box) {
        rect_1d_loon(zargs,
                     color = color,
                     baseplot = baseplot, parent = parent,
                     index="end", ...)

    }

    if (!horizontal) baseplot['swapAxes'] <- TRUE

    ## Scale
    check_zargs(zargs, "ispace")
    l_ispace_config(baseplot = baseplot,
                    ispace = zargs$ispace,
                    xlim = c(0,1), ylim = c(0,1))

    ## Return
    baseplot
}

##' @title Layout plot in 1d
##' @param zargs The argument list as passed from zenplot()
##' @param ... Additional arguments passed to label_1d_loon()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
layout_1d_loon <- function(zargs, ...)
    label_1d_loon(zargs, box = TRUE, ...)

