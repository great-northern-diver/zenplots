## Default 2d plot functions based on loon
##' @title Plot of labels indicating adjacent groups using the interactive loon package
##' @family default 2d plot functions using the interactive loon package
##' @family default 2d plot functions
##' @name group_2d_loon
##' @aliases group_2d_loon
##' @param zargs argument list as passed from \code{\link{zenplot}()}
##' @param glabs group labels being indexed by the plot variables
##'        (and thus of length as the number of variables);
##'        if NULL then they are determined with extract_2d()
##' @param sep group label separator
##' @param size plot size
##' @param rot rotation
##' @param baseplot If non-NULL the base plot on which the plot should be
##'        layered
##' @param parent tk parent for this loon plot widget
##' @param ... Additional arguments passed to text()
##' @return invisible()
##' @author Marius Hofert & Wayne Oldford
##' @note For performance reasons (avoiding having to call extract_2d() twice),
##'       'glabs' is an extra argument
##' @export
group_2d_loon <- function(zargs,
                          glabs = NULL, sep = "\n", size = 8, rot = 0,
                          baseplot = NULL, parent = NULL, ...)
{
    check_zargs(zargs, "turns", "vars", "num")
    turns <- zargs$turns
    vars <- zargs$vars
    num <- zargs$num
    xlim <- 0:1
    ylim <- 0:1
    ii <- c(min(vars[num,]), max(vars[num,])) # variable index
    ii <- if(turns[num-1] == "u" || turns[num] == "u") rev(ii) else ii
    if(is.null(glabs)) {
        glabs <- extract_2d(zargs)$glabs
    } else {
        len.groups <- length(unlist(zargs$x, recursive = FALSE))
        if(length(glabs) != len.groups)
            stop("length(glabs) has to equal the number ",len.groups," of variables in all groups together; consider rep()")
    }
    labs <- paste0(glabs[ii], collapse = "\n") # labels (in the correct order for displaying the group change)
    if(is.null(baseplot))
        baseplot <- loon::l_plot(showLabels = FALSE,
                                 showScales = FALSE,
                                 showGuides = FALSE,
                                 parent = parent)
    loon::l_layer_text(baseplot, text = labs,
                       x = xlim[1] + 0.5 * diff(xlim), y = ylim[1] + 0.5 * diff(ylim),
                       angle = rot, size = size, ...)
    baseplot
}

##' @title Point plot in 2d using the interactive loon package
##' @family default 2d plot functions using the interactive loon package
##' @family default 2d plot functions
##' @name points_2d_loon
##' @aliases points_2d_loon
##' @param zargs The argument list as passed from \code{\link{zenplot}()}
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are displayed
##' @param linkingGroup The initial linking group
##' @param linkingKey List of IDs to link on
##' @param glyph String determining the glyph type to be displayed for points, default is an open circle: "ocircle"
##' @param itemLabel A vector of strings to serve as the item label
##' @param showItemLabels Logical determing whether item labels display on mouse hover
##' @param parent The tk parent for this loon plot widget
##' @param group... A list of arguments passed to group_2d_loon (or NULL)
##' @param ... Additional arguments passed to loon::l_plot()
##' @return A loon plot
##' @author Marius Hofert and Wayne Oldford
##' @export
points_2d_loon <- function(zargs,
                           showLabels = FALSE, showScales = FALSE,
                           showGuides = FALSE, linkingGroup = NULL,
                           linkingKey = NULL, glyph = "ocircle",
                           itemLabel = NULL, showItemLabels = TRUE,
                           parent = NULL, group... = NULL, ...)
{
    r <- extract_2d(zargs)
    x <- as.matrix(r$x)
    y <- as.matrix(r$y)
    xlim <- r$xlim
    ylim <- r$ylim
    same.group <- r$same.group
    check_zargs(zargs, "ispace")
    if(same.group) {
        ## Check for linkingGroup
        if (is.null(linkingGroup))
            linkingGroup <-  paste0("zenplot parent =", parent$ID)
        ## Remove NAs
        ldata <- na_omit_loon(x, y, linkingKey, itemLabel)
        ## TODO fix box or not: if(box) box() # plot the box
        ## Do the plot
        baseplot <-  loon::l_plot(x = ldata$x, y = ldata$y,
                            linkingGroup = linkingGroup,
                            linkingKey = ldata$linkingKey,
                            showLabels = showLabels,
                            showScales = showScales,
                            showGuides = showGuides,
                            glyph = glyph,
                            itemLabel = ldata$itemLabel,
                            showItemLabels = showItemLabels,
                            parent = parent, ...)
        l_ispace_config(baseplot = baseplot,
                        ispace = zargs$ispace,
                        xlim = xlim, ylim = ylim)
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_loon, args)
    }
}

##' @title Density plot in 2d using the interactive loon package
##' @family default 2d plot functions using the interactive loon package
##' @family default 2d plot functions
##' @name density_2d_loon
##' @aliases density_2d_loon
##' @param zargs The argument list as passed from \code{\link{zenplot}()}
##' @param ngrids Number of grid points in each direction. Can be scalar or
##'        a length-2 integer vector.
##' @param ccol A vector (which is then recycled to the appropriate length)
##'        giving the color of the contours
##' @param color Colour used fill if ccol is NULL, a grey palette is used otherwise.
##' @param clwd A vector (which is then recycled to the appropriate length)
##'        giving the line widths of the contours
##' @param lwd Line width used only when clwd = NULL
##' @param linewidth Line width used when both  clwd and lwd are NULL, value of 1 used otherwise.
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are displayed
##' @param linkingGroup The initial linking group
##' @param baseplot If non-null the base plot on which the plot should be layered
##' @param parent The tk parent for this loon plot widget
##' @param group... A list of arguments passed to group_2d_loon (or NULL)
##' @param ... Additional parameters passed to loon::l_layer_line()
##' @return invisible()
##' @author Marius Hofert and Wayne Oldford
##' @export
density_2d_loon <- function(zargs, ngrids = 25,
                            ccol = NULL, color = NULL, clwd = NULL, lwd = NULL,
                            linewidth = 1, showLabels = FALSE,
                            showScales = FALSE, showGuides = FALSE,
                            linkingGroup = NULL,
                            baseplot = NULL, parent = NULL, group... = NULL, ...)
{
    r <- extract_2d(zargs)
    x <- as.matrix(r$x)
    y <- as.matrix(r$y)
    xlim <- r$xlim
    ylim <- r$ylim
    same.group <- r$same.group
    if(same.group) {

        ## Check for linkingGroup
        if (is.null(linkingGroup))
            linkingGroup <-  paste0("zenplot parent =", parent$ID)

        ## Remove NAs
        data <- na_omit_loon(x, y)

        ## TODO fix box or not: if(box) box() # plot the box
        ## Do the plot
        dens <- kde2d(data$x, data$y, n = ngrids)
        contours <- contourLines(dens$x, dens$y, dens$z)
        levels <- sapply(contours, function(contour) contour$level) # list of contour levels
        nLevels <- length(levels) # number of levels
        uniqueLevels <- unique(levels) # unique levels (there could be more than one level curve with the same level)
        nuLevels <- length(uniqueLevels)

        ## Sort out colours
        if(is.null(ccol)) {
            if(is.null(color)) {
                ## Use pallette of default grey scale colors
                basecol <- c("grey80", "grey0")
                palette <- colorRampPalette(basecol, space = "Lab")
                ccol <- palette(nuLevels) # different color for each 1d plot
            } else {
                ccol <- color
            }
        }
        ccol <- rep_len(ccol, nuLevels)
        ccol. <- numeric(nLevels)

        ## Sort out line widths
        if(is.null(clwd)) {
            if(is.null(lwd)) {
                clwd <- linewidth
            } else {
                clwd <- lwd
            }
        }
        clwd <- rep_len(clwd, nuLevels)
        clwd. <- numeric(nLevels)

        ## clty <- rep_len(clty, nuLevels) # could sort these out too using "dash"
        ## Match the levels in the unique levels
        ## clty. <- numeric(nLevels)

        ## Repeat as needed
        for (i in 1:nuLevels) {
            idx <- (1:nLevels)[levels == uniqueLevels [i]]
            ccol.[idx] <- ccol[i]
            clwd.[idx] <- clwd[i]
            ## clty.[idx] <- clty[i]
        }

        ## Set up the base plot if needed
        if (is.null(baseplot))
            baseplot <- loon::l_plot(showLabels = showLabels,
                               showScales = showScales,
                               showGuides = showGuides,
                               parent = parent)

        ## Define the contours
        lapply(1:length(contours), # go over all contours
               function(i){
            contour <- contours[[i]]
            loon::l_layer_line(baseplot,
                         x = contour$x,
                         y = contour$y,
                         color = ccol.[i],
                         linewidth = clwd.[i],
                         label = paste("density =",levels[i]),
                         ## lty = clty.[i],
                         ...)
        })

        ## Deal with ispace
        check_zargs(zargs, "ispace")
        l_ispace_config(baseplot = baseplot,
                        ispace = zargs$ispace,
                        xlim = xlim, ylim = ylim)

        ## Return
        baseplot

    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_loon, args)
    }
}

##' @title Axes arrows in 2d using the interactive loon package
##' @family default 2d plot functions using the interactive loon package
##' @family default 2d plot functions
##' @name axes_2d_loon
##' @aliases axes_2d_loon
##' @param zargs The argument list as passed from \code{\link{zenplot}()}
##' @param angle The angle of the arrow head
##' @param length The length of the arrow head
##' @param eps The distance by which the axes are moved away from the plot region
##' @param linkingGroup The initial linking group
##' @param color Colour used fill if ccol is NULL, a grey palette is used otherwise.
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are displayed
##' @param baseplot If non-null the base plot on which the plot should be layered
##' @param parent The tk parent for this loon plot widget
##' @param group... A list of arguments passed to group_2d_loon (or NULL)
##' @param ... Additional arguments passed to loon::l_plot()
##' @return the loon plot
##' @author Marius Hofert and Wayne Oldford
##' @note Inspired by https://stat.ethz.ch/pipermail/r-help/2004-October/059525.html
##' @export
axes_2d_loon <- function(zargs,
                         angle = 30, length = 0.05, eps = 0.02,
                         linkingGroup = NULL, color = NULL, showLabels = FALSE,
                         showScales = FALSE, showGuides = FALSE,
                         baseplot = NULL, parent = NULL,
                         group... = NULL, ...)
{
    r <- extract_2d(zargs)
    xlim <- r$xlim
    ylim <- r$ylim
    same.group <- r$same.group

    ## Check for linkingGroup
    if (is.null(linkingGroup))
        linkingGroup <-  paste0("zenplot parent =", parent$ID)

    ## Main
    if(same.group) {

        epsx <- eps * diff(xlim)
        epsy <- eps * diff(ylim)
        exrange <- xlim + epsx * c(-1, 1)
        eyrange <- ylim + epsy * c(-1, 1)

        ## Get the base plot if not supplied
        if(is.null(baseplot))
            baseplot <- loon::l_plot(showLabels = showLabels,
                               showScales = showScales,
                               showGuides = showGuides,
                               linkingGroup = linkingGroup,
                               parent = parent)
        if(is.null(color)) color <- baseplot['foreground']

        ## Draw the horizontal axis
        za <- zenarrow("r", length = length, angle = angle)
        maxza <- apply(za, 1, max)
        maxza[2] <- 0
        arrHead <- c(exrange[2], eyrange[1]) + za - maxza
        x_line <- loon::l_layer_line(widget = baseplot,
                               x = exrange, y = rep(eyrange[1],2),
                               label = "Horizontal axis line",
                               color = color, index = "end", ...)
        x_arrowhead <- loon::l_layer_line(widget = baseplot,
                                    x = arrHead[1,], y = arrHead[2,],
                                    label = "Horizontal axis arrowhead",
                                    color = color, index = "end", ...)

        ## First create the group layer
        x_arrow <- loon::l_layer_group(widget = baseplot,
                                 label = "Horizontal axis arrow",
                                 index = "end")

        ## Demote the two pieces into the x_arrow
        loon::l_layer_demote(baseplot, x_arrowhead)
        loon::l_layer_demote(baseplot, x_line)

        ## Draw the vertical axis
        za <- zenarrow("u", length = length, angle = angle)
        maxza <- apply(za, 1, max)
        maxza[1] <- 0
        arrHead <- c(exrange[1], eyrange[2]) + za - maxza
        y_line <- loon::l_layer_line(widget = baseplot,
                               x = rep(exrange[1],2), y = eyrange,
                               label = "Vertical axis line",
                               color = color,
                               index = "end", ...)
        y_arrowhead <- loon::l_layer_line(widget = baseplot,
                                    x = arrHead[1,], y = arrHead[2,],
                                    label = "Vertical axis arrowhead",
                                    color = color,
                                    index = "end", ...)
        ## First create the group layer
        y_arrow <- loon::l_layer_group(widget = baseplot,
                                 label = "Vertical axis arrow",
                                 index = "end")

        ## Demote the two pieces into the x_arrow
        loon::l_layer_demote(baseplot, y_arrowhead)
        loon::l_layer_demote(baseplot, y_line)

        ## All together
        axes <- loon::l_layer_group(widget = baseplot,
                              label = "Axis arrows",
                              index = "end")
        loon::l_layer_demote(baseplot, y_arrow)
        loon::l_layer_demote(baseplot, x_arrow)
        check_zargs(zargs, "ispace")
        l_ispace_config(baseplot = baseplot,
                        ispace = zargs$ispace,
                        xlim = xlim, ylim = ylim)

        ## Return
        baseplot

    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_loon, args)
    }
}

##' @title Arrow plot in 2d using the interactive loon package
##' @family default 2d plot functions using the interactive loon package
##' @family default 2d plot functions
##' @name arrow_2d_loon
##' @aliases arrow_2d_loon
##' @param zargs The argument list as passed from \code{\link{zenplot}()}
##' @param loc The (x,y) location of the center of the arrow
##' @param length The length of the arrow
##' @param angle The angle from the shaft to the edge of the arrow head
##' @param linkingGroup The initial linking group
##' @param color The color
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are displayed
##' @param baseplot If non-null the base plot on which the plot should be layered
##' @param parent The tk parent for this loon plot widget
##' @param group... A list of arguments passed to group_2d_loon (or NULL)
##' @param ... Additional parameters passed to loon::l_layer_line()
##' @return the plot (invisibly)
##' @author Marius Hofert and Wayne Oldford
##' @export
arrow_2d_loon <- function(zargs,
                          loc = rep(0.5, 2), length = 0.2, angle = 30,
                          linkingGroup = NULL, color = NULL,
                          showLabels = FALSE, showScales = FALSE,
                          showGuides = FALSE, baseplot = NULL, parent = NULL,
                          group... = NULL, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    turns <- zargs$turns
    num <- zargs$num
    if (is.null(linkingGroup))
        linkingGroup <-  paste0("zenplot parent =", parent$ID)
    if(same.group) {
        arr <- loc + zenarrow(turns[num], length = length, angle = angle)
        if(is.null(baseplot))
            baseplot <- loon::l_plot(showLabels = showLabels,
                               showScales = showScales,
                               showGuides = showGuides,
                               linkingGroup = linkingGroup,
                               parent = parent)
        if(is.null(color))
            color <- baseplot['foreground']
        loon::l_layer_line(widget = baseplot, x = arr[1,], y = arr[2,], color = color, ...)
        baseplot
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_loon, args)
    }
}

##' @title Rectangle plot in 2d using the interactive loon package
##' @family default 2d plot functions using the interactive loon package
##' @family default 2d plot functions
##' @name rect_2d_loon
##' @aliases rect_2d_loon
##' @param zargs The argument list as passed from \code{\link{zenplot}()}
##' @param loc.x x-location of rectangle
##' @param loc.y y-location of rectangle
##' @param color Colour of the rectangle outline
##' @param fill Colour of the rectangle interior
##' @param lwd line width for rectangle outline
##' @param linkingGroup The initial linking group (ignored)
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are displayed
##' @param baseplot If non-null the base plot on which the plot should be layered
##' @param parent The tk parent for this loon plot widget
##' @param group... A list of arguments passed to group_2d_loon (or NULL)
##' @param ... Additional parameters passed to loon::l_layer_text(...)
##' @return The base loon::l_plot with the added text layer
##' @author Marius Hofert and Wayne Oldford
##' @export
rect_2d_loon <- function(zargs, loc.x = NULL, loc.y = NULL, color = NULL,
                         fill = NULL, lwd = 1, linkingGroup = NULL,
                         showLabels = FALSE, showScales = FALSE,
                         showGuides = FALSE, baseplot = NULL,
                         parent = NULL, group... = NULL, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group

    if (is.null(linkingGroup))
        linkingGroup <-  paste0("zenplot parent =", parent$ID)

    res <- if(same.group) {
        if(is.null(baseplot))
            baseplot <- loon::l_plot(showLabels = showLabels,
                               showScales = showScales,
                               showGuides = showGuides,
                               linkingGroup = linkingGroup,
                               parent = parent)
        if(is.null(color)) color <- baseplot['foreground']
        if(is.null(fill)) fill <- baseplot['background']
        label <- paste("Rectangle:", colnames(r$x))
        if (is.null(loc.x)) loc.x <- 0:1
        if (is.null(loc.y)) loc.y <- 0:1

        loon::l_layer_rectangle(baseplot,
                          x = loc.x,
                          y = loc.y,
                          label = label,
                          color = fill,
                          linecolor = color,
                          linewidth = lwd,
                          ...)
        check_zargs(zargs, "ispace")
        l_ispace_config(baseplot = baseplot,
                        ispace = zargs$ispace)
        baseplot
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_loon, args)
    }
}

##' @title Label plot in 2d using the interactive loon package
##' @family default 2d plot functions using the interactive loon package
##' @family default 2d plot functions
##' @name label_2d_loon
##' @aliases label_2d_loon
##' @param zargs The argument list as passed from \code{\link{zenplot}()}
##' @param loc The location of the label
##' @param label The label to be used
##' @param rot The rotation of the label
##' @param size The font size
##' @param box A \code{\link{logical}} indicating whether the label is to be enclosed
##'        in a box.
##' @param color Color of the label (and of box when \code{box = TRUE}).
##' @param linkingGroup The initial linking group
##' @param showLabels Logical determining whether axis labels are displayed
##' @param showScales Logical determining whether scales are displayed
##' @param showGuides Logical determining whether the background guidelines are displayed
##' @param baseplot If non-null the base plot on which the plot should be layered
##' @param parent The tk parent for this loon plot widget
##' @param group... A list of arguments passed to group_2d_loon (or NULL)
##' @param ... Additional parameters passed to loon::l_layer_text(...)
##' @return The base loon::l_plot with the added text layer
##' @author Marius Hofert and Wayne Oldford
##' @export
label_2d_loon <- function(zargs,
                          loc = NULL, label = NULL, rot = 0, size = 8,
                          box = FALSE, color = NULL,
                          linkingGroup = NULL, showLabels = FALSE,
                          showScales = FALSE, showGuides = FALSE,
                          baseplot = NULL, parent = NULL,
                          group... = NULL, ...)
{
    r <- extract_2d(zargs)
    same.group <- r$same.group
    vlabs <- r$vlabs
    vars <- zargs$vars
    num <- zargs$num
    ## Check for linkingGroup
    if (is.null(linkingGroup))
        linkingGroup <-  paste0("zenplot parent =", parent$ID)
    if(same.group) {
        ## TODO fix box or not: if(box) box() # plot the box
        ## Do the plot
        if(is.null(baseplot))
            baseplot <- loon::l_plot(showLabels = showLabels,
                               showScales = showScales,
                               showGuides = showGuides,
                               linkingGroup = linkingGroup,
                               parent = parent)
        xlab <- vlabs[vars[num, 1]]
        ylab <- vlabs[vars[num, 2]]
        if(is.null(color)) color <- baseplot['foreground']
        if(is.null(label)) label <- paste0("(",xlab,", ",ylab,")")
        if (is.null(loc)) loc <- c(0.50, 0.25)

        loon::l_layer_text(baseplot, text = label,
                     x = loc[1], y = loc[2], angle = rot, size = size,
                     color = color, ...)
        if (box) {
            rect_2d_loon(zargs,
                         color = color,
                         baseplot = baseplot, parent = parent,
                         index="end", ...)

        }
        check_zargs(zargs, "ispace")
        l_ispace_config(baseplot = baseplot,
                        ispace = zargs$ispace,
                        xlim = c(0,1), ylim = c(0,1))
        baseplot
    } else {
        args <- c(list(zargs = zargs), group...)
        do.call(group_2d_loon, args)
    }
}

##' @title Layout plot in 2d using the interactive loon package
##' @param zargs The argument list as passed from \code{\link{zenplot}()}
##' @param ... Additional arguments passed to label_2d_grid()
##' @return A loon plot
##' @author Marius Hofert and Wayne Oldford
##' @note Here we also pass '...' to group_2d_loon() (to easily adjust
##'       font size etc.)
layout_2d_loon <- function(zargs, ...)
    label_2d_loon(zargs, loc = c(0.5, 0.5),
                  box = TRUE, group... = list(...), ...)

