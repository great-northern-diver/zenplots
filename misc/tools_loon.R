## Tools for *_*_loon functions


## Functions to check whether a plot is a plot in one (histogram) or two variables
## These functions should actually be in 'loon'
l_is_plot <- function (plot) grepl("plot", plot)
l_is_hist <- function (plot) grepl("hist", plot)

##' @title Helper function to remove NAs for loon plots
##' @param x The vector of x values (required)
##' @param y The vector of y values (optional) of the same length as x;
##'        if NULL then it's ignored.
##' @param linkingKey The vector of keys used to define links between points,
##'        of the same length as x; if NULL it will be 0:(length(x)-1).
##' @param itemlabel The vector of labels for the points,
##'        of the same length as x; if NULL it will be constructed.
##' @return A list(x, y, linkingKey, itemlabel) where any NA in x or y will
##'         have been omitted from all
##' @author R. W. Oldford
na_omit_loon <- function(x, y = NULL, linkingKey = NULL, itemlabel = NULL)
{
    if (missing(x)) stop("x must be provided")
    ## Check for linkingKey
    stopifnot(length(x) > 0)
    if (is.null(linkingKey)) linkingKey <- 0:(length(x)-1) # default 0:(n-1)
    ## Check NA
    notNA <- !is.na(x)
    if (!is.null(y)) {
        notNA <- notNA & !is.na(y)
        y <- y[notNA,,drop = FALSE]
    }
    x <- x[notNA,,drop = FALSE]
    linkingKey <- linkingKey[notNA]
    ## Fix up itemlabel
    if(is.null(itemlabel)) {
        itemlabel  <- rownames(x)
        if(is.null(itemlabel)) {
            itemlabel <- sapply(linkingKey, function (key) paste0("point", key))
        } else itemlabel <- itemlabel[notNA]
    }
    ## Return
    list(x = x, # if (is.null(x)) NULL else list(x),
         y = y, # if (is.null(y)) NULL else list(y),
         linkingKey = linkingKey, itemlabel = itemlabel)
}

##' @title Configuring a loon plot to accommodate ispace
##' @param baseplot The plot to be modified
##' @param ispace The inner space (in [0,1])
##' @param x The x data
##' @param y The y data
##' @param xlim The x-axis limits; if NULL, the data limits are used
##' @param ylim The y-axis limits; if NULL, the data limits are used
##' @param ... Additional arguments passed to l_configure
##' @return The baseplot
##' @author R. W. Oldford
l_ispace_config <- function(baseplot, ispace = NULL,
                            x = NULL, y = NULL, xlim = NULL, ylim = NULL, ...)
{
    if (is.null(ispace)) ispace <- rep(0.2, 4)
    if(length(ispace) != 4) ispace <- rep(ispace, length.out = 4)
    stopifnot(0 <= ispace, ispace <= 1)
    ## x values
    if (is.null(x)) x <- baseplot['x']
    xrange <- if(is.null(xlim)) {
        if (is.null(x)| length(x) == 0) x <- 0:1
        range(x, na.rm = TRUE)
    } else xlim
    if (diff(xrange) == 0) {
        xrange <- xrange + c(-1,1)
    }
    deltaXrange <- diff(xrange) * (1 + sum(ispace[c(2,4)]))
    xLeft <- xrange[1] - ispace[2] * diff(xrange)
    ## y values
    if (l_is_plot(baseplot)) {
        if (is.null(y)) y <- baseplot['y']
        yrange <- if(is.null(ylim)) {
            if (is.null(y)| length(y) == 0) y <- 0:1
            range(y, na.rm = TRUE)
        } else ylim
    } else
        yrange <- c(baseplot['panY'],
                    baseplot['panY'] + baseplot['deltaY']/baseplot['zoomY'])
    if (diff(yrange) == 0) yrange <- yrange + c(-1,1)
    deltaYrange <- diff(yrange) * (1 + sum(ispace[c(1,3)]))
    yBottom <- yrange[1] - ispace[1] * diff(yrange)
    ## The configuration
    l_configure(baseplot,
                panX = xLeft, panY = yBottom,
                zoomX = baseplot['deltaX']/ deltaXrange,
                zoomY = baseplot['deltaY']/ deltaYrange, ...)
    ## Return
    baseplot
}
