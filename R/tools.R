### Graphical tools ############################################################

##' @title Defining an arrow
##' @param turn The direction in which the arrow shall point ("l", "r", "d", "u")
##' @param length The length of the arrow in [0,1] from tip to base
##' @param angle The angle
##' @return A 3-column matrix containing the (x,y) coordinates of the left
##'         edge end point, the arrow head and the right edge end point
##' @author Marius Hofert
zenarrow <- function(turn, angle = 80, length = 1, coord.scale = 1)
{
    stopifnot(0 <= angle, angle <= 180)
    th <- angle * pi / 180 # convert from angle to radians
    ## Determine head and two edges (center = (0,0)) of an arrow pointing 'right'
    ## Arrow head
    head <- c(length * 0.5, 0) # arrow head
    ## Left edge (in direction of the arrow)
    th2 <- th/2 # half the angle
    left <- c(length * (-0.5), coord.scale * length * tan(th2)) # end point of left edge of the arrow head
    ## => first component ('width') is 1 * length; the unit of the second
    ##    component is the same as the first (Cartesian coordinate system)
    ## Right edge (in direction of the arrow)
    right <- c(left[1], -left[2]) # end point of right edge of the arrow head
    ## Now turn the base arrow appropriately
    rot <- switch(turn,
                  "l" = {   pi },
                  "r" = {    0 },
                  "d" = { 3*pi/2 },
                  "u" = {   pi/2 },
                  stop("Wrong 'turn'"))
    rot.mat <- matrix(c(cos(rot), -sin(rot), sin(rot), cos(rot)),
                      nrow = 2, ncol = 2, byrow = TRUE)
    left <- rot.mat %*% left
    right <- rot.mat %*% right
    head <- rot.mat %*% head
    ## Return
    cbind(left = left, head = head, right = right) # (2, 3)-matrix
}

##' @title Function to set up plot region for graphics plots
##' @param xlim x-axis limits
##' @param ylim y-axis limits
##' @param plot... arguments passed to the underlying 'plot()'
##' @return invisible()
##' @author Marius Hofert
plot_region <- function(xlim, ylim, plot... = NULL)
{
    if(is.null(plot...)) {
        plot(NA, xlim = xlim, ylim = ylim, type = "n", ann = FALSE, axes = FALSE, log = "")
    } else {
        fun <- function(...) plot(NA, xlim = xlim, ylim = ylim, ...)
        do.call(fun, plot...)
    }
}

##' @title Constructing the viewports for grid functions
##' @param ispace inner space (in [0,1])
##' @param xlim x-axis limits; if NULL, the data limits are used
##' @param ylim y-axis limits; if NULL, the data limits are used
##' @param x x data (only used if is.null(xlim)); if NULL, 0:1 is used
##' @param y y data (only used if is.null(ylim)); if NULL, 0:1 is used
##' @param ... Additional arguments passed to the underlying viewport()
##' @return The viewport
##' @author Marius Hofert
##' @note Ideas from dataViewport() and extendrange()
##'       Omitted check:
##'       if(length(ispace) != 4) ispace <- rep(ispace, length.out = 4)
##'       stopifnot(0 <= ispace, ispace <= 1)
vport <- function(ispace, xlim = NULL, ylim = NULL, x = NULL, y = NULL, ...)
{
  if(is.null(xlim) && is.null(ylim) && is.null(x) && is.null(y)) {
    ## Non-data viewport
    viewport(x = unit(ispace[2], "npc"),
             y = unit(ispace[1], "npc"),
             just = c("left", "bottom"),
             width  = unit(1-sum(ispace[c(2,4)]), "npc"),
             height = unit(1-sum(ispace[c(1,3)]), "npc"))
  } else {
    ## Data viewport
    ran.x <- if(is.null(xlim)) {
      if(is.null(x)) x <- 0:1
      range(x, na.rm = TRUE)
    } else xlim
    ran.y <- if(is.null(ylim)) {
      if(is.null(y)) y <- 0:1
      range(y, na.rm = TRUE)
    } else ylim
    viewport(xscale = ran.x + c(-ispace[2], ispace[4]) * diff(ran.x),
             yscale = ran.y + c(-ispace[1], ispace[3]) * diff(ran.y), ...)
  }
}

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
##' @param itemLabel The vector of labels for the points,
##'        of the same length as x; if NULL it will be constructed.
##' @return A list(x, y, linkingKey, itemLabel) where any NA in x or y will
##'         have been omitted from all
##' @author R. W. Oldford
na_omit_loon <- function(x, y = NULL, linkingKey = NULL, itemLabel = NULL)
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
    ## Fix up itemLabel
    if(is.null(itemLabel)) {
        itemLabel  <- rownames(x)
        if(is.null(itemLabel)) {
            itemLabel <- sapply(linkingKey, function (key) paste0("point", key))
        } else itemLabel <- itemLabel[notNA]
    }
    ## Return
    list(x = x, # if (is.null(x)) NULL else list(x),
         y = y, # if (is.null(y)) NULL else list(y),
         linkingKey = linkingKey, itemLabel = itemLabel)
}

##' @title Configuring a loon plot to accommodate ispace
##' @param baseplot The plot to be modified
##' @param ispace The inner space (in [0,1])
##' @param x The x data
##' @param y The y data
##' @param xlim The x-axis limits; if NULL, the data limits are used
##' @param ylim The y-axis limits; if NULL, the data limits are used
##' @param ... Additional arguments passed to loon::l_configure
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
    loon::l_configure(baseplot,
                panX = xLeft, panY = yBottom,
                zoomX = baseplot['deltaX']/ deltaXrange,
                zoomY = baseplot['deltaY']/ deltaYrange, ...)
    ## Return
    baseplot
}


### Technical tools ############################################################

##' @title Converting an Occupancy Matrix
##' @param x an occupancy matrix
##' @param to symbols being mapped to by the occupancy matrix
##' @return matrix of encoded entries of the occupancy matrix
##' @author Marius Hofert
convert_occupancy <- function(x, to = c("", "<", ">", "v", "^"))
{
    stopifnot(x %in% c("", "l", "r", "d", "u"), length(to) == 5)
    new.vals <- to[ match(x, table = c("", "l", "r", "d", "u")) ]
    if(is.matrix(x)) {
        matrix(new.vals, ncol = ncol(x))
    } else {
        new.vals
    }
}

##' @title Auxiliary Function for Constructing Default n2dcols
##' @param n2dplots The number of variates (= nfaces)
##' @param method One of "letter", "square", "A4", "golden", "legal"
##' @return An odd integer for n2dcols
##' @author Wayne Oldford
n2dcols_aux <- function(n2dplots, method = c("letter", "square", "A4", "golden", "legal"))
{
    method <- match.arg(method)
    scaling <- switch(method,
                      "golden" = (1 + sqrt(5))/2,
                      "square" = 1,
                      "letter" = 11/8.5,
                      "legal" = 14/8.5,
                      "A4" = sqrt(2),
                      stop("Wrong 'method'"))
    n2dcols <- max(3,
                   ## - n2dcol is never less than 3
                   ## - n2dplots = (ncols - 1) * nrows (one column is essentially missing
                   ##   due to missing plots at alternating sides at the left/right border)
                   round(0.5 * (1 + sqrt( 1 + 4 * n2dplots / scaling)))
                   ## This used to be (but no justification known):
                   ## round(0.5 * (1 + sqrt( 1 + 4 * (n2dplots - 1) / scaling)))
                   )
    if ((n2dcols %% 2) == 0) n2dcols + 1 else n2dcols # the default should be odd
}

##' @title Check the Turns (Number/Type)
##' @param turns The turns
##' @param n2dplots The number of 2d plots
##' @param first1d A logical indicating whether the first 1d plot should be plotted
##' @param last1d A logical indicating whether the last 1d plot should be plotted
##' @return TRUE (unless it fails)
##' @author Marius Hofert
turn_checker <- function(turns, n2dplots, first1d, last1d)
{
    ## Check the type of the turns
    if(!is.character(turns) || !all(turns %in% c("d", "u", "r", "l")))
        stop("'turns' not all in 'd', 'u', 'r' or 'l'")

    ## Check the length of the turns
    nturns <- length(turns)
    l <- as.numeric(!first1d) + as.numeric(!last1d) # 0 (first1d = last1d = TRUE), 1 (precisely one FALSE) or 2 (first1d = last1d = FALSE)
    if(nturns != 2 * n2dplots - l + 1)
        stop("Number of turns (= ",nturns,") is unequal to 2 * 'n2dplots' - ",l," + 1 = ",2*n2dplots-l+1)
    TRUE
}

##' @title Check Argument for Being a Vector, Matrix, Data Frame or a List of such
##' @param x A vector, matrix, data.frame or list of such
##' @return A logical indicating whether x is of the above type
##' @author Marius Hofert
is.standard <- function(x) {
    if(!is.vector(x, mode = "list")) { # has to be a vector, matrix or data.frame
        is.vector(x) || is.matrix(x) || is.data.frame(x)
    } else { # recursion
        all(vapply(x, is.standard, NA))
    }
}

##' @title Determine the number of columns if is.standard(x)
##' @param x A numeric vector, matrix, data.frame or a list of such.
##' @return The number of data columns of 'x'
##' @author Marius Hofert
num_cols <- function(x)
{
    if(!is.standard(x))
        stop("'x' must be a vector, matrix, data.frame, or a list of such.")
    if(is.vector(x, mode = "list")) {
        sum(sapply(x, num_cols))
    } else { # 'x' must be a vector, matrix or data.frame
        if(is.vector(x) || is.data.frame(x)) x <- as.matrix(x)
        ncol(x)
    }
}


### Tools for constructing your own plot1d and plot2d functions ################

##' @title Determining the indices of the x and y variables of the current plot
##' @param zargs The argument list as passed from zenplot().
##' @return A vector of length two giving the plot variables (integers)
##' @author Marius Hofert
##' @note This is exported so that one doesn't always have to figure
##'       out whether the variables (axes) in the current plot need
##'       to be switched manually.
plot_indices <- function(zargs)
    zargs$vars[zargs$num,] # access 2-column matrix of plot variables at current plot number

##' @title Auxiliary function for burst()
##' @param x A vector, matrix or data.frame (or a (pure) list, but that we don't use here)
##' @param labs The variable labels:
##'        - if NULL, no labels are used
##'        - if of length 1, use this label and append 1:ncol(x)
##'          but only if x doesn't have any column names (otherwise use the latter)
##'        - if of length ncol(x), use that
##'          but only if x doesn't have any column names (otherwise use the latter)
##' @return 'x' as a list of named columns
##' @author Marius Hofert
##' @note - Performance critical (no checks here)
##'       - Data frames always have default names. They are possibly
##'         ugly but we have to use them here as we cannot
##'         determine whether they were assigned automatically or
##'         on purpose.
burst_aux <- function(x, labs = "V")
{
    ## Construct labels
    if(is.vector(x)) x <- as.matrix(x)
    nc <- ncol(x)
    if(is.null(labs)) {
        labs. <- rep("", nc) # no names
    } else {
        labs. <- colnames(x)
        if(is.null(labs.)) { # ... then construct the labels
            labs. <- if(length(labs) == 1) {
                paste0(labs, seq_len(nc)) # construct labels
            } else labs # must be of the right length then
        }
    }

    ## Split 'x' and use correct labels
    x <- if(is.matrix(x)) {
        .Call(col_split, x)
    } else { # works for data.frame (= lists of columns; and (general) lists)
        unclass(x)
    }
    names(x) <- labs. # put labels in
    x # return
}

##' @title Auxiliary function for checking and converting the data argument of zenplot()
##' @param x A numeric vector, matrix, data.frame or a list of such
##' @param labs A list with components
##'        'group': the label names for group labels (or NULL for no group labels)
##'        'var': the variable labels (or NULL for no variable labels)
##'        'sep': the separator between group and variable labels
##'        'group2d': a logical indicating whether labels of group_2d_*() plots
##'                   are affected by 'group = NULL' (or still printed)
##'        If labs = NULL, neither group nor variable labels are used
##' @return A list with components
##'         'xcols': a list containing the column vectors of x
##'         'groups': the group number for each column of x
##'         'vars': the variable number (within each group) for each column of x
##'         'glabs': the group label for each column of x
##'         'labs': the group and variable labels for each column of x
##' @author Marius Hofert
##' @note Performance critical
burst <- function(x, labs = list())
{
    ## Checks
    if(!is.standard(x))
        stop("'x' must be a vector, matrix, data.frame, or a list of such.\nIf not, consider writing your own 'plot1d' and 'plot2d'.")
    if(!is.null(labs)) {
        ## With this construction, the user gets the following defaults
        ## even when (s)he only specifies less than the three components
        nms <- names(labs)
        if(all(is.na(pmatch("group", table = nms, duplicates.ok = TRUE))))
            labs$group <- "G"
        if(all(is.na(pmatch("var", table = nms, duplicates.ok = TRUE))))
            labs$var <- "V"
        if(all(is.na(pmatch("sep", table = nms, duplicates.ok = TRUE))))
            labs$sep <- ", "
        if(all(is.na(pmatch("group2d", table = nms, duplicates.ok = TRUE))))
            labs$group2d <- FALSE
    }

    ## Distinguish the cases
    if(is.vector(x, mode = "list")) { # proper list (and not a data.frame)

        ngrps <- length(x) # number of groups (=number of sublists)
        if(ngrps == 0) stop("'x' has to have positive length.")

        ## Burst all groups
        x. <- x # dummy for calling burst_aux() on
        names(x.) <- NULL # remove names for burst as these group names show up in variable names otherwise
        col.lst <- lapply(x., burst_aux, labs = if(is.null(labs)) NULL else labs[["var"]])
        xcols <- unlist(col.lst, recursive = FALSE) # x as a list of columns
        gsizes <- vapply(col.lst, length, NA_real_) # group sizes
        groups <- rep(1:ngrps, times = gsizes) # group numbers
        vars <- unlist(lapply(gsizes, seq_len), use.names = FALSE) # variable numbers
        vlabs <- if(is.null(labs)) NULL else names(xcols) # variable labels; unlist(lapply(col.lst, FUN = names), use.names = FALSE)

        ## Build group labels
        ## - If is.null(labs[["group"]]) and labs[["group2d"]] = TRUE, omit group labels
        ## - If no group labels are given at all, construct them;
        ## - If some group labels are given, use (only) them and omit the others
        glabs <- if(is.null(labs)) {
            NULL
        } else if(is.null(labs[["group"]]) && labs[["group2d"]]) {
            r <- rep("", ngrps)
            r[groups] # expand
        } else {
            nms <- names(x) # use the names of 'x' as group labs, or, if NULL, build them
            r <- if(is.null(nms)) {
                labs.group <- labs[["group"]]
                if(length(labs.group) == 1) { # if of length 1, append number
                    paste0(labs.group, 1:ngrps)
                } else { # otherwise, use that
                    ## stopifnot(length(labs.group) == ngrps)
                    labs.group
                }
            } else nms
            r[groups] # expand
        }

        ## Build joint labels used for 'x' only (unless is.null(labs))
        if(!is.null(labs)) {
            is.null.labs.group <- is.null(labs[["group"]])
            is.null.labs.var <- is.null(labs[["var"]])
            labs <- if(is.null.labs.group && is.null.labs.var) {
                character(length(xcols)) # no labels
            } else { # if at least one is given (not both NULL)
                if(is.null.labs.group) { # only use var labels
                    vlabs
                } else if(is.null.labs.var) { # only use group labels
                    glabs
                } else { # use both labels
                    trimws(paste(glabs, vlabs, sep = labs[["sep"]]))
                }
            }
            names(xcols) <- labs # use these group and variable labels as labels of the columns
        }

    } else { ## if(is.vector(x) || is.matrix(x) || is.data.frame(x)) {

        xcols <- burst_aux(x, labs = if(is.null(labs)) NULL else labs[["var"]]) # columns with names
        l <- length(xcols)
        groups <- rep(1, l)
        vars <- seq_len(l)
        glabs <- NULL
        vlabs <- names(xcols)

    } ## else stop("Wrong 'x'.")

    ## Return result
    list(xcols = xcols, # list of columns (with 'full labels' = group and variable labels)
         groups = groups, # group numbers
         vars = vars, # variable numbers
         glabs = glabs, # group labels (NULL unless 'x' is a list)
         vlabs = vlabs) # variable labels
}

##' @title A list of columns
##' @param x A list of columns
##' @return A list where each column is converted to data (range() works,
##'         can be plotted, etc.)
##' @author Marius Hofert
##' @note See plot.default -> xy.coords()
as_numeric <- function(x)
    lapply(x, function(x.) {
        if (is.language(x.)) {
            if (inherits(x., "formula") && length(x.) == 3) {
                x. <- eval(x.[[2L]], environment(x.))
            } else stop("Invalid first argument.")
        } else if (is.matrix(x.) || is.data.frame(x.)) {
            x. <- data.matrix(x.)[,1]
        } else {
            if (is.factor(x.)) x. <- as.numeric(x.)
        }
        if (inherits(x., "POSIXt")) x. <- as.POSIXct(x.)
        as.double(x.)
    })

##' @title Checking whether certain arguments appear in zargs
##' @param zargs The argument list as passed from zenplot()
##' @param ... The arguments to check zargs for
##' @return A logical indicating whether some arguments are missing in zargs
##' @author Marius Hofert
check_zargs <- function(zargs, ...)
{
    args <- list(...)
    miss <- args[which(!(args %in% names(zargs)))]
    missSome <- length(miss) > 0
    if(missSome)
        stop("Missing arguments ",paste(sQuote(miss), collapse = ", "),
             ". Consider providing your own functions.")
    missSome
}

##' @title Extracting information for our default/provided plot1d()
##' @param zargs The argument list as passed from zenplot(); this must at least
##'        contain 'x', 'orientations', 'vars', 'num', 'lim' and 'labs'.
##' @return A list with
##'         - the data x to plot in the 1d plot;
##'         - a list with all columns of x;
##'         - the group numbers (for each column of x);
##'         - the variable numbers (for each column of x);
##'         - the group labels (for each column of x);
##'         - the variable labels (for each column of x);
##'         - horizontal (logical);
##'         - the (x-)axis limits
##' @author Marius Hofert
##' @note Performance critical
extract_1d <- function(zargs)
{
    ## Checks
    check_zargs(zargs, "x", "orientations", "vars", "num", "lim", "labs")

    ## Extract quantities
    x <- zargs$x
    orientations <- zargs$orientations
    vars <- zargs$vars
    lim <- zargs$lim
    labs <- zargs$labs
    num <- zargs$num

    ## Burst x and cache result
    if(!exists("burst.x", envir = .zenplots_burst_envir) || num == 1) {
        xburst <- burst(x, labs = labs) # burst 'x' (=> xcols (with glabs + vlabs), groups, vars, glabs, vlabs)...
        assign("burst.x", xburst, envir = .zenplots_burst_envir) # ... and cache it
    } else {
        xburst <- get("burst.x", envir = .zenplots_burst_envir) # get it...
         if(num == nrow(vars)) rm("burst.x", envir = .zenplots_burst_envir) # ... but remove it again after the last plot
        ## Note: This rm() is only for the case when extract_*d() is called separately.
        ##       In general, 'burst.x' is removed from .zenplots_burst_envir in zenplot()
        ##       as extract_*d() might not be called on last plot (e.g., if plot1d is
        ##       user-provided and does not call extract_*d() or if last1d = FALSE etc.)
    }

    ## Pick out the plot variable index
    ix <- vars[num,1] # index of x

    ## Pick out the data
    xcols <- xburst$xcols
    xcols. <- as_numeric(xcols) # possibly transform all columns so that range() etc. works
    x. <- data.frame(xcols.[[ix]]) # (possibly transformed) data x to be plotted
    ## => Conversion to data.frame allows for column names to be passed through
    names(x.) <- names(xcols[ix])

    ## Determine whether the plot is horizontal
    horizontal <- orientations[num] == "h"

    ## Determine xlim, ylim
    lim.method <- if(is.numeric(lim) && length(lim) == 2) "fixed" else lim
    finite_range <- function(z) range(z[sapply(z, is.finite)]) # see https://stackoverflow.com/questions/8173094/how-to-check-a-data-frame-for-any-non-finite
    switch(lim.method,
    "fixed" = {
        xlim <- lim
    },
    "individual" = {
        xlim <- if(all(is.na(x.))) 0:1 else finite_range(x.) # adapted from plot.default()
    },
    "groupwise" = {
        if(is.list(x) && !is.data.frame(x)) { # multiple groups
            x.. <- unlist(xcols.[xburst$groups == xburst$groups[ix]]) # all x's belonging to the current group
            xlim <- if(all(is.na(x..))) 0:1 else finite_range(x..)
        } else { # no groups = only one group = global
            ax <- vars[,1] # all x indices
            axd <- unlist(xcols.[ax]) # all x data; need the unlisted version here for is.finite()
            xlim <- if(all(is.na(axd))) 0:1 else finite_range(axd)
        }
    },
    "global" = {
        ax <- vars[,1] # all x indices
        axd <- unlist(xcols.[ax]) # all x's; need the unlisted version here for is.finite()
        xlim <- if(all(is.na(axd))) 0:1 else finite_range(axd)
    },
    stop("Wrong 'lim.method'."))

    ## Return
    c(x = list(x.), # this way we keep the column labels
      xburst, # components returned by burst(): xcols (with glabs + vlabs), groups, vars, glabs, vlabs
      horizontal = list(horizontal),
      list(xlim = xlim))
}

##' @title Extracting information for our default/provided plot2d()
##' @param zargs The argument list as passed from zenplot(); this must at least
##'        contain 'x', 'vars', 'num', 'lim' and 'labs'.
##' @return A list with
##'         - the data x and y to plot in the 2d plot;
##'         - a list with all columns of x;
##'         - the group numbers (for each column of x);
##'         - the variable numbers (for each column of x);
##'         - the group labels (for each column of x);
##'         - the variable labels (for each column of x);
##'         - the x- and y-axis limits;
##'         - a logical indicating whether x and y are in the same group
##' @author Marius Hofert
##' @note Performance critical
extract_2d <- function(zargs)
{
    ## Checks
    check_zargs(zargs, "x", "vars", "num", "lim", "labs")

    ## Extract quantities (all but num; see below)
    x <- zargs$x
    vars <- zargs$vars
    lim <- zargs$lim
    labs <- zargs$labs
    num <- zargs$num

    ## Burst x and cache result
    if(!exists("burst.x", envir = .zenplots_burst_envir) || num == 1) {
        xburst <- burst(x, labs = labs) # burst 'x' (=> xcols (with glabs + vlabs), groups, vars, glabs, vlabs)...
        assign("burst.x", xburst, envir = .zenplots_burst_envir) # ... and cache it
    } else {
        xburst <- get("burst.x", envir = .zenplots_burst_envir) # get it...
        if(num == nrow(vars)) rm("burst.x", envir = .zenplots_burst_envir) # ... but remove it again after the last plot
        ## Note: This rm() is only for the case when extract_*d() is called separately.
        ##       In general, 'burst.x' is removed from .zenplots_burst_envir in zenplot()
        ##       as extract_*d() might not be called on last plot (e.g., if plot1d is
        ##       user-provided and does not call extract_*d() or if last1d = FALSE etc.)
    }

    ## Pick out the plot variable indices
    ix <- vars[num,1] # index of x
    iy <- vars[num,2] # index of y

    ## Pick out the data
    xcols <- xburst$xcols
    xcols. <- as_numeric(xcols) # possibly transform all columns so that range() etc. works
    x. <- data.frame(xcols.[[ix]]) # (possibly transformed) data x to be plotted
    y. <- data.frame(xcols.[[iy]]) # (possibly transformed) data y to be plotted
    ## => Conversion to matrix allows for column names
    names(x.) <- names(xcols[ix])
    names(y.) <- names(xcols[iy])

    ## Determine whether they are in the same group and compute xlim, ylim
    same.group <- xburst$groups[ix] == xburst$groups[iy]
    lim.method <- if(is.numeric(lim) && length(lim) == 2) "fixed" else lim
    finite_range <- function(z) range(z[sapply(z, is.finite)]) # see https://stackoverflow.com/questions/8173094/how-to-check-a-data-frame-for-any-non-finite
    switch(lim.method,
    "fixed" = {
        xlim <- lim
        ylim <- lim
    },
    "individual" = {
        xlim <- if(all(is.na(x.))) 0:1 else finite_range(x.) # adapted from plot.default()
        ylim <- if(all(is.na(y.))) 0:1 else finite_range(y.)
    },
    "groupwise" = {
        if(is.list(x) && !is.data.frame(x)) { # multiple groups
            x.. <- unlist(xcols.[xburst$groups == xburst$groups[ix]]) # all x's belonging to the current group
            xlim <- if(all(is.na(x..))) 0:1 else range(x..)
            y.. <- unlist(xcols.[xburst$groups == xburst$groups[iy]])
            ylim <- if(all(is.na(y..))) 0:1 else range(y..)
        } else { # no groups = only one group = global
            ax <- vars[,1] # all x indices
            ay <- vars[,2] # all y indices
            axd <- unlist(xcols.[ax]) # all x data; need the unlisted version here for is.finite()
            xlim <- if(all(is.na(axd))) 0:1 else range(axd)
            ayd <- unlist(xcols.[ay]) # all y data; need the unlisted version here for is.finite()
            ylim <- if(all(is.na(ayd))) 0:1 else range(ayd)
        }
    },
    "global" = {
        ax <- vars[,1] # all x indices
        ay <- vars[,2] # all y indices
        axd <- unlist(xcols.[ax]) # all x's; need the unlisted version here for is.finite()
        xlim <- if(all(is.na(axd))) 0:1 else range(axd)
        ayd <- unlist(xcols.[ay]) # all y's; need the unlisted version here for is.finite()
        ylim <- if(all(is.na(ayd))) 0:1 else range(ayd)
    },
    stop("Wrong 'lim.method'."))

    ## Return
    c(x = list(x.), y = list(y.), # (numeric!) data (possibly converted); list() to keep the column labels
      xburst, # components returned by burst(): xcols (original entries; with glabs + vlabs), groups, vars, glabs, vlabs
      list(xlim = xlim, ylim = ylim, same.group = same.group))
}
