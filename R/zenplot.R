## Unfolding and zenplots


##' @title Unfold the hypercube and produce all information concerning the zenpath
##'        and zenplot layout
##' @param nfaces The number of faces of the hypercube to unfold
##' @param turns A vector of turns (in "u", "d", "l", "r"); contructed if NULL
##' @param n2dcols The number of columns of 2d plots (>= 1) or one of "letter", "square",
##'        "A4", "golden", "legal". Note that n2dcols is ignored if turns is not NULL.
##' @param method The method according to which the path is built; ignored if
##'        turns is not NULL
##' @param first1d A logical indicating whether the first 1d plot should be plotted
##' @param last1d A logical indicating whether the last 1d plot should be plotted
##' @param width1d The width of 1d plots
##' @param width2d The width of 2d plots
##' @return A list containing the path (turns, positions, occupancy matrix)
##'         and the layout (...)
##' @author Marius Hofert and Wayne Oldford
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

##' @title Main function to create a zenplot (possibly a grob), the path and the layout
##' @param x A data object (typically a vector, matrix, data.frame or a list of such
##'        ("standard form"), but can be anything).
##' @param turns A vector of turns (in "u", "d", "l", "r"); if NULL, the turns will be
##'        constructed (at least if x is of standard form).
##' @param first1d A logical indicating whether the first 1d plot should be plotted.
##' @param last1d A logical indicating whether the last 1d plot should be plotted.
##' @param n2dcols The number of columns of 2d plots (>= 1) or one of "letter", "square",
##'        "A4", "golden", "legal". Note that n2dcols is ignored if !is.null(turns).
##' @param n2dplots The number of 2d plots.
##' @param plot1d The function used to create the 1d (edge) plots (omitted if NULL)
##' @param plot2d The function used to create the 2d (face) plots (omitted if NULL)
##' @param zargs A fully named logical vector indicating whether the respective
##'        arguments are passed to plot1d() and plot2d() via the argument 'zargs'.
##'        zargs can maximally contain all variables as given in the default.
##'        If one of those variables does not appear in zargs, it is treated as TRUE
##'        and the corresponding arguments are passed on. If one of them is set to FALSE,
##'        the argument is not passed on.
##' @param lim The plot xlim and ylim (a character string or numeric(2)).
##' @param labs The plot labels to be used; typically as given in the default, but
##'        (again) can be anything as long as plot1d() and plot2d() know how to
##'        deal with it.
##' @param pkg The R package used for plotting.
##' @param method The method according to which the path is built; ignored if
##'        turns is given.
##' @param width1d The width of the 1d plots.
##' @param width2d The width of the 2d plots.
##' @param ospace The outer space around the zenplot.   A vector
##'        of length 4 (bottom, left, top, right) or repeated to be as such.
##'        Values should be in [0,1] when pkg is graphics or grid,
##'        and as number of pixels when pkg is loon.
##' @param ispace The inner space in [0,1] between the figure regions and the (1d/2d)
##'        plot regions. A vector of length 4 (bottom, left, top, right) or repeated
##'        to be as such.
##' @param draw A logical indicating whether plotting should be done.
##' @param ... Additional arguments passed to both plot1d() and plot2d().
##' @return invisible() with:
##'         - graphics: path, layout
##'         - grid: path, layout, grob
##'         - loon: path, layout, loon, toplevel
##' @author Marius Hofert and Wayne Oldford
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
        if(!exists(as.character(substitute(plot1d))))
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
        if(!exists(as.character(substitute(plot2d))))
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
        invisible(list(path = path, layout = layout))

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

        ## Return (the return value of unfold() and the frame grob)
        invisible(list(path = path, layout = layout, grob = fg))

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
        invisible(list(path = path, layout = layout, loon = parent, toplevel = tt))

    },
    stop("Wrong 'pkg'"))
}
