## Layout tools for zenplot()


##' @title Auxiliary function for adjusting a bounding box
##' @param lastturn last turn
##' @param coordslastBB coordinates of the last bounding box
##' @param w width
##' @param h height
##' @return Coordinates of the adjusted bounding box
##' @author Wayne Oldford
adjust_bb <- function(lastturn, coordslastBB, w, h)
{
    stopifnot(lastturn %in% c("d", "u", "r", "l"),
              names(coordslastBB) == c("left", "right", "bottom", "top"),
              w >= 0, h >= 0)
    switch(lastturn,
           "l" = {
               c(coordslastBB["left"] - w,
                 coordslastBB["left"],
                 coordslastBB["bottom"],
                 coordslastBB["top"])
           },
           "r" = {
               c(coordslastBB["right"],
                 coordslastBB["right"] + w,
                 coordslastBB["bottom"],
                 coordslastBB["top"])
           },
           "d" = {
               c(coordslastBB["left"],
                 coordslastBB["right"],
                 coordslastBB["bottom"] - h,
                 coordslastBB["bottom"])
           },
           "u" = {
               c(coordslastBB["left"],
                 coordslastBB["right"],
                 coordslastBB["top"],
                 coordslastBB["top"] + h)
           },
           stop("Wrong 'turns'"))
}

##' @title Compute the layout of the zen plot
##' @param turns turns (character vector consisting if "u", "d", "l", "r")
##' @param n2dplots the number of 2d plots (faces of the hypercube to be laid out)
##' @param first1d logical indicating whether the first 1d plot should be plotted
##' @param last1d logical indicating whether the last 1d plot should be plotted
##' @param width1d width of 1d plots
##' @param width2d width of 2d plots
##' @return list containing
##'         1) the plot orientations (c("h", "s", "v", "s", ...))
##'         2) the plot dimensions (1d plot, 2d plot, 1d plot, ...)
##'         3) the variable numbers plotted (an (nPlots, 2)-matrix)
##'         4) the total width of the layout
##'         5) the total height of the layout
##'         6) coordinates of the bounding boxes
##' @author Marius Hofert and Wayne Oldford
get_layout <- function(turns, n2dplots, first1d = TRUE, last1d = TRUE, width1d = 1, width2d = 10)
{
    l <- as.numeric(!first1d) + as.numeric(!last1d) # 0 (first1d = last1d = TRUE), 1 (precisely one TRUE) or 2 (first1d = last1d = FALSE)
    stopifnot(n2dplots >= 0, width1d > 0, width2d > 0)
    indices <- 1:(n2dplots+1)
    turn_checker(turns = turns, n2dplots = n2dplots, first1d = first1d, last1d = last1d)
    dimensions <- c(rep(1:2, n2dplots), 1) # plot dimensions (1d plot, 2d plot, 1d plot, ...)
    if(!first1d) dimensions <- dimensions[-1]
    if(!last1d) dimensions <- dimensions[-length(dimensions)]
    nPlots <- length(dimensions) # number of plots; >= 1 (checked)
    orientations <- rep("s", nPlots) # plot orientations ("s"=square, "h"=horizontal, "v"=vertical)

    ## 1) Determine positions of bounding boxes (in terms of default width and
    ##    height units); for each plot, start at zero
    coordsBB <- matrix(0, nrow = nPlots, ncol = 4,
                       dimnames = list(NULL, c("left", "right", "bottom", "top"))) # (nPlots, 4)-matrix

    ##    Now we have to build the variable selections and their information
    vars <- matrix(0, nrow=nPlots, ncol=2, dimnames=list(NULL, c("x", "y"))) # (nPlots, 2)-matrix

    ## 2) Loop over all plots starting from the 2nd

    ## 2.1) Deal with first plot
    if (dimensions[1]==1) {
        lastVar <- 1
        curVar <- lastVar
        vars[1,] <- indices[c(1, 1)]
        if (turns[1] %in% c("u", "d")) {
            orientations[1] <- "h"
	    coordsBB[1, "right"] <- width2d
	    coordsBB[1, "top"]   <- width1d
        } else if(turns[1] %in% c("l", "r")) {
            orientations[1] <- "v"
	    coordsBB[1, "right"] <- width1d
	    coordsBB[1, "top"]   <- width2d
        } else stop("Wrong 'turns'")
    } else {
        lastVar <- 1
        curVar <- 2
        vars[1,] <- indices[c(1, 2)]
        lastVar <- curVar
        coordsBB[1, "right"] <- width2d
        coordsBB[1, "top"]   <- width2d
    }

    ## 2.2) Actual loop
    for(i in 1+seq_len(nPlots-1)) { # 2,3,...,nPlots
        if(dimensions[i] == 1) { # current plot is a 1d plot
            vars[i,] <- indices[rep(curVar, 2)] # determine 1d plot variables
            coordsBB[i,] <-
                switch(turns[i], # determine 1d plot bounding box
                       "l" = {
                           orientations[i]="v"
                           adjust_bb(turns[i-1], coordslastBB=coordsBB[i-1,],
                                     w=width1d, h=width2d)
                       },
                       "r" = {
                           orientations[i]="v"
                           adjust_bb(turns[i-1], coordslastBB=coordsBB[i-1,],
                                     w=width1d, h=width2d)
                       },
                       "d" = {
                           orientations[i]="h"
                           adjust_bb(turns[i-1], coordslastBB=coordsBB[i-1,],
                                     w=width2d, h=width1d)
                       },
                       "u" = {
                           orientations[i]="h"
                           adjust_bb(turns[i-1], coordslastBB=coordsBB[i-1,],
                                     w=width2d, h=width1d)
                       },
                       stop("Wrong 'turns' for 1d plot"))
        } else { # current plot is a 2d plot
            curVar <- curVar + 1
            vars[i,] <- indices[switch(turns[i], # determine 2d plot variables
                               "l" = {
                                   c(lastVar, curVar)
                               },
                               "r" = {
                                   c(lastVar, curVar)
                               },
                               "d" = {
                                   c(curVar, lastVar)
                               },
                               "u" = {
                                   c(curVar, lastVar)
                               },
                               stop("Wrong 'turns' for 2d plot"))]
            coordsBB[i,] <- adjust_bb(turns[i-1], coordslastBB=coordsBB[i-1,],
                                      w=width2d, h=width2d) # determine 2d plot bounding box
            lastVar <- curVar
        }
    }

    ## Return
    layoutWidth  <- diff(range(coordsBB[,c(  "left", "right")])) # total width
    layoutHeight <- diff(range(coordsBB[,c("bottom", "top")])) # total height
    list(orientations = orientations, # vector of plot orientations ("s"=square, "h"=horizontal, "v"=vertical)
         dimensions = dimensions, # plot dimensions (1d plot, 2d plot, 1d plot, ...)
         vars = vars, # (nPlots, 2)-matrix of variables
         layoutWidth = layoutWidth, layoutHeight = layoutHeight, # total width and height
         boundingBoxes = coordsBB) # bounding boxes
}
