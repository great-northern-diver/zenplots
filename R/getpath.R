## Move/turns/path tools for zenplot()


##' @title Determine the new position when moving from the current position
##'        in a given direction
##' @param curpos current position (i, j) in the occupancy matrix
##' @param dir direction in which we move ("d", "u", "r" or "l")
##' @param method choice of method ("in.occupancy" means the (current/new)
##'        position is given in terms of (row, column) indices in the
##'        occupancy matrix; "in.plane" means the directions are
##'        interpreted as in the (x,y)-plane).
##' @return new position in the occupancy matrix
##' @author Marius Hofert and Wayne Oldford
move <- function(curpos, dir, method = c("in.occupancy", "in.plane"))
{
    method <- match.arg(method)
    curpos +
        if (method == "in.plane") {
            switch(dir,
                   "d" = { c( 0, -1) },
                   "u" = { c( 0,  1) },
                   "r" = { c( 1,  0) },
                   "l" = { c(-1,  0) },
                   stop("Wrong 'dir'"))

        } else {
            switch(dir,
                   "d" = { c( 1,  0) },
                   "u" = { c(-1,  0) },
                   "r" = { c( 0,  1) },
                   "l" = { c( 0, -1) },
                   stop("Wrong 'dir'"))
        }
}

##' @title Compute turns for zigzag
##' @param nPlots total number of plots
##' @param n2dcols number of columns of 2d plots (>= 1)
##' @param method character string indicating which zigzag method to use
##' @return turns
##' @author Marius Hofert and Wayne Oldford
get_zigzag_turns <- function(nPlots, n2dcols,
                             method = c("tidy", "double.zigzag", "single.zigzag"))
{
    ## Main idea: Determine the pattern which repeats rowblock-wise
    ##            (after going to the right and then back to the left)
    stopifnot(nPlots >= 4, # smaller nPlots relate to special cases dealt with in get_path()
              n2dcols >= 2)
    method <- match.arg(method)
    ## 1) Define the horizontal 2d pattern of turns
    h2dpattern <- rep(c("r", "l"), each = 2*(n2dcols-1)) # r,r,..., l,l,... for 2d plots
    ## 2) Define the vertical 2d subpattern
    v2dsubpattern <- if(method == "single.zigzag") rep("d", n2dcols-1) else {
        if(n2dcols <= 2) "d" else c(rep_len(c("d", "u" ), n2dcols-3), "d", "d") # up/down for 2d plots for one row (length = n2dcols - 1; so, e.g. only from right to left)
    }
    ## 3) Define the vertical 2d pattern
    v2dpattern <- rep(v2dsubpattern, times = 2) # up/down for 2d plots for one block of rows (r,r,..., l,l,...)
    ## 4) Merge the vertical 2d pattern into the horizontal 2d pattern
    h2dpattern[2*seq_along(v2dpattern)] <- v2dpattern # merge the ups/downs into h2dpattern
    ## 5) Repeat the merged 2d pattern to account for 1d plots
    overallpattern <- rep(h2dpattern, each = 2) # bring in 1d plots (by repeating each direction twice)
    ## 6) Repeat the overall pattern according to the total number of (1d or 2d) plots
    c("d", rep_len(overallpattern, nPlots-1)) # attach the first 1d plot separately and repeat the repeat pattern as often as required
}

##' @title Determine the next position to move to and the turn out of there
##' @param plotNo current plot number
##' @param nPlots total number of plots
##' @param curpath the current path
##' @return a list containing the next position to move to (nextpos) and the turn
##'         out of there (nextout); Interpretation:
##'         nextpos: position of plot number plotNo+1 in the (non-trimmed) occupancy matrix
##'         nextout: turn out of nextpos
##' @author Marius Hofert and Wayne Oldford
##' @note - This assumes that the last plot is a 1d plot!
##'       - It also assumes that first1d = TRUE; will be adapted later in get_path()
##'         in case first1d = FALSE.
##'       - We start in (1, 2) and also have an additional last column in the occupancy
##'         matrix to have the first and last column left in case we end up there with
##'         the last 1d plot; this cannot happen for 'zigzag' but for 'tidy'.
next_move_tidy <- function(plotNo, nPlots, curpath)
{
    stopifnot(plotNo >= 1, nPlots >= 4, # smaller nPlots relate to special cases dealt with in get_path()
              is.list(curpath))
    nPlotsLeft <- nPlots - plotNo # number of plots left
    if(nPlotsLeft <= 0)
        stop("Wrong number of plots left.") # next_move_tidy() should not be called in this case

    ## 1) Deal with special cases first
    if(plotNo==1) return(list(nextpos=c(2, 2), nextout="r")) # 1st plot (1d); next move/turn is "r"
    if(plotNo==2) return(list(nextpos=c(2, 3), nextout="r")) # 2nd plot (2d); next move/turn is "r"
    if(plotNo==3) # 3rd plot (1d); next move/turn is either up (only if there's only 1 1d plot left) or down
        return(list(nextpos=c(2, 4), nextout=if(nPlotsLeft <= 2) "u" else "d"))

    ## 2) Now plotNo >= 4 and there are >= 1 plots left
    curpos <- curpath$positions[plotNo,] # current position
    curin  <- curpath$turns[plotNo-1] # turn into the current position
    curout <- curpath$turns[plotNo] # turn out of the current position
    nextpos <- move(curpos, curout) # next position to move to

    ## 3) If plotNo is even (2d plot)
    ##    Note: This case also applies if nPlotsLeft==1
    if(plotNo %%2 == 0)
        return(list(nextpos=nextpos, nextout=curout))

    ## Now we are in the case plotNo is >= 5, odd (1d plot) and there are >= 2 plots left

    ## 4) Determine the current horizontal moving direction.
    ##    If the current 1d plot is vertical (or: horizontal), the horizontal direction is the
    ##    turn into the current (or: last) position.
    ##    => We simply consider the turn into the current position and the
    ##       one before to determine the horizontal moving direction.
    rinlast2 <- "r" %in% curpath$turns[(plotNo-2):(plotNo-1)]
    linlast2 <- "l" %in% curpath$turns[(plotNo-2):(plotNo-1)]
    if((rinlast2 + linlast2) != 1) # defensive programming
        stop("Algorithm to determine horizontal moving direction is wrong. This should not happen.")
    horizdir <- if(rinlast2) "r" else "l" # current horizontal moving direction

    ##    Determine the distance to the margin of the occupancy matrix in the horizontal moving direction
    ncolOcc <- ncol(curpath$occupancy)
    dist <- if(horizdir=="r") ncolOcc-curpos[2] else curpos[2]-1
    stopifnot(dist >= 0) # defensive programming

    ## 5) We are sitting at a 1d plot and have to determine how to leave
    ##    the next 2d plot
    posExists <- function(pos, occupancy) # aux function for checking the existence of a position in the occupancy matrix
        (1 <= pos[1] && pos[1] <= nrow(occupancy)) &&
            (1 <= pos[2] && pos[2] <= ncol(occupancy))
    nextout <- if(curout %in% c("d", "u")) { # 5.1)

        ## 5.1) The 1d plot is horizontal (curout "u" or "d")
        if(dist == 0) stop("dist == 0. This should not happen.")
        ##      If we have at most 2 plots left, decide where to put the last 1d plot
        if(nPlotsLeft <= 2) { # 5.1.1)
            ## Check the location of the 2d plot which comes after the next 2d plot
            ## in opposite horizontal moving direction.
            pos2check <- c(curpos[1] + if(curout=="u") -1 else 1, curpos[2] + if(horizdir=="r") -2 else 2)
            exists <- posExists(pos2check, occupancy = curpath$occupancy)
            ## If it does not exist (can only happen if curout="d" in which case
            ## the occupancy matrix is missing a new row), then put the
            ## last 1d plot in opposite horizontal moving direction if we are near
            ## the margin (otherwise we would occupy an additional column) and put it
            ## in the horizontal moving direction otherwise (if we are 'inside' the
            ## occupancy matrix)
            if(!exists) { # we will be in a new row (curout must be "d" in this case)
                stopifnot(curout=="d") # defensive programming
                if(dist <= 2) {
                    if(horizdir == "r") "l" else "r"
                } else {
                    horizdir
                }
            } else { # exists
                ## If this position exists, then change the horizontal moving direction
                ## if and only if it is not occupied (otherwise we can't go there)
                if(curpath$occupancy[pos2check[1], pos2check[2]] == "") { # not occupied
                    if(horizdir == "r") "l" else "r"
                } else {
                    horizdir
                }
            }
        } else { # 5.1.2) nPlotsLeft >= 3 (=> at least two more 2d plots)
            ## Change the horizontal moving direction if and only if we are at
            ## the boundary (clear).
            if(dist <= 2) {
                if(horizdir == "r") "l" else "r"
            } else {
                horizdir
            }
        }

    } else { # 5.2) curout "l" or "r"

        ## 5.2) The 1d plot is vertical (curout "l" or "r")
        if(curpath$turns[plotNo-2] %in% c("l", "r")) # defensive programming; how we entered last 2d plot must be l or r
            stop("Last 2d plot was entered in the wrong direction. This should not happen.")
        if(dist <= 1) {
            stop("dist <= 1. This should not happen.") # ... as we don't call next_move_tidy() for the last 1d plot
        } else { # dist >= 2; note that dist==2 and dist==3 are possible

            ## 5.2.1) Auxiliary function to determine how many plots fit in the next U-turn
            UturnLength <- function(curpos, horizdir, occupancy) {
                ## Check whether 1 or 2 plot(s) fit in
                pos2check <- c(curpos[1]-2, curpos[2] + if(horizdir=="r") 1 else -1)
                exists <- posExists(pos2check, occupancy=occupancy)
                if(exists && (occupancy[pos2check[1], pos2check[2]] != "")) return(1)
                if(!exists) return(2) # ... we can't put in more plots
                ## Check whether 4 plots fit in
                pos2check <- pos2check + c(0, if(horizdir=="r") 2 else -2)
                exists <- posExists(pos2check, occupancy=occupancy)
                if(!exists || (exists && (occupancy[pos2check[1], pos2check[2]] != "")))
                    return(4) # ... we can't put in more plots
                ## Check whether 6 plots fit in
                pos2check <- pos2check + c(2, 0)
                exists <- posExists(pos2check, occupancy=occupancy)
                if(!exists || (exists && (occupancy[pos2check[1], pos2check[2]] != "")))
                    return(6) # ... we can't put in more plots
                ## Check whether 8 or >= 10 plots fit in
                pos2check <- pos2check + c(0, if(horizdir=="r") 2 else -2)
                exists <- posExists(pos2check, occupancy=occupancy)
                if(!exists || (exists && (occupancy[pos2check[1], pos2check[2]] != "")))
                    return(8) else return(10) # here means "at least 10"
            }
            ## Determine the number of plots along the U-turn starting from the current position
            Ulen <- UturnLength(curpos, horizdir = horizdir, occupancy = curpath$occupancy)

            ## 5.2.2) If we are in the second row or there are more plots left than
            ##        can fit into a U-turn, go down, else go up.
            ##        Note: if Ulen >= 10, we can always take the U-turn, so we can always go up
            if(curpos[1] <= 2 || (Ulen < 10 && nPlotsLeft > Ulen)) "d" else "u"

        }

    }

    ## 6) Return
    list(nextpos = nextpos, nextout = nextout) # nextout = turn out of next position
}

##' @title Computing the path according to the provided method
##' @param turns The turns
##' @param n2dcols The number of columns of 2d plots (>= 1) or one of "letter", "square",
##'        "A4", "golden", "legal". Note that n2dcols is ignored if turns is not NULL.
##' @param n2dplots The number of 2d plots to be laid out
##' @param method A character string indicating the method according to which the
##'        path is built
##' @param first1d A logical indicating whether the first 1d plot should be plotted
##' @param last1d A logical indicating whether the last 1d plot should be plotted
##' @return the path, a list containing the turns, the positions (indices in the
##'         occupancy matrix) and the the occupancy matrix
##' @author Marius Hofert and Wayne Oldford
get_path <- function(turns = NULL, n2dcols = c("letter", "square", "A4", "golden", "legal"),
                     n2dplots, method = c("tidy", "double.zigzag", "single.zigzag", "rectangular"),
                     first1d = TRUE, last1d = TRUE)
{
    ## 1) Deal with the case that turns have been given (we need to construct
    ##    the positions in the occupancy matrix and the occupancy matrix itself)
    if(!is.null(turns)) {

        ## 1.1) Initialization
        hlim <- c(0, 0) # horizontal limits covered so far
        vlim <- c(0, 0) # vertical limits covered so far
        positions <- matrix(0, nrow=length(turns), ncol=2,
                            dimnames=list(NULL, c("x", "y"))) # matrix of positions
        loc <- c(0, 0) # where we are at the moment (start)
        ## 1.2) Loop
        if(length(turns) > 1) { # if length(turns)==1, we only have one 1d plot (nothing to do as positions is already initialized with 0)
            for(i in 2:length(turns)) { # loop over all turns
                loc <- move(loc, dir=turns[i-1]) # move to the next location according to turns
                positions[i,] <- loc # update positions
                if(loc[1] < hlim[1]) {
                    hlim[1] <- loc[1] # extend the lower bound of hlim if necessary
                } else if(loc[1] > hlim[2]) {
                    hlim[2] <- loc[1] # extend the upper bound of hlim if necessary
                }
                if(loc[2] < vlim[1]) {
                    vlim[1] <- loc[2] # extend the lower bound of vlim if necessary
                } else if(loc[2] > vlim[2]) {
                    vlim[2] <- loc[2] # extend the upper bound of vlim if necessary
                }
            }
        }
        ## 1.3) Shift
        min.pos <- apply(positions, 2, min) # get minimal visited row/column position
        positions <- sweep(positions, 2, min.pos) + 1 # substract these and add (1,1)

        ## 1.4) Compute the occupancy matrix
        occupancy <- matrix("", nrow=max(positions[,"x"]), ncol=max(positions[,"y"])) # occupancy matrix; note: already trimmed by construction
        for(i in 1:nrow(positions)) # loop over positions and fill occupancy matrix accordingly
            occupancy[positions[i,1], positions[i,2]] <- switch(turns[i],
                                                                "l" = { "l" },
                                                                "r" = { "r" },
                                                                "d" = { "d" },
                                                                "u" = { "u" },
                                                                stop("Wrong 'turns'"))

        ## (Early) return
        return(list(turns = turns, positions = positions, occupancy = occupancy)) # return here; avoids huge 'else' below

    }

    ## 2) Now consider the case where turns has not been given => construct the path

    ## Checking
    stopifnot(n2dplots >= 0)
    if(is.character(n2dcols)) n2dcols <- n2dcols_aux(n2dplots, method = n2dcols)
    stopifnot(length(n2dcols) == 1, n2dcols >= 1)
    if(n2dplots >= 2 && n2dcols < 2)
        stop("If n2dplots >= 2, n2dcols must be >= 2.")
    method <- match.arg(method)

    ## 2.1) Deal with method = "rectangular" first
    if(method == "rectangular") {
        nPlots <- 2 * n2dplots + 1 - !first1d - !last1d
        ## Determine the number or rows required
        n2drows <- ceiling(n2dplots/n2dcols)
        ## Determine 'turns'
        turns <- unlist(lapply(rep(c("r", "l"), length.out = n2drows),
                               function(t) c("d", rep(t, 2*(n2dcols-1)), "d"))) # correct for a *full last* row if first1d == TRUE and last1d == FALSE
        if(!first1d) turns <- turns[-1] # remove first element
        if(last1d) turns <- c(turns, "d") # append last element
        turns <- turns[seq_len(nPlots)] # grab out those we need
        ## Determine 'positions'
        positions <- matrix(, nrow = nPlots, ncol = 2, dimnames = list(NULL, c("x", "y"))) # positions
        if(nPlots >= 1) positions[1,] <- c(1, 1) # first plot
        if(nPlots >= 2) {
            for(plotNo in 2:nPlots) {
                turnOOcur <- turns[plotNo-1] # turn out of last position
                if(turnOOcur == "r") {
                    positions[plotNo,] <- positions[plotNo-1,] + c(0,1)
                } else if(turnOOcur == "l") {
                    positions[plotNo,] <- positions[plotNo-1,] + c(0,-1)
                } else if(turnOOcur == "d") {
                    positions[plotNo,] <- positions[plotNo-1,] + c(1,0)
                } else stop("Wrong turn in 'rectangular' method. This should not happen.")
            }
        }
        ## Determine occupancy matrix
        occupancy <- matrix(0, nrow = positions[nPlots,1], ncol = min(max(positions[,2]), 2*n2dcols-1)) # positions
        for(i in 1:nPlots) {
            occupancy[positions[i,1], positions[i,2]] <- switch(turns[i],
                                                                "l" = { "l" },
                                                                "r" = { "r" },
                                                                "d" = { "d" },
                                                                "u" = { "u" }, # should not happen here
                                                                stop("Wrong 'turns'"))
        }
        ## Return
        return(list(turns = turns, positions = positions, occupancy = occupancy))
    }

    ## 2.2) We start by dealing with three special cases (the same for all methods)
    nPlots <- 2 * n2dplots + 1 # total number of plots (1d and 2d; assumes first1d = TRUE; last1d = TRUE (will be trimmed off below if necessary))
    path <- if(nPlots <= 3) {
        switch(nPlots,
           { # nPlots = 1
               turns <- "d"
               positions <- matrix(c(1,1), ncol=2, dimnames=list(NULL, c("x", "y")))
               occupancy <- matrix("d", nrow=1, ncol=1)
               list(turns=turns, positions=positions, occupancy=occupancy)
           },
           { # nPlots = 2
               turns <- c("d", "r")
               positions <- matrix(c(1,1, 2,1), ncol=2, byrow=TRUE,
                                   dimnames=list(NULL, c("x", "y")))
               occupancy <- matrix(c("d", "r"), nrow=2, ncol=1)
               list(turns=turns, positions=positions, occupancy=occupancy)
           },
           { # nPlots = 3
               turns <- c("d", "r", "r")
               positions <- matrix(c(1,1, 2,1, 2,2), ncol=2, byrow=TRUE,
                                   dimnames=list(NULL, c("x", "y")))
               occupancy <- matrix(c("d","", "r","r"), nrow=2, ncol=2, byrow=TRUE)
               list(turns=turns, positions=positions, occupancy=occupancy)
           },
               stop("Wrong 'nPlots'"))
    } else {

        ## nPlots >= 4 (repeating order depends on n2dcols)
        turns <- character(nPlots) # how we leave every plot (1d or 2d)
        switch(method,
               "double.zigzag" =, "single.zigzag" = { # 2.2.1)

                   ## Main idea: Determine all turns right away, then build positions and
                   ##            occupancy matrix all from the turns
                   turns <- get_zigzag_turns(nPlots, n2dcols=n2dcols, method=method)

                   ## Build positions and occupancy matrix

                   ## Setup
                   ncolOcc <- 2*n2dcols-1 # number of columns in the occupancy matrix
                   occupancy <- matrix("", nrow=1, ncol=ncolOcc) # occupancy matrix
                   positions <- matrix(0, nrow=nPlots, ncol=2, dimnames=list(NULL, c("x", "y"))) # positions

                   ## Init
                   curpos <- c(1, 1) # current position
                   positions[1,] <- curpos # occupy (row, col)
                   occupancy[curpos[1], curpos[2]] <- turns[1]

                   ## Loop over all remaining plots
                   maxrowOcc <- 1
                   for(plotNo in 2:nPlots) {
                       ## Update position
                       nextpos <- move(curpos, dir=turns[plotNo-1])
                       positions[plotNo,] <- nextpos # occupy location (row, col)
                       curpos <- nextpos
                       if(curpos[1] > maxrowOcc) { # expand occupancy matrix by one row
                           occupancy <- rbind(occupancy, rep("", ncolOcc))
                           maxrowOcc <- maxrowOcc + 1
                       }
                       ## Update occupancy matrix
                       occupancy[nextpos[1], nextpos[2]] <- turns[plotNo]
                   }

                   ## Trim last columns of "" from occupancy matrix
                   occupancy <- occupancy[,seq_len(max(positions[,2])), drop=FALSE]

                   ## Build path
                   list(turns=turns, positions=positions, occupancy=occupancy)

               },
               "tidy" = { # 2.2.2)

                   ## Main idea: Build turns, positions and occupancy as we go along

                   ## Setup
                   turns <- character(nPlots) # vector of turns out of current position
                   ncolOcc <- 2*n2dcols+1 # number of columns in the occupancy matrix (2*n2dcols-1 + left/right one more)
                   occupancy <- matrix("", nrow=1, ncol=ncolOcc) # occupancy matrix
                   positions <- matrix(0, nrow=nPlots, ncol=2, dimnames=list(NULL, c("x", "y"))) # positions

                   ## Init
                   turns[1] <- "d" # turn out of current position
                   positions[1,] <- c(1, 2) # occupy (row, col)
                   occupancy[1, 2] <- "d" # initialize occupancy matrix there

                   ## Loop over all remaining plots
                   maxrowOcc <- 1
                   for(plotNo in 2:nPlots) {
                       ## Next move
                       nextmove <- next_move_tidy(plotNo-1, nPlots=nPlots,
                                                  curpath=list(turns=turns,
                                                               positions=positions,
                                                               occupancy=occupancy))
                       ## Update turn
                       turns[plotNo] <- nextmove$nextout # nextout = turn out of next position
                       ## Update position
                       nextpos  <- nextmove$nextpos
                       positions[plotNo,] <- nextpos # occupy location (row, col)
                       if(nextpos[1] > maxrowOcc) { # expand occupancy matrix by one row
                           occupancy <- rbind(occupancy, rep("", ncolOcc))
                           maxrowOcc <- maxrowOcc + 1
                       }
                       ## Update occupancy matrix
                       occupancy[nextpos[1], nextpos[2]] <-
                           switch(turns[plotNo], # update occupancy matrix
                                  "l" = { "l" },
                                  "r" = { "r" },
                                  "d" = { "d" },
                                  "u" = { "u" },
                                  stop("Wrong 'turns' at plotNo ", plotNo))
                   }

                   ## Trim last columns of 0s from occupancy matrix if necessary
                   occupancy <- occupancy[,seq_len(max(positions[,2])), drop=FALSE]
                   ## Trim first column of 0s and adjust positions if necessary
                   if(all(occupancy[,1]=="")) { # first column
                       occupancy <- occupancy[,-1]
                       positions[,2] <- positions[,2] - 1
                   }

                   ## Build path
                   list(turns=turns, positions=positions, occupancy=occupancy)

               },
               stop("Wrong 'method'"))

    }

    ## Trim path if necessary
    ## Idea: Take corresponding (first/last) turn to determine where to trim the
    ##       occupancy matrix and the positions.
    if(!first1d) {
        ## Trim turns
        first1d.turn.out <- path$turns[1] # get turn out of first 1d plot
        turns <- path$turns[-1] # trim

        ## Trim occupancy matrix
        occupancy <- path$occupancy
        rm <- positions[1,] # position to be removed (= replaced by 0)
        occupancy[rm[1], rm[2]] <- "" # remove position
        switch(first1d.turn.out,
        "l" = { # check whether we can trim last column
            jj <- ncol(occupancy)
            if(all(occupancy[,jj] == ""))
                occupancy <- occupancy[,-jj, drop = FALSE] # trim; no shift in positions necessary!
        },
        "r" = { # check whether we can trim first column
            if(all(occupancy[,1] == "")) {
                occupancy <- occupancy[,-1, drop = FALSE] # trim
                path$positions[,2] <- path$positions[,2] - 1 # shift all positions
            }
        },
        "d" = { # check whether we can trim first row
            if(all(occupancy[1,] == "")) {
                occupancy <- occupancy[-1,, drop = FALSE] # trim
                path$positions[,1] <- path$positions[,1] - 1 # shift all positions
            }
        },
        "u" = { # check whether we can trim last row
            ii <- nrow(occupancy)
            if(all(occupancy[ii,] == ""))
                occupancy <- occupancy[-ii,, drop = FALSE] # trim; no shift in positions necessary!
        }, stop("Wrong 'first1d.turn.out'."))

        ## Trim positions
        positions <- path$positions[-1,, drop = FALSE] # trim

        ## Define (trimmed) path
        path <- list(turns = turns, positions = positions, occupancy = occupancy)
    }
    if(!last1d) {
        ## Trim turns
        n <- length(path$turns)
        last1d.turn.out <- path$turns[n] # get turn out of last 1d plot
        turns <- path$turns[-n] # trim

        ## Trim occupancy matrix
        occupancy <- path$occupancy
        rm <- positions[n,] # position to be removed (= replaced by 0)
        occupancy[rm[1], rm[2]] <- "" # remove position
        switch(last1d.turn.out,
        "l" = { # check whether we can trim first column
            if(all(occupancy[,1] == "")) {
                occupancy <- occupancy[,-1, drop = FALSE] # trim
                path$positions[,2] <- path$positions[,2] - 1 # shift all positions
            }
        },
        "r" = { # check whether we can trim last column
            jj <- ncol(occupancy)
            if(all(occupancy[,jj] == ""))
                occupancy <- occupancy[,-jj, drop = FALSE] # trim; no shift in positions necessary!
        },
        "d" = { # check whether we can trim last row
            ii <- nrow(occupancy)
            if(all(occupancy[ii,] == ""))
                occupancy <- occupancy[-ii,, drop = FALSE] # trim; no shift in positions necessary!
        },
        "u" = { # check whether we can trim first row
            if(all(occupancy[1,] == "")) {
                occupancy <- occupancy[-1,, drop = FALSE] # trim
                path$positions[,1] <- path$positions[,1] - 1 # shift all positions
            }
        }, stop("Wrong 'last1d.turn.out'."))

        ## Trim positions
        positions <- path$positions[-n,, drop = FALSE] # trim

        ## Define (trimmed) path
        path <- list(turns = turns, positions = positions, occupancy = occupancy)
    }

    ## Return the path
    path
}
