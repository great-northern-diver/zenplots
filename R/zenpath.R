## Tools for computing a path through all variables which can then be plotted
## with a zen plot
##' @title Extract Pairs from a Path of Indices
##' @usage extract_pairs(x, n)
##' @description Extracts pairs from a path of indices, representing the path
##' by the pairs (connected by common variable) and return a shortened path.
##' @family tools related to constructing zenpaths
##' @param x the path, a \code{\link{vector}} or
##' \code{\link{list}} of indices of the variables to be plotted.
##' @param n A \code{\link{vector}} of length two giving the number
##' of pairs to extract from the path \code{x} (if \code{NULL}, all pairs are
##' returned (nothing extracted); if of length one, it is replicated in the pair).
##' The first number corresponds to the beginning of the path,
##' the second to the end; at least one of the two numbers should be >= 1.
##' @return returns an object of the same type as the input
##' \code{x} but (possibly) shortened. It extracts the first/last so-many
##' pairs of \code{x}.
##' @author Marius Hofert and Wayne Oldford
##' @seealso \code{\link{zenplot}()} which provides the zenplot.
##' @export
##' @import PairViz
##' @examples
##' ## Begin with a path
##' (zp <- zenpath(c(3, 5), method = "eulerian.cross")) # integer(2) argument
##'
##' ## Extract the first two pairs and last four of indices
##' extract_pairs(zp, n = c(2, 4))
##'
##' ## Extract the first and last three pairs of indices
##' extract_pairs(zp, n = 3) # the 3 is repeated automatically
##'
##'
extract_pairs <- function(x, n)
{
    if(is.null(n))
        return(x) # nothing extracted
    if(length(n) == 1) n <- rep(n, 2)
    if((length(n) != 2) || any(n < 0))
        stop("'n' must be NULL or one or two integers >= 0.")
    if(sum(n) <= 0)
        stop("At least one component of 'n' should be >= 1.")
    stopifnot(is.list(x) || (is.numeric(x) && is.vector(x)))
    if(is.list(x)) { # x is a list of indices

        ## Auxiliary function
        truncation_point <- function(x, num, first = TRUE) {
            l <- 0
            i.ind <- 1:length(x)
            if(!first) i.ind <- rev(i.ind)
            for(i in i.ind) { # iterates over x top-down (if first = TRUE) or bottom-up (if first = FALSE)
                j.ind <- seq_len(length(x[[i]])-1) # iterates over the pairs in x[[i]]
                if(!first) j.ind <- rev(j.ind)
                for(j in j.ind) { # iterates over the pairs in the vector x[[i]]
                    l <- l + 1
                    if(l >= num)
                        return(c(i, j)) # i = sublist; j = first index where last pair of interest begins
                }
            }
        }

        ## Determine the first part
        if(n[1] == 0) x.first <- NULL else {
            x.first <- x
            ij <- truncation_point(x, num = n[1])
            i <- ij[1]
            j <- ij[2]
            x.first[[i]] <- head(x.first[[i]], n = j+1) # grab out the first part of the line
            x.first <- head(x.first, n = i) # truncate the rest of the list
        }

        ## Determine the last part
        if(n[2] == 0) x.last <- NULL else {
            x.last <- x
            ij <- truncation_point(x, num = n[2], first = FALSE)
            i <- ij[1]
            j <- ij[2]
            x.last[[i]] <- tail(x.last[[i]], n = length(x.last[[i]])-j+1) # grab out the last part of the line
            x.last <- tail(x.last, n = length(x)-i+1) # truncate the first part of the list
        }

        ## Return
        res <- c(x.first, x.last)
        if(is.list(res) && length(res) == 1) unlist(res) else res

    } else { # x is a vector of indices

        c(head(x, n = n[1] + 1), tail(x, n = n[2] + 1))

    }
}

##' @title Connecting Possibly Overlapping Pairs Into a List of Paths
##' @usage connect_pairs(x, duplicate.rm = FALSE)
##' @name connect_pairs
##' @aliases connect_pairs
##' @description Pairs, given as rows of a \code{\link{matrix}},
##' \code{\link{data.frame}}, or \code{\link{list}}, are processed to return
##' a list of paths, each identifying the connected pairs in the rows of \code{x}.
##' @family tools related to constructing zenpaths
##' @param x two-column \code{\link{matrix}}, \code{\link{data.frame}}, or
##' a \code{\link{list}} containing vectors of length two representing
##' the pairs to be connected.
##' @param duplicate.rm \code{\link{logical}} indicating whether equal
##' pairs (up to permutation) are to be omitted.
##' @return A \code{\link{list}} each of whose elements give a path of connected pairs.
##' Each list element is a vector of length at least 2
##' (longer vectors > 2 in length identify the pairs connected in a path).
##' @author Marius Hofert and Wayne Oldford
##' @seealso \code{\link{zenplot}()} which provides the zenplot.
##' @export
##' @examples
##' ## First something simple.
##' (pairs <- matrix(c(1,2,2,3,3,5,5,7,8,9), ncol = 2, byrow = TRUE))
##' ## Connect pairs into separate paths defined by the row order.
##' connect_pairs(pairs)
##'
##' ## Now something different
##' nVars <- 5
##' pairs <- expand.grid(1:nVars, 1:nVars)
##' ## and take those where
##' (pairs <- pairs[pairs[,1] < pairs[,2],])
##' connect_pairs(pairs)
##'
##' ## Something more complicated.
##' ## Get weights
##' set.seed(27135)
##' x <- runif(choose(nVars,2)) # weights
##'
##' ## We imagine pairs identify edges of a graph with these weights
##' ## Get a zenpath ordering the edges based on weights
##' (zp <- zenpath(x, pairs = pairs, method = "strictly.weighted"))
##'
##' ## And connect these giving the list of paths
##' connect_pairs(zp)
##'
connect_pairs <- function(x, duplicate.rm = FALSE)
{
    if(is.list(x)) {
        if(is.data.frame(x)) { # matrix-like data frame
            stopifnot(ncol(x) == 2)
            x <- as.matrix(x)
        } else { # proper list
            stopifnot(all(sapply(x, function(x.) length(x.) == 2)))
            x <- matrix(unlist(x), ncol = 2, byrow = TRUE)
        }
    }
    stopifnot(is.matrix(x), nrow(x) >= 1, is.logical(duplicate.rm))
    if(duplicate.rm) {
        swapped <- rep(FALSE, nrow(x))
        for(i in seq_len(nrow(x))) {
            if(x[i,1] > x[i,2]) {
                x[i,] <- rev(x[i,])
                swapped[i] <- TRUE
            }
        }
        dupl <- duplicated(x)
        x <- x[!dupl,] # only grab out the unique pairs
        swapped <- swapped[!dupl]
        for(i in seq_len(nrow(x))) {
            if(swapped[i]) x[i,] <- rev(x[i,]) # swap back
        }
    }
    if(!is.matrix(x)) x <- rbind(x)
    nr <- nrow(x)
    res <- vector("list", length = nr) # result list of variables/indices
    l <- 1 # index where to add next element in res
    vec <- integer(2*nr) # most of it is 0, but c() to an empty vector is about 50x slower
    vec[1:2] <- x[1,] # start with first two connected variables
    v <- 2 # index of the last element in vec
    for(i in 2:nr) { # go over all variable pairs
        ## Deal with first 2 variables of a new group (we can still rev() these variables)
        if(v == 2) {
            matches <- x[i-1,] %in% x[i,] # which of the two entries in the previous row is in the current row
            sm <- sum(matches) # number of variables in the current row also present in the previous row
            if(sm == 2 && duplicate.rm)
                ## Note: - This case should actually not happen due to the removal of the duplicates above
                ##       - Without the duplicate.rm part above, this would only remove *adjacent* duplicates
                ## warning("Found two equal pairs (up to permutation) in rows ",i-1," and ",i,".")
                next
            if(sm >= 1)
                if(matches[1]) vec[1:2] <- vec[2:1] # if the first one matches, flip the elements
        }
        ## Now check whether the last variable in vec is found in the current row of pairs
        ## If so, add the *other* variable from the current row of pairs; otherwise start a new group
        is.valid.match <- vec[v] == x[i,]
        stopifnot(sum(is.valid.match) <= 1) # fail-safe programming
        if(any(is.valid.match)) {
            j <- which(!is.valid.match) # index of the *other* variable (the new one to add)
            vec[v+1] <- x[i,j]
            v <- v+1 # update index of last element in vec
        } else { # start a new group
            res[l] <- list(vec[1:v]) # add old vector
            l <- l+1 # update index where to add next element in res
            vec[1:2] <- x[i,] # define the new vec (entry is reversed above if necessary)
            v <- 2 # update index of last element in vec
        }
        if(i == nr) res[l] <- list(vec[1:v]) # add last built vector
    }
    res[1:l]
}

##' @title Turn pairs or paths into a graph
##' @family tools related to constructing zenpaths
##' @usage graph_pairs(x, var.names = NULL, edgemode = c("undirected", "directed"))
##' @name graph_pairs
##' @aliases graph_pairs
##' @description Pairs are processed to produce a graph with the elements
##' of the pairs as vertices and the pairs as undirected edges.
##' The result can be displayed using \code{\link{plot}()}.
##' @param x \code{\link{matrix}} or \code{\link{list}} of pairs along a zenpath.
##'        Can also be a list containing vectors representing paths in the graph.
##'        Every path must be of length at least 2 (i.e. each vector element of
##'        the list).
##' @param var.names names of the variables appearing in \code{x}.
##' @param edgemode type of edges to be used: either \code{"undirected"} (the default)
##'        or \code{"directed"} (in which case the order of the nodes in each pair matters).
##' @return a \code{\link{graphNEL}} object; can be displayed using
##' \code{\link{plot}()}.
##' @author Marius Hofert and Wayne Oldford
##' @seealso \code{\link{zenplot}()} which provides the zenplot.
##' @note \code{\link{zenplot}()} never use directed graphs nor graphs with isolated (disconnected) nodes.
##' @export
##' @examples
##' ## To display the graphs constructed the packages
##' ## graph and Rgraphviz packages need to be loaded
##' library(graph)
##' library(Rgraphviz)
##' ##
##' ## Get some pairs
##' pairs <- matrix(c(1,2, 5,1, 3,4, 2,3, 4,2), ncol = 2, byrow = TRUE)
##' g <- graph_pairs(pairs)
##' ## which can be displayed using plot(g)
##' plot(g)
##'
##' ## Build a graph from a list of paths
##' paths <- list(3:1, c(3,5,7), c(1,4,7), c(6,7))
##' gp <- graph_pairs(paths)
##' ## graph package draws with grid, so clear
##' grid.newpage()
##' plot(gp)
##'
##' ## Nodes do not need to be numbers
##' alpha_paths <- list(letters[3:1], letters[c(3,5,7)],
##'                     letters[c(1,4,7)], letters[c(6,7)])
##' grid.newpage()
##' plot(graph_pairs(alpha_paths))
##'
##' ## Zenplots never uses this feature but you could
##' ## build a directed graph with a single isolated node
##' dg <- graph_pairs(alpha_paths,
##'                   var.names = c(letters[1:7], "ALONE"),
##'                   edgemode = "directed" )
##' grid.newpage()
##' plot(dg)
##'
graph_pairs <- function(x, var.names = NULL,
                        edgemode = c("undirected", "directed"))
{
    edgemode <- match.arg(edgemode)
    ## If x a list (even with different lengths of its components, so
    ## 'grouped'), convert x to a 2-column matrix
    if(is.list(x) && !is.data.frame(x)) {
        stopifnot(all(sapply(x, function(x.) length(x.) >= 2)))
        x.. <- lapply(x, function(x.) {
            l <- length(x.)
            if(l > 2) {
                c(x.[1], rep(x.[2:(l-1)], each = 2), x.[l]) # recycle all elements except first and last
            } else x.
        })
        x <- matrix(unlist(x..), ncol = 2, byrow = TRUE)
    }
    ## => x is now a two-column matrix of the (ordered) pairs to be graphed
    ##    according to the weights

    ## Deal with weights
    ## Works but not needed
    ## if(!is.null(weights)) {
    ##     if(is.vector(weights)) {
    ##         stopifnot(length(weights) == nrow(x))
    ##     } else if(is.matrix(weights)) {
    ##         stopifnot(nrow(weights) == ncol(weights))
    ##         weights <- weights[x] # grab out
    ##     } else stop("'weights' must either be a vector or a square matrix")
    ##     ## => weights is now a vector
    ## }

    ## Build vertex names
    var.x <- as.character(sort(unique(as.vector(x)))) # vertices in x as characters
    if(is.null(var.names)) {
        var.names <- var.x
    } else {
        # var.names must be a character vector
        var.names <- as.character(var.names)
        # Must have at least as many names in
        # var.names as in var.x
        if(length(var.names) < length(var.x)){
            stop("'var.names' must be at least of length ",length(var.x))
        }
        # check that var.names contain all of var.x
        var.x_notin_var.names <-setdiff(var.x, var.names)
        if (length(var.x_notin_var.names) != 0) {
            stop(paste("var.names are missing",
                       paste(var.x_notin_var.names, collapse = ", "),
                       "from `x`."))
        }
    }


    ## Build graph
    ftM2graphNEL(x, V = var.names, edgemode = edgemode) # possibly uneven, disconnected
}

##' @title Splitting a Matrix into a List of Matrices
##' @family tools related to constructing zenpaths
##' @usage groupData(x, indices, byrow = FALSE)
##' @name groupData
##' @aliases groupData
##' @description Takes a matrix \code{x} and groups its rows (or columns)
##' as specified by \code{indices}.  Returns a list of matrices, one for each group.
##' @param x A \code{\link{matrix}} (or an object
##'          convertible to such via \code{\link{as.matrix}()}).
##' @param indices list of vectors of indices according to
##'        which \code{x} is grouped; each vector of indices define a group.
##' @param byrow \code{\link{logical}} indicating whether the grouping is
##'        done by row (\code{byrow = TRUE})
##'        or by column (\code{byrow = FALSE}, the default).
##' @return A \code{\link{list}} of matrices (one per group).
##'         Such a list, grouped by columns, is then typically passed on to \code{\link{zenplot}()}.
##' @author Marius Hofert and Wayne Oldford
##' @seealso \code{\link{zenplot}()} which provides the zenplot.
##' @export
##' @examples
##' ## get a matrix
##' x <- matrix(1:15, ncol = 3)
##' colGroups <- list(c(1,2), list(2:3))
##' rowGroups <- list(c(1,4), list(2:3))
##' groupData(x, indices = colGroups)
##' groupData(x, indices = rowGroups, byrow = TRUE)
##'
##'
groupData <- function(x, indices, byrow = FALSE)
{
    if(length(dim(x)) != 2)
       stop("'x' needs to have two dimensions")
    stopifnot(is.list(indices))
    if(byrow)
        lapply(indices, function(ii) x[unlist(ii), , drop = FALSE])
    else lapply(indices, function(ii) x[, unlist(ii), drop = FALSE])
}

##' @title Indexing a Matrix or Data Frame According to Given Indices
##' @usage indexData(x, indices)
##' @family tools related to constructing zenpaths
##' @param x A \code{\link{matrix}} or \code{\link{data.frame}}
##'       (most useful for the latter).
##' @param indices vector of column indices of \code{x}
##'        (typically obtained from \code{\link{zenpath}()}).
##' @return An object as \code{x}
##'        (typically a \code{\link{data.frame}} or
##'         \code{\link{matrix}}) containing \code{x}
##'         indexed by \code{indices}.
##' @author Marius Hofert and Wayne Oldford
##' @note Useful for constructing data.frames without .1, .2, ... in their
##'       names when indexing a data.frame with a zenpath.
##' @seealso \code{\link{zenplot}()} which provides the zenplot.
##' @export
##' @examples
##' ## The function is handiest for data frames
##' ## where we want to reuse the variable names
##' ## without adding a suffix like ".1" etc.
##' ## For example,
##' x <-  BOD  # Biochemical Oxygen Demand data in base R
##' indices <- rep(1:2, 2)
##' ## now compare
##' indexData(x, indices)
##' ## to
##' x[, indices]
##' ## zenplots prefer not to have the suffixes.
##'
indexData <- function(x, indices)
{
    if(length(dim(x)) != 2)
       stop("'x' needs to have two dimensions")
    res <- x[, indices]
    names(res) <- names(x)[indices]
    res
}



## \references{
##   Hofert, M., Oldford, W. (2015). Zigzag Expanded Navigation Plots.
##   \emph{} \bold{}(), --.
## }

##' @title Construct a Path of Indices to Order Variables
##' @usage
##' zenpath(x, pairs = NULL,
##'         method = c("front.loaded", "back.loaded",
##'                    "balanced", "eulerian.cross",
##'                    "greedy.weighted", "strictly.weighted"),
##'         decreasing = TRUE)
##' @family tools related to constructing zenpaths
##' @description Constructing zenpaths and tools for extracting,
##' connecting and displaying pairs, as well as
##' grouping and indexing data structures.
##' @name zenpath
##' @aliases zenpath
##' @param x
##' \describe{for \code{method}
##'         \describe{
##'             \item{\code{"front.loaded"}:}{single \code{\link{integer}} >= 1.}
##'             \item{\code{"back.loaded"}:}{as for \code{method = "front.loaded"}.}
##'             \item{\code{"balanced"}:}{as for \code{method = "front.loaded"}.}
##'             \item{\code{"eulerian.cross"}:}{two \code{\link{integer}}s >= 1
##'                 representing the group sizes.}
##'             \item{\code{"greedy.weighted"}:}{\code{\link{numeric}} weight
##'                 \code{\link{vector}} (or
##'                                       \code{\link{matrix}} or distance matrix).}
##'             \item{\code{"strictly.weighted"}:}{as for
##'                 \code{method = "greedy.weighted"}.}
##'         }
##'     }
##'
##' @param pairs a two-column \code{\link{matrix}} containing (row-wise)
##' the pairs of connected variables to be sorted according to the
##' weights. Note that the resulting graph must be connected 
##' (i.e. any variable can be reached from any other variable 
##' following the connections given by \code{pairs}).
##' The \code{pairs} argument is only used for the \code{method}s
##' \code{greedy.weighted} and \code{strictly.weighted} and can be
##' \code{NULL} (in which case a default is constructed in lexicographical order).
##' @param method \code{\link{character}} string indicating the sorting
##' method to be used. Available methods are:
##' \describe{
##'        \item{\code{"front.loaded"}:}{Sort all pairs such that the first variables appear
##'                        the most frequently early in the sequence;
##'                        an Eulerian path; note that it might be slightly
##'                        longer than the number of pairs because, first, an even
##'                        graph has to be made.}
##'        \item{\code{"back.loaded"}:}{Sort all pairs such that the later variables appear
##'                       the most frequently later in the sequence;
##'                       an Eulerian path (+ see front.loaded concerning length)}
##'        \item{\code{"balanced"}:}{Sort all pairs such that all variables appear in
##'                    balanced blocks throughout the sequence
##'                    (a Hamiltonian Decomposition; Eulerian, too).}
##'        \item{\code{"eulerian.cross"}:}{Generate a sequence of pairs such that
##'                          each is formed with one variable from each group.}
##'        \item{\code{"greedy.weighted"}:}{Sort all pairs according to a greedy (heuristic)
##'                             Euler path with \code{x} as weights visiting each
##'                             edge precisely once.}
##'        \item{\code{"strictly.weighted"}:}{
##'         Strictly respect the order of the weights - so the first, second,
##'         third, and so on, adjacent pair of numbers of the output of
##'         \code{zenpath()} corresponds to the pair with largest,
##'         second-largest, third-largest, and so on, weight.
##'        }
##'        }
##' @param decreasing A \code{\link{logical}} indicating whether the
##' sorting is done according to increasing or decreasing weights.
##' @return Returns a sequence of variables (indices or names,
##' possibly a list of such), which can then be used to index the data
##' (via \code{\link{groupData}()}for plotting via \code{\link{zenplot}()}.
##' @author Marius Hofert and Wayne Oldford
##' @seealso \code{\link{zenplot}()} which provides the zenplot.
##' @export
##' @examples
##' ## Some calls of zenpath()
##' zenpath(10) # integer argument
##' ## Note that the result is of length 50 > 10 choose 2 as the underlying graph has to
##' ## be even (and thus edges are added here)
##' (zp <- zenpath(c(3, 5), method = "eulerian.cross")) # integer(2) argument
##'
zenpath <- function(x, pairs = NULL,
                    method = c("front.loaded", "back.loaded", "balanced",
                               "eulerian.cross", "greedy.weighted",
                               "strictly.weighted"),
                    decreasing = TRUE)
{
    method <- match.arg(method)
    switch(method,
    "front.loaded" = {

        stopifnot(is.numeric(x))
        if(length(x) != 1 || x %% 1 != 0 || x < 1)
            stop("'x' has to be an integer >= 1 for method = \"front.loaded\".")
        if(x > 1) rev((x:1)[eseq(x)]) else 1

    },
    "back.loaded" = {

        stopifnot(is.numeric(x))
        if(length(x) != 1 || x %% 1 != 0 || x < 1)
            stop("'x' has to be an integer >= 1 for method = \"back.loaded\".")
        if(x > 1) eseq(x) else 1

    },
    "balanced" = {

        stopifnot(is.numeric(x))
        if(length(x) != 1 || x %% 1 != 0 || x < 1)
            stop("'x' has to be an integer >= 1 for method = \"balanced\".")
        if(x > 1) hpaths(x, matrix = FALSE) else 1

    },
    "eulerian.cross" = {

        stopifnot(is.numeric(x))
        if(length(x) != 2 || any(x %% 1 != 0) || any(x < 1))
            stop("'x' has to be an integer vector of length 2 with entries >= 1.")
        g1 <- seq_len(x[1])
        g2 <- x[1] + seq_len(x[2])
        as.numeric(eulerian(bipartite_graph(g1, g2)))

    },
    "greedy.weighted" =, "strictly.weighted" = {

        ## Deal with missing 'x' if pairs are given
        if(missing(x)) {
            if(missing(pairs))
                stop("'pairs' need to be specified for method = ",method,".")
            if(!is.matrix(pairs)) pairs <- as.matrix(pairs)
            ## Now pairs are given but 'x' is missing => construct 'x'
            x <- 1:nrow(pairs) # 'decreasing' is dealt with differently for the different methods
        }

        ## If 'x' is a matrix or distance matrix, convert it to a vector
        if(is.matrix(x) || inherits(x, "dist"))
            x <- as.vector(as.dist(x)) # => lower triangular matrix as vector

        ## Check
        if(!is.vector(x))
            stop("'x' needs to be a vector (or matrix, or distance matrix).")

        ## Check pairs
        if(is.null(pairs)) {
            ## Check if length(x) is of the form 'n*(n-1)/2'
            nVars <- (1+sqrt(1+8*length(x)))/2
            if(nVars %% 1 != 0)
                stop("'x' has to be of length n*(n-1)/2 for some n >= 2.")
            ## Build matrix of (all) pairs
            pairs <- expand.grid(1:nVars, 1:nVars)
            pairs <- pairs[pairs[,1] > pairs[,2],] # => Pairs = (2, 1), (3, 1), ... (=> lower triangular matrix)
            pairs <- as.matrix(pairs)
            rownames(pairs) <- NULL
            colnames(pairs) <- NULL
        }
        if(!is.matrix(pairs)) pairs <- as.matrix(pairs)
        nr <- nrow(pairs)
        stopifnot(length(x) == nr, nr >= 1, ncol(pairs) == 2)
        ## Check whether none of the edges is given more than once (fail-safe programming)
        ## Note: pairs. is not used anymore below!
        pairs. <- pairs
        for(i in 1:nr) { # sort the pairs (only for checking)
            if(pairs.[i,1] < pairs.[i,2]) {
                tmp <- pairs.[i,1]
                pairs.[i,1] <- pairs.[i,2]
                pairs.[i,2] <- tmp
            }
        }
        if(nrow(unique(pairs.)) != nrow(pairs.)) # use sorted 'pairs' to unique-ify and check
            stop("'pairs' needs to have unique rows (possibly after applying rev()).")

        ## Now distinguish between the methods
        if(method == "greedy.weighted") { # method "greedy.weighted"

            if(decreasing) x <- -x
            eul <- eulerian(ftM2graphNEL(ft = pairs, W = x, edgemode = "undirected"))
            eul.lst <- lapply(eul, as.numeric) # returns character otherwise
            if(is.vector(eul)) as.numeric(eul) else eul.lst

        } else { # method "strictly.weighted"

            pairs. <- pairs[order(x, decreasing = decreasing),] # sort pairs according to decreasing/increasing weights
            lst.indices <- split(pairs., f = row(pairs.)) # result list of variables/indices
            names(lst.indices) <- NULL # remove names
            lst.indices

        }

    },
    stop("Wrong 'method'"))
}

