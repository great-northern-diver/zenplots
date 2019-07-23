##' @title Find the Pairs with Smallest (or Largest) Entry in the (Lower)
##'        Triangular Area of a Symmetric Matrix
##' @param x A symmetric matrix (of distances)
##' @param n Number of extreme values to be returned
##' @param method A character string indicating the method to be used
##' @param decreasing A logical indicating whether the sorting is done in
##'        decreasing order (the default)
##' @param use.names A logical indicating whether colnames(x) are to be
##'        used (if not NULL)
##' @return A (n, 3)-matrix with the n largest/smallest/both
##'         values in the symmetric matrix x (3rd column) and the
##'         corresponding indices (1st and 2nd column)
##' @author Marius Hofert and Wayne Oldford
extreme_pairs <- function(x, n = 6, method = c("largest", "smallest", "both", "all"),
                          decreasing = TRUE, use.names = FALSE)
{
    ## Checks
    if(!is.matrix(x)) x <- as.matrix(x)
    d <- ncol(x)
    method <- match.arg(method)
    stopifnot(n >= 1, d >= 2, nrow(x) == d, is.logical(use.names))

    ## Build (row, col)-matrix
    ind <- as.matrix(expand.grid(1:d, 1:d)[,2:1])
    ind <- ind[ind[,1]<ind[,2],] # pick out indices as they appear in the upper triangular matrix
    colnms <- colnames(x)
    if(use.names && !is.null(colnms))
        ind <- matrix(colnms[as.matrix(ind)], ncol = 2)

    ## Merge with entries to a (row, col, value)-data frame
    ## Note that, since x is symmetric, values of the *lower* triangular
    ## matrix as a vector matches indices in 'ind'
    val <- data.frame(ind, x[lower.tri(x)], stringsAsFactors = FALSE)
    colnames(val) <- c("row", "col", "value")
    rownames(val) <- NULL

    ## Now grab out the 'extreme' pairs and values
    ii <- 1:nrow(ind) # d*(d-1)/2 pairs
    switch(method,
    "largest" = {
        if(n > nrow(ind))
            stop("'n' needs to be less than or equal to the number of pairs (",nrow(ind),")")
        res <- val[order(val[,3], decreasing = decreasing),] # sort data frame according to values
        if(decreasing) {
            res[head(ii, n = n),] # the largest values (decreasing order = large to small)
        } else {
            res[tail(ii, n = n),] # the largest values (increasing order = small to large)
        }
    },
    "smallest" = {
        if(n > nrow(ind))
            stop("'n' needs to be less than or equal to the number of pairs (",nrow(ind),")")
        res <- val[order(val[,3], decreasing = decreasing),] # sort data frame according to values
        if(decreasing) {
            res[rev(tail(ii, n = n)),] # the smallest values (increasing order = small to large)
        } else {
            res[rev(head(ii, n = n)),] # the smallest values (decreasing order = large to small)
        }
    },
    "both" = {
        if(n > floor(nrow(ind)/2))
            stop("'n' needs to be less than or equal to the number of pairs half (",floor(nrow(ind)/2),")")
        res <- val[order(val[,3], decreasing = decreasing),] # sort data frame according to values
        res[c(head(ii, n = n), tail(ii, n = n)),]
        ## For decreasing = TRUE, this is: largest values (large to small) and smallest values (large to small)
        ## For decreasing = FALSE, this is: smallest values (small to large) and largest values (small to large)
    },
    "all" = {
        val[order(val[,3], decreasing = decreasing),] # sort data frame according to values
    },
    stop("Wrong 'method'"))
}

##' @title Compute a Graph Showing the Pairs with Largest (or Smallest)
##'        n Values in a Symmetric Matrix
##' @param x Symmetric matrix
##' @param n Number of extreme values to be returned
##' @param method A character string indicating the method to be used
##' @param decreasing A logical indicating whether the sorting is done in
##'        decreasing order (the default)
##' @param use.names A logical indicating whether colnames(x) are to be
##'        used (if not NULL)
##' @return graphNEL object (can be plotted with Rgraphviz)
##' @author Marius Hofert and Wayne Oldford
extreme_pairs_graph <- function(x, n = 6, method = c("largest", "smallest", "both", "all"),
                                decreasing = TRUE, use.names = FALSE)
{
    ## Call extreme_pairs_graph(); returns (row, col, value)-matrix
    ## containing the 'extreme' pairs
    xtr.pairs <- as.matrix(extreme_pairs(x, n = n, method = method,
                                         decreasing = decreasing, use.names = FALSE))
    colnames(xtr.pairs) <- NULL # remove 'row', 'col', 'value'

    ## Build vertex names
    ft <- xtr.pairs[,1:2] # from/to
    v.ind <- sort(unique(as.vector(ft))) # indices of vertices
    colnms <- colnames(x)
    v.nms <- if(use.names && !is.null(colnms)) { # determine vertex names
        ft[,1:2] <- colnms[ft[,1:2]] # put the variable names in ft
        colnms[v.ind]
    } else as.character(v.ind)

    ## Build graph
    ftM2graphNEL(ft, W = xtr.pairs[,3], V = v.nms, edgemode = "undirected") # possibly uneven, disconnected
}





