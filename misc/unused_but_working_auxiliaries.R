##' @title Creating a Gray Color with Alpha Blending
##' @param n a number determining the alpha levels if alpha is NULL
##'        (= number of distinguishable layers on each pixel); n/10 = number of
##'        points needed to saturate
##' @param h see ?hcl
##' @param c see ?hcl
##' @param l see ?hcl
##' @param alpha see ?hcl; if NULL, then alpha is determined from 'n'
##' @param fixup see ?hcl
##' @return hcl alpha blended gray color
##' @author Marius Hofert and Wayne Oldford
gray_alpha_blend <- function(n, h = 260, c = 0, l = 65, alpha = NULL, fixup = TRUE)
    hcl(h, c = c, l = l, alpha = if(is.null(alpha)) 1/(n/10 + 1) else alpha, fixup = fixup)

##' @title Scale Data to Component-Wise Be in [0,1]
##' @param x A matrix or data.frame or a list of vectors
##' @param method character string indicating the method to be used
##'        scale all vectors of x with the same linear
##'        transformation or scale each vector of x itself
##' @param ... additional arguments passed to range() or rank()
##' @return x scaled to [0,1]
##' @author Marius Hofert
##' @note Pass through NA columns
scale01 <- function(x, method = c("columnwise", "all", "pobs"), ...)
{
    ## Convert inputs to a list of (column) vectors
    is.mat <- is.matrix(x)
    is.df <- is.data.frame(x)
    if(is.mat || is.df)
        x <- as.list(as.data.frame(x))
    len <- length(x)
    stopifnot(is.list(x), len >= 1)
    ## Note: When called from zenplot(), 'x' is already a list (but could
    ##       potentially contain factors. We deal with them as pairs() does.
    for(i in seq_len(len)) {
        if(is.factor(x[[i]]) || is.logical(x[[i]]))
            x[[i]] <- as.numeric(x[[i]])
        if(!is.numeric(unclass(x[[i]])))
            stop("Non-numeric argument to scale01()")
    }

    ## Scale
    method <- match.arg(method)
    res <- switch(method,
    "columnwise" = {
        lapply(x, function(x.) {
            if(all(is.na(x.))) return(x.) # no scaling done
            ran <- range(x., na.rm=TRUE, ...)
            dff <- diff(ran)
            if(dff==0)
                stop("Cannot scale non-NA data 'x': Division by 0 (as diff(range()) == 0)") # all non-NA data points are the same
            (x.-ran[1])/dff
        })
    },
    "all" = {
        x.vals <- unlist(x)
        if(all(is.na(x.vals))) return(x) # no scaling done
        ran <- range(x.vals, na.rm=TRUE, ...)
        dff <- diff(ran)
        if(dff==0)
            stop("Cannot scale non-NA data 'x': Division by 0 (as diff(range()) == 0)") # all non-NA data points are the same
        lapply(x, function(x.) (x.-ran[1])/diff(ran))
    },
    "pobs" = {
        lapply(x, function(x.) rank(x., na.last="keep", ...)/(length(x.)+1))
    },
    stop("Wrong 'method'"))

    ## Return
    if(is.mat) {
        matrix(unlist(res), ncol=length(res))
    } else if(is.df) {
        as.data.frame(res)
    } else res
}


