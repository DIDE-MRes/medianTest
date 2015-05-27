
#' Monte Carlo Median test
#'
#' This function tests the equality of medians of two groups of observations.
#' The statistic used is the difference between the medians of the two groups: median(group1) - median(group2).
#'
#'
#' @param x a numeric vector
#' @param fac a factor defining the two groups
#' @param n the number of permutations
#' @param ... other arguments passed to \code{as.randtest}
#'
#' @importFrom ade4 as.randtest
#'
#' @export
#'
#' @examples
#' ## simulate data
#' set.seed(1)
#' titer <- c(rexp(100, rate=0.002), rexp(20, rate=0.0005), # control
#'        rexp(55, rate=0.006), rexp(15, rate=0.0005)) # treatment
#'
#' type <- factor(rep(c("control","treatment"), c(120,70)))
#'
#' VL <- data.frame(titer=titer, type=type)
#'
#' ## plot data
#' if(require(ggplot2)){
#' ggplot(VL, aes(x=titer)) + geom_density(aes(fill=type), alpha=.5, adjust=4)
#' }
#'
#' ## compare t.test and median test
#' t.test(titer~type)
#'
#' medianTest(titer,type)
#'

medianTest <- function(x, fac, n=999, ...){
    ## check dependencies
    if(!require(ade4)) stop("ade4 is not installed")

    ## function to get the statistic
    f1 <- function(x, fac) {
        medians <- tapply(x, fac, median)
        return(medians[1] - medians[2])
    }

    ## get original statistic
    stat.ori <- f1(x,fac)

    ## get permuted values
    stat.perm <- replicate(n, f1(x, sample(fac)))

    ## return output
    out <- as.randtest(sim=stat.perm, obs=stat.ori, ...)
    return(out)
}
