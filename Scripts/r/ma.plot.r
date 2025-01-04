function (A, M, subset = sample(1:length(M), min(c(10000, length(M)))), 
    show.statistics = TRUE, span = 2/3, family.loess = "gaussian", 
    cex = 2, plot.method = c("normal", "smoothScatter", "add"), 
    add.loess = TRUE, lwd = 1, lty = 1, loess.col = "red", ...) 
{
    plot.method <- match.arg(plot.method)
    fn.call <- list(...)
    sigma <- IQR(M)
    mean <- median(M)
    if (!is.element("ylim", names(fn.call))) {
        yloc <- max(M)
    }
    else {
        yloc <- max(fn.call$ylim)
    }
    if (!is.element("xlim", names(fn.call))) {
        xloc <- max(A)
    }
    else {
        xloc <- max(fn.call$xlim)
    }
    if (plot.method == "smoothScatter") {
        plotmethod <- "smoothScatter"
    }
    else if (plot.method == "add") {
        plotmethod <- "add"
    }
    else {
        plotmethod <- "normal"
    }
    aux <- loess(M[subset] ~ A[subset], degree = 1, span = span, 
        family = family.loess)$fitted
    if (plotmethod == "smoothScatter") {
        smoothScatter(A, M, ...)
    }
    else if (plotmethod == "add") {
        points(A, M, cex = cex, ...)
    }
    else {
        plot(A, M, cex = cex, ...)
    }
    if (add.loess) {
        o <- order(A[subset])
        A <- A[subset][o]
        M <- aux[o]
        o <- which(!duplicated(A))
        lines(approx(A[o], M[o]), col = loess.col, lwd = lwd, 
            lty = lty)
    }
    abline(0, 0, col = "blue")
    if (show.statistics) {
        txt <- format(sigma, digits = 3)
        txt2 <- format(mean, digits = 3)
        text(xloc, yloc, paste(paste("Median:", txt2), paste("IQR:", 
            txt), sep = "\n"), cex = cex, adj = c(1, 1))
    }
}
<bytecode: 0x7fa0643efe98>
<environment: namespace:affy>
