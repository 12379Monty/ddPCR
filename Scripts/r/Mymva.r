Myma.plot <- function (A, M, subset = sample(1:length(M), min(c(10000, length(M)))), 
    show.statistics = TRUE, span = 2/3, family.loess = "gaussian", 
    cex = 2, plot.method = c("normal", "smoothScatter", "add"), 
    add.loess = TRUE, lwd = 1, lty = 1, loess.col = "red", digits=3, add.iqr=T,...) 
{
    plot.method <- match.arg(plot.method)
    fn.call <- list(...)
    sigma <- IQR(M,na.rm=T)
    mean <- median(M, na.rm=T)
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
    if(add.iqr) legend('topright', legend=round(sigma,digits), bty='n')
    abline(0, 0, col = "blue")
    if (show.statistics) {
        txt <- format(sigma, digits = 3)
        txt2 <- format(mean, digits = 3)
		text(xloc, yloc, paste(paste("Med:", txt2), paste("IQR:", 
		    txt), sep = "\n"), cex = cex, adj = c(1, 1))
	    }
	}#Myma.plot
	##############################################################

	Mymva.pairs <- 
	function (x, labels = colnames(x), log.it = TRUE, span = 2/3, 
	    family.loess = "gaussian", digits = 3, line.col = 2, main = "MVA plot", 
	    cex = 2, ...) 
	{
	    if (log.it) 
		x <- log2(x)
    J <- dim(x)[2]
    frame()
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    par(mfrow = c(J, J), mgp = c(0, 0.2, 0), mar = c(1, 1, 1, 
        1), oma = c(1, 1.4, 2, 1))
    for (j in 1:(J - 1)) {
        par(mfg = c(j, j))
        plot(1, 1, type = "n", xaxt = "n", yaxt = "n", xlab = "", 
            ylab = "")
        text(1, 1, labels[j], cex = cex)
        for (k in (j + 1):J) {
            par(mfg = c(j, k))
            yy <- x[, j] - x[, k]
            xx <- (x[, j] + x[, k])/2
            sigma <- IQR(yy,na.rm=T)
            mean <- median(yy, na.rm=T)
            Myma.plot(xx, yy, tck = 0, show.statistics = FALSE, 
                pch = ".", xlab = "", ylab = "", tck = 0, span = span, digits=digits,
                ...)
            par(mfg = c(k, j))
            txt <- format(sigma, digits = digits)
            txt2 <- format(mean, digits = digits)
            plot(c(0, 1), c(0, 1), type = "n", ylab = "", xlab = "", 
                xaxt = "n", yaxt = "n")
            text(0.5, 0.5, paste(paste("Med:", txt2), paste("IQR:", 
                txt), sep = "\n"), cex = cex)
        }
    }
    par(mfg = c(J, J))
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", xlab = "", 
        ylab = "")
    text(1, 1, labels[J], cex = cex)
    mtext("A", 1, outer = TRUE, cex = 1.5)
    mtext("M", 2, outer = TRUE, cex = 1.5, las = 1)
    mtext(main, 3, outer = TRUE, cex = 1.5)
    invisible()
}# Mymva.pairs <- 
################################################

