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
            mean <- median(yy)
            ma.plot(xx, yy, tck = 0, show.statistics = FALSE, 
                pch = ".", xlab = "", ylab = "", tck = 0, span = span, 
                ...)
            par(mfg = c(k, j))
            txt <- format(sigma, digits = digits)
            txt2 <- format(mean, digits = digits)
            plot(c(0, 1), c(0, 1), type = "n", ylab = "", xlab = "", 
                xaxt = "n", yaxt = "n")
            text(0.5, 0.5, paste(paste("Median:", txt2), paste("IQR:", 
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
}
<bytecode: 0x7fa060af1518>
<environment: namespace:affy>
