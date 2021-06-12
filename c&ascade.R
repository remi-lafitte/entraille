a <- setNames(c(35, 55, 65, 75, 80, 80), letters[1:6])
a



barplot(a, main="Manual tuning", ylab="y label", col="#000080", border=NA, space=0.35)
## helper builds a string describing a range
range.string <- function(x) {
  paste0("Data range is [", min(x), ", ", max(x), "]")
}
## barplot with several custom settings and components
base.barplot.1 <- function(x, main="Custom plot function", ylab="y label") {
  ## start with a plot with bars, but nothing else
  barpos <- barplot(x, col="#000080", axes=FALSE, axisnames=FALSE,
                    border=NA, space=0.35)
  ## add custom components
  axis(1, at=barpos[,1], labels=names(x), lwd=0, col="#111111", cex.axis=1.2,
       line=-0.35)
  axis(2, col.ticks="#444444", col.axis="#444444", cex.axis=1.2, lwd=1.2, las=1,
       tck=-0.03, lwd.ticks=1.2)
  mtext(main, adj=0, line=2.2, cex=1.1)
  mtext(range.string(x), adj=0, line=0.9, cex=0.8, col="#444444")
  mtext(ylab, side=2, cex=0.8, line=3, col="#444444")
}
base.barplot.1(a)
