

# fig 6. sample size vs. FCS and XC

tab <- read.csv("table/summary_table.csv")
tiff(paste0("plots/", "Fig6.tif"), 1200, 1200, pointsize=36)

plot(tab$nseed, tab$XCadj, xlab="Number of seedbank samples (log scale)", ylab="Score", las=1, col="red", log="x")
points(tab$nseed, tab$FCS, col="blue", las=1, xlim=c(0,1), pch=2, cex=0.75)
legend("topleft", legend = c("XC", "FCS"), col=c("red", "blue"), pch=c(1, 2), cex=0.75)

dev.off()

