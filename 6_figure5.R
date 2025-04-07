
#Figure 5: SDM Vs. Adjusted range size

tab <- read.csv("table/summary_table.csv")
tab$range_size <- tab$range_size / 1000000
tab$suitable_area <- tab$suitable_area / 1000000

# remove three cases where SDM predicts all of Africa to be suitable due to a lack of points
tab <- tab[tab$suitable_area <= 30, ]


tiff(paste0("plots/", "Fig5.tif"), 1200, 1200, pointsize=36)

par(mar = c(4.5, 4.5, 2, 2))

plot(tab$suitable_area, tab$range_size, xlab=expression("Suitable area (10"^6*" km"^2*")"),
		ylab=expression("Range size (10"^6*" km"^2*")"), col="red", las=1, cex=1, pch=20, xlim=c(0,13), ylim=c(0,13))
abline(0,1, lwd=2, col="gray")
text(12, 11.5, substitute(italic("y=x")), col="dark gray", srt=45, cex=1.25)

dev.off()

