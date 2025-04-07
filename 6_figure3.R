
#Figure 3: XC scores of wild African Vigna species.

s <- read.csv("table/summary_table.csv")
s <- s[order(s$XCadj), ]

tiff(paste0("plots/", "Fig3.tif"), 1100, 1300, pointsize=36)

par(mar=c(4,1,1,3))
plot(s$XCadj, 1:nrow(s), cex=.5, pch=20, axes=FALSE, xlab="XC scores", ylab="", xlim=c(0, 1))
text(s$XCadj, 1:nrow(s), s$species, cex=.4, pos=4, xpd=T)
axis(1)

dev.off()


