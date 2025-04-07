

dxs <- read.csv("data/final/XCagg.csv")
#cor(dxs$geodst, dxs$envdst, method="pearson")

tiff(paste0("plots/", "Fig4.tif"), 1100, 1300, pointsize=30)

plot(dxs$geodst, dxs$envdst, xlab = "Geographic distance (km)", ylab = "Environmental distance (km)", xlim=c(0,800), ylim=c(0,1600), axes=FALSE, yaxs = "i", xaxs = "i")
axis(1)
axis(2, at=seq(0,1600,400), las=1)

m <- lm(envdst ~ geodst, data=dxs)
cf <- round(coefficients(m), 2)
txt <- paste0("y = ", round(cf[1],1), " + ", cf[2], " 	x")
abline(m, col="red", lty=2, lwd=1.5)
text(700, 925, substitute(italic(txt), list(txt=txt)), col="red", cex=.8, xpd=TRUE, srt=25)
abline(0, 1, lwd=2, col="gray")
text(700, 750, substitute(italic("y=x")), col="dark gray", srt=25)

# Plot species of note
spsp <- c("debanensis", "venulosa", "membranacea", "mudenia",  "longifolia", "platyloba", "monantha", "pygmaea")
sp <- dxs[dxs$sp %in% spsp, ]
text(sp$geodst, sp$envdst, sp$sp, pos=4, cex=0.7)
text(dxs$geodst[dxs$sp == "bequaertii"], dxs$envdst[dxs$sp == "bequaertii"], "bequaertii", pos=2, cex=0.7)
text(dxs$geodst[dxs$sp == "microsperma"], dxs$envdst[dxs$sp == "microsperma"], "microsperma", pos=3, cex=0.7)
text(dxs$geodst[dxs$sp == "juncea"], dxs$envdst[dxs$sp == "juncea"], "juncea", pos=1, cex=0.7)
text(dxs$geodst[dxs$sp == "platyloba"], dxs$envdst[dxs$sp == "platyloba"], "platyloba", pos=4, cex=0.7)

dev.off()

