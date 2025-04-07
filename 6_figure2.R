
#### Fig 2.  number of observations per species

library(terra)

vignas <- XSdata::vigna_occ()

tab1 <- as.data.frame(table(vignas$species))
colnames(tab1) <- c("spp", "n")
tab2 <- as.data.frame(table(vignas$species[vignas$sample_type=="seed"]))
colnames(tab2) <- c("spp", "seed")
d <- merge(tab1, tab2, by="spp", all.x=TRUE)
d$seed[is.na(d$seed)] <- 0
d <- d[order(d$n), ]

afr <- terra::readRDS("data/intermediate/africa.rds")
sv <- terra::vect(vignas, crs="+proj=longlat")

tiff(paste0("plots/", "Fig2.tif"), 2600, 1400, pointsize=32)

par(mfrow=c(1, 2), mar=c(3, 1, 1, 3))
par(mfrow=c(1, 2), mar=c(5, 1.5, 1, 3))

terra::plot(afr, col="gray95", border="gray", las=1, ylim=c(-35,25), mar=c(1,1.5,0,0), box=F, axes=F)
terra::plot(sv[sv$sample_type == "non-seed",], add=TRUE, legend="bottomleft", col=rgb(1, 0, 0, 0.2), pch=20, cex=.5)
terra::plot(sv[sv$sample_type == "seed",], add=TRUE, legend="bottomleft", col="blue", pch=20, cex=.5)
lines(afr, col="gray", lwd=2)
# Add scale
terra::sbar(1000, c(-20, -34), cex=.8, below="km")
##Add arrow
terra::north(xy=c(-15, -15), type=1, angle=180, label="S")
text(45, -34, "(A)", cex=1.25, xpd=TRUE)
legend(x = 36, y = 30, legend = c("seed", "other"), col = c("blue", "red"), title = "Sample type", pch = 20, bty = "n", cex = 1.2, xpd = TRUE)


sq <- seq(0,2750,250)
plot(d$n, 1:nrow(d), las=1, cex=1, col="red", axes=F, xlab="Number of samples per species", pch=20, ylab="")
points(d$seed, 1:nrow(d), pch=20, col="blue")
x <- sapply(1:nrow(d), \(i) lines(rbind(cbind(d[i, "seed"], i), cbind(d[i, "n"], i))))

axis(1, at=sq, cex.axis=.7, labels=F)
text(sq, -4, sq, cex=.6, xpd=T)
text(d$n, 1:nrow(d), d$spp, cex=.6, pos=4, xpd=T, col="black")
text(2500, 3, "(B)", cex=1.25, xpd=TRUE)

dev.off()

