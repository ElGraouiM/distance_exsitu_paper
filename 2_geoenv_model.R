
dir.create("plots", FALSE, TRUE)

# get species data
sv <- XSdata::vigna_occ()
#sv <- readRDS("data/intermediate/unique_occurrences.rds")
sv <- terra::vect(sv, c("lon", "lat"), crs="+proj=longlat")

env <- terra::rast("data/intermediate/wc.tif")[[c("bio_1", "bio_12")]]
names(env) <- c("tmp", "prc")

##### create envdist function 
cells <- terra::cellFromXY(terra::rast(), terra::crds(sv))
smp <- sv[!duplicated(cells), ]
gd <- round(terra::distance(smp, unit="km"))
x <- terra::extract(env, smp, ID=F)
ed1 <- dist(x[,1])
ed2 <- dist(x[,2])
g <- data.frame(g=gd, tmp=round(ed1, 1), prc=round(ed2))

a1 <- aggregate(g~tmp, data=g, median)
a2 <- aggregate(g~prc, data=g, median)

b1 <- aggregate(tmp~g, data=g, median)
b2 <- aggregate(prc~g, data=g, median)

mtmp <- loess(g~tmp, data=b1)
p <- predict(mtmp)
e <- b1$g - p
itmp <- e < 250
mtmp <- loess(g~tmp, data=b1[itmp,], span=0.25)

mprc = loess(g~prc, data=b2)
p = predict(mprc)
e = b2$g - p
iprc = e < 250
mprc = loess(g~prc, data=b2[iprc,], span=.25)

saveRDS(mprc, "data/intermediate/m_prc.rds")
saveRDS(mtmp, "data/intermediate/m_tmp.rds")


###############################################
########### figure for supplement ############

tiff(paste0("plots/", "S6_Fig.tif"), 1200, 1200, pointsize=24)

par(mfrow=c(1,2))
tmp <- seq(0, 13, 0.1)
p <- predict(mtmp, data.frame(tmp=tmp))
plot(b1[,2:1], cex=.1, xlim=c(0,13), yaxs="i", xaxs="i", xlab="Temperature difference (°C)", ylab="Geographic distance (km)", col="gray")
points(b1[itmp,2:1], cex=.1, col="blue")
lines(tmp, p, col="red", lwd=2)
legend("bottomright", c("used", "not used"), col=c("blue", "gray"), pch=20, cex=1, pt.cex=.5)

prc <- seq(0, 2100, 10)
p <- predict(mprc, data.frame(tmp=prc))
plot(b2[,2:1], cex=.1, xlim=c(0,2000), yaxs="i", xaxs="i", xlab="Precipitation difference (mm)", ylab="Geographic distance (km)", col="gray")
points(b2[iprc,2:1], cex=.1, col="blue")
lines(prc, p, col="red", lwd=2)
legend("bottomright", c("used", "not used"), col=c("blue", "gray"), pch=20, cex=1.25, pt.cex=.5)

dev.off()

