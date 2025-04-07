

library(terra)

sv <- XSdata::vigna_occ()
sp <- "friesiorum"
sv <- sv[sv$species == sp,]
sv <- terra::vect(sv, c("lon", "lat"), crs="+proj=longlat")
sv$sample_type <- "non-seed"
sv$sample_type[30] <- "seed"
sv$sample_type[8] <- "seed"
sv$sample_type[16] <- "seed"

r <- terra::rast(paste0("data/intermediate/sdm/range/range_", sp, ".tif"))
xy <- c(terra::init(r, "x"), terra::init(r, "y"))
deval <- read.csv("data/intermediate/sdm/evaluation.csv")
afr <- terra::readRDS("data/intermediate/africa.rds")

env <- terra::rast("data/intermediate/wc.tif")[[c("bio_1", "bio_12")]]
names(env) <- c("tmp", "prc")

envdist <- function(x) {
	x$tmp[x$tmp > 13] <- 13 
	x$prc[x$prc > 2000] <- 2000
	p_tmp <- predict(mtmp, x)
	p_pr <- predict(mprc, x)
	rowMeans(cbind(p_tmp, p_pr))
}

mprc <- readRDS("data/intermediate/m_prc.rds")
mtmp <- readRDS("data/intermediate/m_tmp.rds")

tercols <- rev(terrain.colors(25))
th <- deval$equal_sens_spec[deval$species == sp] |> as.numeric()
svsp <- sv[sv$species==sp, ]
seed <- svsp[svsp$sample_type=="seed", ]
ca100 <- terra::aggregate(terra::buffer(svsp, 100000)) |> terra::crop(afr)
ca250 <- terra::aggregate(terra::buffer(svsp, 250000)) |> terra::crop(afr)
r_sdm <- terra::rast(paste0("data/intermediate/sdm/raw/raw_", sp, ".tif")) > th
  
r_100 <- terra::rasterize(ca100, r_sdm, background=0) * 3
m_sdm <- sum(c(r_sdm, terra::mask(r_sdm, ca250)), na.rm=TRUE)
sdm <- terra::mask(terra::ifel(m_sdm == 0, r_100, m_sdm), m_sdm)
  
adj_range <- terra::rast(paste0("data/intermediate/sdm/range/range_", sp, ".tif"))
  
k <- conexus::get_samplesize(adj_range, omega=1/40) 
set.seed(3388)
zones <- try(conexus::make_zones(xy, k$range, k$n, spread=TRUE))

x <- terra::centroids(zones, inside=TRUE)
e <- terra::ext(ca250) + .5

# adjust to square to fill up the available space   
p <- as.vector(e)
a <- c(diff(p[3:4]), diff(p[1:2]))
e <- e * rep((a / min(a)), each=2)
  
# network
XC0 <- conexus::XC(zones, seed[0,], env, envdist)
network0 <- conexus::dst2svect(XC0$dist)
XC <- conexus::XC(zones, seed, env, envdist)
network <- conexus::dst2svect(XC$dist)

tiff("plots/Fig1.tif", 1200, 1200, pointsize=28)

  par(mfrow=c(2,2), mar=rep(.5, 4))
  
  ## A
  plot(sdm, legend=FALSE, mar=.5, axes=FALSE, col=tercols, ylim=c(-35,25))
  lines(afr, col="light gray", lwd=.1)
  lines(ca250, col="blue", lwd=1.5)
  lines(e, lty=2)
  points(svsp, cex=.5)	
  
  add_legend(-20, -17, legend = c("not suitable", "suitable", "unsuitable within 100 km", "suitable within 250 km"), pch = 15, col=c("#F1F2E6", "#E3BB56", "#10A82C", "#84D63C"), cex=0.8, bty="n", y.intersp = 0.9)
  add_legend(-20, -11, legend = "observations", pch=20, bty="n", cex=.8)
  add_legend(-20, -5, legend = "250 km buffer", lty=1, lwd=1.5, col="blue", cex=.8, bty="n")
  text(-14, 28, "(A)", xpd=TRUE, cex=1.5)
  text(35, 28, paste0("V. ", sp), font=3)
  
  ## B
  plot(sdm, legend=FALSE, ext=e, axes=FALSE, mar=.5, background="azure", box=FALSE, col=tercols)
  lines(afr, col="light gray", lwd=2)
  lines(ca250, col="blue", lwd=1.5)
  points(svsp[svsp$sample_type=="non-seed"], pch=1)
  if (nrow(seed) > 0) {
    points(seed, pch=20, col="red", cex=1.5)
    add_legend("bottomleft", legend=c("seed", "not seed"), pch=c(20, 1), col=c("red", "black"), pt.cex=c(1.5,1), title="Observations", bty="o", bg="white")
  } else {
    add_legend("bottomleft", legend="not seed", pch=1, title="Observations", bty="o", bg="white")
  }
  terra::halo(26.3, 5.3, "(B)", cex=1.5, bty="n", hw=.3)
  add_box()
  
  ## C
  i <- extract(zones, seed)[,2] |> unique() 
  zones$seed <- (1:nrow(zones) %in% i) + 1
  plot(afr, col="gray95", border="gray95", mar=.5, axes=FALSE, ext=e, background="azure", box=FALSE)
  lines(afr, col="light gray", lwd=2)
  plot(zones, col=c("orange", "light blue")[zones$seed], add=TRUE, border=gray(.5), lwd=2, alpha=.6)
  add_legend("bottomleft", legend=c("seed", "not seed"), title="Conservation\nzones", fill=c("light blue", "orange"), bty="o", bg="white")
  points(seed, pch=20, col="red", cex=1.5)
  terra::halo(26.3, 5.3, "(C)", cex=1.5, bty="n", hw=.3)
  add_box()
  
  ## D
  plot(afr, col="gray95", border="gray95", mar=.5, axes=FALSE, ext=e, background="azure", box=FALSE)
  lines(afr, col="light gray", lwd=2)
  plot(zones, col=c("orange", "light blue")[zones$seed], add=TRUE, border=gray(.5), lwd=2, alpha=.6)
  lines(network0, col="blue", lwd=network0$weight/250)
  i <- network$w != 1 & network$weight != 0
  lines(network[i], col="red", lwd=network$weight[i]/250)
  lines(network[network$weight==0], lty=3, lwd=3, col="red")
  points(x, cex=2.5, col="white")
  text(x, cex=.8)
  terra::halo(26.3, 5.3, "(D)", cex=1.5, bty="n", hw=.3)
  add_legend("bottomleft", legend=c("original", "changed", "new"), col=c("blue", "red", "red"), lty=c(1,1,3), lwd=c(6,3,3), bg="white", title="Network")
   add_box()

  
 dev.off()


