
#packages needed: terra, geodata, conexus
dir.create("data/final/XC_dist/", FALSE, TRUE)

afr <- terra::readRDS("data/intermediate/africa.rds")

# get species data
sv <- XSdata::vigna_occ()
sv <- terra::vect(sv, c("lon", "lat"), crs="+proj=longlat")
spp <- sort(unique(sv$species))
nspp <- length(spp)

r <- terra::rast(paste0("data/intermediate/sdm/range/range_", spp[1], ".tif"))
xy <- c(terra::init(r, "x"), terra::init(r, "y"))

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

  
get_XC <- function(sp, nruns=1, inclusion, exclusion, omega, sens_analysis=TRUE) {
	# select seed samples 
	print(sp); flush.console()
	vsp <- sv[sv$species == sp, ]
	seed <- vsp[vsp$sample_type == "seed", ]
	x <- terra::rast(paste0("data/intermediate/sdm/suitable/suitable_", sp, ".tif"))
	r <- conexus::get_range(x, vsp, afr, inclusion, exclusion)
	k <- conexus::get_samplesize(r, omega=omega)
	exslst <- lapply(1:nruns, \(...) {
		zones <- try(conexus::make_zones(xy, k$range, k$n, spread=TRUE), silent=TRUE)
		if (inherits(zones, "try-error")) {
			zones <- try(conexus::make_zones(xy, k$range, k$n, spread=TRUE))
			if (inherits(zones, "try-error")) {
				return(NULL)
			}
		}
		exs <- conexus:::XC(zones, seed, env, envdist, maxdist=1500)
		exs$n <- k$n
		exs
	})
	res <- lapply(1:nruns, \(i) {
		exs <- exslst[[i]]
		if (is.null(exs)) return(NULL)
		out <- data.frame(species=sp, run=i, inclusion=inclusion, exclusion=exclusion, omega=omega, nzones=exs$n, XC=exs$XC, dst=NA, envdst=NA, geodst=NA)
		if (!is.null(exs$dist)) {
			out[, c("dst", "envdst", "geodst")] <- colMeans(exs$dist[, c("dst", "envdst", "geodst")], na.rm=TRUE) 
		}
		out
	})
	res <- do.call(rbind, res)

	if (!sens_analysis) {
		lapply(1:nruns, \(i) {
			write.csv(exslst[[i]]$dist, paste0("data/final/XC_dist/", sp, "_run", i, "_dist.csv"), row.names=FALSE)
		})
	}
	res
}


### main analysis 
set.seed(3388)
XClst <- lapply(1:nspp, \(i) get_XC(spp[i], 10, 100, 250, 1/40, sens_analysis=FALSE))
XC <- do.call(rbind, XClst)
write.csv(XC, "data/final/XC.csv", row.names=FALSE)

XCa <- aggregate(XC[,-c(1:2)], XC["species"], median)


## non geo adj 
d <- data.frame(sv[sv$sample_type=="seed", ])
seed <- aggregate(d[,"sample_type"], d["species"], length)
nonref <- XSdata::no_georef()[, c("species", "no_coords_seed")]
seed <- merge(seed, nonref, all.x=TRUE, all=TRUE)
colnames(seed)[2:3] <- c("geo", "no_geo")
seed$geo[is.na(seed$geo)] <- 0
seed$all_seed <- seed$geo + seed$no_geo
XCa <- merge(XCa, seed, all.x=TRUE)
mobs_seed <- loess(XC ~ geo, data=XCa)
XCa$XC_adj <- pmax(XCa$XC, predict(mobs_seed, XCa$no_geo))
XCa$rank_XC <- rank(XCa$XC_adj)


write.csv(XCa, "data/final/XCagg.csv", row.names=FALSE)


####################################
#####   sensitivity analysis   #####
####################################

### omega
omegas <- c(1/20, 1/40, 1/80)
XC_omega <- lapply(omegas, \(omega) {
  print(omega); flush.console()
  set.seed(3388)
  out <- lapply(1:nspp, \(i) get_XC(spp[i], 1, 100, 250, omega))
  do.call(rbind, out)
})
sa_omega <- do.call(rbind, XC_omega)

write.csv(sa_omega, "data/final/sa_omega.csv", row.names=FALSE)



### inclusion/exclusion buffers
buffers <- data.frame(
  inclusion = c(0,   0,   0,   0,  50,  50,  50, 100, 100, 100, 200, 200),
  exclusion = c(0, 125, 250, 500, 125, 250, 500, 125, 250, 500, 250, 500)
)
#buffers = buffers[c(1,2),]

XC_buffer <- lapply(1:nrow(buffers), \(b) {
  print(buffers[b,])
  set.seed(3388)
  out <- do.call(rbind, 
      lapply(c(1:nspp), \(i) {
          get_XC(spp[i], 1, buffers$inclusion[b], buffers$exclusion[b], 1/40)
        }))})

sa_buf <- do.call(rbind, XC_buffer)
sa_buf$buf <- paste0(sa_buf$inclusion, "_", sa_buf$exclusion)

write.csv(sa_buf, "data/final/sa_buf.csv", row.names=FALSE)

