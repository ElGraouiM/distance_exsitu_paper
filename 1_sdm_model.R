

dir.create("data/intermediate/sdm/raw", FALSE, TRUE)
dir.create("data/intermediate/sdm/suitable", FALSE)
dir.create("data/intermediate/sdm/range", FALSE)

wc <- terra::rast("data/intermediate/wc.tif")
afr <- terra::readRDS("data/intermediate/africa.rds")

options(java.parameters = "-Xmx8g")
#spp <- readRDS("data/intermediate/unique_occurrences.rds")
spp <- XSdata::vigna_occ()

spp <- na.omit(unique(spp[, c("species", "lon", "lat")]))
usp <- sort(unique(spp$species))

set.seed(123)
bg <- predicts::backgroundSample(wc[[1]], 10000)

eva <- list()

for (sp in usp) {
  cat(sp, "\n"); flush.console()
  obs <- unique(spp[spp$species==sp, c("lon", "lat")])
  obs <- terra::vect(obs, crs="+proj=longlat")
  
  nfld <- min(5, nrow(obs))
  k <- predicts::folds(obs, nfld)
  keva <- list()
  for (i in 1:nfld) {
    if (nfld == 1) {
      m <- predicts::MaxEnt(wc, obs)
      p <- obs		
    } else {
      m <- predicts::MaxEnt(wc, obs[k!=i,])
      p <- obs[k==i,]
    }
    nbg <- max(100, min(nrow(bg), nrow(p) * 5))
    b <- bg[sample(nbg), ]
    pa <- suppressWarnings(predicts::pa_evaluate(p, b, m, wc))
    keva[[i]] <- cbind(pa@stats, pa@thresholds)
  }
  eva[[sp]] <- data.frame(species=sp, rbind(do.call(rbind, keva) |> colMeans()))
  
  #	if (file.exists(f1)) next
  f1 <- paste0("data/intermediate/sdm/raw/raw_", sp, ".tif")
  f2 <- paste0("data/intermediate/sdm/suitable/suitable_", sp, ".tif")
  f3 <- paste0("data/intermediate/sdm/range/range_", sp, ".tif")
  
  m <- predicts::MaxEnt(wc, obs)
  p <- terra::predict(wc, m, filename=f1, overwrite=TRUE)
  
  th <- eva[[sp]]["equal_sens_spec"][[1]]
  p_suitable <- p >= th
  p_suitable <- terra::subst(p_suitable, FALSE, NA, filename=f2, overwrite=TRUE)
  
  p_range <- conexus::get_range(p_suitable, obs, afr, include=100, exclude=250)
  p_range <- terra::writeRaster(p_range, f3, overwrite=TRUE)
}

reva <- do.call(rbind, eva)
write.csv(reva, "data/intermediate/sdm/evaluation.csv", row.names=FALSE)


## compute the size of the suitable area (raw p/a SDM) and the range
deval <- read.csv("data/intermediate/sdm/evaluation.csv")
sdm_range <- sapply(deval$species, \(sp) {
  th <- deval$equal_sens_spec[deval$species == sp] # |> as.numeric()
  r_suitable <- terra::rast(paste0("data/intermediate/sdm/suitable/suitable_", sp, ".tif"))
  r_range <- terra::rast(paste0("data/intermediate/sdm/range/range_", sp, ".tif"))
  terra::expanse(c(r_suitable, r_range), unit="km")$area 
})

range_sdm <- data.frame(species=colnames(sdm_range), t(sdm_range))
colnames(range_sdm)[2:3] <- c("suitable", "range")
range_sdm$relative <- range_sdm$suitable / range_sdm$range

write.csv(range_sdm, "data/intermediate/sdm/range_size.csv", row.names=FALSE)



