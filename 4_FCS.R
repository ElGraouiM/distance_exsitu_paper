

vignas <- XSdata::vigna_occ()
spp <- sort(unique(vignas$species))
sv <- terra::vect(vignas, c("lon", "lat"), crs="+proj=lonlat")

f <- paste0("data/intermediate/sdm/suitable/suitable_", spp[1], ".tif")
ecoreg <- terra::vect(system.file("ex/tnc_terr_ecoregions.gpkg", package="XSdata")) 
ecoreg <- terra::rasterize(ecoreg, terra::rast(f), 1:nrow(ecoreg))

nonref <- XSdata::no_georef()


FCS <- lapply(spp, \(sp) {
	print(sp); flush.console()
	svsp <- sv[sv$species==sp, ]
	seed <- svsp[svsp$sample_type=="seed", ]
	herb <- svsp[svsp$sample_type=="non-seed", ]
	noref <- unlist(nonref[nonref$species == sp, "no_coords_seed"])
	r <- terra::rast(paste0("data/intermediate/sdm/suitable/suitable_", sp, ".tif"))
	conexus:::FCex(seed, nrow(herb), r, ecoreg, bsize=50, nseed_nogeo=noref, inrange=FALSE)
})

out <- round(do.call(rbind, FCS), 3)
out <- data.frame(species=spp, out)

write.csv(out, "data/final/FCS.csv", row.names=FALSE)

