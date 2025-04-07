
###########################
### table s1            ###
###########################

vignas <- XSdata::vigna_occ()
colnames(vignas)[2:3] <- c("longitude", "latitude")
write.csv(vignas, "table/S1_table.csv")


###########################
### table s2            ###
###########################

stab <- read.csv("table/summary_table.csv")
FC <- read.csv("data/final/FCS.csv")[, c("species", "GRex", "ERex", "SRex", "FCex")]

stab$relative_change_range <- round(100 * (stab$range_size - stab$suitable_area) / stab$suitable_area, 1)

tabS2 <- merge(stab, FC, by="species")
tabS2 <- tabS2[, c("species", "nseed", "nother", "nall", "suitable_area", "range_size", "relative_change_range", "XC", "XCadj", "GRex", "ERex", "SRex", "FCS", "rank_XC", "rank_FCS", "nogeoref_seed")]
tabS2[,-1] <- round(tabS2[,-1], 3)

write.csv(tabS2, "table/S2_table.csv", row.names=FALSE)

###########################
## table s3             ###
###########################

vignas <- XSdata::vigna_occ()
sv <- terra::vect(vignas, geom=c("lon", "lat"), crs="+proj=longlat")
ecoregions <- terra::vect(system.file("ex/tnc_terr_ecoregions.gpkg", package="XSdata")) 

env <- terra::rast("data/intermediate/wc.tif")[[c("bio_1", "bio_12")]]
e <- terra::extract(env, sv, bind=TRUE) |> data.frame()
a <- aggregate(e[, c("bio_1", "bio_12")], e[, c("species", "sample_type")], range, na.rm=TRUE)
b <- aggregate(e[, c("bio_1", "bio_12")], e["species"], range, na.rm=TRUE)
b$sample_type <- "all"
a <- rbind(a, b)
a$range_temp <- round(a$bio_1[,2] - a$bio_1[,1], 1)
a$range_prec <- round(a$bio_12[,2] - a$bio_1[,1])
a$bio_1 <- a$bio_12 <- NULL

eco <- terra::extract(ecoregions[c("ECO_ID_U", "WWF_MHTNAM")], sv)
x <- aggregate(eco[,"ECO_ID_U"], sv[,c("species", "sample_type"), drop=TRUE], \(i) length(unique(i)))
y <- aggregate(eco[,"ECO_ID_U"], sv[,c("species"), drop=TRUE], \(i) length(unique(i)))
y$sample_type <- "all"
x <- rbind(x, y)
colnames(x)[3] <- "n_eco"

tabS3 <- merge(a, x, by=c("species", "sample_type"))
write.csv(tabS3, "table/S3_table.csv", row.names=FALSE)


###########################
### table s4            ###
###########################
write.csv(stab[, c("species", "dst", "geodst", "envdst")], "table/S4_table.csv", row.names=FALSE)
