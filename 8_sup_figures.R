

##########################################
# sup fig 1. sensitivity analysis, omega
##########################################

sa_omega <- read.csv("data/final/sa_omega.csv")
omegas <- unique(sa_omega$omega)
xc_labels <- c("XC - ω=1/40", "XC - ω=1/30", "XC - ω=1/20", "XC - ω=1/10")
aggregate(XC ~ omega, sa_omega, mean)

om <- sa_omega[, c("species", "omega", "XC")]
om$omega <- 1/om$omega
omg <- reshape(om, direction = "wide", idvar="species", timevar = "omega")
round(cor(omg[, -1], method = "pearson"), 3)

x <- omg$XC.40
omg$XC.40 <- NULL

tiff(paste0("plots/", "S1_Fig.tif"), 1100, 1000, pointsize=36)

plot(x, omg[, 2], col="red", xlim=c(0,1), ylim=c(0,1), xlab="XC, ω=1/40", ylab="XC, other value for ω", las=1)
points(x, omg[,3], col="blue")
abline(0,1, col="dark gray")
text(.25, .3, substitute(italic("y=x")), col="dark gray", srt=40)
legend("topleft", legend=c("1/20", "1/80"), title="ω", col=c("red", "blue"), pch=1)

dev.off()





##########################################
# sup fig 2. rangesize vs XC
##########################################

tab <- read.csv("table/summary_table.csv")
tab$range_size <- tab$range_size / 1000000

tiff(paste0("plots/", "S2_Fig.tif"), 1200, 1200, pointsize=36)

plot(tab$range_size, tab$XC, xlab=expression("Range size (10"^6*" km"^2*")"), ylab="Score", col="red", las=1, cex=.75, log="x")
points(tab$range_size, tab$FCS, col="blue", las=1, xlim=c(0,1), pch=2, cex=0.6)
legend("topleft", legend = c("XC", "FCS"), col=c("red", "blue"), pch=c(1, 2), cex=0.75, pt.cex=c(.75, .6))

dev.off()


#####################################################
# sup fig 3. sensitivity analysis, buffer size
#####################################################

sa_buf <- read.csv("data/final/sa_buf.csv")
aggregate(XC ~ buf, sa_buf, mean)

bf <- sa_buf[, c("species", "buf", "XC")]
buf <- reshape(bf, direction = "wide", idvar="species", timevar = "buf")
round(cor(buf[, -1], method = "pearson", use="pairwise.complete.obs"), 3)
x <- buf$XC.100_250
buf$XC.100_250 <- NULL

tiff(paste0("plots/", "S3_Fig.tif"), 1100, 1000, pointsize=36)

cols <- rainbow(ncol(buf))
plot(x, buf[,2], col=cols[2], xlim=c(0,1), ylim=c(0,1), cex=.75, xlab="XC, buffer=100-250", ylab="other buffer size")
for (i in 3:ncol(buf)) {
	points(x, buf[,i], col=cols[i], cex=.75)
}
abline(0,1, col="dark gray")
text(.25, .2, substitute(italic("y=x")), col="dark gray", srt=35)

legend("topleft", legend=gsub("XC.", "", colnames(buf)[-1]), title="buffers", col=cols[-1], pch=1, cex=0.7, ncol=2)


dev.off()



#############################################################
# sup fig 4. XC vs FCS (a) + rank (b)
#############################################################


d <- read.csv("table/summary_table.csv")
sp1 <- c("multinervis", "schimperi", "wittei")
sp2 <- c("unguiculata", "ambacensis", "comosa")
txt1 <- d[d$species %in% sp1, ]
txt2 <- d[d$species %in% sp2, ]


tiff(paste0("plots/", "S4_Fig.tif"), 2200, 1000, pointsize=42)

par(mfrow=c(1, 2), mar=c(4,4,1,2))

#Figure XC/FCS-a: Scatterplot XC Vs FCS
plot(d$XCadj, d$FCS, xlab="XC", ylab="FCS", las=1, xlim=c(0,1), pch=1, col="red", cex=.8)
abline(lm(FCS ~ XC, data = d), lwd=3, lty=2)
text(txt1$XCadj, txt1$FCS, txt1$species, pos=ifelse(txt1$species == "schimperi", 4, 2), cex=.6)
text(txt2$XCadj+0.01, txt2$FCS, txt2$species, pos = ifelse(txt2$species == "unguiculata", 2, 3), cex = .6, xpd = TRUE)
text(0.1, 0.45, "(A)", cex=1.25, xpd=TRUE)

#Figure XC/FCS-b: Ranks our method Vs Khoury's method
plot(d$rank_XC, d$rank_FCS, xlab="XC rank", ylab="FCS rank", las=1, pch=1, col="blue", cex=.8)
abline(lm(rank_FCS ~ rank_XC, data = d), lwd=3, lty=2)
text(17, 61, "(B)", cex=1.25, xpd=TRUE)

dev.off()


#############################################################
# sup fig 5.  climate vs ecoregions
#############################################################

vignas <- XSdata::vigna_occ()
sv <- terra::vect(vignas, crs="+proj=longlat")
afr <- terra::readRDS("data/intermediate/africa.rds")

#Figure 2: scatterplot temperature range and precipitation range vs number of ecoregions
tabS3 <- read.csv("table/S3_table.csv")
seed <- tabS3[tabS3$sample_type=="seed", ]

tiff(paste0("plots/", "S5_Fig.tif"), 2200, 1000, pointsize=32)

par(mfrow=c(1, 2), mar=c(5,5,1,2))
plot(range_temp ~ n_eco, data=seed, pch=20, xlab="Number of ecoregions", ylab="Temperature range (°C)", col="red")
abline(lm(range_temp ~ n_eco, data = seed), col = "gray")
text(48, 1, "(A)", cex=1.5, xpd=TRUE)

plot(range_prec ~ n_eco, data=seed, pch=20, xlab="Number of ecoregions", ylab="Precipitation range (mm)", col="blue")
abline(lm(range_prec ~ n_eco, data = seed), col = "gray")
text(48, 450, "(B)", cex=1.5, xpd=TRUE)

dev.off()
