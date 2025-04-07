
dir.create("table", FALSE, TRUE)

XC <- read.csv("data/final/XCagg.csv")[, c("species", "XC", "XC_adj", "rank_XC", "dst", "envdst", "geodst")]
FCS <- read.csv("data/final/FCS.csv")
range_size <- read.csv("data/intermediate/sdm/range_size.csv")

x <- merge(XC, FCS, by="species", all.x=TRUE)
x <- merge(x, range_size, by="species")

x$rank_FCS <- rank(x$FCex)
x$nall <- (x$nseed + x$nherb)

tab <- x[, c('species', 'nseed', 'nherbarium', 'nall', 'XC', "XC_adj", "rank_XC", 'dst', 'envdst', 'geodst', 'suitable', 'range', 'FCex', 'rank_FCS',
"nseed_nogeo")]

colnames(tab) <- c("species", "nseed", "nother", "nall", "XC", "XCadj", "rank_XC", "dst", "envdst", "geodst", "suitable_area", "range_size", "FCS", "rank_FCS", "nogeoref_seed")

write.csv(tab, "table/summary_table.csv", row.names=FALSE)

