basedir <- "/Users/Timo/Documents/BigData/R_projects/Werkmap/bearing_IMS/1st_test/"
data <- read.table(paste0(basedir, "2003.10.22.12.06.24"), header=FALSE, sep="\t")
head(data)
colnames(data) <- c("b1.x", "b1.y", "b2.x", "b2.y", "b3.x", "b3.y", "b4.x", "b4.y")
summary(data$b1.x)
plot(data$b1.x, t="l") # t="l" means line plot