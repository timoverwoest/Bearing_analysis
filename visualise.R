# Set working directory
basedir <- "/Users/Timo/Documents/BigData/R_projects/Werkmap/bearing_IMS/1st_test/"

# Visualise basic feature extraction
b1b <- read.table(file=paste0(basedir, "../b1.csv"), sep=",", header=FALSE)
b2b <- read.table(file=paste0(basedir, "../b2.csv"), sep=",", header=FALSE)
b3b <- read.table(file=paste0(basedir, "../b3.csv"), sep=",", header=FALSE)
b4b <- read.table(file=paste0(basedir, "../b4.csv"), sep=",", header=FALSE)

par(mfrow=c(2,2))

plot(b1b[, 2], pch=4, col="dodgerblue2", ylab="Frequency", main="1st strongest x", ylim=c(0, 2))
points(b2b[, 2], pch=3, col="darkorchid2")
points(b4b[, 2], pch=1, col="chartreuse")
points(b3b[, 2], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1b[, 7], pch=4, col="dodgerblue2", ylab="Frequency", main="1st strongest y", ylim=c(0, 2))
points(b2b[, 7], pch=3, col="darkorchid2")
points(b4b[, 7], pch=1, col="chartreuse")
points(b3b[, 7], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1b[, 3], pch=4, col="dodgerblue2", ylab="Frequency", main="2nd strongest x", ylim=c(0,10000))
points(b2b[, 3], pch=3, col="darkorchid2")
points(b4b[, 3], pch=1, col="chartreuse")
points(b3b[, 3], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1b[, 8], pch=4, col="dodgerblue2", ylab="Frequency", main="2nd strongest y", ylim=c(0,10000))
points(b2b[, 8], pch=3, col="darkorchid2")
points(b4b[, 8], pch=1, col="chartreuse")
points(b3b[, 8], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1b[, 4], pch=4, col="dodgerblue2", ylab="Frequency", main="3rd strongest x", ylim=c(0,10000))
points(b2b[, 4], pch=3, col="darkorchid2")
points(b4b[, 4], pch=1, col="chartreuse")
points(b3b[, 4], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1b[, 9], pch=4, col="dodgerblue2", ylab="Frequency", main="3rd strongest y", ylim=c(0,10000))
points(b2b[, 9], pch=3, col="darkorchid2")
points(b4b[, 9], pch=1, col="chartreuse")
points(b3b[, 9], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1b[, 5], pch=4, col="dodgerblue2", ylab="Frequency", main="4th strongest x", ylim=c(0,10000))
points(b2b[, 5], pch=3, col="darkorchid2")
points(b4b[, 5], pch=1, col="chartreuse")
points(b3b[, 5], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1b[, 10], pch=4, col="dodgerblue2", ylab="Frequency", main="4th strongest y", ylim=c(0,10000))
points(b2b[, 10], pch=3, col="darkorchid2")
points(b4b[, 10], pch=1, col="chartreuse")
points(b3b[, 10], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1b[, 6], pch=4, col="dodgerblue2", ylab="Frequency", main="5th strongest x", ylim=c(0,10000))
points(b2b[, 6], pch=3, col="darkorchid2")
points(b4b[, 6], pch=1, col="chartreuse")
points(b3b[, 6], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1b[, 11], pch=4, col="dodgerblue2", ylab="Frequency", main="5th strongest y", ylim=c(0,10000))
points(b2b[, 11], pch=3, col="darkorchid2")
points(b4b[, 11], pch=1, col="chartreuse")
points(b3b[, 11], pch=20, col="coral2")
legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

# Visualise Domain, statistical and visually derived features
b1 <- read.table(file=paste0(basedir, "../b1_all.csv"), sep=",", header=TRUE)
b2 <- read.table(file=paste0(basedir, "../b2_all.csv"), sep=",", header=TRUE)
b3 <- read.table(file=paste0(basedir, "../b3_all.csv"), sep=",", header=TRUE)
b4 <- read.table(file=paste0(basedir, "../b4_all.csv"), sep=",", header=TRUE)

par(mfrow=c(2,1))

# Domain specific features: Bearing frequencies
plot(b1[, "BPFI.x"], t="l", ylab="Density", main="B1 ball passing freq. inner race (BPFO) x-dir.")
plot(b1[, "BPFI.y"], t="l", ylab="Density", main="B1 ball passing freq. inner race (BPFO) x-dir.")
plot(b2[, "BPFI.x"], t="l", ylab="Density", main="B2 ball passing freq. inner race (BPFO) x-dir.")
plot(b2[, "BPFI.y"], t="l", ylab="Density", main="B2 ball passing freq. inner race (BPFO) y-dir.")
plot(b3[, "BPFI.x"], t="l", ylab="Density", main="B3 ball passing freq. inner race (BPFO) x-dir.")
plot(b3[, "BPFI.y"], t="l", ylab="Density", main="B3 ball passing freq. inner race (BPFO) y-dir.")
plot(b4[, "BPFI.x"], t="l", ylab="Density", main="B4 ball passing freq. inner race (BPFO) x-dir.")
plot(b4[, "BPFI.y"], t="l", ylab="Density", main="B4 ball passing freq. inner race (BPFO) y-dir.")

plot(b1[, "BPFO.x"], t="l", ylab="Density", main="B1 ball passing freq. outer race (BPFO) x-dir.")
plot(b1[, "BPFO.y"], t="l", ylab="Density", main="B1 ball passing freq. outer race (BPFO) y-dir.")
plot(b2[, "BPFO.x"], t="l", ylab="Density", main="B2 ball passing freq. outer race (BPFO) x-dir.")
plot(b2[, "BPFO.y"], t="l", ylab="Density", main="B2 ball passing freq. outer race (BPFO) y-dir.")
plot(b3[, "BPFO.x"], t="l", ylab="Density", main="B3 ball passing freq. outer race (BPFO) x-dir.")
plot(b3[, "BPFO.y"], t="l", ylab="Density", main="B3 ball passing freq. outer race (BPFO) y-dir.")
plot(b4[, "BPFO.x"], t="l", ylab="Density", main="B4 ball passing freq. outer race (BPFO) x-dir.")
plot(b4[, "BPFO.y"], t="l", ylab="Density", main="B4 ball passing freq. outer race (BPFO) y-dir.")

plot(b1[, "BSF.x"], t="l", ylab="Density", main="B1 ball spin freq. (BSF) x-dir.")
plot(b1[, "BSF.y"], t="l", ylab="Density", main="B1 ball spin freq. (BSF) y-dir.")
plot(b2[, "BSF.x"], t="l", ylab="Density", main="B2 ball spin freq. (BSF) x-dir.")
plot(b2[, "BSF.y"], t="l", ylab="Density", main="B2 ball spin freq. (BSF) y-dir.")
plot(b3[, "BSF.x"], t="l", ylab="Density", main="B3 ball spin freq. (BSF) x-dir.")
plot(b3[, "BSF.y"], t="l", ylab="Density", main="B3 ball spin freq. (BSF) y-dir.")
plot(b4[, "BSF.x"], t="l", ylab="Density", main="B4 ball spin freq. (BSF) x-dir.")
plot(b4[, "BSF.y"], t="l", ylab="Density", main="B4 ball spin freq. (BSF) y-dir.")

# Statistical features: min max quantile median
plot(b1$Min.x,
  type="l",
  col="red",
  xlab="index",
  ylab="Amplitude",
  main="B1 Quantities x-direction",
  ylim = c(min(b1$Min.x), max(b1$Max.x))
)
lines(b1$Max.x, type="l", col="red")
lines(b1$Qu.1.x, type="l", col="blue")
lines(b1$Qu.3.x, type="l", col="blue")
lines(b1$Median.x, type="l", col="black")

plot(b1$Min.y,
  type="l",
  col="red",
  xlab="index",
  ylab="Amplitude",
  main="B1 Quantities y-direction",
  ylim = c(min(b1$Min.y), max(b1$Max.y))
)
lines(b1$Max.y, type="l", col="red")
lines(b1$Qu.1.y, type="l", col="blue")
lines(b1$Qu.3.y, type="l", col="blue")
lines(b1$Median.y, type="l", col="black")

plot(b2$Min.x,
  type="l",
  col="red",
  xlab="index",
  ylab="Amplitude",
  main="B2 Quantities x-direction",
  ylim = c(min(b2$Min.x), max(b2$Max.x))
)
lines(b2$Max.x, type="l", col="red")
lines(b2$Qu.1.x, type="l", col="blue")
lines(b2$Qu.3.x, type="l", col="blue")
lines(b2$Median.x, type="l", col="black")

plot(b2$Min.y,
  type="l",
  col="red",
  xlab="index",
  ylab="Amplitude",
  main="B2 Quantities y-direction",
  ylim = c(min(b2$Min.y), max(b2$Max.y))
)
lines(b2$Max.y, type="l", col="red")
lines(b2$Qu.1.y, type="l", col="blue")
lines(b2$Qu.3.y, type="l", col="blue")
lines(b2$Median.y, type="l", col="black")

plot(b3$Min.x,
  type="l",
  col="red",
  xlab="index",
  ylab="Amplitude",
  main="B3 Quantities x-direction",
  ylim = c(min(b3$Min.x), max(b3$Max.x))
)
lines(b3$Max.x, type="l", col="red")
lines(b3$Qu.1.x, type="l", col="blue")
lines(b3$Qu.3.x, type="l", col="blue")
lines(b3$Median.x, type="l", col="black")

plot(b3$Min.y,
  type="l",
  col="red",
  xlab="index",
  ylab="Amplitude",
  main="B3 Quantities y-direction",
  ylim = c(min(b3$Min.y), max(b3$Max.y))
)
lines(b3$Max.y, type="l", col="red")
lines(b3$Qu.1.y, type="l", col="blue")
lines(b3$Qu.3.y, type="l", col="blue")
lines(b3$Median.y, type="l", col="black")

plot(b4$Min.x,
  type="l",
  col="red",
  xlab="index",
  ylab="Amplitude",
  main="B4 Quantities x-direction",
  ylim = c(min(b4$Min.x), max(b4$Max.x))
)
lines(b4$Max.x, type="l", col="red")
lines(b4$Qu.1.x, type="l", col="blue")
lines(b4$Qu.3.x, type="l", col="blue")
lines(b4$Median.x, type="l", col="black")

plot(b4$Min.y,
  type="l",
  col="red",
  xlab="index",
  ylab="Amplitude",
  main="B4 Quantities y-direction",
  ylim = c(min(b4$Min.y), max(b4$Max.y))
)
lines(b4$Max.y, type="l", col="red")
lines(b4$Qu.1.y, type="l", col="blue")
lines(b4$Qu.3.y, type="l", col="blue")
lines(b4$Median.y, type="l", col="black")

# Visually derived features: frequency bands
plot(b1$LF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="LF Power",
  main="B1 low frequency band x-direction",
)
plot(b1$LF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="LF Power",
  main="B1 low frequency band y-direction",
)
plot(b2$LF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="LF Power",
  main="B2 low frequency band x-direction",
)
plot(b2$LF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="LF Power",
  main="B2 low frequency band y-direction",
)
plot(b3$LF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="LF Power",
  main="B3 low frequency band x-direction",
)
plot(b3$LF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="LF Power",
  main="B3 low frequency band y-direction",
)
plot(b4$LF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="LF Power",
  main="B4 low frequency band x-direction",
)
plot(b4$LF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="LF Power",
  main="B4 low frequency band y-direction",
)

plot(b1$MF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="MF Power",
  main="B1 medium frequency band x-direction",
)
plot(b1$MF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="MF Power",
  main="B1 medium frequency band y-direction",
)
plot(b2$MF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="MF Power",
  main="B2 medium frequency band x-direction",
)
plot(b2$MF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="MF Power",
  main="B2 medium frequency band y-direction",
)
plot(b3$MF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="MF Power",
  main="B3 medium frequency band x-direction",
)
plot(b3$MF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="MF Power",
  main="B3 medium frequency band y-direction",
)
plot(b4$MF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="MF Power",
  main="B4 medium frequency band x-direction",
)
plot(b4$MF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="MF Power",
  main="B4 medium frequency band y-direction",
)

plot(b1$HF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="HF Power",
  main="B1 high frequency band x-direction",
)
plot(b1$HF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="HF Power",
  main="B1 high frequency band y-direction",
)
plot(b2$HF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="HF Power",
  main="B2 high frequency band x-direction",
)
plot(b2$HF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="HF Power",
  main="B2 high frequency band y-direction",
)
plot(b3$HF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="HF Power",
  main="B3 high frequency band x-direction",
)
plot(b3$HF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="HF Power",
  main="B3 high frequency band y-direction",
)
plot(b4$HF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="HF Power",
  main="B4 high frequency band x-direction",
)
plot(b4$HF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="HF Power",
  main="B4 high frequency band y-direction",
)

plot(b1$VHF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="VHF Power",
  main="B1 very high frequency band x-direction",
)
plot(b1$VHF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="VHF Power",
  main="B1 very high frequency band y-direction",
)
plot(b2$VHF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="VHF Power",
  main="B2 very high frequency band x-direction",
)
plot(b2$VHF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="VHF Power",
  main="B2 very high frequency band y-direction",
)
plot(b3$VHF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="VHF Power",
  main="B3 very high frequency band x-direction",
)
plot(b3$VHF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="VHF Power",
  main="B3 very high frequency band y-direction",
)
plot(b4$VHF.pow.x,
  type="l",
  col="black",
  xlab="index",
  ylab="VHF Power",
  main="B4 very high frequency band x-direction",
)
plot(b4$VHF.pow.y,
  type="l",
  col="black",
  xlab="index",
  ylab="VHF Power",
  main="B4 very high frequency band y-direction",
)
