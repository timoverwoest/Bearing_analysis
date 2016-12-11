# Set working directory
basedir <- "/Users/Timo/Documents/BigData/R_projects/Werkmap/bearing_IMS/1st_test/"
# assign the dataset
data <- read.table(paste0(basedir, "2003.10.22.12.06.24"), header=FALSE, sep="\t")
# Show the data
head(data)
# Make understandable collumn headers
colnames(data) <- c("b1.x", "b1.y", "b2.x", "b2.y", "b3.x", "b3.y", "b4.x", "b4.y")
# Show some standard statistics for b1.x
summary(data$b1.x)
# Plot b1.x
plot(data$b1.x, t="l") # t="l" means line plot
# Make and fft of b1.x
b1.x.fft <- fft(data$b1.x)
print(b1.x.fft)
# Ignore the 2nd half, which are complex conjugates of the 1st half, thus take
# (length(b1.x.fft)/2)
# and calculate the Mod (magnitude of each complex number) en seq
amplitude <- Mod(b1.x.fft[1:(length(b1.x.fft)/2)])
# Calculate the frequencies
frequency <- seq(0, 10000, length.out=length(b1.x.fft)/2)
# Plot
plot(amplitude ~ frequency, t="l")
# Look into lower frequencies
plot(amplitude ~ frequency, t="l", xlim=c(0,1000), ylim=c(0,500))
axis(1, at=seq(0,1000,100), labels=FALSE)  # add more ticks
sorted <- sort.int(amplitude, decreasing=TRUE, index.return=TRUE)
top15 <- sorted$ix[1:15] # indexes of the largest 15
top15f <- frequency[top15] # convert indexes to frequencies
# show frequencies of the top 15 amplitudes
print(top15f)
# make aan function of the FFT analysis with sorting and identifieing the top n
# frequencies
fft.profile <- function (dataset, n)
{
	fft.data <- fft(dataset)
	amplitude <- Mod(fft.data[1:(length(fft.data)/2)])
	frequencies <- seq(0, 10000, length.out=length(fft.data)/2)

	sorted <- sort.int(amplitude, decreasing=TRUE, index.return=TRUE)
	top <- sorted$ix[1:n] # indexes of the largest n components
	return (frequencies[top]) # convert indexes to frequencies
}

# How many FFT components should I grab as features?
n <- 5 # top 5

# Set up storage for bearing-grouped data
b1 <- matrix(nrow=0, ncol=(2*n+1))
b2 <- matrix(nrow=0, ncol=(2*n+1))
b3 <- matrix(nrow=0, ncol=(2*n+1))
b4 <- matrix(nrow=0, ncol=(2*n+1))

for (filename in list.files(basedir))
{
	cat("Processing file ", filename, "\n")

	# get the filename as timestamp
	timestamp <- as.character(strptime(filename, format="%Y.%m.%d.%H.%M.%S"))

	data <- read.table(paste0(basedir, filename), header=FALSE, sep="\t")
	colnames(data) <- c("b1.x", "b1.y", "b2.x", "b2.y", "b3.x", "b3.y", "b4.x", "b4.y")

	# Bind the new rows to the bearing matrices
    b1 <- rbind(b1, c(timestamp, fft.profile(data$b1.x, n), fft.profile(data$b1.y, n)))
    b2 <- rbind(b2, c(timestamp, fft.profile(data$b2.x, n), fft.profile(data$b2.y, n)))
    b3 <- rbind(b3, c(timestamp, fft.profile(data$b3.x, n), fft.profile(data$b3.y, n)))
    b4 <- rbind(b4, c(timestamp, fft.profile(data$b4.x, n), fft.profile(data$b4.y, n)))

}

write.table(b1, file=paste0(basedir, "../b1.csv"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(b2, file=paste0(basedir, "../b2.csv"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(b3, file=paste0(basedir, "../b3.csv"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(b4, file=paste0(basedir, "../b4.csv"), sep=",", row.names=FALSE, col.names=FALSE)

# Making plots
# Snippets to graph the output of the basic feature extraction
# Either run this whole script to plot onto one page,
# or paste these paragraphs into the R console for individual plots.

basedir <- "../bearing_IMS/1st_test/"
b1 <- read.table(file=paste0(basedir, "../b1.csv"), sep=",", header=FALSE)
b2 <- read.table(file=paste0(basedir, "../b2.csv"), sep=",", header=FALSE)
b3 <- read.table(file=paste0(basedir, "../b3.csv"), sep=",", header=FALSE)
b4 <- read.table(file=paste0(basedir, "../b4.csv"), sep=",", header=FALSE)


par(mfrow=c(5,2))

# x axis components
plot(b1[, 2], pch=4, col="dodgerblue2", ylab="Frequency", main="1st strongest x", ylim=c(0, 2))
points(b2[, 2], pch=3, col="darkorchid2")
points(b4[, 2], pch=1, col="chartreuse")
points(b3[, 2], pch=20, col="coral2")

legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(b1[, 7], pch=4, col="dodgerblue2", ylab="Frequency", main="1st strongest y", ylim=c(0, 2))
points(b2[, 7], pch=3, col="darkorchid2")
points(b4[, 7], pch=1, col="chartreuse")
points(b3[, 7], pch=20, col="coral2")

legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))


plot(b1[, 3], pch=4, col="dodgerblue2", ylab="Frequency", main="2nd strongest x", ylim=c(0,10000))
points(b2[, 3], pch=3, col="darkorchid2")
points(b4[, 3], pch=1, col="chartreuse")
points(b3[, 3], pch=20, col="coral2")

plot(b1[, 8], pch=4, col="dodgerblue2", ylab="Frequency", main="2nd strongest y", ylim=c(0,10000))
points(b2[, 8], pch=3, col="darkorchid2")
points(b4[, 8], pch=1, col="chartreuse")
points(b3[, 8], pch=20, col="coral2")

plot(b1[, 4], pch=4, col="dodgerblue2", ylab="Frequency", main="3rd strongest x", ylim=c(0,10000))
points(b2[, 4], pch=3, col="darkorchid2")
points(b4[, 4], pch=1, col="chartreuse")
points(b3[, 4], pch=20, col="coral2")

plot(b1[, 9], pch=4, col="dodgerblue2", ylab="Frequency", main="3rd strongest y", ylim=c(0,10000))
points(b2[, 9], pch=3, col="darkorchid2")
points(b4[, 9], pch=1, col="chartreuse")
points(b3[, 9], pch=20, col="coral2")

plot(b1[, 5], pch=4, col="dodgerblue2", ylab="Frequency", main="4th strongest x", ylim=c(0,10000))
points(b2[, 5], pch=3, col="darkorchid2")
points(b4[, 5], pch=1, col="chartreuse")
points(b3[, 5], pch=20, col="coral2")

plot(b1[, 10], pch=4, col="dodgerblue2", ylab="Frequency", main="4th strongest y", ylim=c(0,10000))
points(b2[, 10], pch=3, col="darkorchid2")
points(b4[, 10], pch=1, col="chartreuse")
points(b3[, 10], pch=20, col="coral2")

plot(b1[, 6], pch=4, col="dodgerblue2", ylab="Frequency", main="5th strongest x", ylim=c(0,10000))
points(b2[, 6], pch=3, col="darkorchid2")
points(b4[, 6], pch=1, col="chartreuse")
points(b3[, 6], pch=20, col="coral2")

plot(b1[, 11], pch=4, col="dodgerblue2", ylab="Frequency", main="5th strongest y", ylim=c(0,10000))
points(b2[, 11], pch=3, col="darkorchid2")
points(b4[, 11], pch=1, col="chartreuse")
points(b3[, 11], pch=20, col="coral2")
