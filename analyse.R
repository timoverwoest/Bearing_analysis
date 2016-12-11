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
head(b1.x.fft)
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
