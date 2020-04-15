dataset <- read.arff("EEG.arff")

dataset.pr <- prcomp(dataset[c(1:14)], center = TRUE, scale = TRUE)
summary(dataset.pr)

screeplot(dataset.pr, type = "l", npcs = 15, main = "Wykres pierwszych 15 PC")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Wartoœæ w³asna = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(dataset.pr$sdev^2 / sum(dataset.pr$sdev^2))
plot(cumpro[0:15], xlab = "Numer PC", ylab = "Wariancja sumaryczna", main = "Wykres skumulowanej wariancji")
abline(v = 4, col="blue", lty=5)
abline(h = 0.915, col="blue", lty=5)
legend("topleft", legend=c("Punkt odciêcia dla PC4"),
       col=c("blue"), lty=5, cex=0.6)

library("factoextra")
fviz_pca_ind(dataset.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = dataset$eyeDetection, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Eye Detection") +
  ggtitle("2D PCA-plot") +
  theme(plot.title = element_text(hjust = 0.5)) + xlim(-3, 1.5) + ylim(-1, 1)
