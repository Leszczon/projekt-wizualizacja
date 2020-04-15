library('foreign')
dataset <- read.arff("EEG.arff")

dataset.pr <- prcomp(dataset[c(1:14)], center = TRUE, scale = TRUE)
summary(dataset.pr)

dataset.pr.per <- summary(dataset.pr)$importance[2,]
rows <- rownames(as.data.frame(dataset.pr.per))
ylim <- c(0, 1.2*max(dataset.pr.per))

xx <- barplot(
  dataset.pr.per, main="Dystrybucja Wariancji",
  ylim=ylim,
  xlab="Składnik Wiodący",
  ylab="Procent Wariancji",
  names.arg=rows
)
perc <- round(c(dataset.pr.per), digits = 2)
text(x = xx, y = perc,
     label = perc, pos = 3, cex = 0.8, col = "red")


screeplot(dataset.pr, type = "l", 
          npcs = 14,
          main = "Wykres pierwszych 14 składników wiodących")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Wartość własna (Eigenvalue) = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(dataset.pr$sdev^2 / sum(dataset.pr$sdev^2))
plot(cumpro[0:15], xlab = "Numer PC", ylab = "Wariancja sumaryczna", main = "Wykres skumulowanej wariancji")
abline(v = 4, col="blue", lty=5)
abline(h = 0.915, col="blue", lty=5)
legend("topleft", legend=c("Punkt odcięcia dla PC4"),
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
