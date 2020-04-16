
dataset <- read.csv('eeg_data.csv')

# 960 próbek dla każdej z klas
length(which(dataset$Class == 2))
length(which(dataset$Class == 1))
length(which(dataset$Class == 0))


# Przykładowa dystrybucja klas
cols <- c("red","blue", "green")

plot(
  cbind(dataset$AF3.delta.std, dataset$F7.delta.std),
  xlab = 'F7 delta std',
  ylab = 'AF3 delta std',
  col=cols[ dataset[,1] + 1 ],
  pch=20
)
legend("topleft", 
       legend=c("Class=1", "Class=2", "Class=3"),
       col=cols, lty=5, cex=0.5)


# Dane bez klasy
dataset.body <- dataset[c(2:ncol(dataset))]

# Parzyste kolumny - same std
dataset.body.std <- dataset.body[, c(TRUE,FALSE) ]

# PCA bez centrowania!!!
dataset.pca <- prcomp(
  dataset.body.std,
  center = FALSE, scale = TRUE)
summary(dataset.pca)

# Dystrybucja Wariancji
dataset.pca.per <- summary(dataset.pca)$importance[2,]*100

# Wyświetlamy pierwsze 5
dataset.pca.per.5 <- dataset.pca.per[c(1:5)]
rows <- rownames(as.data.frame(dataset.pca.per.5))
ylim <- c(0, 1.2*max(dataset.pca.per.5))

xx <- barplot(
  dataset.pca.per.5, main="Dystrybucja Wariancji",
  ylim=ylim,
  xlab="Składnik Wiodący",
  ylab="Procent Wariancji",
  names.arg=rows
)
perc <- round(c(dataset.pca.per.5), digits = 1)
text(x = xx, y = perc,
     label = perc, pos = 3, cex = 0.8, col = "red")

# Screeplot

dataset.pca.5 <- dataset.pca[c(1:5)]

screeplot(dataset.pca.5, type = "l", 
          npcs = 5, main = "Wykres pierwszych 5 składników wiodących")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Wartość własna (Eigenvalue) = 1"),
       col=c("red"), lty=5, cex=0.6)

# Suma skumulowana
cumpro <- summary(dataset.pca)$importance[3,]*100
cumpro <- cumpro[1:5]
ylim <- c(0, 1.2*max(cumpro))
barplot(
  cumpro,
  xlab = "Numer PC", ylab = "Wariancja sumaryczna",
  ylim=ylim,
  main = "Wykres skumulowanej wariancji"
)
perc <- round(c(cumpro), digits = 3)
cumpro
text(x = xx, y = perc,
     label = perc, cex = 1, col = "blue")


# PC1 vs PC2 - z konsoli
library("factoextra")
fviz_pca_ind(dataset.pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = factor(dataset$Class),
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Eye Detection") +
  ggtitle("2D PCA-plot") +
  theme(plot.title = element_text(hjust = 0.5)) + xlim(-10, -4) + ylim(-3, 5)

# LDA
require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)

dataset.lda <- lda(
  formula = Class ~ .,
  data = cbind(dataset[1], dataset.body.std)
)
summary(dataset.lda)

dataset.lda.predict <- predict(
  object = dataset.lda,
  newdata = dataset.body.std)

# Proporcje
prop.pca <- dataset.pca$sdev^2/sum(dataset.pca$sdev^2)
prop.pca <- prop.pca[1:2]
prop.lda <- dataset.lda$svd^2/sum(dataset.lda$svd^2)

# Kombinowany dataset
newdataset <- data.frame(
  class = dataset[,"Class"],
  pca = dataset.pca$x [,c(1:2)],
  lda = dataset.lda.predict$x)

# Porównanie
p1 <- ggplot(newdataset) + 
  geom_point(
    aes(lda.LD1, lda.LD2, 
      colour = factor(class),
      shape = factor(class)
      ),
      size = 2.5) + 
  labs(
    x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
    y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

p2 <- ggplot(newdataset) + 
  geom_point(aes(pca.PC1, pca.PC2, 
      colour = factor(class),
      shape = factor(class)
      ), size = 2.5) +
  labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

grid.arrange(p1, p2)
