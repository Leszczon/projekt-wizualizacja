require(MASS)
library('foreign')
library(ggplot2)
dataset <- read.arff("EEG.arff")
#dataset[1,c(1:14)]
#plot(
  #dataset[,c(1:2)],
  #col=dataset[,15]
#)+ xlim(0, 100) + ylim(1000, 1000)

#dataset.size <- dim(dataset)[1]
#how_many_1 <- length(which(dataset$eyeDetection == 1))
#how_many_0 <- dataset.size - how_many_1
#how_many_0
#how_many_1
#prior <- c(how_many_0, how_many_1) / dataset.size
#prior

dataset.lda <- lda(
  formula = eyeDetection ~ .,
  data = dataset
  )

summary(dataset.lda)
#prop.lda = dataset.ld$svd^2/sum(dataset.ld$svd^2)
#dataset.lda$svd

lda.data <- cbind(dataset, predict(dataset.lda)$x)


ggplot(
  data.frame(lda.data),
  aes(x=LD1, y=0, color=eyeDetection)
  ) + geom_point(shape=4, size = 3) + xlim(-5, 5)

ggplot(
  data.frame(lda.data),
  aes(x=LD1, y=eyeDetection, color=eyeDetection)
  ) + geom_point(shape=4, size = 3) + xlim(-5, 5)
