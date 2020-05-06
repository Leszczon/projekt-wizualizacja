# install the kohonen package
# install.packages("kohonen")

# load the kohonen package
library("kohonen")

# scale data
iris.sc = scale(iris[, 1:4])

iris.grid = somgrid(xdim = 5, ydim=5, topo="hexagonal")
iris.som = som(iris.sc, grid=iris.grid, rlen=350, alpha=c(0.05,0.01))
iris.som.neurons = iris.som$codes[[1]]

plot(iris.som, type="changes")
plot(iris.som, type="count")
plot(iris.som, type="dist.neighbours")
plot(iris.som, type="codes")

# dim(iris.som[["codes"]][[1]])
# iris.som[["codes"]][[1]][,4]

coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
plot(iris.som, type = "property", property = iris.som.neurons[,4], main=names(iris.som$data)[4], palette.name=coolBlueHotRed)


## use hierarchical clustering to cluster the codebook vectors
groups = 3
a = hclust(dist(iris.som.neurons))
iris.hc = cutree(hclust(dist(iris.som.neurons)), groups)


# plot
plot(iris.som, type="codes", bgcol=rainbow(groups)[iris.hc])

#cluster boundaries
add.cluster.boundaries(iris.som, iris.hc)

#require(plotrix)
#require(grid)

#plot.default(1:10, xaxt = "n", xlab='Some Letters')
#axis(1, at=1:10, labels=letters[1:10])

plot(iris.sc[,1], iris.sc[,2], pch=16, xaxt='n')
#axis(1, at = seq(10, 200, by = 10), las=2)
#axis(1, xaxp=c(10, 200, 19), las=2)

points(iris.som.neurons[,1], iris.som.neurons[,2], pch=9)
#draw.circle(
#  iris.som.neurons[,1],
#  iris.som.neurons[,2],
#  iris.som$radius
#  )



