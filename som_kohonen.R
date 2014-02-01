
#install.packages("kohonen")
library(kohonen)

close.screen(all=T)

# fix random seed.
set.seed(30)

# initiate grid. 
gr <- somgrid(topo="hexagonal", xdim=10, ydim=7)

# make som map.
iris.som <- som(as.matrix(iris[,1:4]), gr, rlen=200)

par(mfrow=c(1,2)) 
# show map
plot(iris.som, type="codes")

lab.cod <- as.numeric(iris[,5])

plot(iris.som, type="mapping", labels=lab.cod, col=lab.cod)