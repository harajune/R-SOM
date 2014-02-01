# code from http://bi.biopapyrus.net/compute/r-som.html
library(class)

close.screen(all=T)

# define
irisSOM <- function(lliris, init) {
  set.seed(10)
  
  #liris <- scale(lliris[,1:4])
  liris <- lliris[,1:4]
  
  rlen <- 150
  layer <- somgrid("hexagonal", xdim = 10, ydim = 7)
  model <- SOM(liris, grid = layer, alpha=seq(10,0,len=rlen), radii=seq(4,1,len=rlen), init=init)

  # 解析結果の座標情報を求めます
  classes <- as.numeric(knn1(model$codes, liris, 1:70))

  # 同じ座標のデータが重ならないように、座標を少しずらすための乱数を付けます
  x.rand <- runif(nrow(iris), -0.2, 0.2)
  y.rand <- runif(nrow(iris), -0.2, 0.2)
  
  # プロット（左下の図）
  plot(model)
  points(
    model$grid$pts[classes, "x"] + x.rand,   # x座標 
    model$grid$pts[classes, "y"] + y.rand,   # y座標
    pch = as.numeric(lliris[,5]), col = as.numeric(lliris[,5]), cex = 2            # 色やプロットマーカーなど
  )
  symbols(model$grid$pts[,"x"],model$grid$pts[,"y"],circles=rep(0.5,70),inches=F,add=T)
  
}

irisbatchSOM <- function(lliris, init) {
  set.seed(10)
  rlen <- 150
  
  #liris <- scale(lliris[,1:4])
  liris <- lliris[,1:4]
  
  layer <- somgrid("hexagonal", xdim = 10, ydim = 7)
  model <- batchSOM(liris, grid = layer, radii=seq(4,1,len=rlen), init=init)
  
  # 解析結果の座標情報を求めます
  classes <- as.numeric(knn1(model$codes, liris, 1:70))
  
  # 同じ座標のデータが重ならないように、座標を少しずらすための乱数を付けます
  x.rand <- runif(nrow(iris), -0.2, 0.2)
  y.rand <- runif(nrow(iris), -0.2, 0.2)
  
  # プロット（左下の図）
  plot(model)
  points(
    model$grid$pts[classes, "x"] + x.rand,   # x座標 
    model$grid$pts[classes, "y"] + y.rand,   # y座標
    pch = as.numeric(lliris[,5]), col = as.numeric(lliris[,5]), cex = 2            # 色やプロットマーカーなど
  )
  symbols(model$grid$pts[,"x"],model$grid$pts[,"y"],circles=rep(0.5,70),inches=F,add=T)
  
}

set.seed(10)
# サンプルデータをロードします
data(iris)

iris2 <- iris[order(iris[,"Petal.Length"]),]
iris3 <- iris[order(iris[,"Petal.Length"], decreasing=TRUE),]
init <- as.matrix(iris[sample(1L:nrow(iris), 70, replace = FALSE), 1:4, drop = FALSE])

#irisSOM(iris2, init)
#irisSOM(iris3, init)
#irisbatchSOM(iris2, init)
irisbatchSOM(iris3, init)
