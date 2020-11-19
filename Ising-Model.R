binseq <- function(N){
  expand.grid(replicate(N, c(-1,1), simplify = FALSE))
}
all.configs <- function(N){
  X = binseq(N*N)
  data <-list()
  for (i in 1:(2^{N*N})) {
    data[[i]] <- matrix(X[i,],nrow = N, ncol = N)
  }
  data
}
all.configs.2 <-all.configs(2)
all.configs.3 <- all.configs(3)
all.configs.4 <- all.configs(4)

alldown.config <- function(N) {matrix(-1,N,N)}
allup.config <- function(N) {matrix(1,N,N)}

#random config
random.config <- function(N){
  config <-matrix(0,N,N)
  for (i in 1:N) {
    for (j in 1:N){
      config[i,j] <- 2*sample(0:1,1)-1
    }
  }
  config
}
#neighbours
per.neigh <- function(N,i,j) {
  if (i == 1) im1 <- N else im1 <- i-1
  if (j == 1) jm1 <- N else jm1 <- j-1
  if (i == N) ip1 <- 1 else ip1 <- i+1
  if (j == N) jp1 <- 1 else jp1 <- j+1
  list(c(im1,j),c(i,jm1),c(i,jp1),c(ip1,j))
}
#energy
energy <- function(config, J) {
  config <- as.matrix(config)
  N <- nrow(config);
  e <- 0;
  for (i in 1:N) {
    for (j in 1:N) {
      coord <- per.neigh(N,i,j)
      for (b in seq_along(coord)) {
        w <- coord[[b]]; 
        e <- e - (J  as.numeric(config[i,j])  as.numeric(config[w[1],w[2]]))
      }
    }
  }
  e
}
#Image config
plot.config <- function(config){
  image(config, useRaster = TRUE,axes= FALSE, col = grey(seq(0,1)))
}
#Magnetization
magnetization <-function(config){
  sum(as.vector(config))
}