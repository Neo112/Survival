tt <- unique(sort(start))
ll = function(beta, test1){
  pl <- 0
  for(i in which(test1[,status==1])){
    t <- test1[i, stop]
    ri <- which((test1[,start]<t) & (test1[, stop]>=t))
    zj <- test1[ri, z_buy1m]
    fz <- sum(zj*exp(beta * zj))
    fm <- sum(exp(beta * zj))
    pl <- pl + test1[i, z_buy1m] - fz/fm
  }
  return(pl)
}
l2 = function(beta, test1){
  pl <- 0
  for(i in which(test1[,status==1])){
    t <- test1[i, stop]
    ri <- which((test1[,start]<t) & (test1[, stop]>=t))
    zj <- test1[ri, z_buy1m]
    fz <- sum(zj^2*exp(beta * zj))
    fm <- sum(exp(beta * zj))
    pl <- pl + 1 - fz/fm
  }
  return(pl)
}
beta=c(-0.1, 0, 0.1, 0.2)
ll(1)
temp <- lapply(0, l2, test1)
temp
