fcn_BVN_high_moments<- function(mu, Sigma){
  # Isserlis' theorem
  # Isserlis, L. (1918). "On a formula for the product-moment coefficient of any order of a normal frequency distribution in any number of variables". Biometrika 12: 134-139.
  # (x,y) ~ BVN(mu, Sigma)
  Ex = mu[1]
  Ey = mu[2]
  
  Exy = Sigma[1,2]+Ex*Ey
  Exx = Sigma[1,1]+Ex^2
  Eyy = Sigma[2,2]+Ey^2
  Exxy = 2*Ex*Exy + Ey*Sigma[1,1]-Ex^2*Ey
  Exyy = 2*Ey*Exy + Ex*Sigma[2,2]-Ey^2*Ex
  Exxyy = Sigma[1,1]*Sigma[2,2] + 2*Sigma[1,2]^2 +2*Ey*Exxy - 
    Ey^2*Exx - Ex^2*Eyy + 2*Ey*Ex^2*Ey - Ex^2*Ey^2 + 
    2*Ex*Exyy - 4*Ex*Ey*Exy + 2*Ex*Ey^2*Ex
  
  return(list(Ex=Ex, Ey=Ey, Exy=Exy, Exx=Exx, Eyy=Eyy, Exxy=Exxy, Exyy=Exyy, Exxyy=Exxyy) )
}




mu = matrix(c(4,5), ncol=1)
Sigma=( matrix(c(3, 2, 2,4  ),2,2)  )
xy = matrix(rep(mu,10^7),nrow=2) + t(chol(Sigma))%*%matrix(rnorm(2*10^7),nrow=2)
mean(xy[1,]*xy[2,])
mean(xy[1,]*xy[1,])
mean(xy[2,]*xy[2,])
mean(xy[1,]^2*xy[2,])
mean(xy[1,]*xy[2,]^2)
mean(xy[1,]^2*xy[2,]^2)

x = fcn_BVN_high_moments(mu, Sigma)
x 
#### fucntion works fine.