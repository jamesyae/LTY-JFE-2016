fcn_KF_general <- function(theta, Y, xt ) {   
# written by James S. Yae
# theta (a list of 6 elements): parameteres: Notations follow Hamilton's book: TS Analysis, page 377
# Y: data (T x n)  
# xt: additional predictor data (T x k)
  
# Staste: Z(t+1) = F*Z(t) + v(t+1)
# Obs:      y(t) = A'*x(t) + H'*Z(t) + w(t)
# where
# Z(t), v(t+1) : (r x 1)  
# y(t), w(t) : (n x 1) 
# F: (r x r)
# A' : (n x k)  
# H' : (n x r)  
# x(t) : (k x 1) 
# E(v(t)*v(t)') = Q : (r x r)
# E(w(t)*w(t)') = R : (n x n)
  
  A = theta$A
  H = theta$H
  F = theta$F
  Q = theta$Q
  R = theta$R
  
  
  if (sum(abs(eigen( F )$values)>=1)>0) { logpt = -Inf } else { 
    
    T = dim(Y)[1] # data time-length
    n = dim(Y)[2] # number of observations at each period
    r = dim(Q)[1] # number of state variables
    
    logpt = rep(0,T) # log(Likelihood) at each period
    ypred_all = matrix(rep(NA,T*n), ncol=n)
    
    # Initial state follows the stationary distribution
    Zpred = matrix(0,r,1)
    Ppred = matrix(solve( diag(rep(1,r^2)) - F%x%F )%*% matrix(Q, r^2,1),ncol=r)  

    for (tt  in  seq(1,T)) {  
  
      predMSE = t(H)%*%Ppred%*%H + R 
      invpredMSE = solve(predMSE)     
      
      K =  (F%*%Ppred%*%H) %*% invpredMSE  # Kalman gain
      
      if (!is.na(sum(Y[tt,]))) {
        ypred = t(A)%*%t(xt[tt,]) + t(H)%*%Zpred
        FE = t(matrix(Y[tt,],nrow=1)) - ypred 
        logpt[tt] =  -n/2*log(2*pi)  -log(det(predMSE))/2  -  t(FE)%*%invpredMSE%*%FE/2
        ypred_all[tt,] = t(ypred)
        
        Zpred = F%*%Zpred + K%*%FE
        Ppred = (F-K%*%t(H))%*%Ppred%*%t(F-K%*%t(H)) + K%*%R%*%t(K) + Q
        #Ppred = F%*%(Ppred - Ppred%*%H%*%invpredMSE%*%t(H)%*%Ppred   )%*%t(F) + Q
      } else {     # missing value (cannot handle partially missing values at a given period)  
        Zpred = F%*%Zpred             # E[Z(t+1)|y(1:t)]
        Ppred = F%*%Ppred%*%t(F) + Q  # Var[Z(t+1)|y(1:t)]
        #Ppred = F%*%(Ppred - Ppred%*%H%*%invpredMSE%*%t(H)%*%Ppred   )%*%t(F) + Q
      }   
      
      #print(logpt[tt])
    } 
    
  }
    
  return(list(negloglkhd=-sum(logpt), ypred = ypred_all  ) )# return the negative log-likelihood
  
}

