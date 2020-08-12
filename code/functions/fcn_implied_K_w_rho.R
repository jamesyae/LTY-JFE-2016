fcn_implied_K_w_rho<- function(theta) {
 
    
  true.phi = theta[1,]
  est.phi = theta[2,]
  est.Kx = theta[3,]
  est.w  = theta[4,]
  true.sigma.a = exp(theta[5,])
  true.sigma.e = exp(theta[6,])
  true.sigma.n = exp(theta[7,])
  
   

  
  ## computation
  true.w = 1/true.sigma.n^2/(1/true.sigma.n^2+1/true.sigma.e^2)
  true.pis2 = 1/(1/true.sigma.e^2+1/true.sigma.n^2)
  ### b/c pospos_varE = 1/(1/pos_varE + 1/(true.sigma.a^2 + pospos_varE*true.phi^2)  );
  #a = 1/true.sigma.a^2 
  #b = 1-true.phi^2 - true.pis2/true.sigma.a^2  
  #c = -true.pis2
  
  a = 1
  b = (1-true.phi^2)*true.sigma.a^2 - true.pis2
  c = -true.pis2*true.sigma.a^2
  
  ### a x^2 + bx + c = 0
  true.pit1t2 = ( -b + sqrt(   b^2 - 4*a*c  )   ) /a / 2  ;
  true.Kx = true.pit1t2/(true.sigma.a^2 + true.pit1t2  )     ;
  
  varY = ( (1+true.phi^2)*true.sigma.a^2 + true.sigma.e^2 - 2*true.phi^2*true.sigma.a^2  )/(1-true.phi^2)
  covYYh = (true.phi*est.phi*est.Kx*(varY - true.sigma.a^2) + est.w*true.sigma.e^2  )/
            (1-true.phi*est.phi*(1-est.Kx)); 
  varYh = ( est.phi^2*est.Kx^2*varY + est.w^2*(true.sigma.e^2+true.sigma.n^2) + 2*est.phi^2*(1-est.Kx)*est.Kx*covYYh  )/
            (1-est.phi^2*(1-est.Kx)^2); 
  
  covYY1 = true.phi*( varY -  true.sigma.a^2);
  covYhYh1 = est.phi*( (1-est.Kx)*varYh + est.Kx*covYYh );
  covY1Yh = true.phi*covYYh;
  covYh1Y = est.phi*( (1-est.Kx)*covYYh + est.Kx*varY );
  
  
  ACF0 = varY + varYh - 2*covYYh;
  ACF1 = covYY1 + covYhYh1 - covY1Yh - covYh1Y;
  rho1 = ACF1/ACF0 ;  
  
  Imp = list(w = true.w , K=true.Kx, FE_rho = rho1)
  return(Imp)
  
}