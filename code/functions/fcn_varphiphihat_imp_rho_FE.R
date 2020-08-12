#######################################################################
#######################################################################
#######  model implied autocorr in FE due to var(phi-phihat)  #########
#######################################################################
####################################################################### 

# Global variables.
# true.sigma.a 
# true.sigma.e  
# true.sigma.n  

fcn_varphiphihat_imp_rho_FE <- function(calibpar,NN) { 
    
  #### matching mean cov for turncated normal
  invTrcNorm <- function(x) { 
    xx  = matrix(c(x[1],x[2],0,x[3]),ncol=2)  
    fcnval = mtmvnorm(x[c(4,5)], xx%*%t(xx), lower=c(-1,-1),  upper=c(1,1) )
    y = c( (fcnval$tvar-COV_phiphihat)[c(1,2,4)], (fcnval$tmean - c(true.phi0, est.phi0)) )
    
    return(y)
  }
  
  invTrcNorm.start <- function() { 
    fcnval = mtmvnorm(c(true.phi0, est.phi0), COV_phiphihat, lower=c(-1,-1),  upper=c(1,1) )$tvar
    xstart = numeric(3)
    xstart[1] = -fcnval[1,1] + 2*COV_phiphihat[1,1]
    xstart[2] = -fcnval[2,2] + 2*COV_phiphihat[2,2]
    xstart[3] = sqrt(xstart[1]*xstart[2])*COV_phiphihat[1,2]/sqrt(COV_phiphihat[1,1]*COV_phiphihat[2,2])
    xstart = c(t(chol( matrix(c(xstart[1],xstart[3],xstart[3],xstart[2]),ncol=2) ))[c(1,2,4)],c(true.phi0, est.phi0))
    
    return(xstart)
  }
    
    
  fcn_FE_rho_simple <- function(true.phi, est.phi, est.Kx) { 
    
    ## computation
    true.w = 1/true.sigma.n^2/(1/true.sigma.n^2+1/true.sigma.e^2)
    true.pis2 = 1/(1/true.sigma.e^2+1/true.sigma.n^2)
    est.w  = true.w
    
    ### b/c pospos_varE = 1/(1/pos_varE + 1/(true.sigma.a^2 + pospos_varE*true.phi^2)  );
    a = 1
    b = (1-true.phi^2)*true.sigma.a^2 - true.pis2
    c = -true.pis2*true.sigma.a^2
    
    ### a x^2 + bx + c = 0
    true.pit1t2 = ( -b + sqrt(   b^2 - 4*a*c  )   ) /a / 2  ;
    true.Kx = true.pit1t2/(true.sigma.a^2 + true.pit1t2  )     ;    
    
    varY = ( (1+true.phi^2)*true.sigma.a^2 + true.sigma.e^2 - 2*true.phi^2*true.sigma.a^2  )/(1-true.phi^2)
    covYYh = (true.phi*est.phi*est.Kx*(varY - true.sigma.a^2) + est.w*true.sigma.e^2  )/(1-true.phi*est.phi*(1-est.Kx)); 
    varYh = ( est.phi^2*est.Kx^2*varY + est.w^2*(true.sigma.e^2+true.sigma.n^2) + 2*est.phi^2*(1-est.Kx)*est.Kx*covYYh  )/(1-est.phi^2*(1-est.Kx)^2); 
    
    covYY1 = true.phi*( varY -  true.sigma.a^2);
    covYhYh1 = est.phi*( (1-est.Kx)*varYh + est.Kx*covYYh );
    covY1Yh = true.phi*covYYh;
    covYh1Y = est.phi*( (1-est.Kx)*covYYh + est.Kx*varY );
    
    ACF0 = varY + varYh - 2*covYYh;
    ACF1 = covYY1 + covYhYh1 - covY1Yh - covYh1Y;
    rho1 = ACF1/ACF0 
    return(mean(rho1))
  }
    
  
  ## moments to match : COV(phi, phihat)  from mixed models
  load("phi_learn_lmer00.Rdata") 
  var_est_phi       = (phi_learn$phivar)
  var_est_phihat    = (phi_learn$phihatvar)
  var_est_phiphihat = (phi_learn$phiphihatvar)
  cov_est_phiphihat = (var_est_phi+var_est_phihat-var_est_phiphihat)/2
  COV_phiphihat_set = matrix(rbind(var_est_phi,cov_est_phiphihat,cov_est_phiphihat, var_est_phihat),nrow=4)
  
  phi_imp_rho_FE = matrix( rep(NA, 3*length(var_est_phi ) ), ncol=3)

  for (ii in seq(1, (length(var_est_phi )-1)   )) {
    
    ## set parameters for calculations
    true.phi0 = calibpar$phi0[ii]
    est.phi0 = calibpar$phihat0[ii]
    est.Kx = calibpar$Khat[ii]
    
    ## copmute matching truncated BVN distribution
    COV_phiphihat = matrix( COV_phiphihat_set[,ii], 2,2)    
    sols = nleqslv( invTrcNorm.start() , invTrcNorm, control=list(btol=.001))
    t_chol_COV_phiphihat_adj = matrix(c(sols$x[1],sols$x[2],0,sols$x[3]),ncol=2)
    true_est_phi0_adj  = sols$x[4:5]  
    
    ## Simulate phi_i, phihat_i
    if (prod(diag(COV_phiphihat)==0)==1) e_phi = 0*matrix(rnorm(2*NN),nrow=2) else 
      e_phi = t_chol_COV_phiphihat_adj%*%matrix(rnorm(2*NN),nrow=2)
          
    true.phi = true_est_phi0_adj[1] +   e_phi[1,]
    est.phi = true_est_phi0_adj[2] +  e_phi[2,]
    
    statidx = abs(true.phi)<1 & abs(est.phi)<1 
    true.phi = true.phi[statidx] 
    est.phi = est.phi[statidx] 
      
    ## compute impied rho(FE)
    phi_imp_rho_FE[ii,1] = fcn_FE_rho_simple(true.phi , est.phi , est.Kx) 
    phi_imp_rho_FE[ii,2] = fcn_FE_rho_simple(true.phi0, est.phi0, est.Kx)  
    phi_imp_rho_FE[ii,3] = fcn_FE_rho_simple(true.phi0, true.phi0, est.Kx)  
         
    print(ii)
  }

  return(phi_imp_rho_FE)

}