data {
  int<lower=0> ntrials;
  int<lower=0,upper=1> y[ntrials];
  int<lower=0,upper=1> u[ntrials];
  int<lower=0,upper=1> pain[ntrials];
  
  
  
  
}

parameters {
  real omegan;
  real omegapain;
  
  //real<lower=0> beta;
  
  
  
}



transformed parameters {
  real kappa;
  real theta;
  
  
  real mu2[ntrials+1];
  real mu3[ntrials+1];
  real sa2[ntrials+1];
  real sa3[ntrials+1];
  
  real mu1[ntrials];
  real sa2hat[ntrials];
  real mu1hat[ntrials];
  real sa1hat[ntrials];
  real da[ntrials];
  real da2[ntrials];
  real r2[ntrials];
  real w2[ntrials];
  real pi3hat[ntrials];
  real pi3[ntrials];
  real belief[ntrials];
  
  
  kappa = 0;
  theta = 0;
  
  mu2[1] = 0;
  sa2[1] = 2;
  mu3[1] = 0;
  sa3[1] = 2;
  
  mu1hat[1] = 0.5;
  belief[1] = 0.5;
  
  sa2hat[1] = 999;
  sa1hat[1] = 999;
  pi3hat[1] = 999;
  pi3[1] = 999;
  
  

  for (t in 2:ntrials) {
    mu1hat[t] = inv_logit(mu2[t-1]);
    
    if(pain[t] == 1){
    sa2hat[t] = sa2[t-1]+exp(omegapain);
    sa1hat[t] = mu1hat[t]*(1-mu1hat[t]);
    pi3hat[t] = 1/(sa3[t-1]+theta);
    
    r2[t] = (exp(kappa*mu3[t-1]+omegapain)-sa2[t-1])/(sa2[t-1]+exp(kappa*mu3[t-1]+omegapain));
    
    w2[t] = exp(kappa*mu3[t-1]+omegapain)/(sa2[t-1]+exp(kappa*mu3[t-1]+omegapain));
     
    sa2[t] = 1/((1/sa2hat[t])+sa1hat[t]);
    
    
    da[t] = u[t]-mu1hat[t];
    
    
    mu2[t] = mu2[t-1]+da[t]*sa2[t];    
    
    da2[t] = ((sa2[t]+(mu2[t]-mu2[t-1])^2)/(sa2[t-1]+exp(kappa*mu3[t-1]+omegapain)))-1;
    
    }else{
      
      sa2hat[t] = sa2[t-1]+exp(omegan);
      sa1hat[t] = mu1hat[t]*(1-mu1hat[t]);
      pi3hat[t] = 1/(sa3[t-1]+theta);
      
      r2[t] = (exp(kappa*mu3[t-1]+omegan)-sa2[t-1])/(sa2[t-1]+exp(kappa*mu3[t-1]+omegan));
      
      w2[t] = exp(kappa*mu3[t-1]+omegan)/(sa2[t-1]+exp(kappa*mu3[t-1]+omegan));
       
      sa2[t] = 1/((1/sa2hat[t])+sa1hat[t]);
      
      
      da[t] = u[t]-mu1hat[t];
      
      
      mu2[t] = mu2[t-1]+da[t]*sa2[t];    
      
      da2[t] = ((sa2[t]+(mu2[t]-mu2[t-1])^2)/(sa2[t-1]+exp(kappa*mu3[t-1]+omegan)))-1;
      
    }
   

    pi3[t] = pi3hat[t]+(kappa^2/2)*w2[t]*(w2[t]+r2[t]*da2[t]);
    


    sa3[t] = 1/pi3[t];
    
    mu3[t] = mu3[t-1]+sa3[t]*(kappa/2)*w2[t]*da2[t];
    
  
    
}


}



model {
    //priors
  
    target += normal_lpdf(omegapain | -3,3);
    target += normal_lpdf(omegan | -3,3);
    
    
    //likelihood
    target += bernoulli_lpmf(y|mu1hat);
  }




generated quantities{
  real p_omegapain = normal_rng(-3,3);
  real p_omegan = normal_rng(-3,3);  

}

