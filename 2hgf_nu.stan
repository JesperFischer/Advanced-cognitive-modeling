data {
  int<lower=0> ntrials;
  int<lower=0,upper=1> y[ntrials];
  int<lower=0,upper=1> ur[ntrials];
  
  
  
  
}

parameters {
  real omega;
  //real<lower=0> beta;
  real <lower = 0> z;
  real <lower = 0> nu;
  
  
}



transformed parameters {
  
  real mu2[ntrials+1];
  real sa2[ntrials+1];
  
  real mu1[ntrials];
  real sa2hat[ntrials];
  real mu1hat[ntrials];
  real sa1hat[ntrials];
  real da[ntrials];
  real belief[ntrials];
  real belief2[ntrials];
  
  
  //real kappa;
  //real theta;
  
  //kappa = 0;
  //theta = 0;
  
  mu2[1] = 0;
  sa2[1] = 2;
  
  mu1hat[1] = 0.5;
  belief[1] = 0.5;
  belief2[1] = 0.5;
  
  
  sa2hat[1] = 3;
  sa1hat[1] = 0.25;
  
  

  for (t in 2:ntrials) {
    mu1hat[t] = inv_logit(mu2[t-1]);
    sa2hat[t] = sa2[t-1]+exp(omega);
    sa1hat[t] = mu1hat[t]*(1-mu1hat[t]);
    
    da[t] = y[t-1]-mu1hat[t];
     
     
     
    sa2[t] = 1/((1/sa2hat[t])+sa1hat[t]);
    
    
    mu2[t] = mu2[t-1]+da[t]*sa2[t];    
    
    
    belief[t] = mu1hat[t]+(1/(1+nu))*(ur[t]-mu1hat[t]);
    
    //belief[t] = mu1hat[t];
    
    //belief2[t] = (belief[t]^z)/(belief[t]^z+(1-belief[t])^z);
    
}
}



model {
    //priors
  
    target += normal_lpdf(omega | -6,5);
    target += lognormal_lpdf(nu | -5, 0.5);
    target += lognormal_lpdf(z | 0, 1);
    
    
    
    //likelihood
    for(i in 1:ntrials)
      target += bernoulli_lpmf(y[i]|belief[i]);
  }




generated quantities{
  real p_omega = normal_rng(-6,5);
  real p_nu = lognormal_rng(0,0.5);
  

}

