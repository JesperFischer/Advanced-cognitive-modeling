data {
  int<lower=0> ntrials;
  int<lower=0,upper=1> y[ntrials];
  int<lower=0,upper=1> u[ntrials];
  int<lower=0,upper=1> ur[ntrials];
  
  
  
  
}

parameters {
  real omega;
  //real<lower=0> beta;
  
  real <lower = 0, upper = 1> nu;
  real <lower = 0> kappa;
  real <lower = 0> theta;
  
  
}



transformed parameters {
  
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
  
  //real kappa;
  //real theta;
  
  //kappa = 0;
  //theta = 0;
  
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
    sa2hat[t] = sa2[t-1]+exp(kappa*mu3[t-1]+omega);
    sa1hat[t] = mu1hat[t]*(1-mu1hat[t]);
    pi3hat[t] = 1/(sa3[t-1]+theta);
    
    r2[t] = (exp(kappa*mu3[t-1]+omega)-sa2[t-1])/(sa2[t-1]+exp(kappa*mu3[t-1]+omega));
    
    w2[t] = exp(kappa*mu3[t-1]+omega)/(sa2[t-1]+exp(kappa*mu3[t-1]+omega));
     
    sa2[t] = 1/((1/sa2hat[t])+sa1hat[t]);
    
   

    da[t] = u[t]-mu1hat[t];
    
    
    mu2[t] = mu2[t-1]+da[t]*sa2[t];    
    
    da2[t] = ((sa2[t]+(mu2[t]-mu2[t-1])^2)/(sa2[t-1]+exp(kappa*mu3[t-1]+omega)))-1;
    
    pi3[t] = pi3hat[t]+(kappa^2/2)*w2[t]*(w2[t]+r2[t]*da2[t]);
    


    sa3[t] = 1/pi3[t];
    
    mu3[t] = mu3[t-1]+sa3[t]*(kappa/2)*w2[t]*da2[t];
    
    belief[t] = mu1hat[t]+(1/(1+nu))*(ur[t]-mu1hat[t]);
  
    
}


}



model {
    //priors
  
    target += normal_lpdf(omega | -6,5);
    target += lognormal_lpdf(nu | 0, 0.5);
    target += normal_lpdf(theta | -3,2);
    target += lognormal_lpdf(kappa | 0,0.4);
    
    
    //likelihood
    for(i in 1:ntrials)
      target += bernoulli_lpmf(y[i]|belief[i]);
  }




generated quantities{
  real p_omega = normal_rng(-6,5);
  real p_nu = lognormal_rng(0,0.5);
  

}

