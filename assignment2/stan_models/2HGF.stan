data {
  int<lower=0> ntrials;
  int<lower=0,upper=1> y[ntrials];
  int<lower=0,upper=1> u[ntrials];
}

parameters {
  real omega;
  //real<lower=0> beta;
  //real<lower=0> kappa;
  //real theta;
}



transformed parameters {
  real mu2[ntrials];
  real<lower=0> sa2[ntrials];
  real sa2hat[ntrials];
  real mu1hat[ntrials];
  real sa1hat[ntrials];
  real da[ntrials];
  real da2[ntrials];
  real r2[ntrials];
  real w2[ntrials];
  real pi3hat[ntrials];
  real pi3[ntrials];
  real sa3[ntrials];
  real mu3[ntrials];
  real theta = 0;
  real kappa = 0;

  mu2[1] = 0;
  sa2[1] = 4;
  mu3[1] = 0;
  sa3[1] = 4;
  mu1hat[1] = 0.5;
  

    
  for (t in 2:ntrials) {

    sa2hat[t] = sa2[t-1]+exp(kappa*mu3[t-1]+omega);
    mu1hat[t] = inv_logit(mu2[t-1]);
    sa1hat[t] = mu1hat[t]*(1-mu1hat[t]);
    
    da[t] = u[t]-mu1hat[t];
    
    sa2[t] = 1/((1/sa2hat[t])+sa1hat[t]);
    
    mu2[t] = mu2[t-1]+da[t]*sa2[t];
    
    //update third level:
    da2[t] = ((sa2[t]+(mu2[t]-mu2[t-1])^2)/(sa2[t-1]+exp(kappa*mu3[t-1]+omega)))-1;
    
    r2[t] = (exp(kappa*mu3[t-1]+omega)-sa2[t-1])/(sa2[t-1]+exp(kappa*mu3[t-1]+omega));
    
    w2[t] = exp(kappa*mu3[t-1]+omega)/(sa2[t-1]+exp(kappa*mu3[t-1]+omega));
    
    pi3hat[t] = 1/(sa3[t-1]+theta);
    
    pi3[t] = pi3hat[t]+(kappa^2/2)*w2[t]*(w2[t]+r2[t]*da2[t]);
    
    sa3[t] = 1/pi3[t];
    
    mu3[t] = mu3[t-1]+sa3[t]*(kappa/2)*w2[t]*da2[t];
  
    
}


}



model {
    //priors
    target += normal_lpdf(omega | -4,3);
    //target += lognormal_lpdf(beta | 0,1);
    //target += normal_lpdf(theta | -3,2);
    //target += lognormal_lpdf(kappa | 0,0.4);
    
    //likelihood
    target += bernoulli_lpmf(y|mu1hat);
  }




generated quantities{
  real p_omega = normal_rng(-4,3);
  
  
  
}
