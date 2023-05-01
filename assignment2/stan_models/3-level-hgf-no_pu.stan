
functions{
  real normal_lb_rng(real mu, real sigma, real lb){
    real p = normal_lcdf(lb | mu, sigma);
    real u = uniform_rng(p, 1);
    return (sigma * inv_Phi(u)) + mu;  // inverse cdf for value
  }
}
data {
  int<lower=0> ntrials;
  int<lower=0,upper=1> y[ntrials];
  int<lower=0,upper=1> u[ntrials];
  
  
  
}

parameters {
  real omega;
  real <lower=0> beta;
  real <lower = 0> theta;
  //real <lower = 0> kappa;
  
}



transformed parameters {
  
  real mu2[ntrials];
  real mu3[ntrials];
  real <lower=0> sa2[ntrials];
  real <lower=0> sa3[ntrials];
  
  real <lower=0> sa2hat[ntrials];
  real <lower=0> mu1hat[ntrials];
  real <lower=0> sa1hat[ntrials];
  real da[ntrials];
  real da2[ntrials];
  real r2[ntrials];
  real w2[ntrials];
  real <lower=0> pi3hat[ntrials];
  real <lower=0> pi3[ntrials];
  real <lower=0> belief[ntrials];
  real kappa = 1;
  mu2[1] = 0;
  sa2[1] = 2;
  mu3[1] = 0;
  sa3[1] = 2;
  
  belief[1] = 0.5;
  mu1hat[1] = 0.5;
  sa2hat[1] = 2;
  sa1hat[1] = 0.25;
  pi3hat[1] = 2;
  pi3[1] = 3;
  
  

  for (t in 2:ntrials) {
    
    mu1hat[t] = inv_logit(mu2[t-1]);


    sa2hat[t] = sa2[t-1]+exp(kappa*mu3[t-1]+omega);
    
    sa1hat[t] = mu1hat[t]*(1-mu1hat[t]);
    
    pi3hat[t] = 1/(sa3[t-1]+theta);
    
    r2[t] = (exp(kappa*mu3[t-1]+omega)-sa2[t-1])/(sa2[t-1]+exp(kappa*mu3[t-1]+omega));
    
    w2[t] = (exp(kappa*mu3[t-1]+omega))/(sa2[t-1]+exp(kappa*mu3[t-1]+omega));
     
    sa2[t] = 1/((1/sa2hat[t])+sa1hat[t]);
    
    da[t] = u[t]-mu1hat[t];
    
    
    mu2[t] = mu2[t-1]+da[t]*sa2[t];    
    
    da2[t] = ((sa2[t]+(mu2[t]-mu2[t-1])^2)/(sa2[t-1]+exp(kappa*mu3[t-1]+omega)))-1;
    
    pi3[t] = pi3hat[t]+(kappa^2/2)*w2[t]*(w2[t]+r2[t]*da2[t]);

    sa3[t] = 1/pi3[t];
    
    mu3[t] = mu3[t-1]+sa3[t]*(kappa/2)*w2[t]*da2[t];
    
    belief[t] = mu1hat[t]^beta/((mu1hat[t]^beta)+(1-mu1hat[t])^beta);
  
    
}


}



model {
    //priors
  
    
    target += lognormal_lpdf(theta | -2,1.2); 
    target += normal_lpdf(omega | -6,2);
    //target += lognormal_lpdf(kappa | -2,1.2);
    target += lognormal_lpdf(beta  | 0,1);
    
    
    //likelihood
    for (t in 2:ntrials) {
    target += bernoulli_lpmf(y[t]|belief[t]);
  }
}




generated quantities{
  
  real p_omega = normal_rng(-6,2);
  real p_theta = lognormal_rng(-2,1.2);
  real p_kappa = lognormal_rng(-2,1.2);
  real p_beta = lognormal_rng(0,1);
  
  
}

