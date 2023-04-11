data {
  int<lower=0> ntrials;
  int<lower=0,upper=1> y[ntrials];
  int<lower=0,upper=1> u[ntrials];
  int prior;
  real priormean;
  real priorsd;
  
  
  
  
}

parameters {
  real <upper = 0> omega;
  //real<lower=0> beta;
  //real <lower = 0> theta;
  //real <lower = 0> kappa;
  
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
  real theta = 0;
  real kappa = 0;
  
  
  //real kappa = 0.1;
  //real omega = -4;
  //real theta = 0.3;
  mu2[1] = 0;
  sa2[1] = 2;
  mu3[1] = 0;
  sa3[1] = 2;
  
  mu1hat[1] = 0.5;
  sa2hat[1] = 999;
  sa1hat[1] = 999;
  pi3hat[1] = 999;
  pi3[1] = 999;
  


  for (t in 2:ntrials) {
    
    //if(inv_logit(mu2[t-1]) > 0.8){
    //  mu1hat[t] = 0.8;
    //}else if(inv_logit(mu2[t-1]) < 0.2){
    //  mu1hat[t] = 0.2;
    //}else{
      mu1hat[t] = inv_logit(mu2[t-1]);
    //}
    //if(is_nan(mu1hat[t])){
    //  mu1hat[t] = 0.5;
    //}
    
    
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
  
    
}


}



model {
    //priors
  
    
    //target += uniform_lpdf(kappa | 0,4);
    target += normal_lpdf(omega | priormean,priorsd);
    //target += uniform_lpdf(theta | 0,2);
    
    
  if(prior == 0){
  
    //likelihood
    target += bernoulli_lpmf(y|mu1hat);
    
  }
}




generated quantities{
  real p_omega = normal_rng(-4,2);
  //real p_theta = lognormal_rng(0,0.3);
  //real p_kappa = uniform_rng(0,2);
  real sim_resp;
  real sim_resp_t[ntrials];
  
  sim_resp_t = bernoulli_rng(mu1hat);
  sim_resp = sum(sim_resp_t);
}



