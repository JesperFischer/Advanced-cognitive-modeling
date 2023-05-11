

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> S;
  matrix[N,S] rating1;
  matrix[N,S] rating2;
  matrix[N,S] group;
  
}



transformed data{
  matrix<lower=0, upper = 1>[N, S] rating11;
  matrix<lower=0, upper = 1>[N, S] rating22;
  matrix<lower=0, upper = 1>[N, S] groupp;
  
  rating11 = rating1/9;
  rating22 = rating2/9;
  groupp = group/9;
  
}


parameters {
  
  
  real<lower=0, upper = 1> w1_mu;
  real<lower=0> w1_sd;
  
  real<lower=0, upper = 1> w2_mu;
  real<lower=0> w2_sd;
  
  real<lower=0, upper = 1> bias_mu;
  real<lower=0> bias_sd;
  
  real<lower=0> kappa_mu;
  real<lower=0> kappa_sd;
  
  real<lower=0> kappa2_mu;
  real<lower=0> kappa2_sd;
  
  
  vector[S] w1ID_Z;
  vector[S] w2ID_Z;
  vector[S] biasID_Z;
  vector[S] kappaID_Z;
  vector[S] kappa2ID_Z;

}

transformed parameters{
  vector[S] w1ID;
  vector[S] w2ID;
  vector[S] biasID;
  vector[S] kappaID;
  vector[S] kappa2ID;
 
  biasID = biasID_Z * bias_sd;
  w1ID = w1ID_Z * w1_sd;
  w2ID = w2ID_Z * w2_sd;
  kappaID = kappaID_Z * kappa_sd;
  kappa2ID = kappa2ID_Z * kappa2_sd;
  
}
  
model {
  
  target += std_normal_lpdf(to_vector(biasID_Z));
  target += std_normal_lpdf(to_vector(w1ID_Z)); 
  target += std_normal_lpdf(to_vector(w2ID_Z)); 
  target += std_normal_lpdf(to_vector(kappaID_Z));
  target += std_normal_lpdf(to_vector(kappa2ID_Z)); 
 

  
  target += beta_proportion_lpdf(w1_mu |0.5,10);
  
  target += beta_proportion_lpdf(w2_mu |0.5,10);
  
  target += beta_proportion_lpdf(bias_mu |0.5,10);
  
  target += lognormal_lpdf(kappa_mu |1,2);
  
  target += lognormal_lpdf(kappa2_mu |1,2);
  
  
  target += normal_lpdf(bias_sd |0,5)-normal_lccdf(0 | 0,5);
  
  target += normal_lpdf(kappa_sd |0,5)-normal_lccdf(0 | 0,5);
  
  target += normal_lpdf(kappa2_sd |0,5)-normal_lccdf(0 | 0,5);
  
  target += normal_lpdf(w1_sd |0,5)-normal_lccdf(0 | 0,5);
  
  target += normal_lpdf(w2_sd |0,5)-normal_lccdf(0 | 0,5);
  
  
  for(s in 1:S){
    for(i in 1:N){
      target += beta_proportion_lpdf(rating11[i,s] | bias_mu+biasID[s], kappa_mu+kappaID[s]);
      
      target += beta_proportion_lpdf(rating22[i,s] | inv_logit(w1_mu+w1ID[s]*logit(rating11[i,s])+w2_mu+w2ID[s]*logit(groupp[i,s])), kappa2_mu+kappa2ID[s]);
    
    }
  }
}

generated quantities{
  real prior_bias_mu;
  real prior_bias_sd;
  real prior_w1_mu;
  real prior_w1_sd;
  real prior_w2_mu;
  real prior_w2_sd;
  real prior_kappa_mu;
  real prior_kappa_sd;
  real prior_kappa2_mu;
  real prior_kappa2_sd;
  vector[S] prior_bias;
  vector[S] prior_w1;
  vector[S] prior_w2;
  vector[S] prior_kappa;
  vector[S] prior_kappa2;  
  matrix[N, S] log_lik;

  prior_bias_mu = beta_proportion_rng(0.5,10);
  prior_bias_sd = lognormal_rng(-1,1);
  
  prior_w1_mu = beta_proportion_rng(0.5,10);
  prior_w1_sd = lognormal_rng(-1,1);
  
  prior_w2_mu = beta_proportion_rng(0.5,10);
  prior_w2_sd = lognormal_rng(-1,1);
  
  
  prior_kappa_mu = lognormal_rng(0,1);
  prior_kappa_sd = lognormal_rng(-1,1);
  
  prior_kappa2_mu = lognormal_rng(0,1);
  prior_kappa2_sd = lognormal_rng(-1,1);
    
    
    
  for(s in 1:S){
    
      prior_bias[s] = beta_proportion_rng(prior_bias_mu,prior_bias_sd);
      
      prior_w1[s] = beta_proportion_rng(prior_w1_mu,prior_w1_sd);

      prior_w2[s] = beta_proportion_rng(prior_w2_mu,prior_w2_sd);

      prior_kappa[s] = lognormal_rng(prior_kappa_mu,prior_kappa_sd);
      
      prior_kappa2[s] =  lognormal_rng(prior_kappa2_mu,prior_kappa2_sd);
      
      
      
      for(i in 1:N){
        
        log_lik[i,s] = beta_proportion_lpdf(rating22[i,s] | inv_logit(w1_mu+w1ID[s]*logit(rating11[i,s])+w2_mu+w2ID[s]*logit(groupp[i,s])), kappa2_mu+kappa2ID[s]);
      }
      
  }
}

