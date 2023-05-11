

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


// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0, upper = 1> bias_mu;
  real<lower=0> bias_sd;
  
  real<lower=0> kappa_mu;
  real<lower=0> kappa_sd;
  
  real<lower=0> kappa2_mu;
  real<lower=0> kappa2_sd;
  
  vector<lower=0, upper = 1>[S] bias;
  vector<lower=0>[S] kappa;
  vector<lower=0>[S] kappa2;

}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  target += beta_proportion_lpdf(bias_mu |0.5,10);
  
  target += lognormal_lpdf(bias_sd |2,0.5);
  
  target += lognormal_lpdf(kappa_mu |0,1);
  
  target += lognormal_lpdf(kappa_sd |-1,1);
  
  target += lognormal_lpdf(kappa2_mu |0,1);
  
  target += lognormal_lpdf(kappa2_sd |-1,1);
  
  for(s in 1:S){
    
      target += beta_proportion_lpdf(bias[s] | bias_mu,bias_sd);
      target += lognormal_lpdf(kappa[s]      | kappa_mu,kappa_sd);
      target += lognormal_lpdf(kappa2[s]     | kappa2_mu,kappa2_sd);

      
    for(i in 1:N){
      target += beta_proportion_lpdf(rating11[i,s] | bias[s], kappa[s]);
      
      target += beta_proportion_lpdf(rating22[i,s] | inv_logit(0.5*logit(rating11[i,s])+0.5*logit(groupp[i,s])), kappa2[s]);
    
    }
  }
}

generated quantities{
  
  real prior_bias_mu;
  real prior_bias_sd;
  
  real prior_kappa_mu;
  real prior_kappa_sd;
  
  
  real prior_kappa2_mu;
  real prior_kappa2_sd;
  
  vector[S] prior_bias;
  vector[S] prior_kappa;
  vector[S] prior_kappa2;  
  
  
  
  matrix[N,S] sim_rating1;
  matrix[N,S] sim_rating2;
  
  
  matrix[N, S] log_lik;
   
  

  prior_bias_mu = beta_proportion_rng(0.5,10);
  prior_bias_sd = lognormal_rng(2,0.5);
  
  prior_kappa_mu = lognormal_rng(0,1);
  prior_kappa_sd = lognormal_rng(-1,1);
  
  prior_kappa2_mu = lognormal_rng(0,1);
  prior_kappa2_sd = lognormal_rng(-1,1);
    
    
    
    
  for(s in 1:S){
    
      prior_bias[s] = beta_proportion_rng(prior_bias_mu,prior_bias_sd);
      
      prior_kappa[s] = lognormal_rng(prior_kappa_mu,prior_kappa_sd);
      
      prior_kappa2[s] =  lognormal_rng(prior_kappa2_mu,prior_kappa2_sd);
      
      for(i in 1:N){
        
        log_lik[i,s] = beta_proportion_lpdf(rating22[i,s] | inv_logit(0.5*logit(rating11[i,s])+0.5*logit(groupp[i,s])), kappa2[s]);
        
        sim_rating1[i,s] = beta_proportion_rng(bias[s], kappa[s]);
      
        sim_rating2[i,s] = beta_proportion_rng(inv_logit(0.5*logit(rating11[i,s])+0.5*logit(groupp[i,s])), kappa2[s]);
    
        
      }
  }





}

