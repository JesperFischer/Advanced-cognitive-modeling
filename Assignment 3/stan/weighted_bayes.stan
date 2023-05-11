

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] rating1;
  vector[N] rating2;
  vector[N] group;
  
}



transformed data{
  vector<lower=0, upper = 1>[N] rating11;
  vector<lower=0, upper = 1>[N] rating22;
  
  rating11 = rating1/9;
  rating22 = rating2/9;
  
  
  
  
}


// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real bias;
  real<lower=0> kappa;
  real<lower=0> kappa2;
  
  real<lower=0> w1;
  real<lower=0> w2;
  
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target += beta_lpdf(bias |1,1);
  target += beta_lpdf(w1 |1,1);
  target += beta_lpdf(w2 |1,1);
  target += lognormal_lpdf(kappa |3,0.5);
  target += lognormal_lpdf(kappa2 |3,0.5);
  
  
  
  for(i in 1:N){
  target += beta_proportion_lpdf(rating11[i] | bias, kappa);
  target += beta_proportion_lpdf(rating22[i] | inv_logit(w1*logit(rating11[i])+w2*logit(group[i]/9)), kappa2);
}
}


generated quantities{
  real prior_bias;
  real prior_kappa;
  real prior_kappa2;
  
  real prior_w1;
  real prior_w2;
  

  vector[N] sim_rating1;
  vector[N] sim_rating2;
  vector[N] sim_groupp;



  prior_bias = beta_rng(1,1);
  prior_w1 = beta_rng(1,1);
  prior_w2 = beta_rng(1,1);
  
  prior_kappa = lognormal_rng(3,0.5);
  prior_kappa2 = lognormal_rng(3,0.5);
  
    
  sim_groupp = group/9;
  
  
  for(i in 1:N){
    sim_rating1[i] = beta_proportion_rng(bias, kappa);
    sim_rating2[i] = beta_proportion_rng(inv_logit(w1*logit(rating11[i]/9)+w2*logit(group[i]/9)), kappa);
  }
}

