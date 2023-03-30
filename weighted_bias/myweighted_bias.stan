// same function as in R, but ensuring the right scale for w inside the function
// doing it in transformed parameters made the sampler v inefficient
functions{
  real weight_f(real L, real w) {
    return(log((w * exp(L) + 1 - w) /  ((1 - w) * exp(L) + w)));
  }
}


data {
  int<lower=0> N;
  array[N] int y;
  vector[N] Source1;
  vector[N] Source2;
}
parameters {
  real bias;
  real weight1;
  real weight2;
}
model {
  target += normal_lpdf(bias | 0, 1);
  target += normal_lpdf(weight1 | 0, 1.5);
  target += normal_lpdf(weight2 | 0, 1.5);
  
  for (n in 1:N){  
  target += bernoulli_logit_lpmf(y[n] | bias + weight_f(Source1[n], weight1) + weight_f(Source2[n], weight2));
  }
}

generated quantities{
  array[N] real log_lik;
  real bias_prior;
  real weight1_prior;
  real weight2_prior;
  real w1_prior;
  real w2_prior;
  real w1;
  real w2;
  
  bias_prior = normal_rng(0,1);
  
  // Saving prior for weights as they are (for debugging purposes)
  weight1_prior = normal_rng(0,1.5);
  weight2_prior = normal_rng(0,1.5);
  // Saving priors in the appropriate scale
  w1_prior = 0.5 + inv_logit(normal_rng(0,1.5))/2;
  w2_prior = 0.5 + inv_logit(normal_rng(0,1.5))/2;
  
  // Converting weights on the right scale
  w1 = 0.5 + inv_logit(weight1)/2;
  w2 = 0.5 + inv_logit(weight2)/2;
  
  // Saving likelihood for model comparison
  for (n in 1:N){  
    log_lik[n] = bernoulli_logit_lpmf(y[n] | bias + weight_f(Source1[n], weight1) + weight_f(Source2[n], weight2));
  }
  
}
