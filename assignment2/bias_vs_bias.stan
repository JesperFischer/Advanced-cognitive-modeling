
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;
  vector[N] rw1;
  vector[N] rw2;
  vector[N] fb_rw1;
  vector[N] fb_rw2;
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real bias_2;
  real bias_1
}
s
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target +=beta_lpdf(bias_1 | 1,1)
  target +=beta_lpdf(bias_2 | 1,1)
  
  
  target +=bernoulli_lpmf(rw1 | bias_1)
  target +=bernoulli_lpmf(rw2 | bias_2)
  
}

