data {
  int<lower=1> n;
  array[n] int rw1;
  array[n] int rw2;
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real <lower = 0, upper = 1> bias_1;
  real <lower = 0, upper = 1> bias_2;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target +=beta_lpdf(bias_1 | 1,1);
  target +=beta_lpdf(bias_2 | 1,1);
  
  
  for (i in 1:n){
  
    target +=bernoulli_lpmf(rw1[i] |bias_1);
    target +=bernoulli_lpmf(rw2[i] |(1-bias_2));
    
  
  }
  
}

