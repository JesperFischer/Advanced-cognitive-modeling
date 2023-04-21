

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] rating1;
  vector[N] rating2;
  vector[N] group;
  
}



transformed data{
  vector[N] rating11;
  vector[N] rating22;
  
  rating11 = rating1/9;
  rating22 = rating2/9;
  
  
  
  
}


// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real bias;
  real<lower=0> kappa;
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  rating11 ~ beta_proportion_lpdf(bias, kappa);
  rating22 ~ beta_proportion_lpdf(inv_logit(0.5*logit(rating11/9)+0.5*logit(group/9)), kappa);
}

