data {
  int<lower=1> n;
  int<lower=0, upper = 1> prior;
  array[n] int rw1;
  array[n] int rw2;
  real bias1_mean; 
  real bias2_mean; 
  real bias1_sd; 
  real bias2_sd;  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'muwww' and 'sigma'.
parameters {
  real <lower = 0, upper = 1> bias_1;
  real <lower = 0, upper = 1> bias_2;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target +=normal_lpdf(bias_1 | bias1_mean,bias1_sd);
  target +=normal_lpdf(bias_2 | bias2_mean,bias2_sd);
  
  for (i in 1:n){
    if(prior == 0){
      target +=bernoulli_logit_lpmf(rw1[i] |bias_1);
      target +=bernoulli_logit_lpmf(rw2[i] |(1-bias_2));
    }
  
  }
  
}


generated quantities{

  int sim_rw1;
  int sim_rw2;
  
  sim_rw1 = binomial_rng(n,bias_1);
  sim_rw2 = binomial_rng(n,1-bias_2);

  
}
