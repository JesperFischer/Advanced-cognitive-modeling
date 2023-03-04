data {
  int<lower=1> n;
  array[n] int rw1;
  array[n] int rw2;
  array[n] int fb_rw1;
  array[n] int fb_rw2;
  int <lower = 0, upper = 1> prior;
  
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  
  real <lower = 0, upper = 1> bias_1;
  real <lower = 0, upper = 1> bias_2;
  
  real <lower = 0, upper = 1> alpha_1;
  real <lower = 0, upper = 1> alpha_2;

}


transformed parameters{
  array[n] real <lower = 0, upper = 1> belief_1;
  array[n] real <lower = 0, upper = 1> belief_2;
  

  belief_1[1] = bias_1;
  belief_2[1] = bias_2;
  
  for (i in 2:n){
    belief_1[i] = belief_1[i-1]+alpha_1*(rw2[i-1]-belief_1[i-1]);
    belief_2[i] = belief_2[i-1]+alpha_2*(rw1[i-1]-belief_2[i-1]);
  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  target += inv_logit(normal_lpdf(bias_1 | 0,2));
  target += inv_logit(normal_lpdf(bias_2 | 0,2));
  
  target +=beta_lpdf(alpha_1 | 1,1);
  target +=beta_lpdf(alpha_2 | 1,1);
  
  
  if(prior == 0){
    
    for (i in 1:n){
    
      target +=bernoulli_lpmf(rw1[i] | belief_1[i]);
      target +=bernoulli_lpmf(rw2[i] | (1-belief_2[i]));
      
    }
  }
}

