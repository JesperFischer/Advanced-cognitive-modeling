data {
  int<lower=1> n;
  array[n] int rw1;
  array[n] int rw2;
  array[n] int fb_rw1;
  array[n] int fb_rw2;
  
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  
  real <lower = 0, upper = 1> alpha_1w;
  real <lower = 0, upper = 1> alpha_1l;
  
  real <lower = 0, upper = 1> alpha_2w;
  real <lower = 0, upper = 1> alpha_2l;
}


transformed parameters{
  array[n] real <lower = 0, upper = 1> belief_1;
  array[n] real <lower = 0, upper = 1> belief_2;
  

  belief_1[1] = 0.5;
  belief_2[1] = 0.5;
  
  for (i in 2:n){
    if(fb_rw1[i-1])
      belief_1[i] = belief_1[i-1]+alpha_1w*(rw2[i-1]-belief_1[i-1]);
    else
      belief_1[i] = belief_1[i-1]+alpha_1l*(rw2[i-1]-belief_1[i-1]);
      
    if(fb_rw2[i-1])
      belief_2[i] = belief_2[i-1]+alpha_2w*(rw1[i-1]-belief_2[i-1]);
    else
      belief_2[i] = belief_2[i-1]+alpha_2l*(rw1[i-1]-belief_2[i-1]);
  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  target +=beta_lpdf(alpha_1w | 1,1);
  target +=beta_lpdf(alpha_1l | 1,1);
  
  target +=beta_lpdf(alpha_2w | 1,1);
  target +=beta_lpdf(alpha_2l | 1,1);
  
  
  
  
  for (i in 1:n){
  
    target +=bernoulli_lpmf(rw1[i] | belief_1[i]);
    target +=bernoulli_lpmf(rw2[i] | (1-belief_2[i]));
    
  }
  
}

