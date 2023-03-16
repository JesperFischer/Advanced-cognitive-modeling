//defining the data block:
data {
  //number of trials
  int<lower=1> n;
  //choices for the first agent
  array[n] int rw1;
  //choices for the second agent
  array[n] int rw2;
  //feedback the first agent got
  array[n] int fb_rw1;
  //feedback the second agent got
  array[n] int fb_rw2;
  #whether to only sample from the priors or not 
  //0 = no prior,
  //1 = prior only
  int <lower = 0, upper = 1> prior;
  
}

// The parameters of the model
parameters {
  
  //iases for the first trial the agents play
  real  bias_1;
  real  bias_2;
  //learning rates for winning and losing for the first agent
  real alpha_1w;
  real alpha_1l;
  //learning rates for winning and losing for the second agent  
  real alpha_2w;
  real alpha_2l;
}


//here we calculate the expected values.
transformed parameters{
  // we start by defininf an array of beliefs i.e. expected values for each agent
  array[n] real <lower = 0, upper = 1> belief_1;
  array[n] real <lower = 0, upper = 1> belief_2;
  
  //The first trial's belief is just going to be equal to the bias in probability space
  //as we put a normal distribution with a mean and standard deviation for the prior for the bias' we need to constrain it to probability space
  //by using the inverse logit. The same logic goes for the other parameters (learning rates)
  belief_1[1] = inv_logit(bias_1);
  belief_2[1] = inv_logit(bias_2);
  
  //now we loop through all the trials where the learning rule is applied
  for (i in 2:n){
    //when agent1 wins use this
    if(fb_rw1[i-1])
      belief_1[i] = belief_1[i-1]+inv_logit(alpha_1w)*(rw2[i-1]-belief_1[i-1]);
      //if not then he lost so:
    else
      belief_1[i] = belief_1[i-1]+inv_logit(alpha_1l)*(rw2[i-1]-belief_1[i-1]);
      //same goes for agent 2.
    if(fb_rw2[i-1])
      belief_2[i] = belief_2[i-1]+inv_logit(alpha_2w)*(rw1[i-1]-belief_2[i-1]);
    else
      belief_2[i] = belief_2[i-1]+inv_logit(alpha_2l)*(rw1[i-1]-belief_2[i-1]);
  }
}

// The model to be estimated. here we defined our priors and likelihood
model {
  
  //priors: weakly informative priors N(0,1) for all parameters as a start
  target += normal_lpdf(bias_1 | 0,1);
  target += normal_lpdf(bias_2 | 0,1);
  
  target +=normal_lpdf(alpha_1w | 0,1);
  target +=normal_lpdf(alpha_1l | 0,1);
  
  target +=normal_lpdf(alpha_2w | 0,1);
  target +=normal_lpdf(alpha_2l | 0,1);
  
  //sample from the posterior (aka. prior == 0) or just sample from the prior?
  if(prior == 0){
  //likelihood agents beliefs are translated into choices
  //note that agent two uses the complement belief / expected value to guide his choice as he tries to mismatch:
    for (i in 1:n){
      target +=bernoulli_lpmf(rw1[i] | belief_1[i]);
      target +=bernoulli_lpmf(rw2[i] | (1-belief_2[i]));
    }
  }
  
}


// here we do some transformations and prior / posterior predictive checks:
generated quantities{
  
  
  #convinient way to get the parameters in their native (0-1 space)
  real <lower = 0, upper = 1> theta1_prior = inv_logit(bias_1);
  real <lower = 0, upper = 1> theta2_prior = inv_logit(bias_2);
  
  real <lower = 0, upper = 1> alpha1l_prior = inv_logit(alpha_1l);
  real <lower = 0, upper = 1> alpha1w_prior = inv_logit(alpha_1w);
  
  real <lower = 0, upper = 1> alpha2l_prior = inv_logit(alpha_2l);
  real <lower = 0, upper = 1> alpha2w_prior = inv_logit(alpha_2w);
  
  
  // we want to store the posterior / prior simulations for both agents
  array[n] int sim_rw1t;
  array[n] int sim_rw2t;
  
  //now we simulate reponses of the agents as in the model block for each trial i.
  for (i in 1:n){
    sim_rw1t[i] = bernoulli_rng(belief_1[i]);
    sim_rw2t[i] = bernoulli_rng(1-belief_2[i]);
  }
  //now we sum all the responses such that we have both all the trial level responses but also how many times each individual answered 1 and
  //therefore also 0
  int sim_rw1 = sum(sim_rw1t);
  int sim_rw2 = sum(sim_rw2t);
  
}


