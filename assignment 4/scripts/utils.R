

distance <- function(vect1, vect2, w) {
  return(sum(w * abs(vect1 - vect2)))
}


similarity <- function(distance, c) {
  return(exp(-c * distance))
}



stimulus = function(){
  
  arms = c(0,1)
  legs = c(0,1)
  eyes = c(0,1)
  spots = c(0,1)
  color = c(0,1)
  
  stimuli = expand.grid(arms_up = arms,legs_thick = legs, eyes_stalks = eyes, spots_on = spots, color_green = color)%>% 
    mutate(danger = ifelse(spots_on == 1 & eyes_stalks == 1,1,0))
  
  perm = sample(nrow(stimuli))
  
  stimuli = stimuli[perm,]
  
  
  return(stimuli)
}










gcm <- function(w, c, obs, cat_one, quiet = TRUE) {
  # create an empty list to save probability of saying "1" for each trial
  r <- c()
  
  ntrials <- nrow(obs)
  
  for (i in 1:ntrials) {
    # If quiet is FALSE, print every ten trials
    if (!quiet && i %% 10 == 0) {
      print(paste("i =", i))
    }
    
    # if this is the first trial, or there any category with no exemplars seen yet, set the choice to random
    if (i == 1 || sum(cat_one[1:(i - 1)]) == 0 || sum(cat_one[1:(i - 1)]) == (i - 1)) {
      r <- c(r, .5)
    } else {
      similarities <- c()
      # for each previously seen stimulus assess distance and similarity
      for (e in 1:(i - 1)) {
        sim <- similarity(distance(obs[i, ], obs[e, ], w), c)
        similarities <- c(similarities, sim)
      }
      # Calculate prob of saying "1" by dividing similarity to 1 by the sum of similarity to 1 and to 2
      numerator <- 0.5 * sum(similarities[cat_one[1:(i - 1)] == 1])
      denominator <- 0.5 * sum(similarities[cat_one[1:(i - 1)] == 1]) + 0.5 * sum(similarities[cat_one[1:(i - 1)] == 0])
      r <- c(r, numerator / denominator)
    }
  }
  
  return(rbinom(ntrials, 1, r))
}







# function for simulation responses
simulate_responses <- function(agent, w, c) {
  

  stimulus = rbind(stimulus(),stimulus(),stimulus())
  
  
  if (w == "equal") {
    weight <- rdirichlet(1,c(100,100,100,100,100))
  } else if (w == "right") {
    weight <- rdirichlet(1,c(50,50,500,500,50))
  } else if (w == "wrong") {
    weight <- rdirichlet(1,c(200,200,50,50,200))
  }
  
  responses <- gcm(w = weight,
                   c = c,
                   obs = stimulus[,1:5],
                   stimulus[,6])
  
  correct = ifelse(stimulus[,6] == responses, 1, 0)
  
  tmp_simulated_responses <- data.frame(
      trial = seq(nrow(stimulus)),
      sim_response = responses,
      correct = correct,
      performance = cumsum(correct) / seq_along(correct),
      category = stimulus[,6],
      c = c,
      w = w,
      agent = agent,
      stimulus = stimulus[1:5]
    )
  
  return(tmp_simulated_responses)
}


simulate_responses2 <- function(agent, w, c, stimulus) {
  

  responses <- gcm(w = w,
                   c = c,
                   obs = stimulus[,1:5],
                   stimulus[,6])
  
  correct = ifelse(stimulus[,6] == responses, 1, 0)
  
  
  w = data.frame(t(data.frame(w)))
  
  names(w) = c("w1","w2","w3","w4","w5")
  
    
  tmp_simulated_responses <- data.frame(
    trial = seq(nrow(stimulus)),
    sim_response = responses,
    correct = correct,
    performance = cumsum(correct) / seq_along(correct),
    category = stimulus[,6],
    c = c,
    agent = agent,
    stimulus = stimulus[1:5]
  )
  
  tmp_simulated_responses = cbind(tmp_simulated_responses,w)
  
  return(tmp_simulated_responses)
}







simulate_dirichlet <- function(weights, kappa, agents){
  
  w_n <- length(weights)
  
  w_ind <- rdirichlet(agents, weights * kappa)
  
  w_ind_df <- tibble(
    agent = as.factor(rep(seq(agents), each = w_n)),
    value = c(w_ind),
    weight = rep(seq(w_n), agents)
  )
  
  return(w_ind_df)
}



simulate_ml_responses <- function(agents, scalingM, scalingSD, w, kappa) {
  
  
  if (w == "equal") {
    weight <- rdirichlet(1,c(100,100,100,100,100))
  } else if (w == "right") {
    weight <- rdirichlet(1,c(50,50,500,500,50))
  } else if (w == "wrong") {
    weight <- rdirichlet(1,c(200,200,50,50,200))
  }
  
  
  
  w_ind <- rdirichlet(agents, weight * kappa)
  c_ind <- extraDistr::rprop(agents, 1/scalingSD, scalingM/2)*2
  
  
  
  stimulus = rbind(stimulus(),stimulus(),stimulus())
  
  
  
  
  for (i in 1:agents) {
    tmp <- simulate_responses2(i, w = w_ind[i,], c = c_ind[i], stimulus = stimulus)
    tmp$w = w
    if (i == 1) {
      simulated_responses <- tmp
    } else {
      simulated_responses <- rbind(simulated_responses, tmp)
    }
  }
  
  return(simulated_responses)
}

















plot_prior_posterior_update = function(fit,parameters,reals){
  
  
  post = as_draws_df(fit$draws()) %>% 
    select(all_of(parameters)) %>% 
    mutate(posterior = T)
  
  
  priors = as_draws_df(fit$draws()) %>% 
    select(all_of(paste0("prior_",parameters)))%>% 
    mutate(posterior = F) %>%
    rename_with(~sub("^prior_", "", .), starts_with("prior_"))
  
  df = rbind(post,priors)
  
  df %>% pivot_longer(cols = parameters) %>% ggplot(aes(x = value, fill = posterior))+
    geom_histogram(alpha = 0.5, position="identity")+    facet_wrap(~name, scales = "free")+
    theme_classic()+
    geom_vline(data = data.frame(reals, name = names(((reals)))), aes(xintercept = reals))
  
  
}
