##agents.



simple_bayes = function(bias, ntrials){
  
  data = data.frame()
  for(i in 1:ntrials){
    source1 = rprop(1,bias$kappa,bias$bias)
    source22 = sample(c(-3,-2,0,2,3), 1)
    
    while(round(source1,1)*7+1 + source22 > 8 | round(source1,1)*7+1 + source22 < 1){
      source22 = sample(c(-3,-2,0,2,3), 1)
    }
    
    source11 = round(source1,1)*7+1
    source2 = source11+source22
    
    outcome = rprop(1,bias$kappa2,inv_logit_scaled(0.5*logit_scaled(source11/9)+0.5*logit_scaled(source2/9)))
    
    data = rbind(data,data.frame(source1 = source1,
                                 source1_cat = source11, 
                                 source2 = source2/9, 
                                 source2_dif = source22, 
                                 source2_cat = source2, 
                                 outcome = outcome, 
                                 outcome_cat = outcome*9,
                                 kappa = bias$kappa,
                                 kappa2 = bias$kappa2,
                                 bias = bias$bias))
    }
  
  
  return(data)
}






simple_bayes_hier = function(bias_sd, bias_mu, kappa2_sd, kappa2_mu, kappa_sd, kappa_mu, ntrials, subs){

  source1 = array(NA, c(ntrials, subs))
  source11 = array(NA, c(ntrials, subs))
  source2 = array(NA, c(ntrials, subs))
  source22 = array(NA, c(ntrials, subs))
  outcome = array(NA, c(ntrials, subs))
  
  kappa = array(NA, subs)
  kappa2 = array(NA, subs)
  bias = array(NA, subs)
  
  
  
  for (s in 1:subs){
    
    bias[s] = extraDistr::rprop(1, bias_sd, bias_mu)
    
    kappa2[s] = rlnorm(1, kappa2_mu, kappa2_sd)
    
    kappa[s] = rlnorm(1, kappa_mu, kappa_sd)
    
    
    for(i in 1:ntrials){
      source1[i,s] = rprop(1,kappa[s],bias[s])
      source22[i,s] = sample(c(-3,-2,0,2,3), 1)
      
      while(round(source1[i,s],1)*7+1 + source22[i,s] > 8 | round(source1[i,s],1)*7+1 + source22[i,s] < 1){
        source22[i,s] = sample(c(-3,-2,0,2,3), 1)
      }
      
      source11[i,s] = round(source1[i,s],1)*7+1
      source2[i,s] = source11[i,s]+source22[i,s]
      
      outcome[i,s] = rprop(1,kappa2[s],inv_logit_scaled(0.5*logit_scaled(source11[i,s]/9)+0.5*logit_scaled(source2[i,s]/9)))
      
      }
    }
  
  qq = data.frame(source1) %>% mutate(trial = 1:nrow(.)) %>% pivot_longer(cols = starts_with("X"), values_to = "source1") %>% mutate(name = as.factor(name))
  qq1 = data.frame(outcome) %>% mutate(trial = 1:nrow(.)) %>% pivot_longer(cols = starts_with("X"), values_to = "outcome")%>% mutate(name = as.factor(name))
  qq2 = data.frame(source22) %>% mutate(trial = 1:nrow(.)) %>% pivot_longer(cols = starts_with("X"), values_to = "source22")%>% mutate(name = as.factor(name))
  
  
  df = inner_join(qq,qq1)
  df = inner_join(df, qq2)
  
  df$name = as.factor(df$name)
  
  subparm = data.frame(bias = bias, kappa2 = kappa2, kappa = kappa, name = paste0("X", 1:subs))
  
  
  
  data = list(source1 = source1, source1_cat = source11, source2 = source2/9, source2_dif = source22, source2_cat = source2, outcome = outcome, outcome_cat = outcome*9)
  
  return(list(data, subparm, df))
}



weighted_bayes = function(bias, ntrials){
  
  data = data.frame()
  for(i in 1:ntrials){
    source1 = rprop(1,bias$kappa,bias$bias)
    source22 = sample(c(-3,-2,0,2,3), 1)
    
    while(round(source1,1)*7+1 + source22 > 8 | round(source1,1)*7+1 + source22 < 1){
      source22 = sample(c(-3,-2,0,2,3), 1)
    }
    
    source11 = round(source1,1)*7+1
    source2 = source11+source22
    
    outcome = rprop(1,bias$kappa2,inv_logit_scaled(bias$w1*logit_scaled(source11/9)+bias$w2*logit_scaled(source2/9)))
    
    data = rbind(data,data.frame(source1 = source1,
                                 source1_cat = source11, 
                                 source2 = source2/9, 
                                 source2_dif = source22, 
                                 source2_cat = source2, 
                                 outcome = round(outcome,1), 
                                 outcome_cat = outcome*9,
                                 kappa = bias$kappa, 
                                 kappa2 = bias$kappa2,
                                 bias = bias$bias,
                                 w1 = bias$w1,
                                 w2 = bias$w2))
  }
  
  
  return(data)
}


weighted_bayes_hier = function(w1_mu,w1_sd,w2_mu, w2_sd, bias_sd, bias_mu, kappa2_sd, kappa2_mu, kappa_sd, kappa_mu, ntrials, subs){
  
  source1 = array(NA, c(ntrials, subs))
  source11 = array(NA, c(ntrials, subs))
  source2 = array(NA, c(ntrials, subs))
  source22 = array(NA, c(ntrials, subs))
  outcome = array(NA, c(ntrials, subs))
  
  kappa = array(NA, subs)
  kappa2 = array(NA, subs)
  bias = array(NA, subs)
  w1 = array(NA, subs)
  w2 = array(NA, subs)
  
  
  
  for (s in 1:subs){

    w1[s] = extraDistr::rprop(1, w1_sd, w1_mu)
    
    w2[s] = extraDistr::rprop(1, w2_sd, w2_mu)
    
    bias[s] = extraDistr::rprop(1, bias_sd, bias_mu)
    
    kappa2[s] = rlnorm(1, kappa2_mu, kappa2_sd)
    
    kappa[s] = rlnorm(1, kappa_mu, kappa_sd)
    
    
    for(i in 1:ntrials){
      source1[i,s] = rprop(1,kappa[s],bias[s])
      source22[i,s] = sample(c(-3,-2,0,2,3), 1)
      
      while(round(source1[i,s],1)*7+1 + source22[i,s] > 8 | round(source1[i,s],1)*7+1 + source22[i,s] < 1){
        source22[i,s] = sample(c(-3,-2,0,2,3), 1)
      }
      
      source11[i,s] = round(source1[i,s],1)*7+1
      source2[i,s] = source11[i,s]+source22[i,s]
      
      outcome[i,s] = rprop(1,kappa2[s],inv_logit_scaled(w1[s]*logit_scaled(source11[i,s]/9)+w2[s]*logit_scaled(source2[i,s]/9)))
      
    }
  }
  
  qq = data.frame(source1) %>% mutate(trial = 1:nrow(.)) %>% pivot_longer(cols = starts_with("X"), values_to = "source1") %>% mutate(name = as.factor(name))
  qq1 = data.frame(outcome) %>% mutate(trial = 1:nrow(.)) %>% pivot_longer(cols = starts_with("X"), values_to = "outcome")%>% mutate(name = as.factor(name))
  qq2 = data.frame(source22) %>% mutate(trial = 1:nrow(.)) %>% pivot_longer(cols = starts_with("X"), values_to = "source22")%>% mutate(name = as.factor(name))
  
  
  df = inner_join(qq,qq1)
  df = inner_join(df, qq2)
  
  df$name = as.factor(df$name)
  
  subparm = data.frame(w1 = w1, w2 = w2, bias = bias, kappa2 = kappa2, kappa = kappa, name = paste0("X", 1:subs))
  
  
  
  data = list(source1 = source1, source1_cat = source11, source2 = source2/9, source2_dif = source22, source2_cat = source2, outcome = outcome, outcome_cat = outcome*9)
  
  return(list(data, subparm, df))
  
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




plot_prior_posterior_update_sub = function(fit,parameters,reals){
  
  
  
  post = as_draws_df(fit$draws()) %>% 
    select(starts_with(parameters)) %>% select(contains("[")) %>% 
    mutate(posterior = T)
  
  priors = as_draws_df(fit$draws()) %>% 
    select(starts_with(paste0("prior_",parameters))) %>% select(contains("[")) %>% 
    mutate(posterior = F) %>%
    rename_with(~sub("^prior_", "", .), starts_with("prior_"))
  
  
  
  
  df = rbind(post,priors)
  
  df %>% pivot_longer(cols = -posterior) %>% filter(value < 200) %>% ggplot(aes(x = value, fill = posterior))+
    geom_histogram(alpha = 0.5, position="identity")+facet_wrap(~name, nrow = length(parameters), ncol = 10, scales = "free")+
    geom_vline(data = reals, aes(xintercept = value))+
    theme_classic()
  
}
