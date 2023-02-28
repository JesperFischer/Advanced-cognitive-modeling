###models with stan


#go from 2 till 100 trials with 100 simulations for each
parameter_recovery_bias_vs_bias = function(ngames,trials){
  setwd("~/Advanced-cognitive-modeling/assignment2")
  filemodel = "stan_models/bias_vs_bias.stan"
  mod = cmdstan_model(filemodel)
  
  ngames = ngames
  trials = trials
  pr = data.frame()
  
  for (t in 2:trials){
    for (g in 1:ngames){
      seed = g*t
      df = rw_vs_rw(ntrials = t,
                    alpha1_l = 0,
                    alpha1_w = 0,
                    alpha2_l = 0,
                    alpha2_w = 0,
                    bias1 = 0.2,
                    bias2 = 0.8,
                    incentive1 = 0,  # 0 doesn't care about ,  1 does the oppisite
                    incentive2 = 0
      )
      
      df = data.frame(df)
      
      
      #lets fit in stan:
      
      data = list(rw1 = df$rw1, rw2 = df$rw2, n = nrow(df))
      
      
      fit <- mod$sample(
        data = data, 
        seed = seed, 
        chains = 4, 
        parallel_chains = 4,
        refresh = 0)
      
      sum = fit$summary()
      
      
      df = sum %>% select("mean","variable") %>% filter(str_detect(variable, "bias") == T) %>% mutate(seed = seed, trials = t)
      
      pr = rbind(pr,df)
    }
    
    
    
    
  }
  
  plot = pr %>% ggplot(aes(x = trials, y = mean, col = variable, fill = variable))+
    geom_dots(scale = 0.6, alpha = 0.5)+
    geom_hline(yintercept = 0.2, col = "red")+
    geom_hline(yintercept = 0.8, col = "blue")+
    scale_fill_manual(values = c("red","blue"))+
    scale_color_manual(values = c("red","blue"))+
    theme_classic()+scale_x_continuous(breaks = seq(1,trials, by = 1))
  
  return(list(data = pr, plot = plot))
}




parameter_recovery_rw_vs_rw = function(ngames,trials, lr1,lr2){
  setwd("~/Advanced-cognitive-modeling/assignment2")
  filemodel = "stan_models/rw_vs_rw.stan"
  mod = cmdstan_model(filemodel)
  
  ngames = ngames
  trials = trials
  pr = data.frame()
  
  for (t in 2:trials){
    for (g in 1:ngames){
      seed = g*t
      df = rw_vs_rw(ntrials = t,
                    alpha1_l = lr1,
                    alpha1_w = lr1,
                    alpha2_l = lr2,
                    alpha2_w = lr2,
                    bias1 = 0.5,
                    bias2 = 0.5,
                    incentive1 = 0,  # 0 doesn't care about ,  1 does the oppisite
                    incentive2 = 0
      )
      
      df = data.frame(df)
      
      
      #lets fit in stan:
      
      data = list(rw1 = df$rw1, rw2 = df$rw2, fb_rw1 = df$feedback_rw1, fb_rw2 = df$feedback_rw2, n = nrow(df))
      
      
      fit <- mod$sample(
        data = data, 
        seed = seed, 
        chains = 4, 
        parallel_chains = 4,
        refresh = 0)
      
      sum = fit$summary()
      
      
      df = sum %>% select("mean","variable") %>% filter(str_detect(variable, "alpha") == T) %>% mutate(seed = seed, trials = t)
      
      pr = rbind(pr,df)
    }
    
    
    
    
  }
  
  plot = pr %>% ggplot(aes(x = trials, y = mean, col = variable, fill = variable))+
    geom_dots(scale = 0.6, alpha = 0.5)+
    geom_hline(yintercept = lr1, col = "red")+
    geom_hline(yintercept = lr2, col = "blue")+
    scale_fill_manual(values = c("red","blue"))+
    scale_color_manual(values = c("red","blue"))+
    theme_classic()+scale_x_continuous(breaks = seq(1,trials, by = 1))
  
  return(list(data = pr, plot = plot))
}






parameter_recovery_rw_gen = function(ngames,start_trials, end_trials, lr1_l, lr1_w,lr2_l,lr2_w){
  setwd("~/Advanced-cognitive-modeling/assignment2")
  filemodel = "stan_models/rw_win_lose_vs_rw.stan"
  mod = cmdstan_model(filemodel)
  
  ngames = ngames
  trials = trials
  pr = data.frame()
  
  for (t in start_trials:end_trials){
    for (g in 1:ngames){
      seed = g*t
      df = rw_vs_rw(ntrials = t,
                    alpha1_l = lr1_l,
                    alpha1_w = lr1_w,
                    alpha2_l = lr2_l,
                    alpha2_w = lr2_w,
                    bias1 = 0.5,
                    bias2 = 0.5,
                    incentive1 = 0,  # 0 doesn't care about ,  1 does the oppisite
                    incentive2 = 0
      )
      
      df = data.frame(df)
      
      
      #lets fit in stan:
      
      data = list(rw1 = df$rw1, rw2 = df$rw2, fb_rw1 = df$feedback_rw1, fb_rw2 = df$feedback_rw2, n = nrow(df))
      
      
      fit <- mod$sample(
        data = data, 
        seed = seed, 
        chains = 4, 
        parallel_chains = 4,
        refresh = 0)
      
      sum = fit$summary()
      
      
      df = sum %>% select("mean","variable") %>% filter(str_detect(variable, "alpha") == T) %>% mutate(seed = seed, trials = t)
      
      pr = rbind(pr,df)
    }
    
    
    
    
  }
  
  plot = pr %>% ggplot(aes(x = trials, y = mean, col = variable, fill = variable))+
    geom_dots(scale = 0.6, alpha = 0.5)+
    geom_hline(yintercept = lr1_l, col = "#FF0000")+
    geom_hline(yintercept = lr1_w, col = "#8B0000")+
    geom_hline(yintercept = lr2_l, col = "#8080FF")+
    geom_hline(yintercept = lr2_w, col = "#000080")+
    scale_fill_manual(values = c("#FF0000","#8B0000","#8080FF","#000080"))+
    scale_color_manual(values = c("#FF0000","#8B0000","#8080FF","000080"))+
    theme_classic()+scale_x_continuous(breaks = seq(1,trials, by = 1))
  
  return(list(data = pr, plot = plot))
}
