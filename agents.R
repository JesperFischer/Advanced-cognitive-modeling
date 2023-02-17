
rw_vs_rw = function(ntrials,alpha1_l,alpha1_w,alpha2_l,alpha2_w,bias1, bias2){
  
  
  ntrials = ntrials
  rw1 = array(NA,ntrials)
  rw2 = array(NA,ntrials)
  expectation1 = array(NA,ntrials)
  expectation2 = array(NA,ntrials)
  
  feedback_rw1 = array(NA,ntrials)
  feedback_rw2 = array(NA,ntrials)
  
  expectation1[1] = bias1
  expectation2[1] = bias2
  
  
  rw1[1] = rbinom(1,1,expectation1[1])
  rw2[1] = rbinom(1,1,expectation2[1])
  
  if(rw1[1] == rw2[1]){
    feedback_rw1[1] = 1
    feedback_rw2[1] = 1-feedback_rw1[1]
  } else{
    feedback_rw1[1] = 0
    feedback_rw2[1] = 1-feedback_rw1[1]
  }
  
  for (i in 2:ntrials){
    
    
    expectation1[i] = rw_agent2(previous_expected = expectation1[i-1],
                                previous_them = rw2[i-1],
                                alpha_w = alpha1_w,
                                alpha_l = alpha1_l,
                                feedback = feedback_rw1[i-1])
    
    rw1[i] = rbinom(1,1,expectation1[i])
    
    expectation2[i] = rw_agent2(previous_expected = expectation2[i-1],
                                previous_them = rw1[i-1],
                                alpha_w = alpha2_w,
                                alpha_l = alpha2_l,
                                feedback = feedback_rw2[i-1])
    
    rw2[i] = rbinom(1,1,expectation2[i])
    #this overwrites the second agents decision as he would otherwise play to match which he shouldn't he tries to not match.
    rw2[i] = 1-rw2[i]
    
    if(rw1[i] == rw2[i]){
      feedback_rw1[i] = 1
      feedback_rw2[i] = 1-feedback_rw1[i]
    } else{
      feedback_rw1[i] = 0
      feedback_rw2[i] = 1-feedback_rw1[i]
    }
    
    
  }
  
  return(list(rw1 = rw1, rw2 = rw2, feedback_rw1 = feedback_rw1,feedback_rw2 = feedback_rw2))
  
}


rw_agent2 = function(previous_expected, previous_them, alpha_w, alpha_l, feedback){
  if(feedback == 1){
    expected = previous_expected + alpha_w * (previous_them-previous_expected)
  }
  if(feedback == 0){
    expected = previous_expected + alpha_l * (previous_them-previous_expected)
  }
  
  return(expected)
}



#plot choices:


plot_choices = function(df){
  
  return(df %>% ggplot()+theme_classic()+geom_line(color = "red",aes(1:ntrials, df$rw1))+geom_line(color = "blue",aes(1:ntrials, df$rw2))+xlab("Trial")+ylab("Choice")+
    ggtitle("Plot of choices of matcher (red) and non-matcher (blue)"))
  
}



plot_cumwin = function(df){
  
  df = df %>% mutate(trials = 1:nrow(df)) %>%  mutate(cumrw1 = cumsum(feedback_rw1)/seq_along(feedback_rw1),
                                                   cumrw2 = cumsum(feedback_rw2)/seq_along(feedback_rw2))
  
  
  return(df %>% ggplot()+theme_classic()+geom_line(color = "red",aes(trials, cumrw1))+geom_line(color = "blue",aes(trials, cumrw2))+xlab("Trial")+ylab("Procent of wins")+
    ggtitle("Plot of Procent of wins of matcher (red) and non-matcher (blue)"))
  
  
}
