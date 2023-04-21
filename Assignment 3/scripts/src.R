##agents.



simple_bayes = function(bias, ntrials){
  
  data = data.frame()
  for(i in 1:ntrials){
    source1 = rprop(1,bias$precision,bias$bias)
    source22 = sample(c(-3,-2,0,2,3), 1)
    
    while(round(source1,1)*7+1 + source22 > 8 | round(source1,1)*7+1 + source22 < 1){
      source22 = sample(c(-3,-2,0,2,3), 1)
    }
    
    source11 = round(source1,1)*7+1
    source2 = source11+source22
    
    outcome = inv_logit_scaled(0.5*logit_scaled(source11/9)+0.5*logit_scaled(source2/9))
    
    data = rbind(data,data.frame(source1 = source1,
                                 source1_cat = source11, 
                                 source2 = source2/9, 
                                 source2_dif = source22, 
                                 source2_cat = source2, 
                                 outcome = round(outcome,1), 
                                 outcome_cat = outcome*9,
                                 precision = bias$precision, 
                                 bias = bias$bias))
    }
  
  
  return(data)
}