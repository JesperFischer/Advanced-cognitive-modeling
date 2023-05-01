


hgf_agentt = function(u, input){
  
  sigmoid = function(x) {
    1 / (1 + exp(-x))}
  

  data <- data.frame(u)
  
  theta = input$theta
  kappa = input$kappa
  omega = input$omega
  beta = input$beta
  
  
  ntrials = nrow(data)
  
  mu1hat = array(NA,ntrials)
  pe1 = array(NA,ntrials)
  pe2 = array(NA,ntrials)
  w2 = array(NA,ntrials)
  r2 = array(NA,ntrials)
  pi3hat = array(NA,ntrials)
  pi3 = array(NA,ntrials)
  sa1hat = array(NA,ntrials)
  mu2 = array(NA,ntrials)
  sa2 = array(NA,ntrials)
  sa2hat = array(NA,ntrials)
  mu3 = array(NA,ntrials)
  sa3 = array(NA,ntrials)
  y = array(NA,ntrials)
  belief = array(NA,ntrials)
  
  
  mu2[1] = input$Inital_mu2
  sa2[1] = input$Inital_prec2
  mu3[1] = input$Inital_mu3
  sa3[1] = input$Inital_prec3
  
  
  for(t in 2:ntrials){
    
    mu1hat[t] = sigmoid(mu2[t-1])
    
    sa2hat[t] = sa2[t-1]+exp(kappa*mu3[t-1]+omega)
    
    sa1hat[t] = mu1hat[t]*(1-mu1hat[t])
    
    pe1[t] = u[t-1]-mu1hat[t]
    
    
    
    
    sa2[t] = 1/((1/sa2hat[t])+sa1hat[t])
    mu2[t] = (mu2[t-1]+pe1[t]*sa2[t])
    
  
    pe2[t] = ((sa2[t]+(mu2[t]-mu2[t-1])^2)/(sa2[t-1]+exp(kappa*mu3[t-1]+omega)))-1
    
    r2[t] = (exp(kappa*mu3[t-1]+omega)-sa2[t-1])/(sa2[t-1]+exp(kappa*mu3[t-1]+omega))
    
    w2[t] = exp(kappa*mu3[t-1]+omega)/(sa2[t-1]+exp(kappa*mu3[t-1]+omega))
    
    pi3hat[t] = 1/(sa3[t-1]+theta)
    
    pi3[t] = pi3hat[t]+(kappa^2/2)*w2[t]*(w2[t]+r2[t]*pe2[t])
    
    sa3[t] = 1/pi3[t]
    
    mu3[t] = mu3[t-1]+sa3[t]*(kappa/2)*w2[t]*pe2[t]
    
    belief[t] = mu1hat[t]^beta/(mu1hat[t]^beta+(1-mu1hat[t])^beta)
    
    y[t] = rbinom(1,1,belief[t])
  
    }
  
  data$sa2hat = sa2hat
  data$mu1hat = mu1hat
  data$sa1hat = sa1hat
  data$pe1 = pe1
  data$pe2 = pe2
  data$mu2 = mu2
  data$r2 = r2
  data$w2 = w2
  data$pi3hat = pi3hat
  data$pi3 = pi3
  data$sa3 = sa3
  data$mu3 = mu3
  data$sa2 = sa2
  data$trial = 1:nrow(data)
  data$y = y
  data$belief = belief
  
  return(data)
}










rm_agent = function(bias,trials){
  u = c()
  for (i in 1:length(trials)){
    u1 = rbinom(n = trials[i], size = 1, prob = bias[i])
    u = c(u,u1)
  }
  return(u)
}



hgf_agent = function(u, input){

  sigmoid = function(x) {
    1 / (1 + exp(-x))}
  
  HGF_sa2hat_update <- function(sa22, kappa, mu33, omega){
    return(sa22+exp(kappa*mu33+omega))}
  
  HGF_mu1hat_update <- function(mu22){
    return(sigmoid(mu22))}
  
  HGF_sa1hat_update <- function(mu1hat){
    return(mu1hat*(1-mu1hat))}
  
  HGF_sa2_update <- function(sa2hat, sa1hat){
    return(1/((1/sa2hat)+sa1hat))}
  
  prediction_error1 <- function(mu1hat, x){
    return(x-mu1hat)}
  
  HGF_mu2_update = function(mu22, pe1,sa2){
    return(mu22+pe1*sa2)}
  
  prediction_error2 <- function(sa2,sa22,mu2,mu22,kappa,mu33,omega){
    return(((sa2+(mu2-mu22)^2)/(sa22+exp(kappa*mu33+omega)))-1)}
  
  HGF_update_r2 <- function(kappa, mu33,omega,sa22){
    return((exp(kappa*mu33+omega)-sa22)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_w2 <- function(kappa, mu33,omega,sa22){
    return(exp(kappa*mu33+omega)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_pi3hat <- function(sa33,theta){
    return(1/(sa33+theta))}
  
  HGF_update_pi3 <- function(pi3hat,kappa,w2,r2,pe2){
    return(pi3hat+(kappa^2/2)*w2*(w2+r2*pe2))}
  
  HGF_update_sa3 <- function(pi3){
    return(1/pi3)}
  
  HGF_update_mu3 <- function(mu33,sa3,kappa,w2,pe2){
    return(mu33+sa3*(kappa/2)*w2*pe2)}
  
    data <- data.frame(u)
  
    theta = input$theta
    kappa = input$kappa
    omega = input$omega
    
    ntrials = nrow(data)
    
    mu1hat = array(NA,ntrials)
    pe1 = array(NA,ntrials)
    pe2 = array(NA,ntrials)
    w2 = array(NA,ntrials)
    r2 = array(NA,ntrials)
    pi3hat = array(NA,ntrials)
    pi3 = array(NA,ntrials)
    sa1hat = array(NA,ntrials)
    mu2 = array(NA,ntrials)
    sa2 = array(NA,ntrials)
    sa2hat = array(NA,ntrials)
    mu3 = array(NA,ntrials)
    sa3 = array(NA,ntrials)
    y = array(NA,ntrials)
    
    mu2[1] = input$Inital_mu2
    sa2[1] = input$Inital_prec2
    mu3[1] = input$Inital_mu3
    sa3[1] = input$Inital_prec3
    
    
    for(t in 2:ntrials){
      #if(HGF_mu1hat_update(mu2[t-1]) > 0.8){
      #  mu1hat[t] = 0.8
      #}else if(HGF_mu1hat_update(mu2[t-1]) < 0.2){
      #  mu1hat[t] = 0.2
      #}else{
      mu1hat[t] = HGF_mu1hat_update(mu2[t-1])
      #}
      sa2hat[t] = HGF_sa2hat_update(sa2[t-1],kappa, mu3[t-1], omega)
      sa1hat[t] = HGF_sa1hat_update(mu1hat[t])
      
      pe1[t] = prediction_error1(mu1hat[t], u[t-1])
      
      
      
      
      sa2[t] = HGF_sa2_update(sa2hat[t],sa1hat[t])
      mu2[t] = HGF_mu2_update(mu2[t-1], pe1[t],sa2[t])
      pe2[t] = prediction_error2(sa2[t],sa2[t-1],mu2[t],mu2[t-1],kappa,mu3[t-1],omega)
      r2[t] = HGF_update_r2(kappa,mu3[t-1],omega,sa2[t-1])
      w2[t] = HGF_update_w2(kappa,mu3[t-1],omega, sa2[t-1])
      pi3hat[t] = HGF_update_pi3hat(sa3[t-1], theta)
      pi3[t] = HGF_update_pi3(pi3hat[t],kappa, w2[t],r2[t],pe2[t])
      sa3[t] = HGF_update_sa3(pi3[t])
      mu3[t] = HGF_update_mu3(mu3[t-1], sa3[t], kappa,w2[t], pe2[t]) 
      y[t] = rbinom(1,1,mu1hat[t])
    }
    
    data$sa2hat = sa2hat
    data$mu1hat = mu1hat
    data$sa1hat = sa1hat
    data$pe1 = pe1
    data$pe2 = pe2
    data$mu2 = mu2
    data$r2 = r2
    data$w2 = w2
    data$pi3hat = pi3hat
    data$pi3 = pi3
    data$sa3 = sa3
    data$mu3 = mu3
    data$sa2 = sa2
    data$trial = 1:nrow(data)
    data$y = y
    return(data)
}



hgf_agent_omega2 = function(us, input){
  
  sigmoid = function(x) {
    1 / (1 + exp(-x))}
  
  HGF_sa2hat_update <- function(sa22, kappa, mu33, omega){
    return(sa22+exp(kappa*mu33+omega))}
  
  HGF_mu1hat_update <- function(mu22){
    return(sigmoid(mu22))}
  
  HGF_sa1hat_update <- function(mu1hat){
    return(mu1hat*(1-mu1hat))}
  
  HGF_sa2_update <- function(sa2hat, sa1hat){
    return(1/((1/sa2hat)+sa1hat))}
  
  prediction_error1 <- function(mu1hat, x){
    return(x-mu1hat)}
  
  HGF_mu2_update = function(mu22, pe1,sa2){
    return(mu22+pe1*sa2)}
  
  prediction_error2 <- function(sa2,sa22,mu2,mu22,kappa,mu33,omega){
    return(((sa2+(mu2-mu22)^2)/(sa22+exp(kappa*mu33+omega)))-1)}
  
  HGF_update_r2 <- function(kappa, mu33,omega,sa22){
    return((exp(kappa*mu33+omega)-sa22)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_w2 <- function(kappa, mu33,omega,sa22){
    return(exp(kappa*mu33+omega)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_pi3hat <- function(sa33,theta){
    return(1/(sa33+theta))}
  
  HGF_update_pi3 <- function(pi3hat,kappa,w2,r2,pe2){
    return(pi3hat+(kappa^2/2)*w2*(w2+r2*pe2))}
  
  HGF_update_sa3 <- function(pi3){
    return(1/pi3)}
  
  HGF_update_mu3 <- function(mu33,sa3,kappa,w2,pe2){
    return(mu33+sa3*(kappa/2)*w2*pe2)}
  
  data <- data.frame(us)
  
  u = data$u
  pain = data$pain
  
  theta = input$theta
  kappa = input$kappa
  omegapain = input$omegapain
  omegan = input$omegan
  
  ntrials = nrow(data)
  
  mu1hat = array(NA,ntrials)
  pe1 = array(NA,ntrials)
  pe2 = array(NA,ntrials)
  w2 = array(NA,ntrials)
  r2 = array(NA,ntrials)
  pi3hat = array(NA,ntrials)
  pi3 = array(NA,ntrials)
  sa1hat = array(NA,ntrials)
  mu2 = array(NA,ntrials)
  sa2 = array(NA,ntrials)
  sa2hat = array(NA,ntrials)
  mu3 = array(NA,ntrials)
  sa3 = array(NA,ntrials)
  y = array(NA,ntrials)
  
  mu2[1] = input$Inital_mu2
  sa2[1] = input$Inital_prec2
  mu3[1] = input$Inital_mu3
  sa3[1] = input$Inital_prec3
  
  
  for(t in 2:ntrials){
    
    mu1hat[t] = HGF_mu1hat_update(mu2[t-1])
    
    if(pain[t-1] == 1){
      sa2hat[t] = HGF_sa2hat_update(sa2[t-1],kappa, mu3[t-1], omegapain)
      sa1hat[t] = HGF_sa1hat_update(mu1hat[t])
      pe1[t] = prediction_error1(mu1hat[t], u[t-1])
      sa2[t] = HGF_sa2_update(sa2hat[t],sa1hat[t])
      mu2[t] = HGF_mu2_update(mu2[t-1], pe1[t],sa2[t])
      pe2[t] = prediction_error2(sa2[t],sa2[t-1],mu2[t],mu2[t-1],kappa,mu3[t-1],omegapain)
      r2[t] = HGF_update_r2(kappa,mu3[t-1],omegapain,sa2[t-1])
      w2[t] = HGF_update_w2(kappa,mu3[t-1],omegapain, sa2[t-1])
    }else{
      sa2hat[t] = HGF_sa2hat_update(sa2[t-1],kappa, mu3[t-1], omegan)
      sa1hat[t] = HGF_sa1hat_update(mu1hat[t])
      pe1[t] = prediction_error1(mu1hat[t], u[t-1])
      sa2[t] = HGF_sa2_update(sa2hat[t],sa1hat[t])
      mu2[t] = HGF_mu2_update(mu2[t-1], pe1[t],sa2[t])
      pe2[t] = prediction_error2(sa2[t],sa2[t-1],mu2[t],mu2[t-1],kappa,mu3[t-1],omegan)
      r2[t] = HGF_update_r2(kappa,mu3[t-1],omegan,sa2[t-1])
      w2[t] = HGF_update_w2(kappa,mu3[t-1],omegan, sa2[t-1])
      
    }

    
    pi3hat[t] = HGF_update_pi3hat(sa3[t-1], theta)
    pi3[t] = HGF_update_pi3(pi3hat[t],kappa, w2[t],r2[t],pe2[t])
    sa3[t] = HGF_update_sa3(pi3[t])
    mu3[t] = HGF_update_mu3(mu3[t-1], sa3[t], kappa,w2[t], pe2[t]) 
    y[t] = rbinom(1,1,mu1hat[t])
  }
  
  data$sa2hat = sa2hat
  data$mu1hat = mu1hat
  data$sa1hat = sa1hat
  data$pe1 = pe1
  data$pe2 = pe2
  data$mu2 = mu2
  data$r2 = r2
  data$w2 = w2
  data$pi3hat = pi3hat
  data$pi3 = pi3
  data$sa3 = sa3
  data$mu3 = mu3
  data$sa2 = sa2
  data$trial = 1:nrow(data)
  data$y = y
  return(data)
}



hgf_agent_nu_pu = function(us, input){
  
  sigmoid = function(x) {
    1 / (1 + exp(-x))}
  
  HGF_sa2hat_update <- function(sa22, kappa, mu33, omega){
    return(sa22+exp(kappa*mu33+omega))}
  
  
  HGF_mu1_update <- function(x,mu22,alpha,etaa,etab){
    return((exp(-((x-etaa)^2)/(2*alpha))*(sigmoid(mu22))) / ((exp(-((x-etaa)^2)/(2*alpha))*(sigmoid(mu22)))+(exp(-((x-etab)^2)/(2*alpha))*(1-sigmoid(mu22)))))}
  
  
  HGF_mu1hat_update <- function(mu22){
    return(sigmoid(mu22))}
  
  HGF_sa1hat_update <- function(mu1hat){
    return(mu1hat*(1-mu1hat))}
  
  HGF_sa2_update <- function(sa2hat, sa1hat){
    return(1/((1/sa2hat)+sa1hat))}
  
  prediction_error1 <- function(mu1, mu22){
    return(mu1-sigmoid(mu22))}
  
  HGF_mu2_update = function(mu22, pe1,sa2){
    return(mu22+pe1*sa2)}
  
  prediction_error2 <- function(sa2,sa22,mu2,mu22,kappa,mu33,omega){
    return(((sa2+(mu2-mu22)^2)/(sa22+exp(kappa*mu33+omega)))-1)}
  
  HGF_update_r2 <- function(kappa, mu33,omega,sa22){
    return((exp(kappa*mu33+omega)-sa22)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_w2 <- function(kappa, mu33,omega,sa22){
    return(exp(kappa*mu33+omega)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_pi3hat <- function(sa33,theta){
    return(1/(sa33+theta))}
  
  HGF_update_pi3 <- function(pi3hat,kappa,w2,r2,pe2){
    return(pi3hat+(kappa^2/2)*w2*(w2+r2*pe2))}
  
  HGF_update_sa3 <- function(pi3){
    return(1/pi3)}
  
  HGF_update_mu3 <- function(mu33,sa3,kappa,w2,pe2){
    return(mu33+sa3*(kappa/2)*w2*pe2)}
  
  data = us
  u <- data$u
  ur = data$ur
  
  theta = input$theta
  kappa = input$kappa
  omega = input$omega
  nu = input$nu
  
  etab = input$etab
  etaa = input$etaa
  alpha = input$alpha
  
  
  ntrials = nrow(data)
  
  mu1hat = array(NA,ntrials)
  mu1 = array(NA,ntrials)
  
  pe1 = array(NA,ntrials)
  pe2 = array(NA,ntrials)
  w2 = array(NA,ntrials)
  r2 = array(NA,ntrials)
  pi3hat = array(NA,ntrials)
  pi3 = array(NA,ntrials)
  sa1hat = array(NA,ntrials)
  mu2 = array(NA,ntrials)
  sa2 = array(NA,ntrials)
  sa2hat = array(NA,ntrials)
  mu3 = array(NA,ntrials)
  sa3 = array(NA,ntrials)
  y = array(NA,ntrials)
  belief = array(NA,ntrials)
  
  
  mu2[1] = input$Inital_mu2
  sa2[1] = input$Inital_prec2
  mu3[1] = input$Inital_mu3
  sa3[1] = input$Inital_prec3
  
  
  for(t in 2:ntrials){
    #priors: (gets cue have to make a prediction:)
    mu1hat[t-1] = sigmoid(mu2[t-1])
    sa1hat[t-1] = HGF_sa1hat_update(mu1hat[t-1])
    sa2hat[t-1] = HGF_sa2hat_update(sa2[t-1],kappa, mu3[t-1], omega)
    pi3hat[t-1] = HGF_update_pi3hat(sa3[t-1], theta)
    r2[t] = HGF_update_r2(kappa,mu3[t-1],omega,sa2[t-1])
    w2[t] = HGF_update_w2(kappa,mu3[t-1],omega, sa2[t-1])
    sa2[t] = HGF_sa2_update(sa2hat[t-1],sa1hat[t-1])
    #now we get stimulus and have to update our beliefs about the cue-stimulus association, however the stimulus is ambiguous (perceptual uncertainty)
    
    #we now therefore have to update our belief about the first level, because we did not perieve the stimulus unambigously (i.e. beleiefing it was 1 or 0):
    #Here etaa and etab could be the ratings the participants gets: i.e. pain = 0.3 and u being the actual stimulus value i.e. pain no pain. 
    mu1[t] = HGF_mu1_update(u[t],mu2[t-1],alpha,etaa,etab)
    
    #now that we have a belief about what the stimulus was and had a prediction about it we can calculate a prediction error:
    
    pe1[t] = prediction_error1(mu1[t], mu2[t-1])
    #which then propagates up the hierarchy and updates the upper levels:
    
    mu2[t] = HGF_mu2_update(mu2[t-1], pe1[t],sa2[t])
    pe2[t] = prediction_error2(sa2[t],sa2[t-1],mu2[t],mu2[t-1],kappa,mu3[t-1],omega)
    pi3[t] = HGF_update_pi3(pi3hat[t-1],kappa, w2[t],r2[t],pe2[t])
    sa3[t] = HGF_update_sa3(pi3[t])
    mu3[t] = HGF_update_mu3(mu3[t-1], sa3[t], kappa,w2[t], pe2[t]) 
    belief[t] = mu1hat[t-1]+(1/(1+nu))*(ur[t]-mu1hat[t-1])
    
    y[t] = rbinom(1,1,belief[t])
  }
  
  
  data$sa2hat = sa2hat
  data$mu1hat = mu1hat
  data$sa1hat = sa1hat
  data$pe1 = pe1
  data$pe2 = pe2
  data$mu2 = mu2
  data$mu1 = mu1
  
  data$r2 = r2
  data$w2 = w2
  data$pi3hat = pi3hat
  data$pi3 = pi3
  data$sa3 = sa3
  data$mu3 = mu3
  data$sa2 = sa2
  data$trial = 1:nrow(data)
  data$y = y
  data$belief = belief
  
  
  return(data)
}





hgf_agent_nu = function(us, input){
  
  sigmoid = function(x) {
    1 / (1 + exp(-x))}
  
  HGF_sa2hat_update <- function(sa22, kappa, mu33, omega){
    return(sa22+exp(kappa*mu33+omega))}
  
  HGF_mu1hat_update <- function(mu22){
    return(sigmoid(mu22))}
  
  HGF_sa1hat_update <- function(mu1hat){
    return(mu1hat*(1-mu1hat))}
  
  HGF_sa2_update <- function(sa2hat, sa1hat){
    return(1/((1/sa2hat)+sa1hat))}
  
  prediction_error1 <- function(mu1hat, x){
    return(x-mu1hat)}
  
  HGF_mu2_update = function(mu22, pe1,sa2){
    return(mu22+pe1*sa2)}
  
  prediction_error2 <- function(sa2,sa22,mu2,mu22,kappa,mu33,omega){
    return(((sa2+(mu2-mu22)^2)/(sa22+exp(kappa*mu33+omega)))-1)}
  
  HGF_update_r2 <- function(kappa, mu33,omega,sa22){
    return((exp(kappa*mu33+omega)-sa22)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_w2 <- function(kappa, mu33,omega,sa22){
    return(exp(kappa*mu33+omega)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_pi3hat <- function(sa33,theta){
    return(1/(sa33+theta))}
  
  HGF_update_pi3 <- function(pi3hat,kappa,w2,r2,pe2){
    return(pi3hat+(kappa^2/2)*w2*(w2+r2*pe2))}
  
  HGF_update_sa3 <- function(pi3){
    return(1/pi3)}
  
  HGF_update_mu3 <- function(mu33,sa3,kappa,w2,pe2){
    return(mu33+sa3*(kappa/2)*w2*pe2)}
  
  data = us
  ur = data$ur
  
  theta = input$theta
  kappa = input$kappa
  omega = input$omega
  nu = input$nu
  z = input$z
  
  
  ntrials = nrow(data)
  
  mu1hat = array(NA,ntrials)
  pe1 = array(NA,ntrials)
  pe2 = array(NA,ntrials)
  w2 = array(NA,ntrials)
  r2 = array(NA,ntrials)
  pi3hat = array(NA,ntrials)
  pi3 = array(NA,ntrials)
  sa1hat = array(NA,ntrials)
  mu2 = array(NA,ntrials)
  sa2 = array(NA,ntrials)
  sa2hat = array(NA,ntrials)
  mu3 = array(NA,ntrials)
  sa3 = array(NA,ntrials)
  y = array(NA,ntrials)
  belief = array(NA,ntrials)
  belief2 = array(NA,ntrials)
  
  
  mu2[1] = input$Inital_mu2
  sa2[1] = input$Inital_prec2
  mu3[1] = input$Inital_mu3
  sa3[1] = input$Inital_prec3
  
  
  for(t in 2:ntrials){
    sa2hat[t] = HGF_sa2hat_update(sa2[t-1],kappa, mu3[t-1], omega)
    mu1hat[t] = HGF_mu1hat_update(mu2[t-1])
    sa1hat[t] = HGF_sa1hat_update(mu1hat[t])
    
    belief[t] = mu1hat[t]+(1/(1+nu))*(ur[t]-mu1hat[t])
    belief2[t] = (belief[t]^z)/(belief[t]^z+(1-belief[t])^z)
    
    y[t] = rbinom(1,1,belief2[t])
    
    pe1[t] = prediction_error1(mu1hat[t], y[t])

    sa2[t] = HGF_sa2_update(sa2hat[t],sa1hat[t])
    mu2[t] = HGF_mu2_update(mu2[t-1], pe1[t],sa2[t])
    pe2[t] = prediction_error2(sa2[t],sa2[t-1],mu2[t],mu2[t-1],kappa,mu3[t-1],omega)
    r2[t] = HGF_update_r2(kappa,mu3[t-1],omega,sa2[t-1])
    w2[t] = HGF_update_w2(kappa,mu3[t-1],omega, sa2[t-1])
    pi3hat[t] = HGF_update_pi3hat(sa3[t-1], theta)
    pi3[t] = HGF_update_pi3(pi3hat[t],kappa, w2[t],r2[t],pe2[t])
    sa3[t] = HGF_update_sa3(pi3[t])
    mu3[t] = HGF_update_mu3(mu3[t-1], sa3[t], kappa,w2[t], pe2[t])
    
    belief[t] = mu1hat[t]+(1/(1+nu))*(ur[t]-mu1hat[t])
    
    
    
  }
  
  data$sa2hat = sa2hat
  data$mu1hat = mu1hat
  data$sa1hat = sa1hat
  data$pe1 = pe1
  data$pe2 = pe2
  data$mu2 = mu2
  data$r2 = r2
  data$w2 = w2
  data$pi3hat = pi3hat
  data$pi3 = pi3
  data$sa3 = sa3
  data$mu3 = mu3
  data$sa2 = sa2
  data$trial = 1:nrow(data)
  data$y = y
  data$belief = belief
  data$belief2 = belief2
  
  
  return(data)
}


#plot nu

plot_hgf_nu = function(data){
  
  
  lvl1 = data %>% ggplot(aes())+
    ggtitle("Prediction")+
    #belief on input
    geom_line(aes(x = trial, y = mu1hat),col = "black")+
    geom_line(aes(x = trial, y = belief),col = "red")+
    #Actual input
    geom_point(aes(x = trial, y = ifelse(u == 1, 1.1, -0.1)), col = "purple")+
    #underlying probability:
    geom_line(aes(x = trial, y = c(rep(obj,trials),NA)), col = "green")+
    geom_line(aes(x = trial, y = c(rep(bias,trials),NA)), col = "blue")+
    #uncertainty on this belief:
    theme_classic()+
    scale_y_continuous(breaks = seq(0,1,by = 0.1))+
    coord_cartesian(ylim = c(-0.2,1.2))+
    theme(text = element_text(size=12))+ggtitle("Black = mu1hat,  red = belief, green = objective truth, blue = bias")
  
  
  
  #Second level:
  
  lvl2 = data %>% ggplot(aes())+ggtitle("Expectations")+
    #belief
    geom_line(aes(x = trial, y = mu2), col = "#4c72b0")+
    #uncertainty:
    geom_ribbon(aes(x = trial, ymax = mu2+sa2, ymin = mu2-sa2), col = "#4c72b0", alpha = 0.3)+
    theme_classic()+
    theme(text = element_text(size=12))
  
  
  #third level:
  
  lvl3 = data %>% ggplot(aes())+ggtitle("Volatility")+
    #belief
    geom_line(aes(x = trial, y = mu3), col = "black")+
    #uncertainty:
    geom_ribbon(aes(x = trial, ymax = mu3+sa3, ymin = mu3-sa3), fill  = "black", alpha = 0.3)+
    theme_classic()+
    theme(text = element_text(size=12))
  
  
  return((lvl3)/(lvl2)/(lvl1))
  
}









hgf_agent_per_un = function(u, input){
  
  sigmoid = function(x) {
    1 / (1 + exp(-x))}
  
  HGF_sa2hat_update <- function(sa22, kappa, mu33, omega){
    return(sa22+exp(kappa*mu33+omega))}
  
  HGF_mu1_update <- function(x,mu22,alpha,etaa,etab){
    return((exp(-((x-etaa)^2)/(2*alpha))*(sigmoid(mu22))) / ((exp(-((x-etaa)^2)/(2*alpha))*(sigmoid(mu22)))+(exp(-((x-etab)^2)/(2*alpha))*(1-sigmoid(mu22)))))}
  
  HGF_sa1hat_update <- function(mu1hat){
    return(mu1hat*(1-mu1hat))}
  
  HGF_sa2_update <- function(sa2hat, sa1hat){
    return(1/((1/sa2hat)+sa1hat))}
  
  prediction_error1 <- function(mu1, mu22){
    return(mu1-sigmoid(mu22))}
  
  HGF_mu2_update = function(mu22, pe1,sa2){
    return(mu22+pe1*sa2)}
  
  prediction_error2 <- function(sa2,sa22,mu2,mu22,kappa,mu33,omega){
    return(((sa2+(mu2-mu22)^2)/(sa22+exp(kappa*mu33+omega)))-1)}
  
  HGF_update_r2 <- function(kappa, mu33,omega,sa22){
    return((exp(kappa*mu33+omega)-sa22)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_w2 <- function(kappa, mu33,omega,sa22){
    return(exp(kappa*mu33+omega)/(sa22+exp(kappa*mu33+omega)))}
  
  HGF_update_pi3hat <- function(sa33,theta){
    return(1/(sa33+theta))}
  
  HGF_update_pi3 <- function(pi3hat,kappa,w2,r2,pe2){
    return(pi3hat+(kappa^2/2)*w2*(w2+r2*pe2))}
  
  HGF_update_sa3 <- function(pi3){
    return(1/pi3)}
  
  HGF_update_mu3 <- function(mu33,sa3,kappa,w2,pe2){
    return(mu33+sa3*(kappa/2)*w2*pe2)}
  
  
  data <- data.frame(u)
  
  theta = input$theta
  kappa = input$kappa
  omega = input$omega
  etab = input$etab
  etaa = input$etaa
  alpha = input$alpha
  
  
  ntrials = nrow(data)
  
  mu1hat = array(NA,ntrials)
  pe1 = array(NA,ntrials)
  pe2 = array(NA,ntrials)
  w2 = array(NA,ntrials)
  r2 = array(NA,ntrials)
  pi3hat = array(NA,ntrials)
  pi3 = array(NA,ntrials)
  sa1hat = array(NA,ntrials)
  mu2 = array(NA,ntrials)
  sa2 = array(NA,ntrials)
  sa2hat = array(NA,ntrials)
  mu3 = array(NA,ntrials)
  sa3 = array(NA,ntrials)
  mu1 = array(NA,ntrials)
  y = array(NA,ntrials)  
  
  mu2[1] = input$Inital_mu2
  sa2[1] = input$Inital_prec2
  mu3[1] = input$Inital_mu3
  sa3[1] = input$Inital_prec3
  
  
  for(t in 1:ntrials){
    #priors: (gets cue have to make a prediction:)
    mu1hat[t] = sigmoid(mu2[t])
    sa1hat[t] = HGF_sa1hat_update(mu1hat[t])
    sa2hat[t] = HGF_sa2hat_update(sa2[t],kappa, mu3[t], omega)
    pi3hat[t] = HGF_update_pi3hat(sa3[t], theta)
    r2[t] = HGF_update_r2(kappa,mu3[t],omega,sa2[t])
    w2[t] = HGF_update_w2(kappa,mu3[t],omega, sa2[t])
    sa2[t+1] = HGF_sa2_update(sa2hat[t],sa1hat[t])
    #now we get stimulus and have to update our beliefs about the cue-stimulus association, however the stimulus is ambiguous (perceptual uncertainty)
    
    #we now therefore have to update our belief about the first level, because we did not perieve the stimulus unambigously (i.e. beleiefing it was 1 or 0):
    #Here etaa and etab could be the ratings the participants gets: i.e. pain = 0.3 and u being the actual stimulus value i.e. pain no pain. 
    mu1[t] = HGF_mu1_update(u[t],mu2[t],alpha,etaa,etab)
    
    #now that we have a belief about what the stimulus was and had a prediction about it we can calculate a prediction error:
    
    pe1[t] = prediction_error1(mu1[t], mu2[t])
    #which then propagates up the hierarchy and updates the upper levels:
    
    mu2[t+1] = HGF_mu2_update(mu2[t], pe1[t],sa2[t+1])
    pe2[t] = prediction_error2(sa2[t+1],sa2[t],mu2[t+1],mu2[t],kappa,mu3[t],omega)
    pi3[t] = HGF_update_pi3(pi3hat[t],kappa, w2[t],r2[t],pe2[t])
    sa3[t+1] = HGF_update_sa3(pi3[t])
    mu3[t+1] = HGF_update_mu3(mu3[t], sa3[t+1], kappa,w2[t], pe2[t]) 
    y[t] = rbinom(1,1,mu1hat[t])
  }
  
  data = data.frame(trial = 1:length(u))
  data$mu1hat = mu1hat
  data$sa1hat = sa1hat
  data$sa2hat = sa2hat
  data$pi3hat = pi3hat
  data$y = y
  data$r2 = r2
  data$w2 = w2
  data$u = u
  
  data$pe1 = pe1
  data$pe2 = pe2
  data$pi3 = pi3
  data$mu1 = mu1
  
  
  data = rbind(NA,data)
  
  
  
  data$sa3 = sa3
  data$mu3 = mu3
  data$sa2 = sa2
  data$mu2 = mu2
  
  
  return(data)
}




plothgfdata = function(data){
  
  q1 = data %>% dplyr::select(u,trial,mu1hat,sa1hat,mu2,sa2,mu3,sa3,y) %>% mutate(level = 1, mu2 = NA, sa2 = NA,mu3 = NA,sa3 = NA)
  q2 = data %>% dplyr::select(u,trial,mu1hat,sa1hat,mu2,sa2,mu3,sa3,y) %>% mutate(level = 2, mu1hat = NA, sa1hat = NA, mu3 = NA, sa3 = NA, u = NA,y = NA)
  q3 = data %>% dplyr::select(u,trial,mu1hat,sa1hat,mu2,sa2,mu3,sa3,y) %>% mutate(level = 3, mu1hat = NA, sa1hat = NA,mu2 = NA, sa2 = NA, u = NA, y = NA)
  
  q3 = rbind(q1,q2,q3)
  
  q3$level = as.factor(q3$level)
  
  
  q3 %>% mutate(level = factor(level, labels = c("Predictions","Expectations","Volatility")),level = factor(level, levels = c("Volatility", "Expectations","Predictions")),lower1 = mu1hat-sa1hat, upper1 = mu1hat+sa1hat, lower2 = mu2-sa2, upper2 = mu2+sa2, lower3 = mu3-sa3, upper3 = mu3+sa3) %>%
    ggplot(aes())+
    geom_line(data = data.frame(level = as.factor("Predictions"), x = 1:length(u), y = rep(bias,trials)),aes(x = 1:length(u), y = rep(bias,trials)))+
    facet_wrap(~level, scales = "free",nrow = 3)+
    geom_line(aes(x = trial, y = mu1hat), col = "#c44e52")+
    geom_point(aes(x = trial, y = u), col = "black")+
    geom_point(aes(x = trial, y = ifelse(y==0, -0.1,1.1)), col = "orange")+
    geom_ribbon(aes(x = trial, ymax = upper1, ymin = lower1), fill = "#4c72b0", alpha = 0.5)+
    geom_ribbon(aes(x = trial, ymax = upper2, ymin = lower2), fill  = "#c44e52", alpha = 0.5)+
    #geom_ribbon(aes(x = trial, ymax = upper3, ymin = lower3), fill  = "black", alpha = 0.5)+
    geom_line(aes(x = trial, y = mu2), col = "#c44e52")+
    geom_line(aes(x = trial, y = mu3), col = "black")+
    theme_classic()+
    theme(text = element_text(size=12))+
    ylab(" ")+
    xlab("Trials")
}




plothgfdata1 = function(data){
  
  q1 = data %>% dplyr::select(u,trial,mu1hat,sa1hat,mu2,sa2,mu3,sa3,y) %>% mutate(level = 1, mu2 = NA, sa2 = NA,mu3 = NA,sa3 = NA)
  q2 = data %>% dplyr::select(u,trial,mu1hat,sa1hat,mu2,sa2,mu3,sa3,y) %>% mutate(level = 2, mu1hat = NA, sa1hat = NA, mu3 = NA, sa3 = NA, u = NA,y = NA)
  q3 = data %>% dplyr::select(u,trial,mu1hat,sa1hat,mu2,sa2,mu3,sa3,y) %>% mutate(level = 3, mu1hat = NA, sa1hat = NA,mu2 = NA, sa2 = NA, u = NA, y = NA)
  
  q3 = rbind(q1,q2,q3)
  
  q3$level = as.factor(q3$level)
  
  
  q3 %>% mutate(level = factor(level, labels = c("Predictions","Expectations","Volatility")),level = factor(level, levels = c("Volatility", "Expectations","Predictions")),lower1 = mu1hat-sa1hat, upper1 = mu1hat+sa1hat, lower2 = mu2-sa2, upper2 = mu2+sa2, lower3 = mu3-sa3, upper3 = mu3+sa3) %>%
    ggplot(aes())+
    geom_line(data = data.frame(level = as.factor("Predictions"), x = 1:length(u), y = rep(rep(bias,trials),times)),aes(x = x, y = y))+
    facet_wrap(~level, scales = "free",nrow = 3)+
    geom_line(aes(x = trial, y = mu1hat), col = "#c44e52")+
    geom_point(aes(x = trial, y = u), col = "black")+
    geom_point(aes(x = trial, y = ifelse(y==0, -0.1,1.1)), col = "orange")+
    geom_ribbon(aes(x = trial, ymax = upper1, ymin = lower1), fill = "#4c72b0", alpha = 0.5)+
    geom_ribbon(aes(x = trial, ymax = upper2, ymin = lower2), fill  = "#c44e52", alpha = 0.5)+
    #geom_ribbon(aes(x = trial, ymax = upper3, ymin = lower3), fill  = "black", alpha = 0.5)+
    geom_line(aes(x = trial, y = mu2), col = "#c44e52")+
    geom_line(aes(x = trial, y = mu3), col = "black")+
    theme_classic()+
    theme(text = element_text(size=12))+
    ylab(" ")+
    xlab("Trials")
}



pr_hgf_2level = function(real_omega,df = dff, times = 2){
  seed = round(runif(1,0,10000000),0)
  
  setwd("~/Advanced-cognitive-modeling/assignment2")
  filemodel = "stan_models/2-level-hgf-no_pu_pr.stan"
  
  mod = cmdstan_model(filemodel)
  
  
  u = rm_agent(df$bias,df$trials)
  
  u = rep(u,times)
  
  input = data.frame(kappa = 0, theta = 0, omega = real_omega,Inital_prec2 = 4,Inital_mu2 = 0,Inital_mu3 = 0,Inital_prec3 = 4)
  dataq = hgf_agent(u,input)[-1,]
  
  data1 = list(ntrials = nrow(dataq), u = dataq$u, y = dataq$y, prior = 0)
  
  fit <- mod$sample(
    seed = seed,
    data = data1,
    chains = 4, 
    parallel_chains = 4,
    refresh = 0
  )
  
  df1 = as_draws_df(fit$summary(c("omega"))) %>% select("mean","variable","sd")  %>%  mutate(seed = seed, trials = times*20, real = real_omega, div = sum(fit$diagnostic_summary()$num_divergent))
  
  return(df1)
}



sensitivity_hgf_2level = function(means, sds,omega){
  seed = round(runif(1,0,10000000),0)
  
  setwd("~/Advanced-cognitive-modeling/assignment2")
  filemodel = "stan_models/2-level-hgf-no_pu_pr_sensitivity.stan"
  
  mod = cmdstan_model(filemodel)
  
  bias = c(0.8,0.2)
  trials = c(40,40)
  u = rm_agent(bias,trials)
  
  input = data.frame(kappa = 0, theta = 0, omega = omega,Inital_prec2 = 4,Inital_mu2 = 0,Inital_mu3 = 0,Inital_prec3 = 4)
  dataq = hgf_agent(u,input)[-1,]
  
  data1 = list(ntrials = nrow(dataq), u = dataq$u, y = dataq$y, prior = 0, priormean = means, priorsd = sds)
  
  fit <- mod$sample(
    seed = seed,
    data = data1,
    chains = 4, 
    parallel_chains = 4,
    refresh = 0
  )
  
  as_draws_df(fit$summary(c("omega"))) %>% select("mean","variable","sd")  %>%  
    mutate(seed = seed, trials = 80, real = omega,priormean = means, priorsd = sds, div = sum(fit$diagnostic_summary()$num_divergent))
  
}

#plot perceptual uncertainty:




plot_hgf_perceptual_uncertainty = function(data){
  
  lvl1 = data %>% ggplot(aes())+
    ggtitle("Prediction")+
    #belief on input
    geom_pointrange(aes(x = trial, y = mu1, ymin = mu1-input$alpha, ymax = mu1+input$alpha), col = "yellow")+
    #Actual input
    geom_point(aes(x = trial, y = ifelse(u == 1, 1.1, -0.1)), col = "purple")+
    #underlying probability:
    geom_line(aes(x = trial, y = c(rep(bias,trials),NA)))+
    #prediction on underlying probability:
    geom_line(aes(x = trial, y = mu1hat), col = "#c44e52")+
    #uncertainty on this belief:
    geom_ribbon(aes(x = trial, ymax = mu1hat+sa1hat, ymin = mu1hat-sa1hat), fill = "#c44e52", alpha = 0.3)+
    theme_classic()+
    scale_y_continuous(breaks = seq(0,1,by = 0.1))+
    coord_cartesian(ylim = c(-0.2,1.2))+
  theme(text = element_text(size=12))
  
  
  
  #Second level:
  
  lvl2 = data %>% ggplot(aes())+ggtitle("Expectations")+
    #belief
    geom_line(aes(x = trial, y = mu2), col = "#4c72b0")+
    #uncertainty:
    geom_ribbon(aes(x = trial, ymax = mu2+sa2, ymin = mu2-sa2), col = "#4c72b0", alpha = 0.3)+
    theme_classic()+
    theme(text = element_text(size=12))
  
  
  #third level:
  
  lvl3 = data %>% ggplot(aes())+ggtitle("Volatility")+
    #belief
    geom_line(aes(x = trial, y = mu3), col = "black")+
    #uncertainty:
    geom_ribbon(aes(x = trial, ymax = mu3+sa3, ymin = mu3-sa3), fill  = "black", alpha = 0.3)+
    theme_classic()+
    theme(text = element_text(size=12))
  
  
  return((lvl3)/(lvl2)/(lvl1))
}



plot_hgf_perceptual_uncertainty_fitted = function(data, data1){
  
  lvl1 = data %>% ggplot(aes())+
    ggtitle("Prediction")+
    #belief on input
    geom_pointrange(aes(x = trial, y = mu1, ymin = mu1-data1$alpha, ymax = mu1+data1$alpha), col = "yellow")+
    #Actual input
    geom_point(aes(x = trial, y = ifelse(u == 1, 1.1, -0.1)), col = "purple")+
    #underlying probability:
    #geom_line(aes(x = trial, y = c(rep(bias,trials),NA)))+
    #prediction on underlying probability:
    geom_line(aes(x = trial, y = mu1hat), col = "#c44e52")+
    #uncertainty on this belief:
    geom_ribbon(aes(x = trial, ymax = mu1hat+sa1hat, ymin = mu1hat-sa1hat), fill = "#c44e52", alpha = 0.3)+
    theme_classic()+
    scale_y_continuous(breaks = seq(0,1,by = 0.1))+
    coord_cartesian(ylim = c(-0.2,1.2))+
    theme(text = element_text(size=12))
  
  
  
  #Second level:
  
  lvl2 = data %>% ggplot(aes())+ggtitle("Expectations")+
    #belief
    geom_line(aes(x = trial, y = mu2), col = "#4c72b0")+
    #uncertainty:
    geom_ribbon(aes(x = trial, ymax = mu2+sa2, ymin = mu2-sa2), col = "#4c72b0", alpha = 0.3)+
    theme_classic()+
    theme(text = element_text(size=12))
  
  
  #third level:
  
  lvl3 = data %>% ggplot(aes())+ggtitle("Volatility")+
    #belief
    geom_line(aes(x = trial, y = mu3), col = "black")+
    #uncertainty:
    geom_ribbon(aes(x = trial, ymax = mu3+sa3, ymin = mu3-sa3), fill  = "black", alpha = 0.3)+
    theme_classic()+
    theme(text = element_text(size=12))
  
  
  return((lvl3)/(lvl2)/(lvl1))
}



plot_hgf_perceptual_no_uncertainty_fitted = function(data){
  library(patchwork)
  
  lvl1 = data %>% ggplot(aes())+
    ggtitle("Prediction")+
    geom_line(data = data.frame(),aes(x = data$trial, y = rep(bias,trials)[-1]))+
    #belief on input
    #geom_pointrange(aes(x = trial, y = mu1, ymin = mu1-data1$alpha, ymax = mu1+data1$alpha), col = "yellow")+
    #Actual input
    geom_point(aes(x = trial, y = u ), col = "black")+
    #underlying probability:
    #geom_line(aes(x = trial, y = c(rep(bias,trials),NA)))+
    #prediction on underlying probability:
    geom_line(aes(x = trial, y = mu1hat), col = "#c44e52")+
    #uncertainty on this belief:
    geom_ribbon(aes(x = trial, ymax = mu1hat+sa1hat, ymin = mu1hat-sa1hat), fill = "#4c72b0", alpha = 0.3)+
    theme_classic()+
    scale_y_continuous(breaks = seq(0,1,by = 0.1))+
    coord_cartesian(ylim = c(-0.2,1.2))+
    theme(text = element_text(size=12))+
    ylab(" ")+
    xlab
  
  
  
  #Second level:
  
  lvl2 = data %>% ggplot(aes())+ggtitle("Expectations")+
    #belief
    geom_line(aes(x = trial, y = mu2), col = "#c44e52")+
    #uncertainty:
    geom_ribbon(aes(x = trial, ymax = mu2+sa2, ymin = mu2-sa2), col = "#c44e52", alpha = 0.3)+
    theme_classic()+
    theme(text = element_text(size=12))+
    ylab(" ")
  
  
  #third level:
  
  lvl3 = data %>% ggplot(aes())+ggtitle("Volatility")+
    #belief
    geom_line(aes(x = trial, y = mu3), col = "black")+
    #uncertainty:
    geom_ribbon(aes(x = trial, ymax = mu3+sa3, ymin = mu3-sa3), fill  = "black", alpha = 0.3)+
    theme_classic()+
    theme(text = element_text(size=12))+
    ylab(" ")
  
  
  return((lvl3)/(lvl2)/(lvl1))
}



plot_prior_post_update = function(model,real,levels){
  posterior = as_draws_df(model$draws())
  if(levels == 3){
    theta = posterior %>% pivot_longer(cols = c("theta","p_theta")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$theta)
    kappa = posterior %>% pivot_longer(cols = c("kappa","p_kappa")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$kappa)
    omega = posterior %>% pivot_longer(cols = c("omega","p_omega")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$omega)
    etaa = posterior %>% pivot_longer(cols = c("etaa","p_etaa")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$etaa)
    etab = posterior %>% pivot_longer(cols = c("etab","p_etab")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$etab)
    alpha = posterior %>% pivot_longer(cols = c("alpha","p_alpha")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$alpha)
    
    return((theta+kappa)/(omega+alpha)/(etab+etaa))  
  }else{
    omega = posterior %>% pivot_longer(cols = c("omega","p_omega")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$omega)
    etaa = posterior %>% pivot_longer(cols = c("etaa","p_etaa")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$etaa)
    etab = posterior %>% pivot_longer(cols = c("etab","p_etab")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$etab)
    alpha = posterior %>% pivot_longer(cols = c("alpha","p_alpha")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$alpha)
    
    return((omega+alpha)/(etab+etaa))  
  }
}




plot_prior_post_update_nopu = function(model,real,levels){
  library(patchwork)
  posterior = as_draws_df(model$draws())
  if(levels == 3){
    theta = posterior %>% pivot_longer(cols = c("theta","p_theta")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$theta)
    kappa = posterior %>% pivot_longer(cols = c("kappa","p_kappa")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$kappa)
    omega = posterior %>% pivot_longer(cols = c("omega","p_omega")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$omega)
    beta = posterior %>% pivot_longer(cols = c("beta","p_beta")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$beta)
    
    return((theta+kappa)/(omega+beta))  
  }else{
    omega = posterior %>% pivot_longer(cols = c("omega","p_omega")) %>% mutate() %>% ggplot(aes(x = value,fill = name))+geom_histogram()+theme_classic()+geom_vline(xintercept = input$omega)

    return((omega))  
  }
}
