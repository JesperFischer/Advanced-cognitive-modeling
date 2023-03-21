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
    
    mu2[1] = input$Inital_mu2
    sa2[1] = input$Inital_prec2
    mu3[1] = input$Inital_mu3
    sa3[1] = input$Inital_prec3
    
    
    for(t in 2:ntrials){
      sa2hat[t] = HGF_sa2hat_update(sa2[t-1],kappa, mu3[t-1], omega)
      mu1hat[t] = HGF_mu1hat_update(mu2[t-1])
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