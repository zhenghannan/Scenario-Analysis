library(readr)
setwd("~/Desktop/")
data = read_csv("20200930154324-Portfolio.csv") #this file is private

##### Basic Settings #####
s0 = round((data$CashDelta/data$Delta)[1],3)
Nweekdays <- Vectorize(function(a, b) 
  sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday")))
Dates1 = as.Date(as.character(data$Maturity))[-1]
Dates2 = as.Date(as.character("2020/09/30"))+rep(0,nrow(data)-1)
data$tau = c(0,Nweekdays(Dates2,Dates1))/252

bs = function(s,k,r,y,vol,t,c){
  d1 = (log(s/k)+(r-y+vol^2/2)*t)/(vol*sqrt(t))
  d2 = d1-vol*sqrt(t)
  if (c=='Call'){
    s*exp(-y*t)*pnorm(d1,0,1) - k*exp(-r*t)*pnorm(d2,0,1)
  }else if(c=='Put'){
    k*exp(-r*t)*pnorm(-d2,0,1) - s*exp(-y*t)*pnorm(-d1,0,1)
  }else{
    s
  }
}
delta = function(s,k,r,y,vol,t,c){
  d1 = (log(s/k)+(r-y+vol^2/2)*t)/(vol*sqrt(t))
  if (c=='Call'){
    exp(-y*t)*pnorm(d1,0,1)
  }else if (c=='Put'){
    -exp(-y*t)*(1-pnorm(d1,0,1))
  }else{
    1
  }
}
gamma = function(s,k,r,y,vol,t,c){
  d1 = (log(s/k)+(r-y+vol^2/2)*t)/(vol*sqrt(t))
  if (c!='ETF'){
    dnorm(d1,0,1)*exp(-y*t)/(s*vol*sqrt(t))
  }else{
    0
  }
}
theta = function(s,k,r,y,vol,t,c){
  d1 = (log(s/k)+(r-y+vol^2/2)*t)/(vol*sqrt(t))
  d2 = d1-vol*sqrt(t)
  if (c=='Call'){
    -(s*dnorm(d1,0,1)*vol*exp(-y*t)/(2*sqrt(t))) + y*s*exp(-y*t)*pnorm(d1,0,1) - r*k*pnorm(d2,0,1)*exp(-r*t)
  }else if (c=='Put'){
    -(s*dnorm(d1,0,1)*vol*exp(-y*t)/(2*sqrt(t))) - y*s*exp(-y*t)*pnorm(-d1,0,1) + r*k*pnorm(-d2,0,1)*exp(-r*t)
  }else{
    0
  }
}
vega = function(s,k,r,y,vol,t,c){
  d1 = (log(s/k)+(r-y+vol^2/2)*t)/(vol*sqrt(t))
  if (c!='ETF'){
    s*exp(-y*t)*sqrt(t)*dnorm(d1,0,1)
  }else{
    0
  }
}

##### Scenario Matrix #####
scenario = function(ds,dv){
  s1 = (1+ds)*s0
  #act_vol = data$ActVol*(1+dv)
  act_vol = data$ActVol+dv
  act_vol = pmax(act_vol,0.0001,na.rm = F)
  VALUE = c()
  DELTA = c()
  GAMMA = c()
  THETA = c()
  VEGA = c()
  for (i in 1:nrow(data)) {
    data_temp = data[i,]
    k_here = data_temp$Strike
    r_here = data_temp$Rate - data_temp$SSRate
    y_here = 0 #data_temp$SSRate
    vol_here = act_vol[i]
    t_here = as.numeric(data_temp$tau)
    c_here = data_temp$Type
    value_here = bs(s1,k_here,r_here,y_here,vol_here,t_here,c_here)*data_temp$Multi*data_temp$NetPos
    delta_here = delta(s1,k_here,r_here,y_here,vol_here,t_here,c_here)*data_temp$Multi*data_temp$NetPos
    gamma_here = gamma(s1,k_here,r_here,y_here,vol_here,t_here,c_here)*data_temp$Multi*data_temp$NetPos
    theta_here = theta(s1,k_here,r_here,y_here,vol_here,t_here,c_here)*data_temp$Multi*data_temp$NetPos/252
    vega_here = vega(s1,k_here,r_here,y_here,vol_here,t_here,c_here)*data_temp$Multi*data_temp$NetPos/100
    VALUE = c(VALUE,value_here)
    DELTA = c(DELTA,delta_here)
    GAMMA = c(GAMMA,gamma_here)
    THETA = c(THETA,theta_here)
    VEGA = c(VEGA,vega_here)
  }
  c(sum(VALUE),sum(DELTA)*s1,sum(GAMMA)*s1^2/100,sum(THETA),sum(VEGA))
}

baseline = c(scenario(0,0))

result = c()
for (ds in c(0,0.05,0.1,0.2,-0.05,-0.1,-0.2)){
  result_temp = c()
  for (dv in c(0,0.1,0.2,-0.1,-0.2)){
    result_temp = cbind(result_temp,(scenario(ds,dv) - baseline)/10000)
  }
  result = rbind(result,result_temp)
}

result = data.frame(result)
colnames(result) = c('Vol+0','Vol+10%','Vol+20%','Vol-10%','Vol-20%')
write_csv(result,"情景矩阵.csv")

##### 理论盈亏 #####
theo_pnl = function(ds,dv){
  under_part = sum(data$CashDelta)*ds + 50*sum(data$CashGamma)*(ds^2)
  vol_part = sum(data$Vega,na.rm = T)*dv*100
  (under_part+vol_part)/10000
}

result_1 = c()
for (ds in c(0,0.05,0.1,0.2,-0.05,-0.1,-0.2)){
  result_temp = c()
  for (dv in c(0,0.1,0.2,-0.1,-0.2)){
    pnl = ((scenario(ds,dv) - baseline))[1]
    if(pnl!=0){
      delta_part = (baseline[2]*ds)/pnl #sum(data$CashDelta)*ds/-114000#(baseline[2]*ds)/pnl
      gamma_part = (baseline[3]*ds^2)/pnl #sum(data$CashGamma)*ds^2/-114000#(baseline[3]*ds^2)/pnl
      vega_part = (baseline[5]*dv*100)/pnl
      resid_part = 1-delta_part-gamma_part-vega_part
      result_temp = cbind(result_temp,c(pnl/10000,delta_part,gamma_part,vega_part,resid_part)) 
    }else{
      result_temp = cbind(result_temp,c(0,0,0,0,0))
    }
  }
  result_1 = rbind(result_1,result_temp)
}
result_1 = data.frame(result_1)
colnames(result_1) = c('Vol+0','Vol+10%','Vol+20%','Vol-10%','Vol-20%')
write_csv(result_1,"scenario_analysis.csv")
