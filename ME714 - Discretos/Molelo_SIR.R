library(tidyverse)

#path1 = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
path2 = "https://raw.githubusercontent.com/datasets/covid-19/main/data/time-series-19-covid-combined.csv"
covid = read_csv(path2)
colnames(covid) = c("Date","Country","State","Confirmed","Recovered","Deaths")

covid_japan = covid %>% filter(Country == "Italy")

total_inf = covid_japan$Confirmed
mortes = covid_japan$Deaths
recuperados = covid_japan$Recovered

#Modelo SIR
R = recuperados + mortes
I = total_inf - R

#série temporal
ts.plot(total_inf[2:length(total_inf)]-total_inf[1:(length(total_inf)-1)]);abline(v = 50)
ts.plot(log(total_inf),type="l",col="black")
lines(log(recuperados),col="green")
lines(log(mortes),col = 'red')
const_i = 200;const_f = 220
ts.plot(total_inf[(const_i + 1):const_f] - total_inf[const_i:(const_f -1)])
ts.plot(log(total_inf[(const_i + 1):const_f] - total_inf[const_i:(const_f -1)]))

#regressão m
const_i = 210;const_f = 220
reg_total_inf = total_inf[const_i:const_f]
reg_total_inf[reg_total_inf == 0] = 1
reg_m = lm(log(reg_total_inf) ~ seq(const_i,const_f))
plot(log(reg_total_inf)); abline(coef(reg_m), col = "blue")
summary(reg_m)
coef(reg_m)[2]

#gamma
gamma = (R[(const_i+1):const_f]-R[const_i:(const_f-1)])/I[const_i:(const_f-1)]
ts.plot(gamma);abline(mean(gamma),0, col = "red")
mean(gamma)


#function SIR
SIR = function(S0, I0, R0, t0, t1, beta, gamma){
  N = S0+I0+R0
  S = S0; R = R0; I = I0
  SS = c(S0); RR = c(R0); II = c(I0)
  dt = 1
  t = t0
  tt = t0
  while(t <= t1){
    dS = -(beta/N)*S*I
    dI = (beta/N)*S*I - gamma*I
    dR = gamma*I
    S = S+dt*dS
    I = I+dt*dI
    R = R+dt*dR
    SS = c(SS,S);II = c(II,I);RR = c(RR,R)
    t = t+dt
    tt = c(tt,t)
  }
  return(list("SS"=SS,
              "II"=II,
              "RR"=RR,
              "tt"=tt))
}
  
#Modelo SIR
R_red = recuperados[const_i:const_f] + mortes[const_i:const_f]
I_red = total_inf[const_i:const_f] - R_red

#R0 = 1.4
gamma_mean = mean(gamma)
beta_mean = 1.4*gamma_mean 
r0 = 1.4
#beta_mean = coef(reg_m)[2] - gamma_mean - 0.18
#transmissão, recuperação,contato rate
as.numeric(r0); gamma_mean; as.numeric(beta_mean)
test_SIR = SIR((6e7-total_inf[length(150)]),
               I_red[length(I_red)],
               R_red[length(R_red)],
               0,800,
               beta_mean,gamma_mean)

const_f = length(total_inf)
ts.plot(test_SIR$SS,ylim=c(0,6e7),main = paste("R0 = ",1.4));lines(test_SIR$RR, col = "green");lines(test_SIR$II,col = 'red')
ts.plot(test_SIR$II, col ="red", ylim=c(0,3000000));abline(h = 3.4*6e7/1000);lines(total_inf[(const_i + 1):const_f]-total_inf[const_i:(const_f -1)]);abline(v = const_i)

#R0 real
gamma_mean = mean(gamma)
beta_mean = coef(reg_m)[2] - gamma_mean
r0 = beta_mean/gamma_mean
#transmissão, recuperação,contato rate
as.numeric(r0); gamma_mean; as.numeric(beta_mean)
test_SIR = SIR((6e7-total_inf[length(150)]),
               I_red[length(I_red)],
               R_red[length(R_red)],
               0,200,
               beta_mean,gamma_mean)

const_f = length(total_inf)
ts.plot(test_SIR$SS,ylim=c(0,6e7),main = paste("R0 = ",round(r0,2)));lines(test_SIR$RR, col = "green");lines(test_SIR$II,col = 'red')
ts.plot(test_SIR$II, col ="red", ylim=c(0,3000000));abline(h = 3.4*6e7/1000)


ts.plot(total_inf[(const_i + 1):const_f]-total_inf[const_i:(const_f -1)]);abline(v = const_i)

