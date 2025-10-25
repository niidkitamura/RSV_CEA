#proxy to run before installing package###
#proxy_url = "http://proxy.nih.go.jp:8080"
#Sys.setenv("http_proxy" = proxy_url)
#Sys.setenv("https_proxy" = proxy_url)
###########################################
 
#install.packages("lhs")

### probablistic sensitivity analysis
##create probablistic value
#install.packages("abind")
rm(list = ls())

library(dplyr)
library(tidyr)
library(abind)
library(ggplot2)
library(abind)
library(lhs)
options(scipen = 999)

###create probability matrix for Jan to Dec
NSIM = 10000
set.seed(120975)  #345437
k = 1
parameter_jan <- read.csv("input/parameter_jan.csv")
parameter_feb <- read.csv("input/parameter_feb.csv")
parameter_mar <- read.csv("input/parameter_mar.csv")
parameter_apr <- read.csv("input/parameter_apr.csv")
parameter_may <- read.csv("input/parameter_may.csv")
parameter_jun <- read.csv("input/parameter_jun.csv")
parameter_jul <- read.csv("input/parameter_jul.csv")
parameter_aug <- read.csv("input/parameter_aug.csv")
parameter_sep <- read.csv("input/parameter_sep.csv")
parameter_oct <- read.csv("input/parameter_oct.csv")
parameter_nov <- read.csv("input/parameter_nov.csv")
parameter_dec <- read.csv("input/parameter_dec.csv")

#View(parameter_jan)
num_row = nrow(parameter_jan) 
parameter_jan <- parameter_jan %>% mutate(heading = paste(pars, Month, sep = "_"))
heading = parameter_jan$heading

cost_log <- function(df){
  df %>% mutate(deterministic.value = case_when(pars == "cost_dinf" & Month<=12 ~ 7.860086,
                                           pars == "cost_dinf" & Month >12 ~ 7.743724,
                                           pars =="cost_hosp" & Month<=12 ~ 11,
                                           pars =="cost_hosp" & Month >12 ~ 10.6,
                                           pars=="cost_death" & Month<=12 ~ 10.9,
                                           pars=="cost_death" & Month >12 ~ 9.92,
                                           TRUE ~ deterministic.value))
}

df <- cost_log(parameter_jan)


probmat_jan <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_jan[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_jan[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_jan[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 = df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}

probmat_jan <- as.data.frame(probmat_jan)
probmat_jan[is.na(probmat_jan)] <- 1
colnames(probmat_jan) <- heading


df <- cost_log(parameter_feb)
probmat_feb <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_feb[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_feb[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_feb[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}
probmat_feb <- as.data.frame(probmat_feb)
probmat_feb[is.na(probmat_feb)] <- 1
colnames(probmat_feb) <- heading


df <- cost_log(parameter_mar)
probmat_mar <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_mar[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_mar[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_mar[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}

probmat_mar <- as.data.frame(probmat_mar)
probmat_mar[is.na(probmat_mar)] <- 1
colnames(probmat_mar) <- heading


df <- cost_log(parameter_apr)
probmat_apr <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_apr[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_apr[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_apr[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}

probmat_apr <- as.data.frame(probmat_apr)
probmat_apr[is.na(probmat_apr)] <- 1
colnames(probmat_apr) <- heading


df <- cost_log(parameter_may)
probmat_may <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_may[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_may[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_may[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}
probmat_may <- as.data.frame(probmat_may)
probmat_may[is.na(probmat_may)] <- 1
colnames(probmat_may) <- heading


df <- cost_log(parameter_jun)
probmat_jun <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_jun[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_jun[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_jun[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}

probmat_jun <- as.data.frame(probmat_jun)
probmat_jun[is.na(probmat_jun)] <- 1
colnames(probmat_jun) <- heading


df <- cost_log(parameter_jul)
probmat_jul <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_jul[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_jul[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_jul[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}
probmat_jul <- as.data.frame(probmat_jul)
probmat_jul[is.na(probmat_jul)] <- 1
colnames(probmat_jul) <- heading


df <- cost_log(parameter_aug)
probmat_aug <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_aug[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_aug[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_aug[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}

probmat_aug <- as.data.frame(probmat_aug)
probmat_aug[is.na(probmat_aug)] <- 1
colnames(probmat_aug) <- heading

df <- cost_log(parameter_sep)
probmat_sep <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_sep[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_sep[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_sep[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}

probmat_sep <- as.data.frame(probmat_sep)
probmat_sep[is.na(probmat_sep)] <- 1
colnames(probmat_sep) <- heading

df <- cost_log(parameter_oct)
probmat_oct <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_oct[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_oct[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_oct[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}
probmat_oct <- as.data.frame(probmat_oct)
probmat_oct[is.na(probmat_oct)] <- 1
colnames(probmat_oct) <- heading


df <- cost_log(parameter_nov)
probmat_nov <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_nov[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_nov[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_nov[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}
probmat_nov <- as.data.frame(probmat_nov)
probmat_nov[is.na(probmat_nov)] <- 1
colnames(probmat_nov) <- heading


df <- cost_log(parameter_dec)
probmat_dec <- matrix(0, nrow=NSIM, ncol = nrow(df))
for(i in 1:nrow(df)) {
  if(df$distribution[i] =="normal"){probmat_dec[,i] <- qnorm(randomLHS(NSIM,k)[,1], 
                                                             mean = df$deterministic.value[i],
                                                             sd = df$sd[i])}
  else if(df$distribution[i] =="lognormal"){probmat_dec[,i] <- qlnorm(randomLHS(NSIM,k)[,1],  
                                                                      meanlog = df$deterministic.value[i], 
                                                                      sdlog = df$sd[i])}
  else {probmat_dec[,i] <- qbeta(randomLHS(NSIM,k)[,1], 
                                 shape1 =df$deterministic.value[i]*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1),
                                 shape2 = (1-df$deterministic.value[i])*(df$deterministic.value[i]*(1-df$deterministic.value[i])/df$sd[i]^2 -1))}
}
probmat_dec <- as.data.frame(probmat_dec)
probmat_dec[is.na(probmat_dec)] <- 1
colnames(probmat_dec) <- heading


Npop <- read.csv("input/npop.csv")
Npop <- Npop[,c('pop2023')]
Npop <- t(as.matrix(Npop))/12  ## matrix ##monthly price

admi_cost =3575
lamab_price = 459147
abrysbo = 23948
price <- read.csv('input/price_pal_mhlw.csv')  ##price_pal_20250724
price <- price %>% mutate(price_pal_preterm = as.numeric(price_pal_preterm),
                          price_pal_down = as.numeric(price_pal_down),
                          price_pal_risk = as.numeric(price_pal_risk),
                          price_nir_preterm = as.numeric(price_nir_preterm),
                          price_nir_down = as.numeric(price_nir_down),
                          price_nir_risk = as.numeric(price_nir_risk))


##price
price[c(1:12),c(6:9)] <- price[c(1:12),c(6:9)] * lamab_price/22500 + admi_cost
price[c(1:12),c(2:5)] <- price[c(1:12),c(2:5)] + admi_cost
price[c(13:24),] <- price[c(13:24),] + admi_cost
price$Month <- seq(1:24)

price_offseason <- array(0, dim= c(nrow = 24, ncol = 9, 12))
for(n in 1:12){
  for(k in 1:9){
    for(month in 1:24){
      if (n<=4) {
        if(month == n | month == n+1 |month==n+2|month ==n+3| month ==n+4 |
           month == n +12| month ==n + 13| month == n + 14 | month == n + 15 | month == n + 16)
        { price_offseason[month,k,n] <- 0
        } else {
          price_offseason[month,k,n] <- price[month,k]
        }}
      else if (n<=8) {
        if(month<=9-n| month>=17-n & month<=17-n+4 |month >= 29-n)
        { price_offseason[month,k,n] <- 0
        } else {
          price_offseason[month,k,n] <- price[month,k]
        }}
      else if(n<=12){
        if(month <= 16-n | month >=22-n & month <=28-n | month>=34-n)
        { price_offseason[month,k,n] <- price[month,k]
        } else {
          price_offseason[month,k,n] <- 0
        }}
    }
  }
}

price_apr <- as.data.frame(price_offseason[,,1])
price_mar <- as.data.frame(price_offseason[,,2])
price_feb <- as.data.frame(price_offseason[,,3])
price_jan <- as.data.frame(price_offseason[,,4])
price_dec <- as.data.frame(price_offseason[,,12])
price_nov <- as.data.frame(price_offseason[,,11])
price_oct <- as.data.frame(price_offseason[,,10])
price_sep <- as.data.frame(price_offseason[,,9])
price_aug <- as.data.frame(price_offseason[,,8])
price_jul <- as.data.frame(price_offseason[,,7])
price_jun <- as.data.frame(price_offseason[,,6])
price_may <- as.data.frame(price_offseason[,,5])

colnames(price_aug) <- colnames(price)
colnames(price_jul) <- colnames(price)
colnames(price_jun) <- colnames(price)
colnames(price_may) <- colnames(price)
colnames(price_apr) <- colnames(price)
colnames(price_mar) <- colnames(price)
colnames(price_feb) <- colnames(price)
colnames(price_jan) <- colnames(price)
colnames(price_dec) <- colnames(price)
colnames(price_nov) <- colnames(price)
colnames(price_oct) <- colnames(price)
colnames(price_sep) <- colnames(price)

price_aug$Month <- seq(1:24)
price_jul$Month <- seq(1:24)
price_jun$Month <- seq(1:24)
price_may$Month <- seq(1:24)
price_apr$Month <- seq(1:24)
price_mar$Month <- seq(1:24)
price_feb$Month <- seq(1:24)
price_jan$Month <- seq(1:24)
price_dec$Month <- seq(1:24)
price_nov$Month <- seq(1:24)
price_oct$Month <- seq(1:24)
price_sep$Month <- seq(1:24)

###palivizumab unit price in off season
pal_unitprice_off <- matrix(0, nrow= 5,ncol = 8)
pal_unitprice_off <- cbind(as.matrix(price[1:5,2:5]),as.matrix(price[13:17, 2:5]))

rownames(pal_unitprice_off) <- c("aug","jul","jun","may","apr")
colnames(pal_unitprice_off) <- colnames(price)[c(2:5,2:5)]

###nirsevimab unit price in off season
nir_unitprice_off <- matrix(0, nrow= 5,ncol = 8)
nir_unitprice_off <- cbind(as.matrix(price[1:5,6:9]),as.matrix(price[13:17, 6:9]))

rownames(nir_unitprice_off) <- c("aug","jul","jun","may","apr")
colnames(nir_unitprice_off) <- colnames(price)[c(6:9,6:9)]


######
psa_season=function(pars)
{
  prob_status_psa = list(pars$probmat[, grep("^p_dinf_[0-9]+$",colnames(pars$probmat))],
                         pars$probmat[, grep("^p_hosp_if_dinf_[0-9]+$", colnames(pars$probmat))],
                         pars$probmat[, grep("^p_die_if_hp_[0-9]+$", colnames(pars$probmat))],
                         pars$probmat[, grep("^p_die_if_dinf_[0-9]+$", colnames(pars$probmat))]
                         )
  
  cost_psa = list(
    pars$probmat[, grep("^cost_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^cost_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^cost_death_[0-9]+$", colnames(pars$probmat))])
  
  qaly_psa = list(
    pars$probmat[, grep("^qaly_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^qaly_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^qaly_die_[0-9]+$", colnames(pars$probmat))])
  
  eff_mv_psa = list(
    pars$probmat[, grep("^eff_mv_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_mv_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_mv_icu_[0-9]+$", colnames(pars$probmat))])
  
  eff_lmab_psa = list(
    pars$probmat[, grep("^eff_lmab_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_icu_[0-9]+$", colnames(pars$probmat))])
  
  eff_pal_psa = list(
    pars$probmat[, grep("^eff_pal_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_pal_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_pal_icu_[0-9]+$", colnames(pars$probmat))])
  
  eff_lmab_highrisk_psa = list(
    pars$probmat[, grep("^eff_lmab_highrisk_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_highrisk_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_highrisk_icu_[0-9]+$", colnames(pars$probmat))])
  
  eff_lmab_seasonal_cu_psa = list(
    pars$probmat[, grep("^eff_lmab_seasonal_cu_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_seasonal_cu_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_seasonal_cu_icu_[0-9]+$", colnames(pars$probmat))])
  
  ###create array
  prob_status_arr <- abind(prob_status_psa, along = 3) # dims: [NSIM, NQUART, 4]
  cost_arr <- abind(cost_psa, along = 3)
  qaly_arr <- abind(qaly_psa, along = 3)
  eff_mv_arr <- abind(eff_mv_psa, along = 3)
  eff_lmab_arr <- abind(eff_lmab_psa, along = 3)
  eff_pal_arr <- abind(eff_pal_psa, along = 3)
  eff_lmab_highrisk_arr <- abind(eff_lmab_highrisk_psa, along =3)
  eff_lmab_seasonal_cu_arr <- abind(eff_lmab_seasonal_cu_psa, along =3)
  
  NQUART = length(price$Month)
  
  #Age specific disease status in the population 
  disease_status_arr <- array(0, dim= c(nrow = NSIM, ncol = NQUART, 3))
  for(i in seq_len(NSIM)){
    for (quart in seq_len(NQUART)) {
      disease_status_arr[i, quart,1] = pars$Npop[1,quart] * prob_status_arr[i,quart,1]# case
      disease_status_arr[i, quart,2] = disease_status_arr[i, quart,1] * prob_status_arr[i,quart,2] # hospital
      disease_status_arr[i, quart,3] = disease_status_arr[i, quart,2] * prob_status_arr[i,quart,3] + 
      disease_status_arr[i,quart,1] * prob_status_arr[i,quart,4] # death
    }
  }
  
  #####cost
  costmat_arr = array(0, dim= c(nrow = NSIM, ncol = NQUART, 3))
  for(i in 1:NSIM){  
    for(quart in 1:NQUART){
      costmat_arr[i,quart,1] = cost_arr[i,quart,1]* disease_status_arr[i,quart,1]
      costmat_arr[i,quart,2] = cost_arr[i,quart,2]* disease_status_arr[i,quart,2]
      costmat_arr[i,quart,3] = cost_arr[i,quart,3]* disease_status_arr[i,quart,3]
    }
  }
  ######qaly
  qalymat_arr = array(0, dim= c(nrow = NSIM, ncol = NQUART, 3))
  for(i in 1:NSIM){  
    for(quart in 1:NQUART){
      qalymat_arr[i,quart,1] = qaly_arr[i,quart,1]* disease_status_arr[i,quart,1]
      qalymat_arr[i,quart,2] = qaly_arr[i,quart,2]* disease_status_arr[i,quart,2]
      qalymat_arr[i,quart,3] = qaly_arr[i,quart,3]* disease_status_arr[i,quart,3]
    }
  }
  
  
  ##discounting
  discountfactor = rep(0, NQUART)
  for (quart in 1:NQUART){
    discountfactor[quart] = (1+ pars$discount)^(-floor((quart-1)/4)) 
  }
  
  ###cost*effect*discount
  #'*effect of palivizumab = 10% of population - high risk group will receive it in the first and second year of life*
  costchangearr = array(0, dim= c(nrow = NSIM, ncol = NQUART, 18)) ####'*4 intervention strategies*
  for (i in 1:NSIM){
    for (quart in 1:NQUART){
      costchangearr[i,quart,1] = costmat_arr[i,quart,1] *discountfactor[quart]
      costchangearr[i,quart,2] = costmat_arr[i,quart,2] *discountfactor[quart]
      costchangearr[i,quart,3] = costmat_arr[i,quart,3] *discountfactor[quart]
      costchangearr[i,quart,4] = costmat_arr[i,quart,1] *discountfactor[quart] * eff_pal_arr[i,quart,1] 
      costchangearr[i,quart,5] = costmat_arr[i,quart,2] *discountfactor[quart] * eff_pal_arr[i,quart,2] 
      costchangearr[i,quart,6] = costmat_arr[i,quart,3] *discountfactor[quart] * eff_pal_arr[i,quart,3] 
      costchangearr[i,quart,7] = costmat_arr[i,quart,1] *discountfactor[quart] * eff_mv_arr[i,quart,1]
      costchangearr[i,quart,8] = costmat_arr[i,quart,2] *discountfactor[quart] * eff_mv_arr[i,quart,2]
      costchangearr[i,quart,9] = costmat_arr[i,quart,3] *discountfactor[quart] * eff_mv_arr[i,quart,3]
      costchangearr[i,quart,10] = costmat_arr[i,quart,1] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,1]
      costchangearr[i,quart,11] = costmat_arr[i,quart,2] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,2]
      costchangearr[i,quart,12] = costmat_arr[i,quart,3] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,3]
      costchangearr[i,quart,13] = costmat_arr[i,quart,1] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,1] 
      costchangearr[i,quart,14] = costmat_arr[i,quart,2] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,2] 
      costchangearr[i,quart,15] = costmat_arr[i,quart,3] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,3] 
      costchangearr[i,quart,16] = costmat_arr[i,quart,1] *discountfactor[quart]* eff_lmab_arr[i,quart,1] 
      costchangearr[i,quart,17] = costmat_arr[i,quart,2] *discountfactor[quart]* eff_lmab_arr[i,quart,2] 
      costchangearr[i,quart,18] = costmat_arr[i,quart,3] *discountfactor[quart]* eff_lmab_arr[i,quart,3] 
    }
  }
  
  
  #qaly*effect
  qalychangearr = array(0, dim= c(nrow = NSIM, ncol = NQUART, 18)) ####'*4 intervention strategies*
  for (i in 1:NSIM){
    for (quart in 1:NQUART){
      qalychangearr[i,quart,1] = qalymat_arr[i,quart,1] *discountfactor[quart]
      qalychangearr[i,quart,2] = qalymat_arr[i,quart,2] *discountfactor[quart]
      qalychangearr[i,quart,3] = qalymat_arr[i,quart,3] *discountfactor[quart]
      qalychangearr[i,quart,4] = qalymat_arr[i,quart,1] *discountfactor[quart] * eff_pal_arr[i,quart,1] 
      qalychangearr[i,quart,5] = qalymat_arr[i,quart,2] *discountfactor[quart] * eff_pal_arr[i,quart,2] 
      qalychangearr[i,quart,6] = qalymat_arr[i,quart,3] *discountfactor[quart] * eff_pal_arr[i,quart,3] 
      qalychangearr[i,quart,7] = qalymat_arr[i,quart,1] *discountfactor[quart] * eff_mv_arr[i,quart,1]
      qalychangearr[i,quart,8] = qalymat_arr[i,quart,2] *discountfactor[quart] * eff_mv_arr[i,quart,2]
      qalychangearr[i,quart,9] = qalymat_arr[i,quart,3] *discountfactor[quart] * eff_mv_arr[i,quart,3]
      qalychangearr[i,quart,10] = qalymat_arr[i,quart,1] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,1]
      qalychangearr[i,quart,11] = qalymat_arr[i,quart,2] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,2]
      qalychangearr[i,quart,12] = qalymat_arr[i,quart,3] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,3]
      qalychangearr[i,quart,13] = qalymat_arr[i,quart,1] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,1] 
      qalychangearr[i,quart,14] = qalymat_arr[i,quart,2] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,2] 
      qalychangearr[i,quart,15] = qalymat_arr[i,quart,3] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,3] 
      qalychangearr[i,quart,16] = qalymat_arr[i,quart,1] *discountfactor[quart]* eff_lmab_arr[i,quart,1] 
      qalychangearr[i,quart,17] = qalymat_arr[i,quart,2] *discountfactor[quart]* eff_lmab_arr[i,quart,2] 
      qalychangearr[i,quart,18] = qalymat_arr[i,quart,3] *discountfactor[quart]* eff_lmab_arr[i,quart,3] 
    }
  }
  
  ###intervention cost
  no_interv_cost <- apply(costchangearr[, 1:24, 1:3],  # [NSIM, 24, 3]
                          1,           # keep margin 1 (simulations)
                          sum          # sum over the other two margins
  )
  
  palivizumab_cost <- apply(costchangearr[, 1:24, 4:6], 1, sum)
  mv_cost <- apply(costchangearr[,1:24,7:9], 1, sum)
  nirsevimab_highrisk_cost <- apply(costchangearr[, 1:24, 10:12], 1, sum)
  nirsevimab_seasonal_cost <- apply(costchangearr[, 1:24, 16:18], 1, sum)
  nirsevimab_seasonal_cu_cost <- apply(costchangearr[, 1:24, 13:15], 1, sum)
  nirsevimab_allyear_cost <- apply(costchangearr[, 1:24, 16:18], 1, sum)
  
  
  
  #qaly loss for each intervention
  no_interv_qaly <- apply(qalychangearr[, 1:24, 1:3], 1, sum)
  palivizumab_qaly <- apply(qalychangearr[, 1:24, 4:6], 1, sum)
  mv_qaly <- apply(qalychangearr[, 1:24, 7:9], 1, sum)
  nirsevimab_highrisk_qaly <- apply(qalychangearr[, 1:24, 10:12], 1, sum)
  nirsevimab_seasonal_qaly <- apply(qalychangearr[, 1:24, 16:18], 1, sum)
  nirsevimab_seasonal_cu_qaly <- apply(qalychangearr[, 1:24, 13:15], 1, sum)
  nirsevimab_allyear_qaly <- apply(qalychangearr[, 1:24, 16:18], 1, sum)


  
  #palivizumab price
  palivizumab_price <- rep(0, NQUART)
  for (month in 1:NQUART) {
    if (month <= 12) {
      palivizumab_price[month] <- 
        pars$Npop[1,month] * 0.064 * pars$price[["price_pal_preterm"]][month] + 
        pars$Npop[1,month] * 0.019 * pars$price[["price_pal_risk"]][month]  + 　　　　　           
        pars$Npop[1,month] * 0.0006 * pars$price[["price_pal_down"]][month] +    
        pars$Npop[1,month] * 0.0024 * pars$price[["price_pal_others"]][month]
    } else {
      palivizumab_price[month] <- 
        pars$Npop[1,month] * 0.019 * pars$price[["price_pal_risk"]][month]  + 　　　　　　
        pars$Npop[1,month] * 0.0006 * pars$price[["price_pal_down"]][month] +
        pars$Npop[1,month] * 0.0024 * pars$price[["price_pal_others"]][month]
    }
  }
  palivizumab_highrisk_price <- rep(sum(palivizumab_price), NSIM)
  
  
  #mv price
  mv_price <- rep(pars$mv_price * sum(pars$Npop[1,c(1:12)]), NSIM)
  
  ##Nirsevimub price
  nirsevimab_price <- rep(0, NQUART)
  for (month in 1:NQUART) {
    if(month<=12){
      nirsevimab_price[month] <- pars$Npop[1,month] * 0.064 * pars$price[["price_nir_preterm"]][1] +
        pars$Npop[1,month] * 0.019 * pars$price[["price_nir_risk"]][1] +
        pars$Npop[1,month] * 0.0006 * pars$price[["price_nir_down"]][1] +
        pars$Npop[1,month] * 0.0024 * pars$price[["price_nir_others"]][1]
    } else {
      nirsevimab_price[month] <- pars$Npop[1,month] * 0.019 * pars$nirsevimab_unitprice2   +
        pars$Npop[1,month] * 0.0006 * pars$nirsevimab_unitprice2  +
        pars$Npop[1,month] * 0.0024 * pars$nirsevimab_unitprice2 
    }
  }
  
  nirsevimab_highrisk_price <- sum(nirsevimab_price)
  
  nirsevimab_seasonal_price <- sum(pars$Npop[1,c(1:12)]) * pars$price[["price_nir_risk"]][1] +
    sum(pars$Npop[1,c(13:24)]) * 0.022 * pars$nirsevimab_unitprice2
  
  
  nirsevimab_seasonal_cu_price <- sum(pars$Npop[1,c(1:12)]) * pars$price[["price_nir_risk"]][1] +
    sum(pars$Npop[1,c(13:24)]) * 0.022 * pars$nirsevimab_unitprice2
  
  nirsevimab_allyear_price  <- sum(pars$Npop[1,c(1:12)]) * pars$price[["price_nir_risk"]][1] +
    sum(pars$Npop[1,c(13:24)]) * 0.022 * pars$nirsevimab_unitprice2
  
  ###ICER
  icer = list()
  icer$no_interv_cost <- no_interv_cost
  icer$no_interv_qaly <- no_interv_qaly
  
  icer$palivizumab_cost <- palivizumab_cost
  icer$palivizumab_qaly <- palivizumab_qaly
  icer$palivizumab_highrisk_price <- palivizumab_highrisk_price
  
  icer$mv_cost <- mv_cost
  icer$mv_qaly <- mv_qaly
  icer$mv_price <- mv_price
  icer$nirsevimab_highrisk_cost <- nirsevimab_highrisk_cost
  icer$nirsevimab_highrisk_qaly <- nirsevimab_highrisk_qaly
  icer$nirsevimab_highrisk_price <- nirsevimab_highrisk_price
  icer$nirsevimab_seasonal_cost <- nirsevimab_seasonal_cost
  icer$nirsevimab_seasonal_qaly <- nirsevimab_seasonal_qaly
  icer$nirsevimab_seasonal_price <- nirsevimab_seasonal_price
  icer$nirsevimab_seasonal_cu_cost <- nirsevimab_seasonal_cu_cost
  icer$nirsevimab_seasonal_cu_qaly <- nirsevimab_seasonal_cu_qaly
  icer$nirsevimab_seasonal_cu_price <- nirsevimab_seasonal_cu_price
  icer$nirsevimab_allyear_cost <- nirsevimab_allyear_cost
  icer$nirsevimab_allyear_qaly <- nirsevimab_allyear_qaly
  icer$nirsevimab_allyear_price <- nirsevimab_allyear_price

  
  return(icer)
}

psa_offseason=function(pars)
{
  prob_status_psa = list(pars$probmat[, grep("^p_dinf_[0-9]+$",colnames(pars$probmat))],
                         pars$probmat[, grep("^p_hosp_if_dinf_[0-9]+$", colnames(pars$probmat))],
                         pars$probmat[, grep("^p_die_if_hp_[0-9]+$", colnames(pars$probmat))],
                         pars$probmat[, grep("^p_die_if_dinf_[0-9]+$", colnames(pars$probmat))])
  
  cost_psa = list(
    pars$probmat[, grep("^cost_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^cost_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^cost_death_[0-9]+$", colnames(pars$probmat))])
  
  qaly_psa = list(
    pars$probmat[, grep("^qaly_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^qaly_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^qaly_die_[0-9]+$", colnames(pars$probmat))])
  
  eff_mv_psa = list(
    pars$probmat[, grep("^eff_mv_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_mv_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_mv_icu_[0-9]+$", colnames(pars$probmat))])
  
  eff_lmab_psa = list(
    pars$probmat[, grep("^eff_lmab_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_icu_[0-9]+$", colnames(pars$probmat))])
  
  eff_pal_psa = list(
    pars$probmat[, grep("^eff_pal_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_pal_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_pal_icu_[0-9]+$", colnames(pars$probmat))])
  
  eff_lmab_highrisk_psa = list(
    pars$probmat[, grep("^eff_lmab_highrisk_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_highrisk_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_highrisk_icu_[0-9]+$", colnames(pars$probmat))])
  
  eff_lmab_seasonal_cu_psa = list(
    pars$probmat[, grep("^eff_lmab_seasonal_cu_dinf_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_seasonal_cu_hosp_[0-9]+$", colnames(pars$probmat))],
    pars$probmat[, grep("^eff_lmab_seasonal_cu_icu_[0-9]+$", colnames(pars$probmat))])
  
  ###create array
  prob_status_arr <- abind(prob_status_psa, along = 3) # dims: [NSIM, NQUART, 4]
  cost_arr <- abind(cost_psa, along = 3)
  qaly_arr <- abind(qaly_psa, along = 3)
  eff_mv_arr <- abind(eff_mv_psa, along = 3)
  eff_lmab_arr <- abind(eff_lmab_psa, along = 3)
  eff_pal_arr <- abind(eff_pal_psa, along = 3)
  eff_lmab_highrisk_arr <- abind(eff_lmab_highrisk_psa, along =3)
  eff_lmab_seasonal_cu_arr <- abind(eff_lmab_seasonal_cu_psa, along =3)
  
  NQUART = length(price$Month)
  
  #Age specific disease status in the population 
  disease_status_arr <- array(0, dim= c(nrow = NSIM, ncol = NQUART, 3))
  for(i in seq_len(NSIM)){
    for (quart in seq_len(NQUART)) {
      disease_status_arr[i, quart,1] = pars$Npop[1,quart] * prob_status_arr[i,quart,1]# case
      disease_status_arr[i, quart,2] = disease_status_arr[i, quart,1] * prob_status_arr[i,quart,2] # hospital
      disease_status_arr[i, quart,3] = disease_status_arr[i, quart,2] * prob_status_arr[i,quart,3] + 
      disease_status_arr[i, quart,1] * prob_status_arr[i, quart,4] # death
    }
  }
  
  #####cost
  costmat_arr = array(0, dim= c(nrow = NSIM, ncol = NQUART, 3))
  for(i in 1:NSIM){  
    for(quart in 1:NQUART){
      costmat_arr[i,quart,1] = cost_arr[i,quart,1]* disease_status_arr[i,quart,1]
      costmat_arr[i,quart,2] = cost_arr[i,quart,2]* disease_status_arr[i,quart,2]
      costmat_arr[i,quart,3] = cost_arr[i,quart,3]* disease_status_arr[i,quart,3]
    }
  }
  ######qaly
  qalymat_arr = array(0, dim= c(nrow = NSIM, ncol = NQUART, 3))
  for(i in 1:NSIM){  
    for(quart in 1:NQUART){
      qalymat_arr[i,quart,1] = qaly_arr[i,quart,1]* disease_status_arr[i,quart,1]
      qalymat_arr[i,quart,2] = qaly_arr[i,quart,2]* disease_status_arr[i,quart,2]
      qalymat_arr[i,quart,3] = qaly_arr[i,quart,3]* disease_status_arr[i,quart,3]
    }
  }
  
  
  ##discounting
  discountfactor = rep(0, NQUART)
  for (quart in 1:NQUART){
    discountfactor[quart] = (1+ pars$discount)^(-floor((quart-1)/4)) 
  }
  
  ###cost*effect*discount
  #'*effect of palivizumab = 8.5% of population - high risk group will receive it in the first and second year of life*
  costchangearr = array(0, dim= c(nrow = NSIM, ncol = NQUART, 18)) ####'*4 intervention strategies*
  for (i in 1:NSIM){
    for (quart in 1:NQUART){
      costchangearr[i,quart,1] = costmat_arr[i,quart,1] *discountfactor[quart]
      costchangearr[i,quart,2] = costmat_arr[i,quart,2] *discountfactor[quart]
      costchangearr[i,quart,3] = costmat_arr[i,quart,3] *discountfactor[quart]
      costchangearr[i,quart,4] = costmat_arr[i,quart,1] *discountfactor[quart] * eff_pal_arr[i,quart,1] 
      costchangearr[i,quart,5] = costmat_arr[i,quart,2] *discountfactor[quart] * eff_pal_arr[i,quart,2] 
      costchangearr[i,quart,6] = costmat_arr[i,quart,3] *discountfactor[quart] * eff_pal_arr[i,quart,3] 
      costchangearr[i,quart,7] = costmat_arr[i,quart,1] *discountfactor[quart] * eff_mv_arr[i,quart,1]
      costchangearr[i,quart,8] = costmat_arr[i,quart,2] *discountfactor[quart] * eff_mv_arr[i,quart,2]
      costchangearr[i,quart,9] = costmat_arr[i,quart,3] *discountfactor[quart] * eff_mv_arr[i,quart,3]
      costchangearr[i,quart,10] = costmat_arr[i,quart,1] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,1]
      costchangearr[i,quart,11] = costmat_arr[i,quart,2] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,2]
      costchangearr[i,quart,12] = costmat_arr[i,quart,3] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,3]
      costchangearr[i,quart,13] = costmat_arr[i,quart,1] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,1] 
      costchangearr[i,quart,14] = costmat_arr[i,quart,2] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,2] 
      costchangearr[i,quart,15] = costmat_arr[i,quart,3] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,3] 
      costchangearr[i,quart,16] = costmat_arr[i,quart,1] *discountfactor[quart]* eff_lmab_arr[i,quart,1] 
      costchangearr[i,quart,17] = costmat_arr[i,quart,2] *discountfactor[quart]* eff_lmab_arr[i,quart,2] 
      costchangearr[i,quart,18] = costmat_arr[i,quart,3] *discountfactor[quart]* eff_lmab_arr[i,quart,3] 
    }
  }
  
  #qaly*effect
  qalychangearr = array(0, dim= c(nrow = NSIM, ncol = NQUART, 18)) ####'*4 intervention strategies*
  for (i in 1:NSIM){
    for (quart in 1:NQUART){
      qalychangearr[i,quart,1] = qalymat_arr[i,quart,1] *discountfactor[quart]
      qalychangearr[i,quart,2] = qalymat_arr[i,quart,2] *discountfactor[quart]
      qalychangearr[i,quart,3] = qalymat_arr[i,quart,3] *discountfactor[quart]
      qalychangearr[i,quart,4] = qalymat_arr[i,quart,1] *discountfactor[quart] * eff_pal_arr[i,quart,1] 
      qalychangearr[i,quart,5] = qalymat_arr[i,quart,2] *discountfactor[quart] * eff_pal_arr[i,quart,2] 
      qalychangearr[i,quart,6] = qalymat_arr[i,quart,3] *discountfactor[quart] * eff_pal_arr[i,quart,3] 
      qalychangearr[i,quart,7] = qalymat_arr[i,quart,1] *discountfactor[quart] * eff_mv_arr[i,quart,1]
      qalychangearr[i,quart,8] = qalymat_arr[i,quart,2] *discountfactor[quart] * eff_mv_arr[i,quart,2]
      qalychangearr[i,quart,9] = qalymat_arr[i,quart,3] *discountfactor[quart] * eff_mv_arr[i,quart,3]
      qalychangearr[i,quart,10] = qalymat_arr[i,quart,1] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,1]
      qalychangearr[i,quart,11] = qalymat_arr[i,quart,2] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,2]
      qalychangearr[i,quart,12] = qalymat_arr[i,quart,3] *discountfactor[quart] * eff_lmab_highrisk_arr[i,quart,3]
      qalychangearr[i,quart,13] = qalymat_arr[i,quart,1] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,1] 
      qalychangearr[i,quart,14] = qalymat_arr[i,quart,2] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,2] 
      qalychangearr[i,quart,15] = qalymat_arr[i,quart,3] *discountfactor[quart]* eff_lmab_seasonal_cu_arr[i,quart,3] 
      qalychangearr[i,quart,16] = qalymat_arr[i,quart,1] *discountfactor[quart]* eff_lmab_arr[i,quart,1] 
      qalychangearr[i,quart,17] = qalymat_arr[i,quart,2] *discountfactor[quart]* eff_lmab_arr[i,quart,2] 
      qalychangearr[i,quart,18] = qalymat_arr[i,quart,3] *discountfactor[quart]* eff_lmab_arr[i,quart,3] 
    }
  }
  
  ###intervention cost
  no_interv_cost <- apply(costchangearr[, 1:24, 1:3],  # [NSIM, 24, 3]
                          1,           # keep margin 1 (simulations)
                          sum          # sum over the other two margins
  )
  
  palivizumab_cost <- apply(costchangearr[, 1:24, 4:6], 1, sum)
  mv_cost <- apply(costchangearr[,1:24,7:9], 1, sum)
  nirsevimab_highrisk_cost <- apply(costchangearr[, 1:24, 10:12], 1, sum)
  nirsevimab_seasonal_cost <- apply(costchangearr[, 1:24, 1:3], 1, sum)
  nirsevimab_seasonal_cu_cost <- apply(costchangearr[, 1:24, 13:15], 1, sum)
  nirsevimab_allyear_cost <- apply(costchangearr[, 1:24, 16:18], 1, sum)
  

  #qaly loss for each intervention
  no_interv_qaly <- apply(qalychangearr[, 1:24, 1:3], 1, sum)
  palivizumab_qaly <- apply(qalychangearr[, 1:24, 4:6], 1, sum)
  mv_qaly <- apply(qalychangearr[, 1:24, 7:9], 1, sum)
  nirsevimab_highrisk_qaly <- apply(qalychangearr[, 1:24, 10:12], 1, sum)
  nirsevimab_seasonal_qaly <- apply(qalychangearr[, 1:24, 1:3], 1, sum)
  nirsevimab_seasonal_cu_qaly <- apply(qalychangearr[, 1:24, 13:15], 1, sum)
  nirsevimab_allyear_qaly <- apply(qalychangearr[, 1:24, 16:18], 1, sum)

  
  ## price of the vaccine/lmAB
  palivizumab_price <- rep(0, NQUART)
  for (month in 1:NQUART) {
    if (month <= 12) {
      palivizumab_price[month] <- 
        pars$Npop[1,month] * 0.064 * pars$pal_unitprice_off[[1]]  +  ####一人平均6.5回Synagis接種, 1月平均 6.5回/12M
        pars$Npop[1,month] * 0.019 * pars$pal_unitprice_off[[2]]  + 　　　　　           　####一人平均5.9回 chd etc.
        pars$Npop[1,month] * 0.0006 * pars$pal_unitprice_off[[3]] +
        pars$Npop[1,month] * 0.0024 * pars$pal_unitprice_off[[4]]
    } else {
      palivizumab_price[month] <- 
        pars$Npop[1,month] * 0.019 * pars$pal_unitprice_off[[6]]  + 　　　　　　
        pars$Npop[1,month] * 0.0006 * pars$pal_unitprice_off[[7]] +
        pars$Npop[1,month] * 0.0024 * pars$pal_unitprice_off[[8]]
    }
  }
  
  palivizumab_highrisk_price <- rep(sum(palivizumab_price), NSIM)
  
  
  ##maternal vaccine price
  mv_price = rep(pars$mv_price * sum(pars$Npop[1,c(1:12)]), NSIM)
  
  ##Nirsevimub price
  nirsevimab_price <- rep(0, NQUART)
  for (month in 1:NQUART){
    if(month<=12){
      nirsevimab_price[month] <- pars$Npop[1,month] * 0.064 * pars$nir_unitprice_off[[1]] +  #preterm
        pars$Npop[1,month] * 0.019 * pars$nir_unitprice_off[[2]] + 　                        #down
        pars$Npop[1,month] * 0.0006 * pars$nir_unitprice_off[[3]] +                          #risk: chd etc
        pars$Npop[1,month] * 0.0024 * pars$nir_unitprice_off[[4]]                           #neuro, meta etc
    } else {
      nirsevimab_price[month] <- pars$Npop[1,month] * 0.019 * pars$nirsevimab_unitprice2 + #down
        pars$Npop[1,month] * 0.0006 * pars$nirsevimab_unitprice2 +                          #risk 
        pars$Npop[1,month] * 0.0024 * pars$nirsevimab_unitprice2             ##at 12month once
    }
  }
  
  nirsevimab_highrisk_price <- rep(sum(nirsevimab_price), NSIM)
  nirsevimab_seasonal_price <- rep(sum(nirsevimab_price),NSIM) ##at 0 month once
  
  nirsevimab_seasonal_cu <- rep(0, NQUART)
  for (month in 1:NQUART) {
    if(month <=12){
      nirsevimab_seasonal_cu[month] <- pars$Npop[1,month] * pars$nir_unitprice_off[3]  #1st season 1回接種 assumed same BW for highrisk group
    } else {
      nirsevimab_seasonal_cu[month] <- pars$Npop[1,month] * 0.019 * pars$nirsevimab_unitprice2 +
        pars$Npop[1,month] * 0.0006 *pars$nirsevimab_unitprice2 +
        pars$Npop[1,month] * 0.00024 * pars$nirsevimab_unitprice2
    }
  }
  
  nirsevimab_seasonal_cu_price <- rep(sum(nirsevimab_seasonal_cu), NSIM)
  
  nirsevimab_allyear_price  <- rep(sum(pars$Npop[1,c(1:12)]) * pars$nir_unitprice_off[[3]] +
    sum(pars$Npop[1,c(13:24)]) * 0.022 * pars$nirsevimab_unitprice2, NSIM)
  
  
  ###ICER
  icer = list()
  icer$no_interv_cost <- no_interv_cost
  icer$no_interv_qaly <- no_interv_qaly
  
  icer$palivizumab_cost <- palivizumab_cost
  icer$palivizumab_qaly <- palivizumab_qaly
  icer$palivizumab_highrisk_price <- palivizumab_highrisk_price
  
  icer$mv_cost <- mv_cost
  icer$mv_qaly <- mv_qaly
  icer$mv_price <- mv_price
  icer$nirsevimab_highrisk_cost <- nirsevimab_highrisk_cost
  icer$nirsevimab_highrisk_qaly <- nirsevimab_highrisk_qaly
  icer$nirsevimab_highrisk_price <- nirsevimab_highrisk_price
  icer$nirsevimab_seasonal_cost <- nirsevimab_seasonal_cost
  icer$nirsevimab_seasonal_qaly <- nirsevimab_seasonal_qaly
  icer$nirsevimab_seasonal_price <- nirsevimab_seasonal_price
  icer$nirsevimab_seasonal_cu_cost <- nirsevimab_seasonal_cu_cost
  icer$nirsevimab_seasonal_cu_qaly <- nirsevimab_seasonal_cu_qaly
  icer$nirsevimab_seasonal_cu_price <- nirsevimab_seasonal_cu_price
  icer$nirsevimab_allyear_cost <- nirsevimab_allyear_cost
  icer$nirsevimab_allyear_qaly <- nirsevimab_allyear_qaly
  icer$nirsevimab_allyear_price <- nirsevimab_allyear_price
  
  return(icer)
}

pars_jan = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_jan,
                price = price_jan,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,        
                discount = 0.02  
)

pars_feb = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_feb,
                price = price_feb,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,          
                discount = 0.02  
)

pars_mar = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_mar,
                price = price_mar,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,        
                discount = 0.02  
)

pars_apr = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_apr,
                price = price_apr,
                nirsevimab_unitprice = lamab_price + admi_cost,  
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,
                nir_unitprice_off = nir_unitprice_off["apr",],
                pal_unitprice_off = pal_unitprice_off["apr",],
                discount = 0.02  
)

pars_may = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_may,
                price = price_may,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,          
                nir_unitprice_off = nir_unitprice_off["may",],
                pal_unitprice_off = pal_unitprice_off["may",],
                discount = 0.02  
)

pars_jun = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_jun,
                price = price_jun,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,       
                nir_unitprice_off = nir_unitprice_off["jun",],
                pal_unitprice_off = pal_unitprice_off["jun",],
                discount = 0.02  
)

pars_jul = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_jul,
                price = price_jul,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,         
                nir_unitprice_off = nir_unitprice_off["jul",],
                pal_unitprice_off = pal_unitprice_off["jul",],
                discount = 0.02  
)

pars_aug = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_aug,
                price = price_aug,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,          
                nir_unitprice_off = nir_unitprice_off["aug",],
                pal_unitprice_off = pal_unitprice_off["aug",],
                discount = 0.02  
)

pars_sep = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_sep,
                price = price_sep,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,          
                discount = 0.02  
)

pars_oct = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_oct,
                price = price_oct,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,         
                discount = 0.02  
)

pars_nov = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_nov,
                price = price_nov,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,         
                discount = 0.02  
)

pars_dec = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                probmat = probmat_dec,
                price = price_dec,
                nirsevimab_unitprice = lamab_price + admi_cost, 
                nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                mv_price = abrysbo + admi_cost,       
                discount = 0.02 
)


df <- 
as.data.frame(psa_season(pars_jan)) +
as.data.frame(psa_season(pars_feb)) +
as.data.frame(psa_season(pars_mar)) +
as.data.frame(psa_offseason(pars_apr)) +
as.data.frame(psa_offseason(pars_may)) +
as.data.frame(psa_offseason(pars_jun)) +
as.data.frame(psa_offseason(pars_jul)) +
as.data.frame(psa_offseason(pars_aug)) +
as.data.frame(psa_season(pars_sep)) +
as.data.frame(psa_season(pars_oct)) +
as.data.frame(psa_season(pars_nov)) +
as.data.frame(psa_season(pars_dec))


# ICER calculation
#create dataframe with the cost, qaly and total price

df <- df %>% mutate(
  pal_no_cost = (palivizumab_cost + palivizumab_highrisk_price) - no_interv_cost,
  pal_no_qaly = no_interv_qaly - palivizumab_qaly,
  
  mv_no_cost = (mv_cost + mv_price) - no_interv_cost,
  mv_no_qaly = no_interv_qaly - mv_qaly,
  
  lmabhr_no_cost = (nirsevimab_highrisk_cost + nirsevimab_highrisk_price) - no_interv_cost,
  lmabhr_no_qaly = no_interv_qaly - nirsevimab_highrisk_qaly,
  
  lmabs_no_cost = (nirsevimab_seasonal_cost + nirsevimab_seasonal_price) - no_interv_cost,
  lmabs_no_qaly = no_interv_qaly - nirsevimab_seasonal_qaly,
  
  lmabs_cu_no_cost = (nirsevimab_seasonal_cu_cost + nirsevimab_seasonal_cu_price) - no_interv_cost,
  lmabs_cu_no_qaly = no_interv_qaly - nirsevimab_seasonal_cu_qaly,
  
  lmaba_no_cost = (nirsevimab_allyear_cost + nirsevimab_allyear_price) - no_interv_cost,
  lmaba_no_qaly = no_interv_qaly - nirsevimab_allyear_qaly)

saveRDS(df,"input/df.rds")


psa <- function(lamab_price, abrysbo){
  price <- read.csv('input/price_pal_mhlw.csv')  ##price_pal_20250724
  price <- price %>% mutate(price_pal_preterm = as.numeric(price_pal_preterm),
                            price_pal_down = as.numeric(price_pal_down),
                            price_pal_risk = as.numeric(price_pal_risk),
                            price_nir_preterm = as.numeric(price_nir_preterm),
                            price_nir_down = as.numeric(price_nir_down),
                            price_nir_risk = as.numeric(price_nir_risk))
  
  
  ##price
  price[c(1:12),] <- price[c(1:12),] * lamab_price/22500 + admi_cost
  price[c(13:24),] <- price[c(13:24),] + admi_cost
  price$Month <- seq(1:24)
  
  price_offseason <- array(0, dim= c(nrow = 24, ncol = 9, 12))
  for(n in 1:12){
    for(k in 1:9){
      for(month in 1:24){
        if (n<=4) {
          if(month == n | month == n+1 |month==n+2|month ==n+3| month ==n+4 |
             month == n +12| month ==n + 13| month == n + 14 | month == n + 15 | month == n + 16)
          { price_offseason[month,k,n] <- 0
          } else {
            price_offseason[month,k,n] <- price[month,k]
          }}
        else if (n<=8) {
          if(month<=9-n| month>=17-n & month<=17-n+4 |month >= 29-n)
          { price_offseason[month,k,n] <- 0
          } else {
            price_offseason[month,k,n] <- price[month,k]
          }}
        else if(n<=12){
          if(month <= 16-n | month >=22-n & month <=28-n | month>=34-n)
          { price_offseason[month,k,n] <- price[month,k]
          } else {
            price_offseason[month,k,n] <- 0
          }}
      }
    }
  }
  
  price_apr <- as.data.frame(price_offseason[,,1])
  price_mar <- as.data.frame(price_offseason[,,2])
  price_feb <- as.data.frame(price_offseason[,,3])
  price_jan <- as.data.frame(price_offseason[,,4])
  price_dec <- as.data.frame(price_offseason[,,12])
  price_nov <- as.data.frame(price_offseason[,,11])
  price_oct <- as.data.frame(price_offseason[,,10])
  price_sep <- as.data.frame(price_offseason[,,9])
  price_aug <- as.data.frame(price_offseason[,,8])
  price_jul <- as.data.frame(price_offseason[,,7])
  price_jun <- as.data.frame(price_offseason[,,6])
  price_may <- as.data.frame(price_offseason[,,5])
  
  colnames(price_aug) <- colnames(price)
  colnames(price_jul) <- colnames(price)
  colnames(price_jun) <- colnames(price)
  colnames(price_may) <- colnames(price)
  colnames(price_apr) <- colnames(price)
  colnames(price_mar) <- colnames(price)
  colnames(price_feb) <- colnames(price)
  colnames(price_jan) <- colnames(price)
  colnames(price_dec) <- colnames(price)
  colnames(price_nov) <- colnames(price)
  colnames(price_oct) <- colnames(price)
  colnames(price_sep) <- colnames(price)
  
  price_aug$Month <- seq(1:24)
  price_jul$Month <- seq(1:24)
  price_jun$Month <- seq(1:24)
  price_may$Month <- seq(1:24)
  price_apr$Month <- seq(1:24)
  price_mar$Month <- seq(1:24)
  price_feb$Month <- seq(1:24)
  price_jan$Month <- seq(1:24)
  price_dec$Month <- seq(1:24)
  price_nov$Month <- seq(1:24)
  price_oct$Month <- seq(1:24)
  price_sep$Month <- seq(1:24)
  
  ###palivizumab unit price in off season
  pal_unitprice_off <- matrix(0, nrow= 5,ncol = 8)
  pal_unitprice_off <- cbind(as.matrix(price[1:5,2:5]),as.matrix(price[13:17, 2:5]))
  
  rownames(pal_unitprice_off) <- c("aug","jul","jun","may","apr")
  colnames(pal_unitprice_off) <- colnames(price)[c(2:5,2:5)]
  
  ###nirsevimab unit price in off season
  nir_unitprice_off <- matrix(0, nrow= 5,ncol = 8)
  nir_unitprice_off <- cbind(as.matrix(price[1:5,6:9]),as.matrix(price[13:17, 6:9]))
  
  rownames(nir_unitprice_off) <- c("aug","jul","jun","may","apr")
  colnames(nir_unitprice_off) <- colnames(price)[c(6:9,6:9)]
 

  pars_jan = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_jan,
                  price = price_jan,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,        
                  discount = 0.02  
  )
  
  pars_feb = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_feb,
                  price = price_feb,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,          
                  discount = 0.02  
  )
  
  pars_mar = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_mar,
                  price = price_mar,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,        
                  discount = 0.02  
  )
  
  pars_apr = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_apr,
                  price = price_apr,
                  nirsevimab_unitprice = lamab_price + admi_cost,  
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,
                  nir_unitprice_off = nir_unitprice_off["apr",],
                  pal_unitprice_off = pal_unitprice_off["apr",],
                  discount = 0.02  
  )
  
  pars_may = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_may,
                  price = price_may,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,          
                  nir_unitprice_off = nir_unitprice_off["may",],
                  pal_unitprice_off = pal_unitprice_off["may",],
                  discount = 0.02  
  )
  
  pars_jun = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_jun,
                  price = price_jun,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,       
                  nir_unitprice_off = nir_unitprice_off["jun",],
                  pal_unitprice_off = pal_unitprice_off["jun",],
                  discount = 0.02  
  )
  
  pars_jul = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_jul,
                  price = price_jul,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,         
                  nir_unitprice_off = nir_unitprice_off["jul",],
                  pal_unitprice_off = pal_unitprice_off["jul",],
                  discount = 0.02  
  )
  
  pars_aug = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_aug,
                  price = price_aug,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,          
                  nir_unitprice_off = nir_unitprice_off["aug",],
                  pal_unitprice_off = pal_unitprice_off["aug",],
                  discount = 0.02  
  )
  
  pars_sep = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_sep,
                  price = price_sep,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,          
                  discount = 0.02  
  )
  
  pars_oct = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_oct,
                  price = price_oct,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,         
                  discount = 0.02  
  )
  
  pars_nov = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_nov,
                  price = price_nov,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,         
                  discount = 0.02  
  )
  
  pars_dec = list(Npop = Npop,   ### 0 and 1 year old population average 2014-2023 
                  probmat = probmat_dec,
                  price = price_dec,
                  nirsevimab_unitprice = lamab_price + admi_cost, 
                  nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
                  mv_price = abrysbo + admi_cost,       
                  discount = 0.02 
  )

df <- 
  as.data.frame(psa_season(pars_jan)) +
  as.data.frame(psa_season(pars_feb)) +
  as.data.frame(psa_season(pars_mar)) +
  as.data.frame(psa_offseason(pars_apr)) +
  as.data.frame(psa_offseason(pars_may)) +
  as.data.frame(psa_offseason(pars_jun)) +
  as.data.frame(psa_offseason(pars_jul)) +
  as.data.frame(psa_offseason(pars_aug)) +
  as.data.frame(psa_season(pars_sep)) +
  as.data.frame(psa_season(pars_oct)) +
  as.data.frame(psa_season(pars_nov)) +
  as.data.frame(psa_season(pars_dec))


df <- df %>% mutate(
  pal_no_cost = (palivizumab_cost + palivizumab_highrisk_price) - no_interv_cost,
  pal_no_qaly = no_interv_qaly - palivizumab_qaly,
  
  mv_no_cost = (mv_cost + mv_price) - no_interv_cost,
  mv_no_qaly = no_interv_qaly - mv_qaly,
  
  lmabhr_no_cost = (nirsevimab_highrisk_cost + nirsevimab_highrisk_price) - no_interv_cost,
  lmabhr_no_qaly = no_interv_qaly - nirsevimab_highrisk_qaly,
  
  lmabs_no_cost = (nirsevimab_seasonal_cost + nirsevimab_seasonal_price) - no_interv_cost,
  lmabs_no_qaly = no_interv_qaly - nirsevimab_seasonal_qaly,
  
  lmabs_cu_no_cost = (nirsevimab_seasonal_cu_cost + nirsevimab_seasonal_cu_price) - no_interv_cost,
  lmabs_cu_no_qaly = no_interv_qaly - nirsevimab_seasonal_cu_qaly,
  
  lmaba_no_cost = (nirsevimab_allyear_cost + nirsevimab_allyear_price) - no_interv_cost,
  lmaba_no_qaly = no_interv_qaly - nirsevimab_allyear_qaly)
 
 name = paste0("df",lamab_price/1000)
 assign(name, df, envir = .GlobalEnv)
} 

psa(1000, 1000)
psa(2000, 2000)
psa(5000, 5000)
psa(8000, 8000)
psa(9000, 9000)
psa(10000, 10000)
psa(11000, 11000)
psa(12000, 12000)
psa(13000, 13000)
psa(14000, 14000)
psa(15000, 15000)
psa(16000, 16000)
psa(17000, 17000)
psa(18000, 18000)
psa(19000, 19000)
psa(20000, 20000)
psa(25000, 25000)

saveRDS(df1,"output/df1.rds")
saveRDS(df2,"output/df2.rds")
saveRDS(df5,"output/df5.rds")
saveRDS(df8,"output/df8.rds")
saveRDS(df9,"output/df9.rds")
saveRDS(df10,"output/df10.rds")
saveRDS(df11,"output/df11.rds")
saveRDS(df12,"output/df12.rds")
saveRDS(df13,"output/df13.rds")
saveRDS(df14,"output/df14.rds")
saveRDS(df15,"output/df15.rds")
saveRDS(df16,"output/df16.rds")
saveRDS(df17,"output/df17.rds")
saveRDS(df18,"output/df18.rds")
saveRDS(df19,"output/df19.rds")
saveRDS(df20,"output/df20.rds")
saveRDS(df25,"output/df25.rds")
