rm(list = ls())
library(dplyr)
library(ggplot2)

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


cost_log <- function(df){
  df %>% mutate(deterministic.value = case_when(pars == "cost_dinf" & Month<=12 ~ 7.860086,
                                                pars == "cost_dinf" & Month >12 ~ 7.743724,
                                                pars =="cost_hosp" & Month<=12 ~ 11,
                                                pars =="cost_hosp" & Month >12 ~ 10.6,
                                                pars=="cost_death" & Month<=12 ~ 10.9,
                                                pars=="cost_death" & Month >12 ~ 9.92,
                                                TRUE ~ deterministic.value))}
hi_lo <- function(df){
  df %>% mutate(hi = ifelse(df$distribution =="lognormal", exp(df$deterministic.value +1.96*df$sd),
                            df$deterministic.value +1.96*df$sd),
                mid = ifelse(df$distribution =="lognormal", exp(df$deterministic.value), df$deterministic.value),
                lo = ifelse(df$distribution =="lognormal", exp(df$deterministic.value -1.96*df$sd), df$deterministic.value - 1.96*df$sd))
}

parameter_jan <- cost_log(parameter_jan)
parameter_jan <- hi_lo(parameter_jan)
parameter_feb <- cost_log(parameter_feb)
parameter_feb <- hi_lo(parameter_feb)
parameter_mar <- cost_log(parameter_mar)
parameter_mar <- hi_lo(parameter_mar)
parameter_apr <- cost_log(parameter_apr)
parameter_apr <- hi_lo(parameter_apr)

parameter_may <- cost_log(parameter_may)
parameter_may <- hi_lo(parameter_may)
parameter_jun <- cost_log(parameter_jun)
parameter_jun <- hi_lo(parameter_jun)
parameter_jul <- cost_log(parameter_jul)
parameter_jul <- hi_lo(parameter_jul)
parameter_aug <- cost_log(parameter_aug)
parameter_aug <- hi_lo(parameter_aug)

parameter_sep <- cost_log(parameter_sep)
parameter_sep <- hi_lo(parameter_sep)
parameter_oct <- cost_log(parameter_oct)
parameter_oct <- hi_lo(parameter_oct)
parameter_nov <- cost_log(parameter_nov)
parameter_nov <- hi_lo(parameter_nov)
parameter_dec <- cost_log(parameter_dec)
parameter_dec <- hi_lo(parameter_dec)


crate_rate <- function(df){
  
  mid <- reshape(idvar = "Month", timevar = "pars",
                 direction = "wide",
                 data = df %>% select(Month, pars, mid))
  
  hi <- reshape(idvar = "Month", timevar = "pars",
                direction = "wide",
                data = df %>% select(Month, pars, hi))
  
  lo <- reshape(idvar = "Month", timevar = "pars",
                direction = "wide",
                data = df %>% select(Month, pars, lo))
  
  new_names <- c("Month","p_dinf","p_hosp_if_dinf","p_die_if_hp","p_die_if_dinf",
                 "cost_dinf","cost_hosp","cost_death",
                 "qaly_dinf","qaly_hosp","qaly_die",
                 "eff_mv_dinf","eff_mv_hosp","eff_mv_icu","eff_lmab_dinf",    
                 "eff_lmab_hosp","eff_lmab_icu","mv_pal_dinf","mv_pal_hosp","mv_pal_icu","mv_nir_dinf",            
                 "mv_nir_hosp","mv_nir_icu","eff_pal_dinf","eff_pal_hosp","eff_pal_icu", 
                 "eff_lmab_highrisk_dinf","eff_lmab_highrisk_hosp","eff_lmab_highrisk_icu",
                 "eff_lmab_seasonal_cu_dinf","eff_lmab_seasonal_cu_hosp","eff_lmab_seasonal_cu_icu")
  colnames(mid) <- new_names
  colnames(hi) <- new_names
  colnames(lo) <- new_names
  
  df_name <- deparse(substitute(df)) # get the name of the input object as a string
  suffix <- substr(df_name, nchar(df_name)-2, nchar(df_name)) # extract last 3 characters from the *name string*
  
  assign(paste0("rate_", suffix), mid, envir = .GlobalEnv)
  assign(paste0("rate_", suffix, "_hi"), hi, envir = .GlobalEnv)
  assign(paste0("rate_", suffix, "_lo"), lo, envir = .GlobalEnv)
}


crate_rate(parameter_jan)
crate_rate(parameter_feb)
crate_rate(parameter_mar)
crate_rate(parameter_apr)
crate_rate(parameter_may)
crate_rate(parameter_jun)
crate_rate(parameter_jul)
crate_rate(parameter_aug)
crate_rate(parameter_sep)
crate_rate(parameter_oct)
crate_rate(parameter_nov)
crate_rate(parameter_dec)



effects_season <-read.csv('input/effect_season.csv')
effects_apr <- read.csv('input/effect_apr.csv')
effects_may <- read.csv('input/effect_may.csv')
effects_jun <- read.csv('input/effect_jun.csv')
effects_jul <- read.csv('input/effect_jul.csv')
effects_aug <- read.csv('input/effect_aug.csv')

effects_season_hi <-read.csv('input/effect_season_hi.csv')
effects_apr_hi <- read.csv('input/effect_apr_hi.csv')
effects_may_hi <- read.csv('input/effect_may_hi.csv')
effects_jun_hi <- read.csv('input/effect_jun_hi.csv')
effects_jul_hi <- read.csv('input/effect_jul_hi.csv')
effects_aug_hi <- read.csv('input/effect_aug_hi.csv')

effects_season_lo <-read.csv('input/effect_season_lo.csv')
effects_apr_lo <- read.csv('input/effect_apr_lo.csv')
effects_may_lo <- read.csv('input/effect_may_lo.csv')
effects_jun_lo <- read.csv('input/effect_jun_lo.csv')
effects_jul_lo <- read.csv('input/effect_jul_lo.csv')
effects_aug_lo <- read.csv('input/effect_aug_lo.csv')


Npop = read.csv("input/npop.csv")
Npop <- Npop[,c('pop2023')]
Npop <- t(as.matrix(Npop))
##monthly cohort population/12
Npop <- Npop/12

NMonth = length(rate_jan$Month)
status = c('C','H','D')  # not including 'N' status

##deterministic cost
cost_det <- rate_jan[,c('cost_dinf','cost_hosp','cost_death')]
cost_det <- t(as.matrix(cost_det))

##deterministic qaly
qaly_det <- rate_jan[,c('qaly_dinf','qaly_hosp','qaly_die')]
qaly_det <- t(as.matrix(qaly_det))

####monthly disease burden 
prob_status_det_jan <- rate_jan[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jan <- t(as.matrix(prob_status_det_jan))

prob_status_det_feb <- rate_feb[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_feb <- t(as.matrix(prob_status_det_feb))

prob_status_det_mar <- rate_mar[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_mar <- t(as.matrix(prob_status_det_mar))

prob_status_det_apr <- rate_apr[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_apr <- t(as.matrix(prob_status_det_apr))

prob_status_det_may <- rate_may[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_may <- t(as.matrix(prob_status_det_may))

prob_status_det_jun <- rate_jun[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jun <- t(as.matrix(prob_status_det_jun))

prob_status_det_jul <- rate_jul[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jul <- t(as.matrix(prob_status_det_jul))

prob_status_det_aug <- rate_aug[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_aug <- t(as.matrix(prob_status_det_aug))

prob_status_det_sep <- rate_sep[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_sep <- t(as.matrix(prob_status_det_sep))

prob_status_det_oct <- rate_oct[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_oct <- t(as.matrix(prob_status_det_oct))

prob_status_det_nov <- rate_nov[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_nov <- t(as.matrix(prob_status_det_nov))

prob_status_det_dec <- rate_dec[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_dec <- t(as.matrix(prob_status_det_dec))


##price for palivizumab and nirsevimab by birth month
admi_cost =3575
lamab_price = 459147
abrysbo = 23948

price <- read.csv('input/price_pal_mhlw.csv')  #price_pal_20250724
price <- price %>% mutate(price_pal_preterm = as.numeric(price_pal_preterm),
                          price_pal_down = as.numeric(price_pal_down),
                          price_pal_risk = as.numeric(price_pal_risk),
                          price_pal_others = as.numeric(price_pal_others),
                          price_nir_preterm = as.numeric(price_nir_preterm),
                          price_nir_down = as.numeric(price_nir_down),
                          price_nir_risk = as.numeric(price_nir_risk),
                          price_nir_others = as.numeric(price_nir_others))

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


icer_season=function(parameters)
{
  ##deterministic probability of disease development
  NMonth = length(rate_jan$Month)
  status = c('C','H','D')  # not including 'N' status
  
  #Age specific disease status in the population 
  disease_status <- matrix(0, length(status), NMonth)
  for (month in 1:NMonth) {
    # infection
    disease_status[1, month] = parameters$NPOP[1,month] * parameters$prob_status[1,month]  # case
    disease_status[2, month] = disease_status[1,month] * parameters$prob_status[2,month] # hospital
    disease_status[3, month] = disease_status[2,month] * parameters$prob_status[3,month] + disease_status[1,month] * parameters$prob_status[4,month] # dead
  }
  
  #####cost
  costmat = matrix(0,length(status),NMonth)
  for(month in 1:NMonth){
    costmat[1,month] = parameters$cost[1,month] * disease_status[1,month]
    costmat[2,month] = parameters$cost[2,month] * disease_status[2,month]
    costmat[3,month] = parameters$cost[3,month] * disease_status[3,month]
  }
  
  ######qaly
  qalymat = matrix(0, length(status), NMonth)
  for(month in 1:NMonth){
    qalymat[1,month] = parameters$qaly[1,month] * disease_status[1,month]
    qalymat[2,month] = parameters$qaly[2,month] * disease_status[2,month]
    qalymat[3,month] = parameters$qaly[3,month] * disease_status[3,month]
  }
  
  ##discounting
  discountfactor = rep(0, NMonth)
  
  for (month in 1:NMonth){
    discountfactor[month] = (1+ parameters$discount)^(-floor((month-1)/12)) 
  }
  
  
  ###cost*effect*discount
  ##'*effect of palivizumab = 10% of population - high risk group will receive it in the first and second year of life*
  costchangemat = matrix(0, 18, NMonth)
  costchangemat[1,] = costmat[1,] *discountfactor
  costchangemat[2,] = costmat[2,] *discountfactor
  costchangemat[3,] = costmat[3,] *discountfactor
  costchangemat[4,] = costmat[1,] *discountfactor * parameters$effects[["eff_pal_dinf"]]
  costchangemat[5,] = costmat[2,] *discountfactor * parameters$effects[["eff_pal_hosp"]]
  costchangemat[6,] = costmat[3,] *discountfactor * parameters$effects[["eff_pal_icu"]]
  costchangemat[7,] = costmat[1,] *discountfactor * parameters$effects[["eff_mv_dinf"]]
  costchangemat[8,] = costmat[2,] *discountfactor * parameters$effects[["eff_mv_hosp"]]
  costchangemat[9,] = costmat[3,] *discountfactor * parameters$effects[["eff_mv_icu"]]
  costchangemat[10,] = costmat[1,] *discountfactor * parameters$effects[["eff_lmab_highrisk_dinf"]] 
  costchangemat[11,] = costmat[2,] *discountfactor * parameters$effects[["eff_lmab_highrisk_hosp"]]
  costchangemat[12,] = costmat[3,] *discountfactor * parameters$effects[["eff_lmab_highrisk_icu"]] 
  costchangemat[13,] = costmat[1,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_dinf"]] 
  costchangemat[14,] = costmat[2,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_hosp"]]
  costchangemat[15,] = costmat[3,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_icu"]] 
  costchangemat[16,] = costmat[1,] *discountfactor * parameters$effects[["eff_lmab_dinf"]] 
  costchangemat[17,] = costmat[2,] *discountfactor * parameters$effects[["eff_lmab_hosp"]] 
  costchangemat[18,] = costmat[3,] *discountfactor * parameters$effects[["eff_lmab_icu"]] 
  
  ##intervention cost
  no_interv_cost = sum(costchangemat[c(1:3),])
  palivizumab_cost = sum(costchangemat[c(4:6),])
  mv_cost = sum(costchangemat[c(7:9),]) 
  nirsevimab_highrisk_cost = sum(costchangemat[c(10:12),]) 
  nirsevimab_seasonal_cost = sum(costchangemat[c(16:18),]) 
  nirsevimab_seasonal_cu_cost = sum(costchangemat[c(13:15),]) 
  nirsevimab_allyear_cost = sum(costchangemat[c(16:18),]) 
  
  
  #qaly*effect*discount
  qalychangemat = matrix(0,18,NMonth)
  qalychangemat[1,] = qalymat[1,] * discountfactor
  qalychangemat[2,] = qalymat[2,] * discountfactor
  qalychangemat[3,] = qalymat[3,] * discountfactor
  qalychangemat[4,] = qalymat[1,] * discountfactor * parameters$effects[["eff_pal_dinf"]]
  qalychangemat[5,] = qalymat[2,] * discountfactor * parameters$effects[["eff_pal_hosp"]] 
  qalychangemat[6,] = qalymat[3,] * discountfactor * parameters$effects[["eff_pal_icu"]]  
  qalychangemat[7,] = qalymat[1,] * discountfactor * parameters$effects[["eff_mv_dinf"]]  
  qalychangemat[8,] = qalymat[2,] * discountfactor * parameters$effects[["eff_mv_hosp"]] 
  qalychangemat[9,] = qalymat[3,] * discountfactor * parameters$effects[["eff_mv_icu"]]
  qalychangemat[10,] = qalymat[1,] *discountfactor * parameters$effects[["eff_lmab_highrisk_dinf"]]
  qalychangemat[11,] = qalymat[2,] *discountfactor * parameters$effects[["eff_lmab_highrisk_hosp"]] 
  qalychangemat[12,] = qalymat[3,] *discountfactor * parameters$effects[["eff_lmab_highrisk_icu"]] 
  qalychangemat[13,] = qalymat[1,] * discountfactor * parameters$effects[["eff_lmab_seasonal_cu_dinf"]]   
  qalychangemat[14,] = qalymat[2,] * discountfactor * parameters$effects[["eff_lmab_seasonal_cu_hosp"]] 
  qalychangemat[15,] = qalymat[3,] * discountfactor * parameters$effects[["eff_lmab_seasonal_cu_icu"]] 
  qalychangemat[16,] = qalymat[1,] * discountfactor * parameters$effects[["eff_lmab_dinf"]]  
  qalychangemat[17,] = qalymat[2,] * discountfactor * parameters$effects[["eff_lmab_hosp"]] 
  qalychangemat[18,] = qalymat[3,] * discountfactor * parameters$effects[["eff_lmab_icu"]]  
  
  
  #qaly loss for each intervention
  no_interv_qaly = sum(qalychangemat[c(1:3),]) 
  palivizumab_qaly = sum(qalychangemat[c(4:6),])
  mv_qaly = sum(qalychangemat[c(7:9),])
  nirsevimab_highrisk_qaly = sum(qalychangemat[c(10:12),]) 
  nirsevimab_seasonal_qaly = sum(qalychangemat[c(16:18),]) 
  nirsevimab_seasonal_cu_qaly = sum(qalychangemat[c(13:15),]) 
  nirsevimab_allyear_qaly = sum(qalychangemat[c(16:18),]) 
  
  ## price of the vaccine/lmAB
  palivizumab_price <- rep(0, NMonth)
  for (month in 1:NMonth) {
    if (month <= 12) {
      palivizumab_price[month] <- 
        parameters$NPOP[1,month] * 0.064 * parameters$price[["price_pal_preterm"]][month] +  ####一人平均6.5回Synagis接種, 1月平均 6.5回/12M
        parameters$NPOP[1,month] * 0.019 * parameters$price[["price_pal_risk"]][month]  + 　　　　　           　####一人平均5.9回 chd etc.
        parameters$NPOP[1,month] * 0.0006 * parameters$price[["price_pal_down"]][month] +    
        parameters$NPOP[1,month] * 0.0024 * parameters$price[["price_pal_others"]][month]
    } else {
      palivizumab_price[month] <- 
        parameters$NPOP[1,month] * 0.019 * parameters$price[["price_pal_risk"]][month]  + 　　　　　　
        parameters$NPOP[1,month] * 0.0006 * parameters$price[["price_pal_down"]][month] +
        parameters$NPOP[1,month] * 0.0024 * parameters$price[["price_pal_others"]][month]
    }
  }
  
  palivizumab_highrisk_price <- sum(palivizumab_price)
  
  
  ##maternal vaccine price
  mv_price = parameters$mv_unitprice * sum(parameters$NPOP[1,c(1:12)])
  
  ##Nirsevimub price
  nirsevimab_price <- rep(0, NMonth)
  for (month in 1:NMonth) {
    if(month<=12){
      nirsevimab_price[month] <- parameters$NPOP[1,month] * 0.064 * parameters$price[["price_nir_preterm"]][1] +
        parameters$NPOP[1,month] * 0.019 * parameters$price[["price_nir_risk"]][1] +
        parameters$NPOP[1,month] * 0.0006 * parameters$price[["price_nir_down"]][1] +
        parameters$NPOP[1,month] * 0.0024 * parameters$price[["price_nir_others"]][1]
    } else {
      nirsevimab_price[month] <- parameters$NPOP[1,month] * 0.019 * parameters$nirsevimab_unitprice2   +
        parameters$NPOP[1,month] * 0.0006 * parameters$nirsevimab_unitprice2  +
        parameters$NPOP[1,month] * 0.0024 * parameters$nirsevimab_unitprice2 
    }
  }
  
  nirsevimab_highrisk_price <- sum(nirsevimab_price)
  
  nirsevimab_seasonal_price <- sum(parameters$NPOP[1,c(1:12)]) * parameters$price[["price_nir_risk"]][1] +
    sum(parameters$NPOP[1,c(13:24)]) * 0.022 * parameters$nirsevimab_unitprice2 
  
  
  nirsevimab_seasonal_cu_price <- sum(parameters$NPOP[1,c(1:12)]) * parameters$price[["price_nir_risk"]][1]  +
    sum(parameters$NPOP[1,c(13:24)]) * 0.022 * parameters$nirsevimab_unitprice2 
  
  
  nirsevimab_allyear_price  <- sum(parameters$NPOP[1,c(1:12)]) * parameters$price[["price_nir_risk"]][1]  +
    sum(parameters$NPOP[1,c(13:24)]) * 0.022 * parameters$nirsevimab_unitprice2 
  
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


icer_offseason=function(parameters)
{
  ##deterministic probability of disease development
  NMonth = length(rate_jan$Month)
  status = c('C','H','D')  # not including 'N' status
  
  #Age specific disease status in the population 
  disease_status <- matrix(0, length(status), NMonth)
  for (month in 1:NMonth) {
    # infection
    disease_status[1, month] = parameters$NPOP[1,month] * parameters$prob_status[1,month]  # case
    disease_status[2, month] = disease_status[1,month] * parameters$prob_status[2,month] # hospital
    disease_status[3, month] = disease_status[2,month] * parameters$prob_status[3,month] + disease_status[1,month] * parameters$prob_status[4,month] # dead
  }
  
  #####cost
  costmat = matrix(0,length(status),NMonth)
  for(month in 1:NMonth){
    costmat[1,month] = parameters$cost[1,month] * disease_status[1,month]
    costmat[2,month] = parameters$cost[2,month] * disease_status[2,month]
    costmat[3,month] = parameters$cost[3,month] * disease_status[3,month]
  }
  
  ######qaly
  qalymat = matrix(0, length(status), NMonth)
  for(month in 1:NMonth){
    qalymat[1,month] = parameters$qaly[1,month] * disease_status[1,month]
    qalymat[2,month] = parameters$qaly[2,month] * disease_status[2,month]
    qalymat[3,month] = parameters$qaly[3,month] * disease_status[3,month]
  }
  
  ##discounting
  discountfactor = rep(0, NMonth)
  
  for (month in 1:NMonth){
    discountfactor[month] = (1+ parameters$discount)^(-floor((month-1)/12)) 
  }
  
  
  ###cost*effect*discount
  ##'*effect of palivizumab = 10% of population - high risk group will receive it in the first and second year of life*
  costchangemat = matrix(0, 18, NMonth)
  costchangemat[1,] = costmat[1,] *discountfactor
  costchangemat[2,] = costmat[2,] *discountfactor
  costchangemat[3,] = costmat[3,] *discountfactor
  costchangemat[4,] = costmat[1,] *discountfactor * parameters$effects[["eff_pal_dinf"]]
  costchangemat[5,] = costmat[2,] *discountfactor * parameters$effects[["eff_pal_hosp"]]
  costchangemat[6,] = costmat[3,] *discountfactor * parameters$effects[["eff_pal_icu"]] 
  costchangemat[7,] = costmat[1,] *discountfactor * parameters$effects[["eff_mv_dinf"]]
  costchangemat[8,] = costmat[2,] *discountfactor * parameters$effects[["eff_mv_hosp"]] 
  costchangemat[9,] = costmat[3,] *discountfactor * parameters$effects[["eff_mv_icu"]]
  costchangemat[10,] = costmat[1,] *discountfactor * parameters$effects[["eff_lmab_highrisk_dinf"]]
  costchangemat[11,] = costmat[2,] *discountfactor * parameters$effects[["eff_lmab_highrisk_hosp"]] 
  costchangemat[12,] = costmat[3,] *discountfactor * parameters$effects[["eff_lmab_highrisk_icu"]]
  costchangemat[13,] = costmat[1,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_dinf"]]
  costchangemat[14,] = costmat[2,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_hosp"]] 
  costchangemat[15,] = costmat[3,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_icu"]]
  costchangemat[16,] = costmat[1,] *discountfactor * parameters$effects[["eff_lmab_dinf"]]
  costchangemat[17,] = costmat[2,] *discountfactor * parameters$effects[["eff_lmab_hosp"]] 
  costchangemat[18,] = costmat[3,] *discountfactor * parameters$effects[["eff_lmab_icu"]] 
  
  ##intervention cost
  no_interv_cost = sum(costchangemat[c(1:3),])
  palivizumab_cost = sum(costchangemat[c(4:6),])
  mv_cost = sum(costchangemat[c(7:9),]) 
  nirsevimab_highrisk_cost = sum(costchangemat[c(10:12),])
  nirsevimab_seasonal_cost = sum(costchangemat[c(1:3),])
  nirsevimab_seasonal_cu_cost = sum(costchangemat[c(13:15),]) 
  nirsevimab_allyear_cost = sum(costchangemat[c(16:18),]) 
  
  
  #qaly*effect*discount
  qalychangemat = matrix(0,18,NMonth)
  qalychangemat[1,] = qalymat[1,] * discountfactor
  qalychangemat[2,] = qalymat[2,] * discountfactor
  qalychangemat[3,] = qalymat[3,] * discountfactor
  qalychangemat[4,] = qalymat[1,] * discountfactor * parameters$effects[["eff_pal_dinf"]]
  qalychangemat[5,] = qalymat[2,] * discountfactor * parameters$effects[["eff_pal_hosp"]]
  qalychangemat[6,] = qalymat[3,] * discountfactor * parameters$effects[["eff_pal_icu"]]
  qalychangemat[7,] = qalymat[1,] * discountfactor * parameters$effects[["eff_mv_dinf"]] 
  qalychangemat[8,] = qalymat[2,] * discountfactor * parameters$effects[["eff_mv_hosp"]]
  qalychangemat[9,] = qalymat[3,] * discountfactor * parameters$effects[["eff_mv_icu"]]
  qalychangemat[10,] = qalymat[1,] * discountfactor * parameters$effects[["eff_lmab_highrisk_dinf"]]
  qalychangemat[11,] = qalymat[2,] * discountfactor * parameters$effects[["eff_lmab_highrisk_hosp"]]
  qalychangemat[12,] = qalymat[3,] * discountfactor * parameters$effects[["eff_lmab_highrisk_icu"]]
  qalychangemat[13,] = qalymat[1,] * discountfactor * parameters$effects[["eff_lmab_seasonal_cu_dinf"]]   
  qalychangemat[14,] = qalymat[2,] * discountfactor * parameters$effects[["eff_lmab_seasonal_cu_hosp"]]
  qalychangemat[15,] = qalymat[3,] * discountfactor * parameters$effects[["eff_lmab_seasonal_cu_icu"]]
  qalychangemat[16,] = qalymat[1,] * discountfactor * parameters$effects[["eff_lmab_dinf"]]   
  qalychangemat[17,] = qalymat[2,] * discountfactor * parameters$effects[["eff_lmab_hosp"]] 
  qalychangemat[18,] = qalymat[3,] * discountfactor * parameters$effects[["eff_lmab_icu"]] 
  
  
  #qaly loss for each intervention
  no_interv_qaly = sum(qalychangemat[c(1:3),]) 
  palivizumab_qaly = sum(qalychangemat[c(4:6),]) 
  mv_qaly = sum(qalychangemat[c(7:9),])
  nirsevimab_highrisk_qaly = sum(qalychangemat[c(10:12),]) 
  nirsevimab_seasonal_qaly = sum(qalychangemat[c(1:3),]) 
  nirsevimab_seasonal_cu_qaly = sum(qalychangemat[c(13:15),]) 
  nirsevimab_allyear_qaly = sum(qalychangemat[c(16:18),])
  
  
  ## price of the vaccine/lmAB
  palivizumab_price <- rep(0, NMonth)
  for (month in 1:NMonth) {
    if (month <= 12) {
      palivizumab_price[month] <- 
        parameters$NPOP[1,month] * 0.064 * parameters$pal_unitprice_off[[1]]  +  ####一人平均6.5回Synagis接種, 1月平均 6.5回/12M
        parameters$NPOP[1,month] * 0.019 * parameters$pal_unitprice_off[[2]]  + 　　　　　           　####一人平均5.9回 chd etc.
        parameters$NPOP[1,month] * 0.0006 * parameters$pal_unitprice_off[[3]] +
        parameters$NPOP[1,month] * 0.0024 * parameters$pal_unitprice_off[[4]]
    } else {
      palivizumab_price[month] <- 
        parameters$NPOP[1,month] * 0.019 * parameters$pal_unitprice_off[[6]]  + 　　　　　　
        parameters$NPOP[1,month] * 0.0006 * parameters$pal_unitprice_off[[7]] +
        parameters$NPOP[1,month] * 0.0024 * parameters$pal_unitprice_off[[8]]
    }
  }
  
  palivizumab_highrisk_price <- sum(palivizumab_price)
  
  
  ##maternal vaccine price
  mv_price = parameters$mv_unitprice * sum(parameters$NPOP[1,c(1:12)])
  
  ##Nirsevimub price
  nirsevimab_price <- rep(0, NMonth)
  for (month in 1:NMonth){
    if(month<=12){
      nirsevimab_price[month] <- parameters$NPOP[1,month] * 0.064 * parameters$nir_unitprice_off[[1]] +  #preterm
        parameters$NPOP[1,month] * 0.019 * parameters$nir_unitprice_off[[2]] + 　                        #down
        parameters$NPOP[1,month] * 0.0006 * parameters$nir_unitprice_off[[3]] +                          #risk: chd etc
        parameters$NPOP[1,month] * 0.0024 * parameters$nir_unitprice_off[[4]]                            #neuro, meta etc
    } else {
      nirsevimab_price[month] <- parameters$NPOP[1,month] * 0.019 * parameters$nirsevimab_unitprice2 + #down
        parameters$NPOP[1,month] * 0.0006 * parameters$nirsevimab_unitprice2 +                          #risk 
        parameters$NPOP[1,month] * 0.0024 * parameters$nirsevimab_unitprice2             ##at 12month once
    }
  }
  
  nirsevimab_highrisk_price <- sum(nirsevimab_price) 
  nirsevimab_seasonal_price <- sum(nirsevimab_price)  ##at 0 month once
  
  nirsevimab_seasonal_cu <- rep(0, NMonth)
  for (month in 1:NMonth) {
    if(month <=12){
      nirsevimab_seasonal_cu[month] <- parameters$NPOP[1,month] * parameters$nir_unitprice_off[[3]]  #1st season 1回接種 assumed same BW for highrisk group
    } else {
      nirsevimab_seasonal_cu[month] <- parameters$NPOP[1,month] * 0.019 * parameters$nirsevimab_unitprice2 +
        parameters$NPOP[1,month] * 0.0006 *parameters$nirsevimab_unitprice2 +
        parameters$NPOP[1,month] * 0.00024 * parameters$nirsevimab_unitprice2
    }
  }
  
  nirsevimab_seasonal_cu_price <- sum(nirsevimab_seasonal_cu)
  
  nirsevimab_allyear_price  <- sum(parameters$NPOP[1,c(1:12)]) * parameters$nir_unitprice_off[[3]] +  ### check change 
    sum(parameters$NPOP[1,c(13:24)]) * 0.022 * parameters$nirsevimab_unitprice2
  
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
  
  icer$nirsevimab_seasonal_cost <- nirsevimab_seasonal_cost
  icer$nirsevimab_seasonal_qaly <- nirsevimab_seasonal_qaly
  icer$nirsevimab_seasonal_price <- nirsevimab_seasonal_price
  icer$nirsevimab_seasonal_cu_cost <- nirsevimab_seasonal_cu_cost
  icer$nirsevimab_seasonal_cu_qaly <- nirsevimab_seasonal_cu_qaly 
  icer$nirsevimab_seasonal_cu_price <- nirsevimab_seasonal_cu_price
  
  icer$nirsevimab_allyear_cost <- nirsevimab_allyear_cost
  icer$nirsevimab_allyear_qaly <- nirsevimab_allyear_qaly
  icer$nirsevimab_allyear_price <- nirsevimab_allyear_price
  icer$nirsevimab_highrisk_cost <- nirsevimab_highrisk_cost
  icer$nirsevimab_highrisk_qaly <- nirsevimab_highrisk_qaly
  icer$nirsevimab_highrisk_price <- nirsevimab_highrisk_price
  
  return(icer)
}


jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)


icer_season(jan)
icer_offseason(apr)

df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_pal_no
icer_mv_no
icer_nir_highrisk_no
icer_nir_seasonal_no
icer_nir_seasonal_cu_no
icer_nir_allyear_no
icer0 <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no, icer_nir_allyear_no)
icer0


#####One way sensitivity analysis####
###disease burden death and other incidence
####monthly disease burden Hi
prob_status_det_jan <- rate_jan_hi[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jan <- t(as.matrix(prob_status_det_jan))

prob_status_det_feb <- rate_feb[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_feb <- t(as.matrix(prob_status_det_feb))

prob_status_det_mar <- rate_mar[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_mar <- t(as.matrix(prob_status_det_mar))

prob_status_det_apr <- rate_apr[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_apr <- t(as.matrix(prob_status_det_apr))

prob_status_det_may <- rate_may[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_may <- t(as.matrix(prob_status_det_may))

prob_status_det_jun <- rate_jun[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jun <- t(as.matrix(prob_status_det_jun))

prob_status_det_jul <- rate_jul[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jul <- t(as.matrix(prob_status_det_jul))

prob_status_det_aug <- rate_aug[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_aug <- t(as.matrix(prob_status_det_aug))

prob_status_det_sep <- rate_sep[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_sep <- t(as.matrix(prob_status_det_sep))

prob_status_det_oct <- rate_oct[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_oct <- t(as.matrix(prob_status_det_oct))

prob_status_det_nov <- rate_nov[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_nov <- t(as.matrix(prob_status_det_nov))

prob_status_det_dec <- rate_dec[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_dec <- t(as.matrix(prob_status_det_dec))

jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)



df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_disease_hi <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no, icer_nir_allyear_no)
icer_disease_hi


###lo
####monthly disease burden 
prob_status_det_jan <- rate_jan_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jan <- t(as.matrix(prob_status_det_jan))

prob_status_det_feb <- rate_feb_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_feb <- t(as.matrix(prob_status_det_feb))

prob_status_det_mar <- rate_mar_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_mar <- t(as.matrix(prob_status_det_mar))

prob_status_det_apr <- rate_apr_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_apr <- t(as.matrix(prob_status_det_apr))

prob_status_det_may <- rate_may_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_may <- t(as.matrix(prob_status_det_may))

prob_status_det_jun <- rate_jun_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jun <- t(as.matrix(prob_status_det_jun))

prob_status_det_jul <- rate_jul_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jul <- t(as.matrix(prob_status_det_jul))

prob_status_det_aug <- rate_aug_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_aug <- t(as.matrix(prob_status_det_aug))

prob_status_det_sep <- rate_sep_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_sep <- t(as.matrix(prob_status_det_sep))

prob_status_det_oct <- rate_oct_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_oct <- t(as.matrix(prob_status_det_oct))

prob_status_det_nov <- rate_nov_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_nov <- t(as.matrix(prob_status_det_nov))

prob_status_det_dec <- rate_dec_lo[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_dec <- t(as.matrix(prob_status_det_dec))


jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)


df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_disease_lo <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no, icer_nir_allyear_no)
icer_disease_lo


###medical cost hi
##deterministic cost
cost_det <- rate_jan_hi[,c('cost_dinf','cost_hosp','cost_death')]
cost_det <- t(as.matrix(cost_det))


####monthly disease burden 
prob_status_det_jan <- rate_jan[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jan <- t(as.matrix(prob_status_det_jan))

prob_status_det_feb <- rate_feb[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_feb <- t(as.matrix(prob_status_det_feb))

prob_status_det_mar <- rate_mar[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_mar <- t(as.matrix(prob_status_det_mar))

prob_status_det_apr <- rate_apr[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_apr <- t(as.matrix(prob_status_det_apr))

prob_status_det_may <- rate_may[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_may <- t(as.matrix(prob_status_det_may))

prob_status_det_jun <- rate_jun[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jun <- t(as.matrix(prob_status_det_jun))

prob_status_det_jul <- rate_jul[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jul <- t(as.matrix(prob_status_det_jul))

prob_status_det_aug <- rate_aug[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_aug <- t(as.matrix(prob_status_det_aug))

prob_status_det_sep <- rate_sep[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_sep <- t(as.matrix(prob_status_det_sep))

prob_status_det_oct <- rate_oct[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_oct <- t(as.matrix(prob_status_det_oct))

prob_status_det_nov <- rate_nov[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_nov <- t(as.matrix(prob_status_det_nov))

prob_status_det_dec <- rate_dec[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_dec <- t(as.matrix(prob_status_det_dec))


jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)



df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_cost_hi <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no,icer_nir_seasonal_cu_no, icer_nir_allyear_no)
icer_cost_hi

###medical cost lo
##deterministic cost
cost_det <- rate_jan_lo[,c('cost_dinf','cost_hosp','cost_death')]
cost_det <- t(as.matrix(cost_det))

####monthly disease burden 
prob_status_det_jan <- rate_jan[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jan <- t(as.matrix(prob_status_det_jan))

prob_status_det_feb <- rate_feb[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_feb <- t(as.matrix(prob_status_det_feb))

prob_status_det_mar <- rate_mar[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_mar <- t(as.matrix(prob_status_det_mar))

prob_status_det_apr <- rate_apr[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_apr <- t(as.matrix(prob_status_det_apr))

prob_status_det_may <- rate_may[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_may <- t(as.matrix(prob_status_det_may))

prob_status_det_jun <- rate_jun[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jun <- t(as.matrix(prob_status_det_jun))

prob_status_det_jul <- rate_jul[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_jul <- t(as.matrix(prob_status_det_jul))

prob_status_det_aug <- rate_aug[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_aug <- t(as.matrix(prob_status_det_aug))

prob_status_det_sep <- rate_sep[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_sep <- t(as.matrix(prob_status_det_sep))

prob_status_det_oct <- rate_oct[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_oct <- t(as.matrix(prob_status_det_oct))

prob_status_det_nov <- rate_nov[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_nov <- t(as.matrix(prob_status_det_nov))

prob_status_det_dec <- rate_dec[,c('p_dinf','p_hosp_if_dinf','p_die_if_hp','p_die_if_dinf')]  
prob_status_det_dec <- t(as.matrix(prob_status_det_dec))

jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)


df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_cost_lo <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no,icer_nir_seasonal_cu_no, icer_nir_allyear_no)
icer_cost_lo


###Qaly high
##deterministic cost
cost_det <- rate_jan[,c('cost_dinf','cost_hosp','cost_death')]
cost_det <- t(as.matrix(cost_det))

##deterministic qaly
qaly_det <- rate_jan_hi[,c('qaly_dinf','qaly_hosp','qaly_die')]
qaly_det <- t(as.matrix(qaly_det))


jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)


df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_qaly_hi <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no,icer_nir_allyear_no)
icer_qaly_hi

##qaly lo
##deterministic cost
cost_det <- rate_jan[,c('cost_dinf','cost_hosp','cost_death')]
cost_det <- t(as.matrix(cost_det))

##deterministic qaly
qaly_det <- rate_jan_lo[,c('qaly_dinf','qaly_hosp','qaly_die')]
qaly_det <- t(as.matrix(qaly_det))


jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)


df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_qaly_lo <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no,icer_nir_allyear_no)
icer_qaly_lo


### effect Hi
##deterministic cost
cost_det <- rate_jan[,c('cost_dinf','cost_hosp','cost_death')]
cost_det <- t(as.matrix(cost_det))

##deterministic qaly
qaly_det <- rate_jan[,c('qaly_dinf','qaly_hosp','qaly_die')]
qaly_det <- t(as.matrix(qaly_det))


jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_effect_hi <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no,icer_nir_allyear_no)
icer_effect_hi

### effect LO
jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)



df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_effect_lo <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no,icer_nir_allyear_no)
icer_effect_lo

# discounting 0 %
jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)


df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_discount_lo <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no,icer_nir_allyear_no)
icer_discount_lo

#discount high 4%
jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #27000
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["apr",],
           pal_unitprice_off = pal_unitprice_off["apr",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["may",],
           pal_unitprice_off = pal_unitprice_off["may",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jun",],
           pal_unitprice_off = pal_unitprice_off["jun",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["jul",],
           pal_unitprice_off = pal_unitprice_off["jul",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           nir_unitprice_off = nir_unitprice_off["aug",],
           pal_unitprice_off = pal_unitprice_off["aug",],
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec,
           nirsevimab_unitprice = lamab_price + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *2 + admi_cost,
           mv_unitprice = abrysbo + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.04  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)


df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_discount_hi <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no,icer_nir_allyear_no)
icer_discount_hi


#####Price
price_jan_lo <- ((price_jan -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0)))/2) + admi_cost
price_jan_lo[price_jan_lo ==3575] <- 0
price_jan_lo <- price_jan_lo %>% mutate(Month = seq(1:24))

price_feb_lo <- ((price_feb -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_feb_lo[price_feb_lo ==3575] <- 0
price_feb_lo <- price_feb_lo %>% mutate(Month = seq(1:24))


price_mar_lo <- ((price_mar -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_mar_lo[price_mar_lo ==3575] <- 0
price_mar_lo <- price_mar_lo %>% mutate(Month = seq(1:24))


price_apr_lo <- ((price_apr -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_apr_lo[price_apr_lo ==3575] <- 0
price_apr_lo <- price_apr_lo %>% mutate(Month = seq(1:24))


price_may_lo <- ((price_may -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_may_lo[price_may_lo ==3575] <- 0
price_may_lo <- price_may_lo %>% mutate(Month = seq(1:24))


price_jun_lo <- ((price_jun -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_jun_lo[price_jun_lo ==3575] <- 0
price_jun_lo <- price_jun_lo %>% mutate(Month = seq(1:24))


price_jul_lo <- ((price_jul -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_jul_lo[price_jul_lo ==3575] <- 0
price_jul_lo <- price_jul_lo %>% mutate(Month = seq(1:24))


price_aug_lo <- ((price_aug -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_aug_lo[price_aug_lo ==3575] <- 0
price_aug_lo <- price_aug_lo %>% mutate(Month = seq(1:24))


price_sep_lo <- ((price_sep -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_sep_lo[price_sep_lo ==3575] <- 0
price_sep_lo <- price_sep_lo %>% mutate(Month = seq(1:24))


price_oct_lo <- ((price_oct -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_oct_lo[price_oct_lo ==3575] <- 0
price_oct_lo <- price_oct_lo %>% mutate(Month = seq(1:24))


price_nov_lo <- ((price_nov -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_nov_lo[price_nov_lo ==3575] <- 0
price_nov_lo <- price_nov_lo %>% mutate(Month = seq(1:24))

price_dec_lo <- ((price_dec -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) /2) + admi_cost
price_dec_lo[price_dec_lo ==3575] <- 0
price_dec_lo <- price_dec_lo %>% mutate(Month = seq(1:24))




### Price LO
jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           nir_unitprice_off = (nir_unitprice_off["apr",]-admi_cost)/2 +admi_cost,
           pal_unitprice_off = (pal_unitprice_off["apr",]-admi_cost)/2 +admi_cost,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           nir_unitprice_off = (nir_unitprice_off["may",]-admi_cost)/2 +admi_cost,
           pal_unitprice_off = (pal_unitprice_off["may",]-admi_cost)/2 +admi_cost,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           nir_unitprice_off = (nir_unitprice_off["jun",]-admi_cost)/2 +admi_cost,
           pal_unitprice_off = (pal_unitprice_off["jun",]-admi_cost)/2 +admi_cost,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           nir_unitprice_off = (nir_unitprice_off["jul",]-admi_cost)/2 +admi_cost,
           pal_unitprice_off = (pal_unitprice_off["jul",]-admi_cost)/2 +admi_cost,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           nir_unitprice_off = (nir_unitprice_off["aug",]-admi_cost)/2 +admi_cost,
           pal_unitprice_off = (pal_unitprice_off["aug",]-admi_cost)/2 +admi_cost,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = (price_nov -admi_cost)/2 + admi_cost,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec_lo,
           nirsevimab_unitprice = lamab_price/2 + admi_cost, ## market price at 2024  #459147
           nirsevimab_unitprice2 = lamab_price + admi_cost,
           mv_unitprice = abrysbo/2 + admi_cost,          ## market price at 2024  #27000
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)


df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_price_lo <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no,icer_nir_allyear_no)
icer_price_lo

price_jan_hi <- ((price_jan -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_jan_hi[price_jan_hi ==3575] <- 0
price_jan_hi <- price_jan_hi %>% mutate(Month = seq(1:24))

price_feb_hi <- ((price_feb -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_feb_hi[price_feb_hi ==3575] <- 0
price_feb_hi <- price_feb_hi %>% mutate(Month = seq(1:24))


price_mar_hi <- ((price_mar -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_mar_hi[price_mar_hi ==3575] <- 0
price_mar_hi <- price_mar_hi %>% mutate(Month = seq(1:24))


price_apr_hi <- ((price_apr -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_apr_hi[price_apr_hi ==3575] <- 0
price_apr_hi <- price_apr_hi %>% mutate(Month = seq(1:24))


price_may_hi <- ((price_may -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_may_hi[price_may_hi ==3575] <- 0
price_may_hi <- price_may_hi %>% mutate(Month = seq(1:24))


price_jun_hi <- ((price_jun -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_jun_hi[price_jun_hi ==3575] <- 0
price_jun_hi <- price_jun_hi %>% mutate(Month = seq(1:24))


price_jul_hi <- ((price_jul -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_jul_hi[price_jul_hi ==3575] <- 0
price_jul_hi <- price_jul_hi %>% mutate(Month = seq(1:24))


price_aug_hi <- ((price_aug -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_aug_hi[price_aug_hi ==3575] <- 0
price_aug_hi <- price_aug_hi %>% mutate(Month = seq(1:24))


price_sep_hi <- ((price_sep -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_sep_hi[price_sep_hi ==3575] <- 0
price_sep_hi <- price_sep_hi %>% mutate(Month = seq(1:24))


price_oct_hi <- ((price_oct -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_oct_hi[price_oct_hi ==3575] <- 0
price_oct_hi <- price_oct_hi %>% mutate(Month = seq(1:24))


price_nov_hi <- ((price_nov -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_nov_hi[price_nov_hi ==3575] <- 0
price_nov_hi <- price_nov_hi %>% mutate(Month = seq(1:24))

price_dec_hi <- ((price_dec -admi_cost) %>% 
                   mutate(across(where(is.numeric),~ replace(.x, .x < 0, 0))) *2) + admi_cost
price_dec_hi[price_dec_hi ==3575] <- 0
price_dec_hi <- price_dec_hi %>% mutate(Month = seq(1:24))


### Price Hi
jan = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jan,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jan_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

feb = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_feb,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_feb_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative costst
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

mar = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_mar,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_mar_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

apr = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_apr,
           effects = effects_apr,
           cost = cost_det,
           qaly = qaly_det,
           price = price_apr_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           nir_unitprice_off = nir_unitprice_off["apr",]*2,
           pal_unitprice_off = pal_unitprice_off["apr",]*2,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

may = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_may,
           effects = effects_may,
           cost = cost_det,
           qaly = qaly_det,
           price = price_may_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           nir_unitprice_off = nir_unitprice_off["may",]*2,
           pal_unitprice_off = pal_unitprice_off["may",]*2,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jun = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jun,
           effects = effects_jun,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jun_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           nir_unitprice_off = nir_unitprice_off["jun",]*2,
           pal_unitprice_off = pal_unitprice_off["jun",]*2,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

jul = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_jul,
           effects = effects_jul,
           cost = cost_det,
           qaly = qaly_det,
           price = price_jul_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           nir_unitprice_off = nir_unitprice_off["jul",]*2,
           pal_unitprice_off = pal_unitprice_off["jul",]*2,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

aug = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_aug,
           effects = effects_aug,
           cost = cost_det,
           qaly = qaly_det,
           price = price_aug_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           nir_unitprice_off = nir_unitprice_off["aug",]*2,
           pal_unitprice_off = pal_unitprice_off["aug",]*2,
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)

sep = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_sep,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_sep_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
oct = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_oct,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_oct_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
nov = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_nov,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_nov_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)
dec = list(NPOP = Npop,   ### 0 and 1 year old population average 2014-2023 
           prob_status = prob_status_det_dec,
           effects = effects_season,
           cost = cost_det,
           qaly = qaly_det,
           price = price_dec_hi,
           nirsevimab_unitprice = lamab_price*2 + admi_cost, ## market price at 2024  #3575 JPY administrative cost
           nirsevimab_unitprice2 = lamab_price *4 + admi_cost,
           mv_unitprice = abrysbo*2 + admi_cost,          ## market price at 2024  #3575 JPY administrative cost
           discount = 0.02  #discount rate 2% (yearly discounting rate recommended by Japanese guideline)
)


df <- rbind(
  as.data.frame(icer_season(jan)),
  as.data.frame(icer_season(feb)),
  as.data.frame(icer_season(mar)),
  as.data.frame(icer_offseason(apr)),
  as.data.frame(icer_offseason(may)),
  as.data.frame(icer_offseason(jun)),
  as.data.frame(icer_offseason(jul)),
  as.data.frame(icer_offseason(aug)),
  as.data.frame(icer_season(sep)),
  as.data.frame(icer_season(oct)),
  as.data.frame(icer_season(nov)),
  as.data.frame(icer_season(dec))
)

res <- sapply(df, function(col) {
  if (is.numeric(col)) sum(col, na.rm = TRUE) else NA
})
res
res <- as.data.frame(t(as.matrix(res)))

icer_pal_no <- ((res$palivizumab_cost + res$palivizumab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$palivizumab_qaly)
icer_mv_no <- ((res$mv_cost + res$mv_price)- res$no_interv_cost)/(res$no_interv_qaly - res$mv_qaly)
icer_nir_highrisk_no <- ((res$nirsevimab_highrisk_cost + res$nirsevimab_highrisk_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)
icer_nir_seasonal_no <- ((res$nirsevimab_seasonal_cost + res$nirsevimab_seasonal_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)
icer_nir_seasonal_cu_no <- ((res$nirsevimab_seasonal_cu_cost + res$nirsevimab_seasonal_cu_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)
icer_nir_allyear_no <- ((res$nirsevimab_allyear_cost + res$nirsevimab_allyear_price)- res$no_interv_cost)/(res$no_interv_qaly - res$nirsevimab_allyear_qaly)

icer_price_hi <- c(icer_pal_no, icer_mv_no, icer_nir_highrisk_no, icer_nir_seasonal_no, icer_nir_seasonal_cu_no,icer_nir_allyear_no)
icer_price_hi


icermat<- rbind(icer0, icer_disease_hi,icer_disease_lo,icer_cost_hi,icer_cost_lo,
                icer_qaly_hi, icer_qaly_lo, icer_effect_hi, icer_effect_lo,
                icer_discount_hi, icer_discount_lo, icer_price_hi, icer_price_lo)

rownames(icermat) <- c("base", "diseasehi", "diseaselo",  "costhi","costlo","qalyhi", "qalylo",
                       "effecthi","effectlo","discounthi","discountlo", "pricehi","pricelo")
icermat <- as.data.frame(t(icermat))
icermat <- icermat %>% mutate(intervention = c("1 pal","2 mv","3 nir_hr","4 nir_seasonal","5 nir_seasonal+catchup","6 nir_allyear"))

icermat <- icermat %>% 
  mutate(
    disease_hi = diseasehi - base,
    disease_lo = diseaselo -base,
    cost_hi= costhi - base,
    cost_lo = costlo - base,
    qaly_hi = qalyhi - base,
    qaly_lo = qalylo - base,
    effect_hi = effecthi -base,
    effect_lo = effectlo - base,
    price_hi = pricehi - base,
    price_lo = pricelo - base,
    discount_hi = discounthi - base,
    discount_lo = discountlo - base)

write.csv(icermat, "output/icer_tornado.csv")


icermat <- icermat %>% select(-c(diseasehi, diseaselo, costhi, costlo, qalyhi, qalylo,
                                 effecthi,effectlo, discounthi, discountlo, pricehi, pricelo))


tornado <- reshape(data = icermat, 
                   varying = list(
                     death = c("disease_lo", "disease_hi"), 
                     cost = c("cost_lo", "cost_hi"), 
                     qaly = c("qaly_lo","qaly_hi"),
                     effect = c("effect_lo","effect_hi"),
                     discount = c("discount_lo","discount_hi"),
                     price = c("price_lo","price_hi")
                   ),
                   v.names = c("disease","cost","qaly","effect","discount","price"), 
                   timevar = "scenario", 
                   times = c("low","high"), 
                   idvar = "intervention", 
                   direction = "long")


tornado <- tornado %>% mutate(id = paste(intervention , scenario, sep =""))
tornado <-   reshape(tornado, varying = c("disease","cost","qaly","effect","discount", "price"), v.names = "icer",
                     timevar=  c("parameter"), times = c("disease","cost", "qaly","effect","discount", "price"), idvar = "id", direction = "long")

tornado$parameter <- factor(tornado$parameter,
                            levels = c("effect","discount","qaly","cost","disease", "price"),
                            labels = c("effect","discount","qaly","cost","disease", "price"))
tornado$scenario <- factor(tornado$scenario,
                           levels = c("low","high"),
                           labels = c("low","high"))


write.csv(tornado, "output/tornado.csv")


library(scales)

tornado_plot <- ggplot(tornado %>% filter(intervention == "2 mv"),aes(x = parameter, y = icer , fill = scenario)) +
  coord_flip() +
  geom_bar(stat= "identity",width = 0.7) +
  scale_fill_manual(name = NULL,
                    labels = c("low","high"),
                    values = c("low" = "navy", "high" = "darkred")) +
  geom_hline(yintercept = 5000000 -icer0[2], linetype = "dashed", color = "black") +
  scale_y_continuous(
    labels = function(x) comma(x + icer0[2])  # show absolute ICER labels
  ) +
  labs(x = "parameter changed", y = "Maternal Vaccine ICER change", title = "One-way sensitivity analysis (Maternal Vaccine)") +
  theme_bw()

tornado_plot
ggsave(plot =tornado_plot, "output/tornado_plot.jpeg", width = 8, height = 4)

tornado_plot_pal <- ggplot(tornado%>% filter(intervention == "1 pal"),aes(x = parameter, y = icer, fill = scenario)) +
  coord_flip() +
  geom_bar(stat= "identity",width = 0.7) +
  scale_fill_manual(name = NULL,
                    labels = c("low","high"),
                    values = c("low" = "navy", "high" = "darkred")) +
  geom_hline(yintercept = 5000000 - icer0[1]
             , linetype = "dashed", color = "black") +
  scale_y_continuous(
    labels = function(x) comma(x + icer0[1])  # show absolute ICER labels
  ) +
  labs(x = "parameter change", y = "Palivizumab ICER change", title = "One-way sensitivity analysis (Palivizumab)") +
  theme_bw()

tornado_plot_pal
ggsave(plot =tornado_plot_pal, "output/tornado_plot_pal.jpeg", width = 8, height = 4)

tornado_plot_nirhr <- ggplot(tornado%>% filter(intervention == "3 nir_hr" & scenario!="lo"),aes(x = parameter, y = icer, fill = scenario)) +
  coord_flip() +
  geom_bar(stat= "identity",width = 0.7) +
  scale_fill_manual(name = NULL,
                    labels = c("low","high"),
                    values = c("low" = "navy", "high" = "darkred")) +
  geom_hline(yintercept = 5000000 - icer0[3], linetype = "dashed", color = "black") +
  scale_y_continuous(
    labels = function(x) comma(x + icer0[3])  # show absolute ICER labels
  ) +
  labs(x = "parameter change", y = "Nirsevimab for high-risk ICER change", title = "One-way sensitivity analysis (Nirsevimab_high-risk") +
  theme_bw()
tornado_plot_nirhr
ggsave(plot =tornado_plot_nirhr, "output/tornado_plot_nirhr.jpeg", width = 8, height = 4)


tornado_plot_nirseas <- ggplot(tornado%>% filter(intervention == "4 nir_seasonal"),aes(x = parameter, y = icer, fill = scenario)) +
  coord_flip() +
  geom_bar(stat= "identity",width = 0.7) +
  scale_fill_manual(name = NULL,
                    labels = c("low","high"),
                    values = c("low" = "navy", "high" = "darkred")) +
  geom_hline(yintercept = 5000000 - icer0[4], linetype = "dashed", color = "black") +
  scale_y_continuous(
    labels = function(x) comma(x + icer0[4])  # show absolute ICER labels
  ) +
  labs(x = "parameter change", y = "Nirsevimab seasonal ICER change", title = "One-way sensitivity analysis (Nirsevimab_seasonal)") +
  theme_bw()
tornado_plot_nirseas
ggsave(plot =tornado_plot_nirseas, "output/tornado_plot_nirseas.jpeg", width =8, height =4)


tornado_plot_seasonalcu <- ggplot(tornado%>% filter(intervention == "5 nir_seasonal+catchup"),aes(x = parameter, y = icer, fill = scenario)) +
  coord_flip() +
  geom_bar(stat= "identity",width = 0.7) +
  scale_fill_manual(name = NULL,
                    labels = c("low","high"),
                    values = c("low" = "navy", "high" = "darkred")) +
  geom_hline(yintercept = 5000000 - icer0[6], linetype = "dashed", color = "black") +
  scale_y_continuous(
    labels = function(x) comma(x + icer0[6])  # show absolute ICER labels
  ) +
  labs(x = "parameter change", y = "Nirsevimab seasonal + catch-up ICER change", title = "One-way sensitivity analysis (Nirsevimab_seasonal + catchup") +
  theme_bw()
tornado_plot_seasonalcu
ggsave(plot =tornado_plot_seasonalcu, "output/tornado_plot_nir_seasonalcu.jpeg", width =8, height =4)


tornado_plot_nirall <- ggplot(tornado%>% filter(intervention == "6 nir_allyear"),aes(x = parameter, y = icer, fill = scenario)) +
  coord_flip() +
  geom_bar(stat= "identity",width = 0.7) +
  scale_fill_manual(name = NULL,
                    labels = c("low","high"),
                    values = c("low" = "navy", "high" = "darkred")) +
  geom_hline(yintercept = 5000000 - icer0[6], linetype = "dashed", color = "black") +
  scale_y_continuous(
    labels = function(x) comma(x + icer0[6])  # show absolute ICER labels
  ) +
  labs(x = "parameter change", y = "Nirsevimab all-year ICER change", title = "One-way sensitivity analysis (Nirsevimab_year round)") +
  theme_bw()
tornado_plot_nirall
ggsave(plot =tornado_plot_nirall, "output/tornado_plot_nirall.jpeg", width =8, height =4)

