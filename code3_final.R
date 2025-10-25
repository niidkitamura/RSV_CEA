###visualize the reduction of the disease and cost
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
  df %>% mutate(deterministic.value = case_when(pars == "cost_dinf" & Month<=12 ~ exp(7.860086),
                                                pars == "cost_dinf" & Month >12 ~ exp(7.743724),
                                                pars =="cost_hosp" & Month<=12 ~ exp(11),
                                                pars =="cost_hosp" & Month >12 ~ exp(10.6),
                                                pars=="cost_death" & Month<=12 ~ exp(10.9),
                                                pars=="cost_death" & Month >12 ~ exp(9.92),
                                                TRUE ~ deterministic.value))}

parameter_jan <- cost_log(parameter_jan)
parameter_feb <- cost_log(parameter_feb)
parameter_mar <- cost_log(parameter_mar)
parameter_apr <- cost_log(parameter_apr)
parameter_may <- cost_log(parameter_may)
parameter_jun <- cost_log(parameter_jun)
parameter_jul <- cost_log(parameter_jul)
parameter_aug <- cost_log(parameter_aug)
parameter_sep <- cost_log(parameter_sep)
parameter_oct <- cost_log(parameter_oct)
parameter_nov <- cost_log(parameter_nov)
parameter_dec <- cost_log(parameter_dec)

crate_rate <- function(df){
  mid <- reshape(idvar = "Month", timevar = "pars",
                 direction = "wide",
                 data = df %>% select(Month, pars, deterministic.value))
  
  new_names <- c("Month","p_dinf","p_hosp_if_dinf","p_die_if_hp","p_die_if_dinf",
                 "cost_dinf","cost_hosp","cost_death",
                 "qaly_dinf","qaly_hosp","qaly_die",
                 "eff_mv_dinf","eff_mv_hosp","eff_mv_icu","eff_lmab_dinf",
                 "eff_lmab_hosp","eff_lmab_icu","mv_pal_dinf","mv_pal_hosp","mv_pal_icu","mv_nir_dinf",
                 "mv_nir_hosp","mv_nir_icu","eff_pal_dinf","eff_pal_hosp","eff_pal_icu",
                 "eff_lmab_highrisk_dinf","eff_lmab_highrisk_hosp","eff_lmab_highrisk_icu",
                 "eff_lmab_seasonal_cu_dinf","eff_lmab_seasonal_cu_hosp","eff_lmab_seasonal_cu_icu")
  colnames(mid) <- new_names
  
  df_name <- deparse(substitute(df)) # get the name of the input object as a string
  suffix <- substr(df_name, nchar(df_name)-2, nchar(df_name)) # extract last 3 characters from the *name string*
  
  assign(paste0("rate_", suffix), mid, envir = .GlobalEnv)
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
lamab_price = 450000
abrysbo = 27000

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
  
  
  ##case averted
  diseasechangemat = matrix(0, 18, NMonth)
  diseasechangemat[1,] = disease_status[1,] *discountfactor
  diseasechangemat[2,] = disease_status[2,] *discountfactor
  diseasechangemat[3,] = disease_status[3,] *discountfactor
  diseasechangemat[4,] = disease_status[1,] *discountfactor * parameters$effects[["eff_pal_dinf"]]
  diseasechangemat[5,] = disease_status[2,] *discountfactor * parameters$effects[["eff_pal_hosp"]]
  diseasechangemat[6,] = disease_status[3,] *discountfactor * parameters$effects[["eff_pal_icu"]]
  diseasechangemat[7,] = disease_status[1,] *discountfactor * parameters$effects[["eff_mv_dinf"]]
  diseasechangemat[8,] = disease_status[2,] *discountfactor * parameters$effects[["eff_mv_hosp"]]
  diseasechangemat[9,] = disease_status[3,] *discountfactor * parameters$effects[["eff_mv_icu"]]
  diseasechangemat[10,] = disease_status[1,] *discountfactor * parameters$effects[["eff_lmab_highrisk_dinf"]] 
  diseasechangemat[11,] = disease_status[2,] *discountfactor * parameters$effects[["eff_lmab_highrisk_hosp"]]
  diseasechangemat[12,] = disease_status[3,] *discountfactor * parameters$effects[["eff_lmab_highrisk_icu"]] 
  diseasechangemat[13,] = disease_status[1,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_dinf"]] 
  diseasechangemat[14,] = disease_status[2,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_hosp"]]
  diseasechangemat[15,] = disease_status[3,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_icu"]] 
  diseasechangemat[16,] = disease_status[1,] *discountfactor * parameters$effects[["eff_lmab_dinf"]] 
  diseasechangemat[17,] = disease_status[2,] *discountfactor * parameters$effects[["eff_lmab_hosp"]] 
  diseasechangemat[18,] = disease_status[3,] *discountfactor * parameters$effects[["eff_lmab_icu"]] 
  
  
  ##intervention cost
  no_interv_case = sum(diseasechangemat[1,])
  no_interv_hp = sum(diseasechangemat[2,])
  no_interv_death = sum(diseasechangemat[3,])
  
  palivizumab_case = sum(diseasechangemat[4,])
  palivizumab_hp = sum(diseasechangemat[5,])
  palivizumab_death = sum(diseasechangemat[6,])
  
  mv_case = sum(diseasechangemat[7,])
  mv_hp = sum(diseasechangemat[8,])
  mv_death = sum(diseasechangemat[9,])
  
  nirsevimab_highrisk_case = sum(diseasechangemat[10,])
  nirsevimab_highrisk_hp = sum(diseasechangemat[11,])
  nirsevimab_highrisk_death = sum(diseasechangemat[12,])
  nirsevimab_seasonal_case = sum(diseasechangemat[16,])
  nirsevimab_seasonal_hp = sum(diseasechangemat[17,])
  nirsevimab_seasonal_death = sum(diseasechangemat[18,])
  nirsevimab_seasonal_cu_case = sum(diseasechangemat[13,])
  nirsevimab_seasonal_cu_hp = sum(diseasechangemat[14,])
  nirsevimab_seasonal_cu_death = sum(diseasechangemat[15,])
  nirsevimab_allyear_case = sum(diseasechangemat[16,])
  nirsevimab_allyear_hp = sum(diseasechangemat[17,])
  nirsevimab_allyear_death = sum(diseasechangemat[18,])
  
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
  icer$no_interv_case <- no_interv_case
  icer$no_interv_hp  <- no_interv_hp
  icer$no_interv_death <- no_interv_death
  icer$palivizumab_case <- palivizumab_case
  icer$palivizumab_hp <- palivizumab_hp
  icer$palivizumab_death <- palivizumab_death
  icer$mv_case <- mv_case
  icer$mv_hp <- mv_hp
  icer$mv_death <- mv_death
  
  icer$nirsevimab_highrisk_case <- nirsevimab_highrisk_case 
  icer$nirsevimab_highrisk_hp <- nirsevimab_highrisk_hp
  icer$nirsevimab_highrisk_death <- nirsevimab_highrisk_death
  icer$nirsevimab_seasonal_case <- nirsevimab_seasonal_case
  icer$nirsevimab_seasonal_hp <- nirsevimab_seasonal_hp
  icer$nirsevimab_seasonal_death <- nirsevimab_seasonal_death
  icer$nirsevimab_seasonal_cu_case <- nirsevimab_seasonal_cu_case
  icer$nirsevimab_seasonal_cu_hp <- nirsevimab_seasonal_cu_hp
  icer$nirsevimab_seasonal_cu_death <- nirsevimab_seasonal_cu_death
  icer$nirsevimab_allyear_case <- nirsevimab_allyear_case
  icer$nirsevimab_allyear_hp <- nirsevimab_allyear_hp 
  icer$nirsevimab_allyear_death <- nirsevimab_allyear_death
  
  
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
  
  ##case averted
  diseasechangemat = matrix(0, 18, NMonth)
  diseasechangemat[1,] = disease_status[1,] *discountfactor
  diseasechangemat[2,] = disease_status[2,] *discountfactor
  diseasechangemat[3,] = disease_status[3,] *discountfactor
  diseasechangemat[4,] = disease_status[1,] *discountfactor * parameters$effects[["eff_pal_dinf"]]
  diseasechangemat[5,] = disease_status[2,] *discountfactor * parameters$effects[["eff_pal_hosp"]]
  diseasechangemat[6,] = disease_status[3,] *discountfactor * parameters$effects[["eff_pal_icu"]]
  diseasechangemat[7,] = disease_status[1,] *discountfactor * parameters$effects[["eff_mv_dinf"]]
  diseasechangemat[8,] = disease_status[2,] *discountfactor * parameters$effects[["eff_mv_hosp"]]
  diseasechangemat[9,] = disease_status[3,] *discountfactor * parameters$effects[["eff_mv_icu"]]
  diseasechangemat[10,] = disease_status[1,] *discountfactor * parameters$effects[["eff_lmab_highrisk_dinf"]] 
  diseasechangemat[11,] = disease_status[2,] *discountfactor * parameters$effects[["eff_lmab_highrisk_hosp"]]
  diseasechangemat[12,] = disease_status[3,] *discountfactor * parameters$effects[["eff_lmab_highrisk_icu"]] 
  diseasechangemat[13,] = disease_status[1,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_dinf"]] 
  diseasechangemat[14,] = disease_status[2,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_hosp"]]
  diseasechangemat[15,] = disease_status[3,] *discountfactor * parameters$effects[["eff_lmab_seasonal_cu_icu"]] 
  diseasechangemat[16,] = disease_status[1,] *discountfactor * parameters$effects[["eff_lmab_dinf"]] 
  diseasechangemat[17,] = disease_status[2,] *discountfactor * parameters$effects[["eff_lmab_hosp"]] 
  diseasechangemat[18,] = disease_status[3,] *discountfactor * parameters$effects[["eff_lmab_icu"]] 
  
  
  ##intervention cost
  no_interv_case = sum(diseasechangemat[1,])
  no_interv_hp = sum(diseasechangemat[2,])
  no_interv_death = sum(diseasechangemat[3,])
  
  palivizumab_case = sum(diseasechangemat[4,])
  palivizumab_hp = sum(diseasechangemat[5,])
  palivizumab_death = sum(diseasechangemat[6,])
  
  mv_case = sum(diseasechangemat[7,])
  mv_hp = sum(diseasechangemat[8,])
  mv_death = sum(diseasechangemat[9,])
  
  nirsevimab_highrisk_case = sum(diseasechangemat[10,])
  nirsevimab_highrisk_hp = sum(diseasechangemat[11,])
  nirsevimab_highrisk_death = sum(diseasechangemat[12,])
  nirsevimab_seasonal_case = sum(diseasechangemat[1,])
  nirsevimab_seasonal_hp = sum(diseasechangemat[2,])
  nirsevimab_seasonal_death = sum(diseasechangemat[3,])
  nirsevimab_seasonal_cu_case = sum(diseasechangemat[13,])
  nirsevimab_seasonal_cu_hp = sum(diseasechangemat[14,])
  nirsevimab_seasonal_cu_death = sum(diseasechangemat[15,])
  nirsevimab_allyear_case = sum(diseasechangemat[16,])
  nirsevimab_allyear_hp = sum(diseasechangemat[17,])
  nirsevimab_allyear_death = sum(diseasechangemat[18,])

  
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
  nirsevimab_seasonal_cost = sum(costchangemat[c(10:12),])
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
  icer$no_interv_case <- no_interv_case
  icer$no_interv_hp  <- no_interv_hp
  icer$no_interv_death <- no_interv_death
  icer$palivizumab_case <- palivizumab_case
  icer$palivizumab_hp <- palivizumab_hp
  icer$palivizumab_death <- palivizumab_death
  icer$mv_case <- mv_case
  icer$mv_hp <- mv_hp
  icer$mv_death <- mv_death
  
  icer$nirsevimab_highrisk_case <- nirsevimab_highrisk_case 
  icer$nirsevimab_highrisk_hp <- nirsevimab_highrisk_hp
  icer$nirsevimab_highrisk_death <- nirsevimab_highrisk_death
  icer$nirsevimab_seasonal_case <- nirsevimab_seasonal_case
  icer$nirsevimab_seasonal_hp <- nirsevimab_seasonal_hp
  icer$nirsevimab_seasonal_death <- nirsevimab_seasonal_death
  icer$nirsevimab_seasonal_cu_case <- nirsevimab_seasonal_cu_case
  icer$nirsevimab_seasonal_cu_hp <- nirsevimab_seasonal_cu_hp
  icer$nirsevimab_seasonal_cu_death <- nirsevimab_seasonal_cu_death
  icer$nirsevimab_allyear_case <- nirsevimab_allyear_case
  icer$nirsevimab_allyear_hp <- nirsevimab_allyear_hp 
  icer$nirsevimab_allyear_death <- nirsevimab_allyear_death
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

View(res)


##disease burden
#no intervention
res$no_interv_case
res$no_interv_hp
res$no_interv_death

##palivizumab
res$no_interv_case - res$palivizumab_case
res$no_interv_hp - res$palivizumab_hp
res$no_interv_death - res$palivizumab_death

##MV
res$no_interv_case - res$mv_case
res$no_interv_hp - res$mv_hp
res$no_interv_death - res$mv_death

##Nir high risk
res$no_interv_case - res$nirsevimab_highrisk_case
res$no_interv_hp - res$nirsevimab_highrisk_hp
res$no_interv_death - res$nirsevimab_highrisk_death

##Nir seasonal
res$no_interv_case - res$nirsevimab_seasonal_case
res$no_interv_hp - res$nirsevimab_seasonal_hp
res$no_interv_death - res$nirsevimab_seasonal_death

##Nir seasonal with catch up
res$no_interv_case - res$nirsevimab_seasonal_cu_case
res$no_interv_hp - res$nirsevimab_seasonal_cu_hp
res$no_interv_death - res$nirsevimab_seasonal_cu_death

##Nir all year
res$no_interv_case - res$nirsevimab_allyear_case
res$no_interv_hp - res$nirsevimab_allyear_hp
res$no_interv_death - res$nirsevimab_allyear_death


###cost saved
res$no_interv_cost

##palivizumab
res$no_interv_cost - res$palivizumab_cost
##MV
res$no_interv_cost - res$mv_cost
##Nir high risk
res$no_interv_cost - res$nirsevimab_highrisk_cost
##Nir seasonal
res$no_interv_cost - res$nirsevimab_seasonal_cost
##Nir seasonal with catch up
res$no_interv_cost - res$nirsevimab_seasonal_cu_cost
##Nir all year
res$no_interv_cost - res$nirsevimab_allyear_cost


###QALY gained(number of vaccinees to gain 1 life year extension )
##palivizumab
(res$no_interv_qaly - res$palivizumab_qaly)/(sum(Npop[1,c(1:12)])*12*0.086+sum(Npop[1,c(13:24)])*12*0.022)

##MV
(res$no_interv_qaly - res$mv_qaly)/(sum(Npop[1,c(1:12)])*12)

##Nir high risk
(res$no_interv_qaly - res$nirsevimab_highrisk_qaly)/(sum(Npop[1,c(1:12)])*12*0.086+sum(Npop[1,c(13:24)])*12*0.022)

##Nir seasonal
(res$no_interv_qaly - res$nirsevimab_seasonal_qaly)/(sum(Npop[1,c(1:12)])*7 + sum(Npop[1,c(13:24)])*12*0.022)

##Nir seasonal with catch up
(res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)/(sum(Npop[1,c(1:12)])*12 +sum(Npop[1,c(13:24)])*12*0.022)

##Nir all year
(res$no_interv_qaly - res$nirsevimab_allyear_qaly)/(sum(Npop[1,c(1:12)])*12 +sum(Npop[1,c(13:24)])*12*0.022)

#results store
options(scipen = 999)
results <-data.frame(
          heading = c("no_interv_case","no_interv_hp","no_interv_death","no_interv_cost","no_interv_qaly",
                  "pal_casechange","pal_hpchange","pal_deathchange",
                  "mv_casechange","mv_hpchange","mv_deathchange",
                  "nirhr_casechange","nirhr_hpchange","nirhr_deathchange",
                  "nirs_casechange","nirs_hpchange","nirs_deathchange",
                  "nirscu_casechange","nirscu_hpchange","nirscu_deathchange",
                  "nira_casechange","nira_hpchange","nira_deathchange",
                  "pal_costchange","mv_costchange","nirhr_costchange","nirs_costchange","nirscu_costchange","nira_costchange",
                  "pal_qalychange","mv_qalychange","nirhr_qalychange","nirs_qalychange","nirscu_qalychange","nira_qalychange",
                  "pal_qaly_per1000","mv_qaly_per1000","nirhr_qaly_per1000","nirs_qaly_per1000","nirscu_qaly_per1000","nira_ly_per1000",
                  "pal_ly","mv_ly","nirhr_ly","nirs_ly","nirscu_ly","nira_ly"),
                
  result = c(res$no_interv_case,
             res$no_interv_hp,
             res$no_interv_death,
             res$no_interv_cost,
             res$no_interv_qaly,
    res$no_interv_case - res$palivizumab_case,
    res$no_interv_hp - res$palivizumab_hp,
    res$no_interv_death - res$palivizumab_death,
    res$no_interv_case - res$mv_case,
    res$no_interv_hp - res$mv_hp,
    res$no_interv_death - res$mv_death,
    res$no_interv_case - res$nirsevimab_highrisk_case,
    res$no_interv_hp - res$nirsevimab_highrisk_hp,
    res$no_interv_death - res$nirsevimab_highrisk_death,
    res$no_interv_case - res$nirsevimab_seasonal_case,
    res$no_interv_hp - res$nirsevimab_seasonal_hp,
    res$no_interv_death - res$nirsevimab_seasonal_death,
    res$no_interv_case - res$nirsevimab_seasonal_cu_case,
    res$no_interv_hp - res$nirsevimab_seasonal_cu_hp,
    res$no_interv_death - res$nirsevimab_seasonal_cu_death,
    res$no_interv_case - res$nirsevimab_allyear_case,
    res$no_interv_hp - res$nirsevimab_allyear_hp,
    res$no_interv_death - res$nirsevimab_allyear_death,
  res$no_interv_cost - res$palivizumab_cost,
  res$no_interv_cost - res$mv_cost,
  res$no_interv_cost - res$nirsevimab_highrisk_cost,
  res$no_interv_cost - res$nirsevimab_seasonal_cost,
  res$no_interv_cost - res$nirsevimab_seasonal_cu_cost,
  res$no_interv_cost - res$nirsevimab_allyear_cost,
  res$no_interv_qaly - res$palivizumab_qaly,
  res$no_interv_qaly - res$mv_qaly,
  res$no_interv_qaly - res$nirsevimab_highrisk_qaly,
  res$no_interv_qaly - res$nirsevimab_seasonal_qaly,
  res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly,
  res$no_interv_qaly - res$nirsevimab_allyear_qaly,
  (res$no_interv_qaly - res$palivizumab_qaly)/(sum(Npop[1,c(1:12)])*12*0.086 + sum(Npop[1,c(13:24)])*12*0.22) * 1000,
  (res$no_interv_qaly - res$mv_qaly)/(sum(Npop[1,c(1:12)])*12) * 1000,
  (res$no_interv_qaly - res$nirsevimab_highrisk_qaly)/(sum(Npop[1,c(1:12)])*12*0.086 + sum(Npop[1,c(13:24)])*12*0.22) * 1000,
  (res$no_interv_qaly - res$nirsevimab_seasonal_qaly)/(sum(Npop[1,c(1:12)])*7 + sum(Npop[1,c(12:24)])*12*0.022) * 1000,
  (res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)/(sum(Npop[1,c(1:12)])*12 + sum(Npop[1,c(13:24)])*12*0.022) * 1000,
  (res$no_interv_qaly - res$nirsevimab_allyear_qaly)/(sum(Npop[1,c(1:12)])*12 + sum(Npop[1,c(13:24)])*12*0.022) * 1000,
  1/((res$no_interv_qaly - res$palivizumab_qaly)/(sum(Npop[1,c(1:12)])*12*0.086 + sum(Npop[1,c(13:24)])*12*0.22)),
  1/((res$no_interv_qaly - res$mv_qaly)/(sum(Npop[1,c(1:12)])*12)),
  1/((res$no_interv_qaly - res$nirsevimab_highrisk_qaly)/(sum(Npop[1,c(1:12)])*12*0.086 + sum(Npop[1,c(13:24)])*12*0.22)),
  1/((res$no_interv_qaly - res$nirsevimab_seasonal_qaly)/(sum(Npop[1,c(1:12)])*7 + sum(Npop[1,c(1:12)])*5*0.086 +sum(Npop[1,c(13:24)])*12*0.022)),
  1/((res$no_interv_qaly - res$nirsevimab_seasonal_cu_qaly)/(sum(Npop[1,c(1:12)])*12 +sum(Npop[1,c(13:24)])*12*0.022)),
  1/((res$no_interv_qaly - res$nirsevimab_allyear_qaly)/(sum(Npop[1,c(1:12)])*12 +sum(Npop[1,c(13:24)])*12*0.022))))

write.csv(results,"output/impact.csv")
