library(ggplot2)
library(scales)
library(dplyr)
options(scipen = 999)
rm(list = ls())
df <- readRDS("input/df.rds")
##CEplane


graph_PSA <- ggplot(df) +
  geom_point(aes(x = pal_no_qaly, y = pal_no_cost,color = "1 sa-mAb for the high-risk group infants")) +
  geom_point(aes(x = mv_no_qaly, y = mv_no_cost, color = "2 MV for all pregnant women")) +
  geom_point(aes(x = lmabhr_no_qaly, y = lmabhr_no_cost, color = "3 la-mAb for the high-risk group infants")) +
  geom_point(aes(x = lmabs_no_qaly, y = lmabs_no_cost, color = "4 la-mAb seasonal administration to infants \nwho were born between September and March\n")) +
  geom_point(aes(x = lmabs_cu_no_qaly, y = lmabs_cu_no_cost, color = "5 la-mAb seasonal administration to infants \nwho were born between September and March along with \ncatch-up administration in September for infants \nwho were born between April and August")) +
  geom_point(aes(x = lmaba_no_qaly, y = lmaba_no_cost, color = "6 la-mAb year-round administration to infants")) +
  labs(title = "Cost-effectiveness plane: Intervention vs No intervention",
       x = "QALY difference",
       y = "Cost difference") +
  scale_colour_manual(name="Intervention",
                      values=c("1 sa-mAb for the high-risk group infants"="black",
                               "2 MV for all pregnant women"="blue",
                               "3 la-mAb for the high-risk group infants" = "orange",
                               "4 la-mAb seasonal administration to infants \nwho were born between September and March\n" = "purple",
                               "5 la-mAb seasonal administration to infants \nwho were born between September and March along with \ncatch-up administration in September for infants \nwho were born between April and August" = "green",
                               "6 la-mAb year-round administration to infants" = "red")) +
  theme_bw() +
  theme(legend.position = c(0.08,0.99),
        legend.justification = c("left", "top"),
        legend.background = element_rect( fill = alpha("white",1),
                                          color = "grey80"),
        legend.key = element_rect(fill = "white", color = NA)) +
  geom_abline(slope = 0, intercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_abline(slope = 5000000, intercept = 0, linetype = "dashed", color = "blue") +
  scale_x_continuous(limits = c(-70, 3500)) + 
  scale_y_continuous(breaks = c(0, 1e11, 2e11, 3e11, 4e11, 5e11), 
                     labels = c("0", "10^11", "2x10^11", "3x10^11", "4x10^11", "5x10^11"))
graph_PSA
ggsave(plot=graph_PSA,"output/graph_psa.jpg", width = 8, height = 6, dpi = 600)


##maximum cost-effectiveness price x-axis
##probablity of cost effectiveness in Y axis
df1 <- readRDS("output/df1.rds")
df2 <- readRDS("output/df2.rds")
df5 <- readRDS("output/df5.rds")
df8 <- readRDS("output/df8.rds")
df9 <- readRDS("output/df9.rds")
df10 <- readRDS("output/df10.rds")
df11 <- readRDS("output/df11.rds")
df12 <- readRDS("output/df12.rds")
df13 <- readRDS("output/df13.rds")
df14 <- readRDS("output/df14.rds")
df15 <- readRDS("output/df15.rds")
df16 <- readRDS("output/df16.rds")
df17 <- readRDS("output/df17.rds")
df18 <- readRDS("output/df18.rds")
df19 <- readRDS("output/df19.rds")
df20 <- readRDS("output/df20.rds")
df25 <- readRDS("output/df25.rds")

NSIM=10000
icer_threshold = 5000000

p <- function(df) {
df <- df %>% mutate(pal_icer = ifelse(pal_no_cost/pal_no_qaly > icer_threshold, 0,1),
                    mv_icer = ifelse(mv_no_cost/mv_no_qaly > icer_threshold, 0,1),
                    lmabhr_icer = ifelse(lmabhr_no_cost/lmabhr_no_qaly > icer_threshold, 0,1),
                    lmabs_icer = ifelse(lmabs_no_cost/lmabs_no_qaly > icer_threshold, 0,1),
                    lmabs_cu_icer = ifelse(lmabs_cu_no_cost/lmabs_cu_no_qaly > icer_threshold,0, 1),
                    lmaba_icer = ifelse(lmaba_no_cost/lmaba_no_qaly > icer_threshold, 0, 1))

 return(c(sum(df$pal_icer)/NSIM,sum(df$mv_icer)/NSIM,sum(df$lmabhr_icer)/NSIM,sum(df$lmabs_icer)/NSIM,sum(df$lmabs_cu_icer)/NSIM,sum(df$lmaba_icer)/NSIM))
}
p(df1)
MCEP <- c(1000,2000,5000,seq(8000,20000, by=1000),25000)

prob_ce <- c(p(df1),p(df2),p(df5),p(df8),p(df9),p(df10),p(df11),p(df12),p(df13),p(df14),p(df15),p(df16),p(df17),p(df18),p(df19),p(df20),p(df25))
unitprice <- c(rep(1000,6),rep(2000,6),rep(6000,6), rep(8000,6),rep(9000,6), rep(10000,6),rep(11000,6),rep(12000,6),rep(13000,6),rep(14000,6), rep(15000,6),
             rep(16000,6),rep(17000,6),rep(18000,6),rep(19000,6),rep(20000,6),rep(25000,6))
group <- rep(c("1 sa-mAb for the high-risk group infants","2 MV for all pregnant women",
               "3 la-mAb for the high-risk group infants",
               "4 la-mAb seasonal administration to infants \nwho were born between September and March\n",
               "5 la-mAb seasonal administration to infants \nwho were born between September and March along with \ncatch-up administration in September for infants \nwho were born between April and August",
               "6 la-mAb year-round administration to infants"),17)
prob_ce <- cbind(unitprice, group, prob_ce)
prob_ce <- as.data.frame(prob_ce)
colnames(prob_ce) <- c("price", "intervention","p")
prob_ce[, "price"] <- as.numeric(prob_ce[, "price"])
prob_ce[, "p"] <- as.numeric(prob_ce[, "p"])

g_probability <- ggplot(data = prob_ce %>% filter(group !="1 Palivizumab"), aes(x = price, y = p, group= intervention, colour = intervention)) +
  geom_line(linewidth = 1.0) +
  labs(x = "unit price of the product(JPY)", y ="probability of being cost-effectiveness", color = "Intervention")+
  scale_colour_manual(values=c( #"1 sa-mAb for the high-risk group infants"="black",
                               "2 MV for all pregnant women"="blue",
                               "3 la-mAb for the high-risk group infants" = "orange",
                               "4 la-mAb seasonal administration to infants \nwho were born between September and March\n" = "purple",
                               "5 la-mAb seasonal administration to infants \nwho were born between September and March along with \ncatch-up administration in September for infants \nwho were born between April and August" = "green",
                               "6 la-mAb year-round administration to infants" = "red"),
                      labels =c("2 MV for all pregnant women","3 la-mAb for the high-risk group infants",
                                "4 la-mAb seasonal administration to infants \nwho were born between September and March\n",
                                "5 la-mAb seasonal administration to infants \nwho were born between September and March along with \ncatch-up administration in September for infants \nwho were born between April and August",
                                "6 la-mAb year-round administration to infants")) +
  theme_bw() +
  theme(legend.position = c(0.05,0.9),
        legend.justification = c("left", "top"),
        legend.background = element_rect( fill = alpha("white",0.6),
                                          color = "grey80"),
        legend.key = element_rect(fill = "white", color = NA)) 

g_probability
ggsave("output/mcep.jpg", plot =g_probability, width = 8, height = 6, dpi = 600)


##CEAC
#Create Threshold Vector
t = 250
Threshold <- seq(0,t*10^6, by=1*10^6)
NSIM=10000

#Create Dataframe

CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
#For loop for Incremental Net benefits Calculation
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost  
}


#ProbCE Calculations
prob_ce_mv <- vector()
for(i in 1:t+1){
  prob_ce_mv[i] <- sum(CEAC[,i]>=0)/NSIM 
}


CEAC<-matrix(NA,nrow=NSIM,ncol=t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$pal_no_qaly - df$pal_no_cost  
}
prob_ce_pal <- vector()
for(i in 1:t+1){
  prob_ce_pal[i] <- sum(CEAC[,i]>=0)/NSIM   
}


CEAC<-matrix(NA,nrow=NSIM,ncol=t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$lmabhr_no_qaly - df$lmabhr_no_cost  
}
prob_ce_lmabhr <- vector()
for(i in 1:t+1){
  prob_ce_lmabhr[i] <- sum(CEAC[,i]>=0)/NSIM   
}


CEAC<-matrix(NA,nrow=NSIM,ncol=t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$lmabs_no_qaly - df$lmabs_no_cost  
}
prob_ce_lmabs <- vector()
for(i in 1:t+1){
  prob_ce_lmabs[i] <- sum(CEAC[,i]>=0)/NSIM   
}


CEAC<-matrix(NA,nrow=NSIM,ncol=t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$lmabs_cu_no_qaly - df$lmabs_cu_no_cost  
}
prob_ce_lmabs_cu <- vector()
for(i in 1:t+1){
  prob_ce_lmabs_cu[i] <- sum(CEAC[,i]>=0)/NSIM   
}



CEAC<-matrix(NA,nrow=NSIM,ncol=t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$lmaba_no_qaly - df$lmaba_no_cost  
}
prob_ce_lmaba <- vector()
for(i in 1:t+1){
  prob_ce_lmaba[i] <- sum(CEAC[,i]>=0)/NSIM   
}


#CEAC Dataframe
CEAC_Data <- data.frame(Threshold,prob_ce_mv, prob_ce_pal, prob_ce_lmabhr,prob_ce_lmabs, prob_ce_lmabs_cu, prob_ce_lmaba)
CEAC_Data

#Plot CEAC
ceac <- ggplot(CEAC_Data)+
  geom_line( aes(x=Threshold,y=prob_ce_mv),color="blue",linewidth=1.0)+
  geom_line( aes(x=Threshold,y=prob_ce_pal),color="black",linewidth=1.0)+
  geom_line( aes(x=Threshold,y=prob_ce_lmabhr),color="orange",linewidth=1.0)+
  geom_line( aes(x=Threshold,y=prob_ce_lmabs),color="purple",linewidth=1.0)+
  geom_line( aes(x=Threshold,y=prob_ce_lmabs_cu),color="green",linewidth=1.0)+
  geom_line( aes(x=Threshold,y=prob_ce_lmaba),color="red",linewidth=1.0)+
  ylim(0,1) +
  xlab("Willingness to Pay per QALY")+
  ylab("Probability of Cost-Effectiveness")+
  ggtitle("The Cost Effectiveness Acceptability Curve")+
  geom_vline(xintercept = 5000000, linetype = "dashed", color = "black") +
  #  geom_hline(yintercept = 0.069, linetype ="dashed", color ="black") +
  theme_bw()
ceac
ggsave("output/ceac.jpg", plot = ceac, width = 8, height = 6, dpi = 300)


#####################
##################### not used
#changing the price of MV, focusing on maternal vaccine
df <- df %>% mutate(mv_no_cost0 = (mv_cost + 10000) - no_interv_cost,
                    mv_no_cost1 = (mv_cost + 11000) - no_interv_cost,
                    mv_no_cost2 = (mv_cost + 12000) - no_interv_cost,
                    mv_no_cost3 = (mv_cost + 13000) - no_interv_cost,
                    mv_no_cost4 = (mv_cost + 14000) - no_interv_cost,
                    mv_no_cost5 = (mv_cost + 15000) - no_interv_cost,
                    mv_no_cost6 = (mv_cost + 16000) - no_interv_cost,
                    mv_no_cost7 = (mv_cost + 17000) - no_interv_cost,
                    mv_no_cost8 = (mv_cost + 18000) - no_interv_cost,
                    mv_no_cost9 = (mv_cost + 19000) - no_interv_cost,
                    mv_no_cost10 = (mv_cost + 20000) - no_interv_cost)

t = 150
Threshold <- seq(0, t*10^5, by=1*10^5)
NSIM=10000

#Create Dataframe

CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost0  
}
prob_ce_mv0 <- vector()
for(i in 1:t+1){
  prob_ce_mv0[i] <- sum(CEAC[,i]>=0)/NSIM 
}


CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost1
}
prob_ce_mv1 <- vector()
for(i in 1:t+1){
  prob_ce_mv1[i] <- sum(CEAC[,i]>=0)/NSIM 
}


CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost2
}
prob_ce_mv2 <- vector()
for(i in 1:t+1){
  prob_ce_mv2[i] <- sum(CEAC[,i]>=0)/NSIM 
}

CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost3
}
prob_ce_mv3 <- vector()
for(i in 1:t+1){
  prob_ce_mv3[i] <- sum(CEAC[,i]>=0)/NSIM 
}

CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost4
}
prob_ce_mv4 <- vector()
for(i in 1:t+1){
  prob_ce_mv4[i] <- sum(CEAC[,i]>=0)/NSIM 
}


CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost5 
}
prob_ce_mv5 <- vector()
for(i in 1:t+1){
  prob_ce_mv5[i] <- sum(CEAC[,i]>=0)/NSIM 
}


CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost6 
}
prob_ce_mv6 <- vector()
for(i in 1:t+1){
  prob_ce_mv6[i] <- sum(CEAC[,i]>=0)/NSIM 
}


CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost7
}
prob_ce_mv7 <- vector()
for(i in 1:t+1){
  prob_ce_mv7[i] <- sum(CEAC[,i]>=0)/NSIM 
}

CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost8
}
prob_ce_mv8 <- vector()
for(i in 1:t+1){
  prob_ce_mv8[i] <- sum(CEAC[,i]>=0)/NSIM 
}


CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost9
}
prob_ce_mv9 <- vector()
for(i in 1:t+1){
  prob_ce_mv9[i] <- sum(CEAC[,i]>=0)/NSIM 
}


CEAC<-matrix(NA,nrow=NSIM,ncol= t+1)
for(i in 1:t+1){
  CEAC[,i]<-Threshold[i]*df$mv_no_qaly - df$mv_no_cost10
}
prob_ce_mv10 <- vector()
for(i in 1:t+1){
  prob_ce_mv10[i] <- sum(CEAC[,i]>=0)/NSIM 
}


CEAC_Data <- data.frame(Threshold,prob_ce_mv0, prob_ce_mv1,prob_ce_mv2, prob_ce_mv3, prob_ce_mv4, prob_ce_mv5, prob_ce_mv6,
                        prob_ce_mv7, prob_ce_mv8, prob_ce_mv9, prob_ce_mv10)
CEAC_Data

#Plot CEAC
ceac <- ggplot(CEAC_Data)+
  geom_line( aes(x=Threshold,y=prob_ce_mv0 ,color="6 MV price 30,000 JPY (current price)"),linewidth=1.0, show.legend = TRUE)+
  geom_line( aes(x=Threshold,y=prob_ce_mv1,color="1 MV price 5,000 JPY"),linewidth=1.0, show.legend = TRUE)+
  geom_line( aes(x=Threshold,y=prob_ce_mv2,color="2 MV price 10,000 JPY"),linewidth=1.0, show.legend = TRUE)+
  geom_line( aes(x=Threshold,y=prob_ce_mv3,color="3 MV price 15,000 JPY"),linewidth=1.0, show.legend = TRUE)+
  geom_line( aes(x=Threshold,y=prob_ce_mv4,color="4 MV price 20,000 JPY"),linewidth=1.0, show.legend = TRUE)+
  geom_line( aes(x=Threshold,y=prob_ce_mv5,color="5 MV price 25,000 JPY"),linewidth=1.0, show.legend = TRUE)+
  geom_line( aes(x=Threshold,y=prob_ce_mv6,color="7 MV price 50,000 JPY"),linewidth=1.0, show.legend = TRUE)+
  ylim(0,1)+
  xlab("Willingness to Pay per QALY")+
  ylab("Probability of Cost-Effectiveness")+
  ggtitle("The Cost Effectiveness Acceptability Curve")+
  geom_vline(xintercept = 5000000, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-100, 15000000)) +
  scale_colour_manual(name="Intervention",
                      values=c("6 MV price 30,000 JPY (current price)"="blue",
                               "1 MV price 5,000 JPY"="grey40",
                               "2 MV price 10,000 JPY" = "grey60",
                               "3 MV price 15,000 JPY" = "lightblue",
                               "4 MV price 20,000 JPY" = "lightblue1",
                               "5 MV price 25,000 JPY" = "skyblue",
                               "7 MV price 50,000 JPY" = "purple")) +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.95,0.05),
        legend.justification = c("right", "bottom"),
        legend.background = element_rect( fill = alpha("white",0.6),
                                          color = "grey80"),
        legend.key = element_rect(fill = "white", color = NA)) 
ceac
ggsave("output/ceac_mv.jpg", plot = ceac, width = 6, height = 6, dpi = 300)
