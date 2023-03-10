library(dplyr)
library(MASS)
library(matrixcalc)
library(corpcor)
library(psych)

start_year<-2000 #start year - will simulate from this exposure year up until and including 2020
no.records<-1000 #on start year
annual_membership_growth_perc<-15 #percentage annual membership growth per year
claim_rate<- 30 #per thousand

seed_set=180
year<-start_year
datalist = list()

repeat {
  
  set.seed(seed_set)
  
  
  # Correlations between variables are specified below
  # This works best when creating synthetic dataset using copulas with continuous variables 
  # However, in this scenario the resulting data set will not have the correlations specified 
  # below since I am also including categorical variables
  # However this approach does still help create a dataset with somewhat realistic correlations 
  
  adj=1
  
  cor_ab<-0.6/adj   # age & sum.assured
  cor_ac<--0.3/adj  # age & gender
  cor_ad<-0.1/adj   # age & occ
  cor_ae<-1/adj   # age & claim
  cor_af<--0.8/adj  # age & location
  cor_ag<-0.0/adj  # age & smoker status
  
  cor_bc<--0.1/adj  # sum.assured & gender 
  cor_bd<--0.6/adj  # sum.assured & occ
  cor_be<-1/adj  # sum.assured & claim
  cor_bf<--0.8/adj  # sum.assured & location
  cor_bg<-0.0/adj  # sum.assured & smoker status
  
  cor_cd<-0.4/adj   # gender & occ
  cor_ce<--1/adj # gender & claim
  cor_cf<--0.8/adj  # gender & location
  cor_cg<-0.0/adj  # gender & smoker status
  
  cor_de<-1/adj  # occ & claim
  cor_df<-0.8/adj   # occ & location
  cor_dg<-0.0/adj   # occ & smoker status
  
  cor_ef<-1/adj  # claim & location
  cor_eg<-1/adj  # claim & smoker status
  
  cor_fg<-0.0/adj  # location & smoker status
  
  sdev_a<-1
  sdev_b<-1
  sdev_c<-1
  sdev_d<-1
  sdev_e<-1
  sdev_f<-1
  sdev_g<-1
  
  cov_ab<-cor_ab*sdev_a*sdev_b
  cov_ac<-cor_ac*sdev_a*sdev_c
  cov_ad<-cor_ad*sdev_a*sdev_d
  cov_ae<-cor_ae*sdev_a*sdev_e
  cov_af<-cor_af*sdev_a*sdev_f
  cov_ag<-cor_ag*sdev_a*sdev_g
  
  cov_bc<-cor_bc*sdev_b*sdev_c
  cov_bd<-cor_bd*sdev_b*sdev_d
  cov_be<-cor_be*sdev_b*sdev_e
  cov_bf<-cor_bf*sdev_b*sdev_f
  cov_bg<-cor_bg*sdev_b*sdev_g
  
  cov_cd<-cor_cd*sdev_c*sdev_d
  cov_ce<-cor_ce*sdev_c*sdev_e
  cov_cf<-cor_cf*sdev_c*sdev_f
  cov_cg<-cor_cg*sdev_c*sdev_g
  
  cov_de<-cor_de*sdev_d*sdev_e
  cov_df<-cor_df*sdev_d*sdev_f
  cov_dg<-cor_dg*sdev_d*sdev_g
  
  cov_ef<-cor_ef*sdev_e*sdev_f
  cov_eg<-cor_eg*sdev_e*sdev_g
  
  cov_fg<-cor_fg*sdev_f*sdev_g
  
  mean_a<-0
  mean_b<-0
  mean_c<-0
  mean_d<-0
  mean_e<-0
  mean_f<-0
  mean_g<-0
  
  covm<-cbind(c(sdev_a^2,cov_ab,cov_ac,cov_ad,cov_ae,cov_af,cov_ag),
              c(cov_ab,sdev_b^2,cov_bc,cov_bd,cov_be,cov_bf,cov_bg),
              c(cov_ac,cov_bc,sdev_c^2,cov_cd,cov_ce,cov_cf,cov_cg),
              c(cov_ad,cov_bd,cov_cd,sdev_d^2,cov_de,cov_df,cov_dg),
              c(cov_ae,cov_be,cov_ce,cov_de,sdev_e^2,cov_ef,cov_eg),
              c(cov_af,cov_bf,cov_cf,cov_df,cov_ef,sdev_f^2,cov_fg),
              c(cov_ag,cov_bg,cov_cg,cov_dg,cov_eg,cov_fg,sdev_g^2))
  
  if (is.positive.definite(covm)==TRUE) {
    covm_use=covm
  } else {
    covm_use=make.positive.definite(covm)
  }
  
  # 1. Draw any number of variables from a joint normal distribution.
  z <- as.data.frame(mvrnorm(no.records, mu = c(mean_a,mean_b,mean_c,mean_d,mean_e,mean_f,mean_g), 
                             Sigma=covm_use, 
                             empirical = TRUE))
  
  round(cor(z,method='spearman'),2)
  
  # 2. Apply the univariate normal CDF of variables to derive probabilities for each variable.
  pvars.V1 <- pnorm(z$V1)
  pvars.V2 <- pnorm(z$V2)
  pvars.V3 <- pnorm(z$V3)
  pvars.V4 <- pnorm(z$V4)
  pvars.V5 <- pnorm(z$V5)
  pvars.V6 <- pnorm(z$V6)
  pvars.V7 <- pnorm(z$V7)
  
  # 3. Finally apply the inverse CDF of any distribution to simulate draws from that distribution.
  x1<-pvars.V1
  age <- qgamma(pvars.V1,shape=50,scale=0.8)
  yrs_service <- qgamma(pvars.V7,shape=20,scale=0.5)
  
  
  x2<-pvars.V2
  sum.assured <- round(qlnorm(pvars.V2,meanlog=11,sdlog=0.7),-3)
  salary <- sum.assured/2 # sum assured is a multiple of salary 
  
  var1 <- qgamma(pvars.V3,shape=15,scale=0.9)
  var2 <- qgamma(pvars.V4,shape=2,scale=0.5)+20
  var3 <- qgamma(pvars.V3,shape=20,scale=0.1)+357
  var4 <- qgamma(pvars.V6,shape=30,scale=0.3)+9742
  var5 <- qgamma(pvars.V7,shape=75,scale=0.7)+12
  
  #binning normal distribution into categorical variables so not applying inverse to these
  x3 <- pvars.V3
  x4 <- pvars.V4
  x5 <- pvars.V5
  x6 <- pvars.V6
  x7 <- pvars.V7
  df <- as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7))
  
  vo2max <- qgamma(pvars.V3,shape=10,scale=0.7)+22
  bmi <- qgamma(pvars.V1,shape=6,scale=0.4)+26
  
  df<-df%>%mutate(gender=ifelse(df$x3 <=0.6,0,1)) #just two bins, male is 0 / female is 1
  
  
  # cumulative probabilities for each bin - three bins here
  df<-df%>%mutate(occ=cut(df$x4, breaks = c(0,0.35,0.72,1)))
  df<-df%>%mutate(occ=ifelse(df$occ =='(0,0.35]',1,ifelse(df$occ =='(0.35,0.72]',2,3)))
  
  df<-df%>%mutate(claim=ifelse(df$x5 <=(1-claim_rate/1000),0,1)) #just two bins, no claim last year is 0 / yes is 1
  df<-df%>%mutate(smoker_status=ifelse(df$x5 <=0.8,0,1)) #just two bins, NS is 0 / S is 1 (using x5 so will be highly correlated with claims)
  df<-df%>%mutate(compulsory=ifelse(df$x3 <=0.4,0,1))
  
  # cumulative probabilities for each bin - three bins here
  df<-df%>%mutate(location=cut(df$x6, breaks = c(0,0.15,0.65,1)))
  df<-df%>%mutate(location=ifelse(df$location =='(0,0.15]',1,ifelse(df$location =='(0.15,0.65]',2,3)))
  
  df2<-as.data.frame(cbind(df$x1,age,df$x2,sum.assured,salary,df$x3,df$gender,df$x4,df$occ,df$x5,df$claim,df$x6,df$location,df$smoker_status,df$compulsory,yrs_service,vo2max,bmi,var1,var2,var3,var4,var5))%>%
    rename(x1=V1,
           x2=V3,
           x3=V6,
           gender=V7,
           x4=V8,
           occ=V9,
           x5=V10,
           claim=V11,
           x6=V12,
           location=V13,
           smoker_status=V14,
           compulsory=V15)
  
  mcor<-round(cor(df2),2)
  
  lower<-mcor
  lower[lower.tri(mcor)]<-""
  lower<-as.data.frame(lower)
  lower
  
  # we see that the correlations work better when we have more categories since 
  # this becomes more like a continuous distribution
  
  
  df3<- dplyr::select(df2,age,sum.assured,salary,gender,occ,claim,location,smoker_status,compulsory, yrs_service,vo2max,bmi,var1,var2,var3,var4,var5)
  
  mcor<-round(cor(df3),2)
  
  lower<-mcor
  lower[lower.tri(mcor)]<-""
  lower<-as.data.frame(lower)
  lower
  
  # changing 1/0 indicators to more meaningful indicators
  df4<-df3%>%mutate(gender=ifelse(df3$gender ==0,"M","F"),
                    location=ifelse(df3$location ==1,"site1",ifelse(df3$location ==2,"site2","site3")))
  
  df5<-df4%>%mutate(
    occ.desc=case_when(
      occ==1~"job1",
      occ==2~"job2",
      occ==3~"job3"
    )
  )
  
  df6<-df5%>%mutate(
    claim=case_when(
      claim==0~"N",
      claim==1~"Y"
    )
  )
  
  df6<-df6%>%
    mutate(exposure_year=year)
  
  datalist[[year]] <- df6 # add it to your list
  
  year<-year+1
  seed_set=seed_set+1
  no.records<-round(no.records*(1+annual_membership_growth_perc/100),0)
  
  if (year == 2021){
    break
  }
  
}

##### Write data to csv ####

insurance_data<-data.table::rbindlist(datalist)

wd<-getwd()
write.csv(insurance_data,paste0(wd,"/Life_Insurance_data.csv"))