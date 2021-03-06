###################
# FLAG FACILITIES #
###################
setwd("~/Documents/data/EHR")
SMR.data <- read.csv("smr.csv")

##PREPROCESSING

#remove missing data, if no pval there is at least one missing year
SMR.data2=SMR.data[is.na(SMR.data$pm)==FALSE,]
SMR.data2=SMR.data2[!SMR.data2$provfs=="Short",]
SMR.data2$provfs <- as.numeric(as.character(SMR.data2$provfs))
#quantile(SMR.data2$pt, .1)
SMR.data2=SMR.data2[SMR.data2$pt>quantile(SMR.data2$pt, .1),]
x <- table(SMR.data2$provfs)
x <- names(x[x==4])
SMR.data2 <- SMR.data2[SMR.data2$provfs %in% x,]

##SETUP

#calculate O-E
SMR.data2$O_E=SMR.data2$dial_drd-SMR.data2$expectda

unique_facility=unique(SMR.data2$provfs)

inf_x=function(x){
  y=x
  for (i in 1:length(x)){
    y[i]=min(x[1:i])
  }
  return(y)
}

theta1=log(2)
theta2=-log(2)

c1=1/theta1-1
c2=-0.5/theta2-1

h <- data.frame(E=c(2,5,10,15,20), h1=c(4.08, 5.34, 6.36, 6.81, 7.25), h2=c(3.00, 4.36, 5.50, 6.10, 6.46))

#initialize data frame
alarm <- data.frame(id=unique_facility, upper_alarm=logical(length(unique_facility)), lower_alarm=logical(length(unique_facility)))


for(i in 1:length(unique_facility)){
  facility=SMR.data2[SMR.data2$provfs==unique_facility[i],]
  h1 <- numeric(4)
  h2 <- numeric(4)
  for(j in 1:4){
    d <- which.min(abs(h$E-facility$expectda[j]))
    h1[j] <- h$h1[d]
    h2[j] <- h$h2[d]
  }
  M1_t_pre <- facility$O_E-c1*facility$expectda
  M2_t_pre <- -facility$O_E+c2*facility$expectda
  M1_t_pre2 <- inf_x(M1_t_pre)
  M2_t_pre2 <- inf_x(M2_t_pre)
  M1_t=M1_t_pre2-facility$O_E+c1*facility$expectda+h1
  M2_t=M2_t_pre2+facility$O_E-c2*facility$expectda+h2
  
  for (j in 1:(length(M1_t)-1)) {
    if (M1_t[j]<0)  for (k in (j+1):length(M1_t)) {
      M1_t_pre2[k]=min(M1_t_pre2[k], -h1/2)
    }
    if (M2_t[j]<0) for (k in (j+1):length(M1_t)) {
      M2_t_pre2[k]=max(M2_t_pre2[k], -h2/2)
    }
  }
  
  M1_t=M1_t_pre2-facility$O_E+c1*facility$expectda+h1
  M2_t=M2_t_pre2+facility$O_E-c2*facility$expectda+h2
  O_E_upper=facility$O_E+M1_t
  O_E_lower=facility$O_E-M2_t
  
  if(TRUE %in% (facility$O_E>O_E_upper)){
    alarm$upper_alarm[i] <- TRUE
  }
  if(TRUE %in% (facility$O_E<O_E_lower)){
    alarm$lower_alarm[i] <- TRUE
  }
}
