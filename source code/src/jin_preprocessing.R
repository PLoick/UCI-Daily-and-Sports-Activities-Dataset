
#sample 1
daily_sport_activity_data_1<-
  daily_sport_activity_data[c(1:125),-c(1:4)]

#find minimum values in each column in sample 1
daily_sport_activity_data_1_min<- 
  apply(daily_sport_activity_data_1,2,min)
#find maximum values in each column in sample 1
daily_sport_activity_data_1_max<- 
  apply(daily_sport_activity_data_1,2,max)
#find mean values in each column in sample 1
daily_sport_activity_data_1_mean<- 
  apply(daily_sport_activity_data_1,2,mean)
# find variance for every 25 rows in each column in sample 1
n<-25
daily_sport_activity_data_1_var<-
  aggregate(daily_sport_activity_data_1,list(rep(1:(nrow(daily_sport_activity_data_1)%/%n+1),each=n,len=nrow(daily_sport_activity_data_1))),var,na.rm=TRUE)[-1]
#find skewness in each column in sample 1
daily_sport_activity_data_1_skew<-skewness(daily_sport_activity_data_1)
#find kurtosis in each column in sample 1
daily_sport_activity_data_1_kurt<-kurtosis(daily_sport_activity_data_1)
# 5 peaks of DFT in each column

daily_sport_activity_data_1_DFT<-apply(daily_sport_activity_data_1,2,fft)
daily_sport_activity_data_1_DFT

# 11 autocorrelation samples in each column
acf(daily_sport_activity_data_1)


# merge in one matrix
daily_sport_activity_data_1_re<-
  rbind(daily_sport_activity_data_1_min,daily_sport_activity_data_1_max,daily_sport_activity_data_1_mean,daily_sport_activity_data_1_var,daily_sport_activity_data_1_skew,daily_sport_activity_data_1_kurt)
rownames(daily_sport_activity_data_1_re)<-c("min","max","mean","var1","var2","var3","var4","var5","skew","kurt")

dim(daily_sport_activity_data_1_re)

v1 = sort(unlist(daily_sport_activity_data_1_re),decreasing=TRUE)

feature1<-matrix(v1,450,1) # 1170




#sample 2
daily_sport_activity_data_2<-
  daily_sport_activity_data[c(126:250),-c(1:4)]

#find minimum values in each column in sample 2
daily_sport_activity_data_2_min<- 
  apply(daily_sport_activity_data_2,2,min)
#find maximum values in each column in sample 2
daily_sport_activity_data_2_max<- 
  apply(daily_sport_activity_data_2,2,max)
#find mean values in each column in sample 2
daily_sport_activity_data_2_mean<- 
  apply(daily_sport_activity_data_2,2,mean)
# find variance for every 25 rows in each column in sample 2
n<-25
daily_sport_activity_data_2_var<-
  aggregate(daily_sport_activity_data_2,list(rep(1:(nrow(daily_sport_activity_data_2)%/%n+1),each=n,len=nrow(daily_sport_activity_data_2))),var,na.rm=TRUE)[-1]
#find skewness in each column in sample 2
daily_sport_activity_data_2_skew<-skewness(daily_sport_activity_data_2)
#find kurtosis in each column in sample 2
daily_sport_activity_data_2_kurt<-kurtosis(daily_sport_activity_data_2)
# 5 peaks of DFT in each column


# 11 autocorrelation samples in each column

# merge in one matrix
daily_sport_activity_data_2_re<-
  rbind(daily_sport_activity_data_2_min,daily_sport_activity_data_2_max,daily_sport_activity_data_2_mean,daily_sport_activity_data_2_var,daily_sport_activity_data_2_skew,daily_sport_activity_data_2_kurt)




#sample 3
daily_sport_activity_data_3<-
  daily_sport_activity_data[c(251:375),-c(1:4)]

#find minimum values in each column in sample 3
daily_sport_activity_data_3_min<- 
  apply(daily_sport_activity_data_3,2,min)
#find maximum values in each column in sample 3
daily_sport_activity_data_3_max<- 
  apply(daily_sport_activity_data_3,2,max)
#find mean values in each column in sample 3
daily_sport_activity_data_3_mean<- 
  apply(daily_sport_activity_data_3,2,mean)
# find variance for every 25 rows in each column in sample 3
n<-25
daily_sport_activity_data_3_var<-
  aggregate(daily_sport_activity_data_3,list(rep(1:(nrow(daily_sport_activity_data_3)%/%n+1),each=n,len=nrow(daily_sport_activity_data_3))),var,na.rm=TRUE)[-1]
#find skewness in each column in sample 3
daily_sport_activity_data_3_skew<-skewness(daily_sport_activity_data_3)
#find kurtosis in each column in sample 3
daily_sport_activity_data_3_kurt<-kurtosis(daily_sport_activity_data_3)
# 5 peaks of DFT in each column


# 11 autocorrelation samples in each column

# merge in one matrix
daily_sport_activity_data_3_re<-
  rbind(daily_sport_activity_data_3_min,daily_sport_activity_data_3_max,daily_sport_activity_data_3_mean,daily_sport_activity_data_3_var,daily_sport_activity_data_3_skew,daily_sport_activity_data_3_kurt)




#sample 4
daily_sport_activity_data_4<-
  daily_sport_activity_data[c(376:500),-c(1:4)]

#find minimum values in each column in sample 4
daily_sport_activity_data_4_min<- 
  apply(daily_sport_activity_data_4,2,min)
#find maximum values in each column in sample 4
daily_sport_activity_data_4_max<- 
  apply(daily_sport_activity_data_4,2,max)
#find mean values in each column in sample 4
daily_sport_activity_data_4_mean<- 
  apply(daily_sport_activity_data_4,2,mean)
# find variance for every 25 rows in each column in sample 4
n<-25
daily_sport_activity_data_4_var<-
  aggregate(daily_sport_activity_data_4,list(rep(1:(nrow(daily_sport_activity_data_4)%/%n+1),each=n,len=nrow(daily_sport_activity_data_4))),var,na.rm=TRUE)[-1]
#find skewness in each column in sample 4
daily_sport_activity_data_4_skew<-skewness(daily_sport_activity_data_4)
#find kurtosis in each column in sample 4
daily_sport_activity_data_4_kurt<-kurtosis(daily_sport_activity_data_4)
# 5 peaks of DFT in each column


# 11 autocorrelation samples in each column



# merge in one matrix
daily_sport_activity_data_4_re<-
  rbind(daily_sport_activity_data_4_min,daily_sport_activity_data_4_max,daily_sport_activity_data_4_mean,daily_sport_activity_data_4_var,daily_sport_activity_data_4_skew,daily_sport_activity_data_4_kurt)




#sample 5
daily_sport_activity_data_5<-
  daily_sport_activity_data[c(501:625),-c(1:4)]

#find minimum values in each column in sample 5
daily_sport_activity_data_5_min<- 
  apply(daily_sport_activity_data_5,2,min)
#find maximum values in each column in sample 5
daily_sport_activity_data_5_max<- 
  apply(daily_sport_activity_data_5,2,max)
#find mean values in each column in sample 5
daily_sport_activity_data_5_mean<- 
  apply(daily_sport_activity_data_5,2,mean)
# find variance for every 25 rows in each column in sample 5
n<-25
daily_sport_activity_data_5_var<-
  aggregate(daily_sport_activity_data_5,list(rep(1:(nrow(daily_sport_activity_data_5)%/%n+1),each=n,len=nrow(daily_sport_activity_data_5))),var,na.rm=TRUE)[-1]
#find skewness in each column in sample 5
daily_sport_activity_data_5_skew<-skewness(daily_sport_activity_data_5)
#find kurtosis in each column in sample 5
daily_sport_activity_data_5_kurt<-kurtosis(daily_sport_activity_data_5)
# 5 peaks of DFT in each column


# 11 autocorrelation samples in each column

# merge in one matrix
daily_sport_activity_data_5_re<-
  rbind(daily_sport_activity_data_5_min,daily_sport_activity_data_5_max,daily_sport_activity_data_5_mean,daily_sport_activity_data_5_var,daily_sport_activity_data_5_skew,daily_sport_activity_data_5_kurt)



#sample 6
daily_sport_activity_data_6<-
  daily_sport_activity_data[c(626:750),-c(1:4)]

#find minimum values in each column in sample 6
daily_sport_activity_data_6_min<- 
  apply(daily_sport_activity_data_6,2,min)
#find maximum values in each column in sample 6
daily_sport_activity_data_6_max<- 
  apply(daily_sport_activity_data_6,2,max)
#find mean values in each column in sample 6
daily_sport_activity_data_6_mean<- 
  apply(daily_sport_activity_data_6,2,mean)
# find variance for every 25 rows in each column in sample 6
n<-25
daily_sport_activity_data_6_var<-
  aggregate(daily_sport_activity_data_6,list(rep(1:(nrow(daily_sport_activity_data_6)%/%n+1),each=n,len=nrow(daily_sport_activity_data_6))),var,na.rm=TRUE)[-1]
#find skewness in each column in sample 6
daily_sport_activity_data_6_skew<-skewness(daily_sport_activity_data_6)
#find kurtosis in each column in sample 6
daily_sport_activity_data_6_kurt<-kurtosis(daily_sport_activity_data_6)
# 5 peaks of DFT in each column


# 11 autocorrelation samples in each column


# merge in one matrix
daily_sport_activity_data_6_re<-
  rbind(daily_sport_activity_data_6_min,daily_sport_activity_data_6_max,daily_sport_activity_data_6_mean,daily_sport_activity_data_6_var,daily_sport_activity_data_6_skew,daily_sport_activity_data_6_kurt)
