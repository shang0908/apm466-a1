#import data
data <- read.csv("/Users/shangyangle/Desktop/dataset.csv")
#close price
close_price_data <- rbind(data$X1.10.2022,data$X1.11.2022,data$X1.12.2022,data$X1.13.2022,data$X1.14.2022,
                          data$X1.17.2022,data$X1.18.2022,data$X1.19.2022,data$X1.20.2022,data$X1.21.2022)
#recorded 10 dates
recorded_date <- c("2022-01-10","2022-01-11","2022-01-12","2022-01-13","2022-01-14","2022-01-17","2022-01-18",
                   "2022-01-19","2022-01-20","2022-01-21")
#maturity dates for each 10 bonds
maturity_date <- c("2023-06-01","2024-06-01","2022-03-01","2022-06-01","2023-06-01","2022-06-01","2025-06-01",
                   "2026-06-01","2027-06-01","2028-05-31")
#convert coupon_payment into numeric number without percentage
coupon_payment <- as.numeric(sub("%","",data$coupon))/100
library("jrvFinance")
#calculate yield to maturity
ytm <- matrix(nrow=10, ncol=10)
for (j in c(1:10)){
  close_price = close_price_data[,j]
  for (i in c(1:10)){
    ytm[i,j] = bond.yield(settle=recorded_date[i],mature=maturity_date[j],coupon=coupon_payment[j],freq=2,price=close_price[i],comp.freq=2)
  }
}
ytm
#calculate time to maturity
time_to_maturity <- matrix(nrow=10,ncol=10)
for (i in c(1:10)){
  for (j in c(1:10)){
    time_to_maturity[i,j] = yearFraction(d1=recorded_date[i],d2=maturity_date[j],freq=2)
  }
}
#calculate dirty price
dirty_price <- matrix(nrow=10,ncol=10)
for (i in c(1:10)){
  for (j in c(1:10)){
    dirty_price[i,j]=bond.TCF(settle=recorded_date[i],mature=maturity_date[j],coupon=coupon_payment[j],freq=2)$accrued+
      close_price_data[i,j]
  }
}
#plot 5-year yield curve
color <- c("red","orange","plum","yellow","green","blue","purple","pink","rosybrown","khaki")
year <- c(1/2,1,3/2,2,5/2,3,7/2,4,9/2,5)
plot(year,ytm[1,],xlab="number of year",ylab="YTM",ylim=c(0.0023,0.03),main="5-year yield curve",col="red",type="b")+
  lines(year,ytm[2,],col="orange",type="o")+
  lines(year,ytm[3,],col="plum",type="o")+
  lines(year,ytm[4,],col="yellow",type="o")+
  lines(year,ytm[5,],col="green",type="o")+
  lines(year,ytm[6,],col="blue",type="o")+
  lines(year,ytm[7,],col="purple",type="o")+
  lines(year,ytm[8,],col="pink",type="o")+
  lines(year,ytm[9,],col="rosybrown",type="o")+
  lines(year,ytm[10,],col="khaki",type="o")
  legend("topleft",legend=recorded_date,lty=c(1,1,1,1,1,1,1,1,1,1),col=color,cex=0.4)
#calculate bond1 spot rate
s1_1 <- log(100.8695/100.015)/1.391667
s1_2 <- log(100.8537/100.015)/1.388889+s1_1
s1_3 <- log(100.8488/100.015)/1.386111+s1_2
s1_4 <- log(100.8120/100.015)/1.383333+s1_3
s1_5 <- log(100.7792/100.015)/1.380556+s1_4
s1_6 <- log(100.6767/100.015)/1.372222+s1_5
s1_7 <- log(100.6278/100.015)/1.369444+s1_6
s1_8 <- log(100.6360/100.015)/1.366667+s1_7
s1_9 <- log(100.6472/100.015)/1.363889+s1_8
s1_10 <- log(100.6963/100.015)/1.361111+s1_9
s1 <- c(s1_1,s1_2,s1_3,s1_4,s1_5,s1_6,s1_7,s1_8,s1_9,s1_10)
#calculate bond2 spot rate
s2_1 <- log(103.1958/100.025)/2.391667
s2_2 <- log(103.1378/100.025)/2.388889+s2_1
s2_3 <- log(103.1347/100.025)/2.386111+s2_2
s2_4 <- log(103.0917/100.025)/2.383333+s2_3
s2_5 <- log(103.0786/100.025)/2.380556+s2_4
s2_6 <- log(102.8744/100.025)/2.372222+s2_5
s2_7 <- log(102.8114/100.025)/2.369444+s2_6
s2_8 <- log(102.7533/100.025)/2.366667+s2_7
s2_9 <- log(102.7903/100.025)/2.363889+s2_8
s2_10 <- log(102.8772/100.025)/2.361111+s2_9
s2 <- c(s2_1,s2_2,s2_3,s2_4,s2_5,s2_6,s2_7,s2_8,s2_9,s2_10) 
#calculate bond3 spot rate
s3_1 <- log(100.2172/100.005)/0.1416667
s3_2 <- log(100.2166/100.005)/0.1388889+s3_1
s3_3 <- log(100.2139/100.005)/0.1361111+s3_2
s3_4 <- log(100.2113/100.005)/0.1333333+s3_3
s3_5 <- log(100.2097/100.005)/0.1305556+s3_4
s3_6 <- log(100.2039/100.005)/0.1222222+s3_5
s3_7 <- log(100.2003/100.005)/0.1194444+s3_6
s3_8 <- log(100.2027/100.005)/0.1166667+s3_7
s3_9 <- log(100.2041/100.005)/0.1138889+s3_8
s3_10 <- log(100.2084/100.005)/0.1111111+s3_9
s3 <- c(s3_1,s3_2,s3_3,s3_4,s3_5,s3_6,s3_7,s3_8,s3_9,s3_10)
#calculate bond4 spot rate
s4_1 <- log(104.3971/100.0925)/0.3916667
s4_2 <- log(104.3928/100.0925)/0.3888889+s4_1
s4_3 <- log(104.3875/100.0925)/0.3861111+s4_2
s4_4 <- log(104.3352/100.0925)/0.3833333+s4_3
s4_5 <- log(104.3259/100.0925)/0.3805556+s4_4
s4_6 <- log(104.3499/100.0925)/0.3722222+s4_5
s4_7 <- log(104.3376/100.0925)/0.3694444+s4_6
s4_8 <- log(104.3383/100.0925)/0.3666667+s4_7
s4_9 <- log(104.2970/100.0925)/0.3638889+s4_8
s4_10 <- log(104.3127/100.0925)/0.3611111+s4_9
s4 <- c(s4_1,s4_2,s4_3,s4_4,s4_5,s4_6,s4_7,s4_8,s4_9,s4_10)  
#calculate bond5 spot rate
s5_1 <- log(110.4957/100.08)/1.391667
s5_2 <- log(110.4809/100.08)/1.388889+s5_1
s5_3 <- log(110.4591/100.08)/1.386111+s5_2
s5_4 <- log(110.4023/100.08)/1.383333+s5_3
s5_5 <- log(110.3766/100.08)/1.380556+s5_4
s5_6 <- log(110.3002/100.08)/1.372222+s5_5
s5_7 <- log(110.2454/100.08)/1.369444+s5_6
s5_8 <- log(110.2557/100.08)/1.366667+s5_7
s5_9 <- log(110.2209/100.08)/1.363889+s5_8
s5_10 <- log(110.2921/100.08)/1.361111+s5_9
s5 <- c(s5_1,s5_2,s5_3,s5_4,s5_5,s5_6,s5_7,s5_8,s5_9,s5_10)
#calculate bond6 spot rate
s6_1 <- log(101.0549/100.0275)/0.3916667
s6_2 <- log(101.0476/100.0275)/0.3888889+s6_1
s6_3 <- log(101.0472/100.0275)/0.3861111+s6_2
s6_4 <- log(101.0228/100.0275)/0.3833333+s6_3
s6_5 <- log(101.0175/100.0275)/0.3805556+s6_4
s6_6 <- log(101.0024/100.0275)/0.3722222+s6_5
s6_7 <- log(100.9990/100.0275)/0.3694444+s6_6
s6_8 <- log(100.9837/100.0275)/0.3666667+s6_7
s6_9 <- log(100.9873/100.0275)/0.3638889+s6_8
s6_10 <- log(101.0089/100.0275)/0.3611111+s6_9
s6 <-c(s6_1,s6_2,s6_3,s6_4,s6_5,s6_6,s6_7,s6_8,s6_9,s6_10)
#calculate bond7 spot rate
s7_1 <- log(103.1638/100.0225)/3.391667
s7_2 <- log(103.2000/100.0225)/3.388889+s7_1
s7_3 <- log(103.1612/100.0225)/3.386111+s7_2
s7_4 <- log(103.1675/100.0225)/3.383333+s7_3
s7_5 <- log(103.0787/100.0225)/3.380556+s7_4
s7_6 <- log(102.8225/100.0225)/3.372222+s7_5
s7_7 <- log(102.6488/100.0225)/3.369444+s7_6
s7_8 <- log(102.6050/100.0225)/3.366667+s7_7
s7_9 <- log(102.6013/100.0225)/3.363889+s7_8
s7_10 <- log(102.7825/100.0225)/3.361111+s7_9
s7 <-c(s7_1,s7_2,s7_3,s7_4,s7_5,s7_6,s7_7,s7_8,s7_9,s7_10)
#calculate bond8 spot rate
s8_1 <- log(100.29750/100.015)/4.391667
s8_2 <- log(100.34167/100.015)/4.388889+s8_1
s8_3 <- log(100.31083/100.015)/4.386111+s8_2
s8_4 <- log(100.33000/100.015)/4.383333+s8_3
s8_5 <- log(100.21917/100.015)/4.380556+s8_4
s8_6 <- -log(99.90167/100.015)/4.372222+s8_5
s8_7 <- -log(99.74083/100.015)/4.369444+s8_6
s8_8 <- -log(99.63500/100.015)/4.366667+s8_7
s8_9 <- -log(99.64417/100.015)/4.363889+s8_8
s8_10 <- -log(99.88333/100.015)/4.361111+s8_9
s8 <-c(s8_1,s8_2,s8_3,s8_4,s8_5,s8_6,s8_7,s8_8,s8_9,s8_10)
#calculate bond9 spot rate
s9_1 <- -log(97.40333/100.01)/5.391667
s9_2 <- -log(97.45111/100.01)/5.388889+s9_1
s9_3 <- -log(97.41889/100.01)/5.386111+s9_2
s9_4 <- -log(97.45167/100.01)/5.383333+s9_3
s9_5 <- -log(97.32944/100.01)/5.380556+s9_4
s9_6 <- -log(96.94278/100.01)/5.372222+s9_5
s9_7 <- -log(96.70056/100.01)/5.369444+s9_6
s9_8 <- -log(96.62333/100.01)/5.366667+s9_7
s9_9 <- -log(96.62611/100.01)/5.363889+s9_8
s9_10 <- -log(96.93389/100.01)/5.361111+s9_9
s9 <-c(s9_1,s9_2,s9_3,s9_4,s9_5,s9_6,s9_7,s9_8,s9_9,s9_10)
#calculate bond10 spot rate
s10_1 <- log(102.7672/100.02)/6.391667
s10_2 <- log(102.8228/100.02)/6.388889+s10_1
s10_3 <- log(102.7683/100.02)/6.386111+s10_2
s10_4 <- log(102.8039/100.02)/6.383333+s10_3
s10_5 <- log(102.6644/100.02)/6.380556+s10_4
s10_6 <- log(102.2011/100.02)/6.372222+s10_5
s10_7 <- log(101.9017/100.02)/6.369444+s10_6
s10_8 <- log(101.8272/100.02)/6.366667+s10_7
s10_9 <- log(101.9128/100.02)/6.363889+s10_8
s10_10 <- log(102.2933/100.02)/6.361111+s10_9
s10 <-c(s10_1,s10_2,s10_3,s10_4,s10_5,s10_6,s10_7,s10_8,s10_9,s10_10)
#plotthe spot curve
color <- c("red","orange","plum","yellow","green","blue","purple","pink","rosybrown","khaki")
year <- c(1/2,1,3/2,2,5/2,3,7/2,4,9/2,5)
plot(year,s1, xlab="number of year", ylab="spot rate",main="the spot curve",ylim=c(0.0042,1.5),col="red", type="b")+
  lines(year,s2,col="orange",type="o")+
  lines(year,s3,col="plum",type="o")+
  lines(year,s4,col="yellow",type="o")+
  lines(year,s5,col="green",type="o")+
  lines(year,s6,col="blue",type="o")+
  lines(year,s7,col="purple",type="o")+
  lines(year,s8,col="pink",type="o")+
  lines(year,s9,col="rosybrown",type="o")+
  lines(year,s10,col="khaki",type="o")
  legend("topleft", legend=recorded_date,lty=c(1,1,1,1,1,1,1,1,1,1),col=color,cex=0.7)
#calculate 10 bonds forward rate on each day 
f1 <- c(s1_2-s1_1,s1_3-s1_2,s1_4-s1_3,s1_5-s1_4,s1_6-s1_5,s1_7-s1_6,s1_8-s1_7,s1_9-s1_8,s1_10-s1_9)
f2 <- c(s2_2-s2_1,s2_3-s2_2,s2_4-s2_3,s2_5-s2_4,s2_6-s2_5,s2_7-s2_6,s2_8-s2_7,s2_9-s2_8,s2_10-s2_9)
f3 <- c(s3_2-s3_1,s3_3-s3_2,s3_4-s3_3,s3_5-s3_4,s3_6-s3_5,s3_7-s3_6,s3_8-s3_7,s3_9-s3_8,s3_10-s3_9)
f4 <- c(s4_2-s4_1,s4_3-s4_2,s4_4-s4_3,s4_5-s4_4,s4_6-s4_5,s4_7-s4_6,s4_8-s4_7,s4_9-s4_8,s4_10-s4_9)
f5 <- c(s5_2-s5_1,s5_3-s5_2,s5_4-s5_3,s5_5-s5_4,s5_6-s5_5,s5_7-s5_6,s5_8-s5_7,s5_9-s5_8,s5_10-s5_9)
f6 <- c(s6_2-s6_1,s6_3-s6_2,s6_4-s6_3,s6_5-s6_4,s6_6-s6_5,s6_7-s6_6,s6_8-s6_7,s6_9-s6_8,s6_10-s6_9)
f7 <- c(s7_2-s7_1,s7_3-s7_2,s7_4-s7_3,s7_5-s7_4,s7_6-s7_5,s7_7-s7_6,s7_8-s7_7,s7_9-s7_8,s7_10-s7_9)
f8 <- c(s8_2-s8_1,s8_3-s8_2,s8_4-s8_3,s8_5-s8_4,s8_6-s8_5,s8_7-s8_6,s8_8-s8_7,s8_9-s8_8,s8_10-s8_9)
f9 <- c(s9_2-s9_1,s9_3-s9_2,s9_4-s9_3,s9_5-s9_4,s9_6-s9_5,s9_7-s9_6,s9_8-s9_7,s9_9-s9_8,s9_10-s9_9)
f10 <- c(s10_2-s10_1,s10_3-s10_2,s10_4-s10_3,s10_5-s10_4,s10_6-s10_5,s10_7-s10_6,s10_8-s10_7,s10_9-s10_8,s10_10-s10_9)
#plot forward rate curve
color <- c("red","orange","plum","yellow","green","blue","purple","pink","rosybrown","khaki")
year <- c(1,3/2,2,5/2,3,7/2,4,9/2,5)
plot(year,f1, xlab="number of year", ylab="forward rate",main="forward rate curve",ylim=c(0.0003,0.3),col="red", type="b")+
  lines(year,f2,col="orange",type="o")+
  lines(year,f3,col="plum",type="o")+
  lines(year,f4,col="yellow",type="o")+
  lines(year,f5,col="green",type="o")+
  lines(year,f6,col="blue",type="o")+
  lines(year,f7,col="purple",type="o")+
  lines(year,f8,col="pink",type="o")+
  lines(year,f9,col="rosybrown",type="o")+
  lines(year,f10,col="khaki",type="o")
  legend("topleft", legend=recorded_date,lty=c(1,1,1,1,1,1,1,1,1),col=color,cex=0.7)
#put these forward rate into matrix
forward_matrix <- cbind(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10)
#calculate yield matrix
yield_matrix <- matrix(c(log(ytm[1,]),log(ytm[3,]/ytm[1,]),log(ytm[5,]/ytm[3,]),log(ytm[7,]/ytm[5,]),log(ytm[9,]/ytm[7,])))
#calculate covariance of yield matrix
cov(yield_matrix)
#calculate eigenvalues&eigenvectors of yield matrix
eigen(cov(yield_matrix))
#calculate forward rate matrix
forward_rate_matrix <- matrix(c(log(forward_matrix[1,]),log(forward_matrix[3,]/forward_matrix[1,]),log(forward_matrix[5,]/forward_matrix[3,]),log(forward_matrix[7,]/forward_matrix[5,]),log(forward_matrix[9,]/forward_matrix[7,])))
#calculate covariance of forward rate matrix
cov(forward_rate_matrix)
#calculate eigenvalues&eigenvectors of forward rate matrix
eigen(cov(forward_rate_matrix))
