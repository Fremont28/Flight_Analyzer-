library(ggbeeswarm)
library(ggplot2)

#This code analyzes and provides statistical tests (normality tests, power analysis, t-tests, and wilcoxon tests)
#on South American flight data scraped from FlightAware.com 

mezcal<-read.csv("flights_delays.csv")


hist(mezcal$tiempo1,xlab="Takeoff Delays",col="black") 

#standard deviation bars 
sd2<-which(mezcal$tiempo1>1*sd(mezcal$tiempo1) |mezcal$tiempo1< -1*sd(mezcal$tiempo1))
sd3<-which(mezcal$timepo1>2*sd(mezcal$tiempo1) | mezcal$tiempo1< -2*sd(mezcal$tiempo1))
sd1<-which(mezcal$tiempo1<1*sd(mezcal$tiempo1)| mezcal$tiempo1 > -1*sd(mezcal$tiempo1))                       

mezcal$flag[sd1]<-"within 1 sd"
mezcal$flag[sd2]<-"within 2 sd"
mezcal$flag<-as.factor(mezcal$flag)

#flight delay histograms 
ggplot(mezcal, aes(x=tiempo1, fill=flag)) + geom_histogram(binwidth=.5, bins=100, alpha=1, breaks=c(-Inf,min(subset(mezcal, flag=="within 2 sd")$tiempo1),
        min(subset(mezcal, flag=="within 1 sd")$tiempo1), max(subset(mezcal, flag=="within 2 sd")$timepo1), max(subset(mezcal, flag=="within 2 sd")$tiempo1, Inf)) + geom_vline(aes(xintercept = mean(tiempo1), colour="Mean & Median"),color='black',size=2)+ 
        geom_text(aes(x=mean(mezcal$tiempo1)-0.6,y=15000,label="Mean & \n Median")) + xlab("Value of observation") + ylab("Number of observations")
                                                           
p1 <- ggplot(mezcal, aes(x=tiempo1)) +  geom_histogram(binwidth=.5, colour="black", fill="white") + ggtitle("")+
        xlab("Delays (min)")+ylab("Count")
                                                           
#density plot 
p2 <-ggplot(mezcal, aes(x=tiempo1)) + geom_density() + ggtitle("Density plot") 
                                                           
#boxplot                                        
p3 <- ggplot(mezcal, aes(x=1, y=tiempo1)) + geom_boxplot() + ggtitle("Boxplot") + xlab("")  + theme(axis.title.x=element_blank(),
                                                                                                                                                               axis.text.x=element_blank(),
                                                                                                                                                               axis.ticks.x=element_blank())
#beeswarm plot                                                                                                            
p4 <- ggplot(mezcal) +  geom_beeswarm(aes(x=1,y=tiempo1)) + ggtitle("") +ylab("Delays (min)") +theme(axis.title.x=element_blank(),
                                                                                                                                                                axis.text.x=element_blank(),
                                                                                                                                                                axis.ticks.x=element_blank())
                                                          
#Density plots with semi-transparent fill bogota vs. sau paulo                                                     
sau_bog=subset(mezcal,city=="Bogata" | city=="Sau_Paulo")                                                           
                                                           
g1 <- ggplot(sau_bog, aes(x=tiempo1, fill=city)) + geom_density() + xlab("Flight Delays (min)")
g1                                                          
                                                           
#boxplot  
g2 <- ggplot(sau_bog, aes(x=city, y=tiempo1, fill=city)) + geom_boxplot() +
  guides(fill=FALSE) + ylab("Flight Delays (min)")
g2

                                                           
g3 <- ggplot(sau_bog, aes(x=tiempo1, fill=city)) + geom_histogram(alpha=.3, bw=0.1) + xlab("Fertilizer adoption")
                                                         

g4 <- ggplot(sau_bog, aes(x=tiempo1, fill=city)) + geom_beeswarm(aes(x=0, y=tiempo1, colour=city)) + ylab("Flight Delays (min) ") + theme(axis.title.x=element_blank(),
                                                                                                                                          axis.text.x=element_blank(),
                                                                                                                                          axis.ticks.x=element_blank())
#split cities into separate dataframes                                                                                                              
d1=subset(mezcal, city=="Sau_Paulo")
d2=subset(mezcal,city=="Bogata")                                                        
                                                        
qqnorm(d1$tiempo1,main="QQ Plot")
qqline(d1$tiempo1)

qqnorm(d2$tiempo1,main="QQ Plot")
qqline(d2$tiempo1)

#get r-squared value?
qn=qqnorm(log(sau_bog$tiempo1), plot.it=FALSE)
rsq_1<-cor(qn$x,qn$y)

qn=qqnorm(sau_bog$tiempo1,plot.it=FALSE)
rsq_2<-cor(qn$x,qn$y) #0.87 non-normal??

shapiro.test(sau_bog$tiempo1) #P<0.01                                                           
                                                         
#test non-normal data???
#power size?
cohen_d <- function(d1,d2) {  
  
  m1 <- mean(d1, na.rm=TRUE)
  m2 <- mean(d2, na.rm=TRUE)
  s1 <- sd(d1, na.rm=TRUE)
  s2 <- sd(d2, na.rm=TRUE)
  spo <- sqrt((s1**2 + s2**2)/2)
  d <- (m1 - m2)/spo
  rpb <- d / sqrt((d**2)+4)
  ret <- list("rpb" = rpb, "effectsi" = d)
  return(ret)  }

norm <-d1$tiempo1   #d1-sau paulo 
norm2 <- d2$tiempo1 #d2 bogota 
inp <- cohen_d(norm, norm2)

#power test (for normally distn data)
pwr.t.test(n= NULL, d= inp$effectsi, power=0.8, alternative="two.sided")

#power test for non-normal distn data (MC Power) power de 0.8? 
MCpower = function(sample1, sample2, size) {
  
  reps = 1000
  results  <- sapply(1:reps, function(r) {
    resample1 <- sample(sample1, size=size, replace=TRUE) 
    resample2 <- sample(sample2, size=size, replace=TRUE) 
    test <- wilcox.test(resample1, resample2, alternative="two.sided",paired=FALSE, correct=TRUE)
    test$p.value
  })
  sum(results<0.05)/reps
}

non_norm_power<-MCpower(d1$tiempo1,d2$tiempo1,100)

nSims <- 500 #number of simulations

#initialise empty vectors for appending to
x1 <- c()
x2 <- c()
p1 <- c()
p2 <- c()

#run simulations
for(i in 1:nSims){ #for each simulated experiment
  set.seed(i)
  sim_a <- d1$tiempo1
  sim_b <- d2$tiempo1
  sim_b[which(is.nan(sim_b)==TRUE)] <- 1
  sim_a[which(is.nan(sim_a)==TRUE)] <- 1
  inp <- cohen_d(sim_a, sim_b) # effect size
  x1[i] <- pwr.t.test(d= inp$effectsi , n =length(sim_a), sig.level = 0.05, power = NULL)$power
  x2[i] <- MCpower(sim_a, sim_b,  size=150) 
  p1[i] <- t.test(sim_a,sim_b)$p.value
  p2[i] <- wilcox.test(sim_a,sim_b, paired=FALSE, correct=TRUE)$p.value
}

hist(sim_b, bins=20)
shapiro.test(sim_b)
par(mfrow = c(1, 2))
hist(x1, main="Histogram of normal powers ", xlab=("Observed power"), xlim =c(0,1))
hist(x2, main="Histogram of MC powers", xlab=("Observed power"), xlim =c(0,1))

#wilcoxon test
wilcox.test(d1$tiempo1 , d2$tiempo1 ,paired=FALSE, correct=TRUE)                                                           
                                                           
                                                           
                                                           
                                                           
                                                           
                                                           