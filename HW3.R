# A clinical trial was conducted to compare the effectiveness of three drugs. 100 patients were
# randomly assigned to each drug (300 total patients), and Y1 = 12, Y2 = 18, and Y3 = 10 patients
# had successful outcomes in the three drug groups. Using uniform priors for the success probabilities
# of each drug,

# 1. Compute and plot the posterior distribution of the success probability for each drug.

prior1 <- 1 #prior is uniform(0,1)
grid <- seq(0,1, length=1000) #grid for parameter

#drug1
#post1 <-choose(100,12) * p1^12 * (1-p1)^88 *prior1 #or we can use dbinom(12,100,p1)*prior1
post1 <- dbinom(12,100,grid)*prior1 #posterior 
post1 <- post1/sum(post1) #standardize
par(mfrow = c(2,2)) #2 by 2 multipanel plots
plot(grid,post1, type = "l",main="posterior drug 1, prior U(0,1)",xlab="parameter",
     ylab="posterior density") #plot posterior density

#drug2
#post2 <-choose(100,18) * p2^18 * (1-p2)^82 *prior1 #or we can use dbinom(18,100,p1)*prior1
post2 <- dbinom(18,100,grid)*prior1 #posterior
post2 <- post2/sum(post2) #standardize
plot(grid,post2, type = "l", main="posterior drug 2, prior U(0,1)",xlab="parameter",
     ylab="posterior density") #plot posterior density

#drug3
#post3 <-choose(100,10) * p3^10 * (1-p3)^90 *prior1
post3 <- dbinom(10,100,grid)*prior1 #posterior
post3 <- post3/sum(post3) #standardize
plot(grid,post3, type = "l",main="posterior drug 3, prior  U(0,1)",xlab="parameter",
     ylab="posterior density") #plot posterior density


#combining all posterior distribution plots of drugs

plot(grid,post1,type="l",col='green',
     main="Parameter vs posterior density \nfor 
     each drug \nwith prior U(0,1)",
     xlab="parameter",ylab="posterior density")
points(grid,post2,type="l",col='blue')
points(grid,post3,type="l",col='red')
legend("topright",c("drug 1","drug 2","drug 3"), lty=c(1,1),lwd=c(2.5,2.5),
       col=c("green","blue","red"),
       cex = 0.65,ncol=2,bty="n") 


# 2. Compute the posterior probability that drug 2 is the best drug.

#using monte carlo sampling to compute posterior probability that drug 2 is the best

#generate 1000k random sample from posterior distribution of each drugs

post_drug1 <- rbeta(1000000,13,89) 
post_drug2 <- rbeta(1000000,19,83) 
post_drug3 <- rbeta(1000000,11,91) 
mean(post_drug1<post_drug2 & post_drug3<post_drug2) #mean of number of posterior probability drug 2 that 
#is larger than posterior probability drug 1 and drug 3

# 3. Repeat (a) and (b) but this time, consider the prior in which the prior mean is 40/300 =
# 0.133 and the prior standard deviation is square root of (0.133)(1 ??? 0.133)/300. Explain why this is a
# reasonable prior.

prior2 <- dbeta(grid,40,260) #prior is beta(40,260)

#drug1
#post21 <-choose(100,12) * grid^12 * (1-grid)^88 *prior2 #or we can use dbinom(12,100,grid)*prior2
post21 <- dbinom(12,100,grid)*prior2 #posterior
post21 <- post21/sum(post21) #standardize
par(mfrow = c(2,2))
plot(grid,post21, type = "l",main="posterior drug 1, prior Beta(40,260)",xlab="parameter",
     ylab="posterior density") #plot posterior density

#drug2
#post22 <-choose(100,18) * grid^18 * (1-grid)^82 *prior2 #or we can use dbinom(18,100,grid)*prior2
post22 <- dbinom(18,100,grid)*prior2 #posterior
post22 <- post22/sum(post22) #standardize
plot(grid,post22, type = "l", main="posterior drug 2, prior Beta(40,260)",xlab="parameter",
     ylab="posterior density") #plot posterior density

#drug3
#post23 <-choose(100,10) * grid^10 * (1-grid)^90 *prior2
post23 <- dbinom(10,100,grid)*prior2 #posterior
post23 <- post23/sum(post23) #standardize
plot(grid,post23, type = "l",main="posterior drug 3, prior Beta(40,260)",xlab="parameter",
     ylab="posterior density") #plot posteriior density


#combining all posterior distribution plots of drugs

plot(grid,post21,type="l",col='green',main="Parameter vs posterior density \nfor each drug 
     \nwith prior Beta(40,260)",
     xlab="parameter",ylab="posterior density")
points(grid,post22,type="l",col='blue')
points(grid,post23,type="l",col='red')
legend("topright",c("drug 1","drug 2","drug 3"), lty=c(1,1),lwd=c(2.5,2.5),
       col=c("green","blue","red"),
       cex = 0.65,ncol=2,bty="n") 

#using monte carlo sampling to compute posterior probability that drug 2 is the best using 
#prior beta(40,260)

#generate 1000k random sample from posterior distribution of each drugs

post_drug21 <- rbeta(1000000,52,348) #posterior distribution for drug 1 is beta(52,348)
post_drug22 <- rbeta(1000000,58,342) #posterior distribution for drug 2 is beta(58,342)
post_drug23 <- rbeta(1000000,50,350) #posterior distribution for drug 3 is beta(50,350)
mean(post_drug21<post_drug22 & post_drug23<post_drug22) #mean of number of posterior probability of 
#drug 2 that is larger than posterior probability drug 1 and drug 3
