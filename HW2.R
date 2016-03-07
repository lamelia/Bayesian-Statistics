#Codes homework 2

# 1. Assume the Bayesian model with likelihood Y |?? ??? Binomial(n, ??) and prior ?? ??? Beta(a, b).

# (a) Write a function that uses Monte Carlo sampling to estimate the posterior mean and
# standard deviation of ?? given we observe Y = y. The function should take inputs y, n,
# a, and b. Given these inputs, the function should generate 1, 000, 000 samples of (??, Y )
# (by first drawing ?? from a beta distribution and then Y |?? from a binomial distribution),
# extract the samples with Y = y, and return the mean and standard deviation of ?? for
# these samples. Include code for this function in your write-up.

#make function with inputs y, n, a, and b
f.mc <- function(y,n,a,b){ 
  
  #generate 1000k theta samples from beta distribution
  theta_sample <- rbeta(1000000,a,b) 
  
  #generate 1000k y samples given each theta samples
  #using for loop
  m <- 1000000
  y_sample <- rep(0,m) #replicate 0 as many as 1000k 
  for(i in 1:m){
    
    #generate y samples from binomial distribution given probability of theta samples
    y_sample[i] <- rbinom(1,n,theta_sample[i]) 
  }
  
  #mean and std deviation of theta sample given y samples
  mean_sample <-mean(theta_sample[y_sample==y])
  std_sample <- sd(theta_sample[y_sample==y])
  
  #mean and std deviation output
  output <- list(mean=mean_sample,stddev=std_sample)
  return(output)
}

# (b) Use the code from (1) with n = 10 and a = b = 1 to compute the posterior mean and
# standard deviation for ?? for all y = 0, 1, . . . , n and plot the posterior mean and standard
# deviation as a function of y.

#posterior mean and std deviation for ?? for all y = 0, 1, . . . , n

#replicate 0 in variable mean_sample_b and std_sample_b as many as n=11
set.seed(123)
mean_sample_b <- rep(0,11)
std_sample_b <- rep(0,11)

#using for loop, create vector i with 0:10, inside {} will be excecuted 11 times
for (i in 0:10){
  func <- f.mc(i,10,1,1) #run function f.mc(y,n,a,b) where y=i, n=10, a=b=1
  mean_sample_b[i+1] <- func[1] #get posterior mean for ?? for every i where i in 0 to 10
  std_sample_b[i+1] <- func[2] #get posterior std deviation for ?? for every i where i in 0 to 10 
}

mean_sample_b <- unlist(mean_sample_b) #convert mean_sample_b from list to vector format
std_sample_b <- unlist(std_sample_b) #convert std_sample_b from list to vector format

output_b <- list(mean=mean_sample_b,stddev=std_sample_b)

#posterior mean and std deviation of theta for all y, where a=b=1
output_b

y_ <- c(0:10)

par(mfrow = c(1,2)) #2 by 2 multi-paneled plot

#plot posterior mean and std deviation with y 
plot(y_,std_sample_b, main="y vs std deviation posterior (b)")
plot(y_,mean_sample_b, main="y vs mean posterior (b)")


# (c) Use the code from (1) with n = 10 and a = b = 10 to compute the posterior mean and
# standard deviation for ?? for all y = 0, 1, . . . , n and plot the posterior mean and standard
# deviation as a function of y.

#posterior mean and std deviation for ?? for all y = 0, 1, . . . , n

#replicate 0 in variable mean_sample_c and std_sample_c as many as n=11
mean_sample_c <- rep(0,11)
std_sample_c <- rep(0,11)

#using for loop, create vector i with 0:10, inside {} will be excecuted 11 times
for (i in 0:10){
  func2 <- f.mc(i,10,10,10) #run function f.mc(y,n,a,b) where y=i, n=a=b=10
  mean_sample_c[i+1] <- func2[1] #get posterior mean for ?? for every i where i in 0 to 10
  std_sample_c[i+1] <- func2[2] #get posterior std deviation for ?? for every i where i in 0 to 10 
}

mean_sample_c <- unlist(mean_sample_c) #convert mean_sample_c from list to vector format
std_sample_c <- unlist(std_sample_c) #convert std_sample_c from list to vector format

output_c <- list(mean=mean_sample_c,stddev=std_sample_c)

#posterior mean and std deviation of theta for all y, where a=b=10
output_c

par(mfrow = c(1,2))
#create mean posterior plots for a=b=1 and a=b=10
plot(y_,mean_sample_b, col='blue', pch=16, main="y vs mean posterior",xlab='y', ylab='mean_sample')
points(y_,mean_sample_c, col = 'green', pch = 16)

#create standard deviation posterior plots for a=b=1 and a=b=10
plot(y_,std_sample_b, col='blue', pch=16, main="y vs std deviation posterior",xlab='y', ylab='stddev_sample')
points(y_,std_sample_c, col = 'green', pch = 16)


# 2. Assume that data is distributed Y ??? Poisson(??) and the prior is uniform over the set ?? ???
# {0, 1, 2, . . . , 19, 20}.

# (a) Plot the prior, compute the prior mean and standard deviation, and find an interval so
# that ?? is in the interval with prior probability 0.9.

#prior distribution is discrete uniform with ?? = 0,..,20

lambda <- 0:20 #lambda set
prob <- 1/21   #discrete uniform probability equals to 1/21
prior <- rep(prob,21) #replicate probability as many as 21 times

#mean of prior 
mean <- sum(lambda*prior)

#std deviation of prior
var <- (sum((lambda^2)*prior)) - (sum(lambda*prior))^2 #variance of prior 
stddev <- sqrt(var)  #get standard deviation

#find interval with prior prob =0.9 or sum of probability equal to 0.9
intvl <- sum(prior[1:19]) #try interval of ?? from 0 to 18
#intvl #probability equal to 0.9047, not exact 0.9

output_prior <- c(mean=mean,std_dev=stddev,interval=intvl)
output_prior



# (b) Now we observe Y = 2. Plot the posterior, compute the posterior mean and standard
# deviation, and find an interval so that ?? is in the interval with posterior probability 0.9.

#plot for posterior probability with Y=2
y <- 2

#posterior probability formula: f(y|lambda) = (lambda^y exp(-lambda)/factorial(y)) * (1/21) / f(y)
post <- (1/(21*factorial(y)))*(lambda^y * exp(-lambda)) #calculate posterior probability for each lambda
sum(post) #sum of posterior probability  does not equal 1, instead this equals to 0.04743558. 
#To make it equal 1, divide post to sum of posterior probability.
#this division is f(y), or the constant in posterior formula. 

constant <- 1/sum(post) #calculate constant of f(y) 
post <- post*constant #calculate new posterior probability after multiplying by constant.
sum(post) #sum of posterior probability now equals 1

#create posterior probability plot
par(mfrow = c(1,2))

#plot the prior
plot(lambda,prior,type="h",lwd=2,col="blue",ylab="probability",main="Prior distribution plot") 
points(lambda,prior,pch=16,cex=1,col="dark red")

#plot the posterior
plot(lambda,post,type="h",lwd=2,col="blue",ylab="probability",xlab="lambda",main="Posterior distribution plot")
points(lambda,post,pch=16,cex=1,col="dark red")

#mean posterior
mean_post <- sum(lambda*post)

#std deviation posterior
var_post <- (sum((lambda^2)*post)) - (sum(lambda*post))^2 #variance of posterior
stddev_post <- sqrt(var_post) #standard deviation of posterior

#find interval with posterior prob =0.9 
#try interval from 0 to 5
intvl_post <- sum(post[1:6])
#intvl_post #probability equal to 0.9129, not exact 0.9

output_post <- c(mean=mean_post,std_dev=stddev_post,interval=intvl_post)

output_post
