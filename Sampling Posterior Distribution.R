################# Sampling Importance Resampling

SIR <- function(N){
  
  target_dist <- function(x) (1/4)*exp(-abs(x)/2) # Target distribution
  candidate_dist <- function(x) rnorm(x,0,10) # Candidate distribution
  candid_rand <- c() # Store the random values from the
  # candidate distribution
  
  for (i in 1:N){ # Step 1 of algorithm
    candid_rand[i] <- candidate_dist(1)
  }
  
  target_prob <- target_dist(candid_rand) # Step 2 of algorithm
  norm_prob <- target_prob/sum(target_prob) # Step 3 of algorithm
  
  # Note that I resample a lot more points than N (10*N)
  bootstrap <- sample(candid_rand,size=10*N,replace=TRUE,prob=norm_prob) 
  # Sample with replacement - step 4 of the algorithm
  
  # The histogram in white corresponds to draws from the candidate distribution
  # (prior to resampling).
  
  hist(candid_rand,breaks=20,xlab="x values",
       main="Histogram of Draws from the Candidate Distribution (White) \n 
       and Resampled Points (Cyan)")
  hist(bootstrap,add=TRUE, col=rgb(0.5,1,1,0.7),breaks=30)
  return(bootstrap)
}

Q_1 <- SIR(2000) # N = 2,000 points

# Note that I resample a lot more points than N (10*N) in 
# the bootstrap step. 


################# Rejection Sampling

Rejection <- function(n){
  set.seed(6) # Random seed for reproducibility
  z <- seq(-25,25,0.1) # Sample values from -25 to 25
  target_dist <- function(x) (1/4)*exp(-abs(x)/2) # Target distribution
  proposal_dist <- function(x) dnorm(x,0,10) # Proposal distribution
  
  M <- ceiling(max(target_dist(z)/proposal_dist(z))) # Calculate M
  
  # Three vectors to keep track of the accepted points, rejected
  # points, and total number of points
  accepts <- c()
  reject <- c()
  points <- c()
  i <- 0
  while (i !=n){
    x_i <- rnorm(1,0,10) # Get a point from our proposal distribution
    points <- c(points,x_i) # Add to vector
    u <- runif(1,min=0,max=1) # Generate a number from uniform distribution
    if (u < (target_dist(x_i)/(M*proposal_dist(x_i)))){
      accepts <- c(accepts,x_i) # acceptance
      i = i+1
    } else{
      reject <- c(reject,x_i) # rejection
    }
  }
  hist(points,breaks=20,xlab="x values",main="Histogram of All Samples (White) \n 
       and Accepted Samples (Cyan)")
  hist(accepts,add=TRUE, col=rgb(0.5,1,1,0.7),breaks=30)
  return(accepts)
}

# The histogram in white corresponds to draws from the candidate distribution.

Q_2 <- Rejection(2000) # n = 2,000 points
