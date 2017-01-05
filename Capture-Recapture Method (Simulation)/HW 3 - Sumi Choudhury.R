#########################
#         HW 3          # 
#########################


# SDGB 7844: Statistical Methods and Computation I
# Sumi Choudhury

############### 1 ###############
N <- 5000
n1 <- 100
n2 <- 100

# sample names (1 to N)
x_N <- 1:N

# tagged value will be 'No' for all N samples initially
tagged_N <- c(rep("No", N))

# recaptured value will be 'No' for all N samples initially
recaptured_N <- c(rep("No", N))

# create a dataset(data frame with 3 columns 
data_set <- data.frame(x_N, tagged_N, recaptured_N, stringsAsFactors=FALSE)

# give column names to data frame
colnames(data_set) <- c("SampleNo", "Tagged", "Recaptured")

# find n1 random samples to tag
tagged_n1 <- sample(x_N, n1)

# mark those samples in data frame as 'Yes' in Tagged column
data_set$Tagged[which(data_set$SampleNo %in% tagged_n1)] <- "Yes"

# find n2 random samples to recapture
recaptured_n2 <- sample(x_N, n2)

# mark those samples in data frame as 'Yes' in Recaptured column
data_set$Recaptured[which(data_set$SampleNo %in% recaptured_n2)] <- "Yes"

#determine m2, N_LP
m2 <- length(which(data_set$Tagged == "Yes" & data_set$Recaptured == "Yes"))

#compute Lincoln-Peterson Estimate of population
N_LP <- n1 * n2 / m2

#print m2, Lincoln-Peterson Estimate of population
m2
N_LP

############### 2 ###############

# function to simulate the capture-recapture procedure
capture_recapture_funct <- function(N, n1, n2, iterations) {
  x_N <- 1:N
  m2_all <- c()
  N_LP_all <- c()
  for (i in 1: iterations){
    tagged_N <- c(rep("No", N))
    recaptured_N <- c(rep("No", N))
    data_set <- data.frame(x_N, tagged_N, recaptured_N, stringsAsFactors=FALSE)
    colnames(data_set) <- c("SampleNo", "Tagged", "Recaptured")

    tagged_n1 <- sample(x_N, n1)
    data_set$Tagged[which(data_set$SampleNo %in% tagged_n1)] <- "Yes"
  
    recaptured_n2 <- sample(x_N, n2)
    data_set$Recaptured[which(data_set$SampleNo %in% recaptured_n2)] <- "Yes"
  
    m2 <- length(which(data_set$Tagged == "Yes" & data_set$Recaptured == "Yes"))
    N_LP <- n1 * n2 / m2
    m2_all <- c(m2_all, m2)
    N_LP_all <- c(N_LP_all, N_LP)
  }
  sim <- data.frame(m2_all, N_LP_all)
  colnames(sim) <- c("m2", "N_LP")
  mylist <- list()
  mylist[[1]] <- sim
  mylist[[2]] <- N
  return(mylist)
}

N <- 5000
n1 <- 100
n2 <- 100
iterations <- 1000

# run simulation for 1,000 iterations for a population
output <- capture_recapture_funct(N, n1, n2, iterations)
sim <- output[[1]]
head(sim)
tail(sim)
N <- output[[2]]
N

# make a histogram of the resulting N_LP vector
hist(sim$N_LP, xlab="N_LP", ylab="Frequency", main="Lincoln-Peterson Estimates")

# indicate N in plot
abline(v=N, col="red")

############### 3 ###############
# calculate percentage of estimated population values that are infinite
total_inf_values <- length(which(sim$N_LP == "Inf"))
total_inf_values
P <- total_inf_values * 100 / iterations
P


############### 4 ###############

# vector to store Chapman estimates
N_C_all = c()

# compute Chapman estimate of population for all simulations
for(i in 1:iterations) {
  N_C <- ((n1 + 1) * (n2 + 1)) / (sim$m2[i] + 1) - 1
  N_C_all <- c(N_C_all, N_C)
}

# make a histogram of the resulting N_C vector
hist(N_C_all, xlab="N_C", ylab="Frequency", main="Chapman Estimates")

# indicate N in plot
abline(v=N, col="green")

############### 5 ###############

# calculate bias in Lincoln-Peterson estimates
thetacap_N_LP = 0
for(i in 1:iterations) {
  thetacap_N_LP <- thetacap_N_LP + sim$N_LP[i]
}
bias_N_LP = thetacap_N_LP / iterations - N
bias_N_LP

# calculate bias in Chapman estimates
thetacap_N_C = 0
for(i in 1:iterations) {
  thetacap_N_C <- thetacap_N_C + N_C_all[i]
}
bias_N_C = thetacap_N_C / iterations - N
bias_N_C

############### 6 ###############
# written in document

############### 7 ###############
chapman_estimator_func <- function(N, iterations, sample_sizes) {
n_all = c()
bias_NC_all = c()
variance_all = c()
  for(n in sample_sizes)  {
    thetacap_N_C = 0
	variance_N_C_total = 0
    for (i in 1: iterations) {
	  n1 <- n
	  n2 <- n
	  x_N <- 1:N
      tagged_N <- c(rep("No", N))
      recaptured_N <- c(rep("No", N))
      data_set <- data.frame(x_N, tagged_N, recaptured_N, stringsAsFactors=FALSE)
      colnames(data_set) <- c("SampleNo", "Tagged", "Recaptured")
      
	  tagged_n1 <- sample(x_N, n1)
      data_set$Tagged[which(data_set$SampleNo %in% tagged_n1)] <- "Yes"
      
	  recaptured_n2 <- sample(x_N, n2)
      data_set$Recaptured[which(data_set$SampleNo %in% recaptured_n2)] <- "Yes"
      
	  m2 <- length(which(data_set$Tagged == "Yes" & data_set$Recaptured == "Yes"))
      
	  N_C <- ((n1 + 1) * (n2 + 1)) / (m2 + 1) - 1
	  thetacap_N_C <- thetacap_N_C + N_C
	  
	  var_tmp = ((n1 + 1) * (n2 + 1) * (n2 - m2) * (n1 - m2)) / ((m2 + 1) * (m2 + 1) * (m2 + 2))
	  variance_N_C_total <- variance_N_C_total + var_tmp
   }
    bias_N_C = thetacap_N_C / iterations - N
	variance_N_C = variance_N_C_total / iterations
	n_all <- c(n_all, n) 
	bias_NC_all = c(bias_NC_all, bias_N_C)
    variance_all = c(variance_all, variance_N_C)
  }
  sim_data <- data.frame(n_all, bias_NC_all, variance_all)
  colnames(sim_data) <- c("n", "bias_NC", "variance")
  return(sim_data)
}
# run this function for N = 100,000 AND 1000 iterations AND multiple sample sizes
chapman_estimations <- chapman_estimator_func(100000, 1000, seq(from=100, to=5000, by=50))
chapman_estimations


# plot bias versus n
xrange <- range(chapman_estimations$n) 
yrange <- range(chapman_estimations$bias_NC) 
par(pch=18)
plot(chapman_estimations$n, chapman_estimations$bias_NC, type="o",col="red", xlab="n (sample size)", ylab="chapman bias" ) 
title("Chapman Estimator Bias")

# plot variance versus n
xrange <- range(chapman_estimations$n) 
yrange <- range(chapman_estimations$variance) 
par(pch=20)
plot(chapman_estimations$n, chapman_estimations$variance, type="o",col="blue", xlab="n (sample size)", ylab="variance" ) 
title("Sample Size Vs Variance")
