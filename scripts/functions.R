#requires data as vectors
#Takes the difference of data1-data2
bootstrap_median_diffs <- function(data_1, data_2, N){
  #storage vector; N is the number of bootstrap replicates
  stat.store <- rep(0, N)
  
  l <- length(data_1)
  m <- length(data_2)
  
  #run the bootstrap
  for(i in 1:N){
    #get a random sample of positions
    positions1 <- sample(1:l, size=l, replace=TRUE)
    positions2 <- sample(1:m, size=m, replace=TRUE)
    
    #resample data
    resamp_data_1 <- data_1[positions1]
    resamp_data_2 <- data_2[positions2]
    
    #calculate and store test statistic, which here is
    #...the difference of medians
    stat.store[i] <- median(resamp_data_1)-median(resamp_data_2)
  }
  return(stat.store)
}

#parametric, population is distributed as a binomial (n, p)
### We know n: that's the sample size. p is what we generate estimates
###...of.
parametric_bootstrap <- function(data.1, data.2, N){
  l <- length(data.1)
  m <- length(data.2)
  
  #estimate p from data.1
  p.1 <- sum(data.1)/l
  #estimate p from data.2
  p.2 <- sum(data.2)/m
}