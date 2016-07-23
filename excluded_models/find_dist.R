library(MASS)
#Usage: 
#data, numeric vector of observations of unknown distribution
#point, the new ditance to measure its p-value
#sample, rate of subsampling (0.5 means that a sample 50% of data will be considered) 
fitData <- function(data, point, sample=1){
  fit <- c("uniform","normal", "logistic", "lognormal", "gamma", "weibull", "cauchy")
  distrib = list()
  numfit <- length(fit)
  results = matrix(0, ncol=5, nrow=numfit)
  
  for(i in 1:numfit){
    distrib[[i]] = fit[i]
  }
  
  # take a sample of dataset
  n = round(length(data)*sample)
  data = sample(data, size=n, replace=F)
  
  for(i in 1:numfit) {
    if(distrib[[i]] == "gamma") {
      gf_shape = "gamma"
      fd_g <- fitdistr(data, "gamma")
      est_shape = fd_g$estimate[[1]]
      est_rate = fd_g$estimate[[2]]
      
      ks = ks.test(data, "pgamma", shape=est_shape, rate=est_rate)
      # add to results
      results[i,] = c(gf_shape, est_shape, est_rate, ks$statistic, ks$p.value)
      gamma_val = pgamma(point, shape=est_shape, rate=est_rate)
      gamma_val = min(gamma_val, 1-gamma_val)
    }
    
    else if(distrib[[i]] == "unif"){
      gf_shape = "unif"
      fd_u <- fitdist(data, "unif")
      est_min = fd_u$estimate[[1]]
      est_max = fd_u$estimate[[2]]
      
      ks = ks.test(data, "punif", min=est_min, max=est_max)
      
      # add to results
      results[i,] = c(gf_shape, est_min, est_max, ks$statistic, ks$p.value)
      uniform_val = punif(point, min=est_min, max=est_max)
      uniform_val = min(uniform_val, 1-uniform_val)
    }
    
    else if(distrib[[i]] == "weibull"){
      gf_shape = "weibull"
      fd_w <- fitdistr(data,densfun=dweibull,start=list(scale=1,shape=2))
      est_shape = fd_w$estimate[[1]]
      est_scale = fd_w$estimate[[2]]
      
      ks = ks.test(data, "pweibull", shape=est_shape, scale=est_scale)
      # add to results
      results[i,] = c(gf_shape, est_shape, est_scale, ks$statistic, ks$p.value)
      weibull_val = pweibull(point, shape=est_shape, scale=est_scale)
      weibull_val = min(weibull_val, 1-weibull_val)
    }
    
    else if(distrib[[i]] == "normal"){
      gf_shape = "normal"
      fd_n <- fitdistr(data, "normal")
      est_mean = fd_n$estimate[[1]]
      est_sd = fd_n$estimate[[2]]
      
      ks = ks.test(data, "pnorm", mean=est_mean, sd=est_sd)
      # add to results
      results[i,] = c(gf_shape, est_mean, est_sd, ks$statistic, ks$p.value)
      normal_val = pnorm(point, mean=est_mean, sd=est_sd)
      normal_val = min(normal_val, 1-normal_val)
    }
    
    else if(distrib[[i]] == "logistic"){
      gf_shape = "logistic"
      fd_l <- fitdistr(data, "logistic")
      est_location = fd_l$estimate[[1]]
      est_scale = fd_l$estimate[[2]]
      
      ks = ks.test(data, "plogis", location=est_location, scale=est_scale)
      # add to results
      results[i,] = c(gf_shape, est_location, est_scale, ks$statistic, ks$p.value)
      logistic_val = plogis(point, location=est_location, scale=est_scale)
      logistic_val = min(logistic_val, 1-logistic_val)
    }
    
    else if(distrib[[i]] == "lognormal"){
      gf_shape = "lognormal"
      fd_ln <- fitdistr(data, "lognormal")
      est_meanlog = fd_ln$estimate[[1]]
      est_sdlog = fd_ln$estimate[[2]]
      
      ks = ks.test(data, "plnorm", meanlog=est_mean, sdlog=est_sdlog)
      # add to results
      results[i,] = c(gf_shape, est_location, est_scale, ks$statistic, ks$p.value)
      lnorm_val = plnorm(point, meanlog=est_meanlog, sdlog=est_sdlog)
      lnorm_val = min(lnorm_val, 1 - lnorm_val)
    }
    
    else if(distrib[[i]] == "cauchy"){
      gf_shape = "cauchy"
      fd_g <-  fitdistr(data, "cauchy")
      est_location  = fd_g$estimate[[1]]
      est_scale = fd_g$estimate[[2]]
      
      ks = ks.test(data, "pcauchy", location=est_location, scale=est_scale)
      # add to results
      results[i,] = c(gf_shape, est_location, est_scale, ks$statistic, ks$p.value)
      cauchy_val = pcauchy(point, location=est_location , scale=est_scale)
      cauchy_val = min(cauchy_val, 1 - cauchy_val)
    }
  }
  
  results = rbind(c("distribution", "param1", "param2", "ks stat", "ks pvalue"), results)
  #print(results)
  max_val = which.max( results[,5] )
  dist_name = results[max_val,1]

  if(dist_name == "gamma") {
    return(gamma_val)
  }
  
  else if(dist_name == "uniform") {
    return(uniform_val)
  }
  
  else if(dist_name == "weibull") {
    return(weibull_val)
  }
  
  else if(dist_name == "normal") {
    return(normal_val)
  }
  
  else if(dist_name == "logistic") {
    return(logistic_val)
  }
  
  else if(dist_name == "lognormal") {
    return(lnorm_val)
  }
  
  else if(dist_name == "cauchy") {
    return(cauchy_val)
  }
  
  else {
    return(0)
  }
  
}


#example: normal 
data = rnorm(100000, mean=5, sd=0.75)
res = fitData(data, 3, sample=1)

# example: gamma
data= rgamma(100000, shape=3.2, rate=1.3)
res = fitData(data, 4, sample=0.1)
