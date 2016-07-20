# Functions to remove outliers and fill missing values in a time series
# Nothing for multiple seasonality yet.

# na.interp fills in missing values
# Uses linear interpolation for non-seasonal series
# Adds seasonality based on a periodic stl decomposition with seasonal series
# Argument lambda allows for Box-cox transformation

na.interp <- function(x, lambda=NULL)
{
  missng <- is.na(x)
  # Do nothing if no missing values
  if(sum(missng)==0L)
    return(x)
  
  # Convert to ts
  if(is.null(tsp(x)))
    x <- ts(x)
  if(!is.null(dim(x)))
    stop("The time series is not univariate.")
  
  #Transform if requested
  if(!is.null(lambda))
    x <- BoxCox(x, lambda=lambda)
  
  freq <- frequency(x)
  tspx <- tsp(x)
  n <- length(x)
  tt <- 1:n
  idx <- tt[!missng]
  
  if(freq <= 1 | n <= 2*freq) # Non-seasonal -- use linear interpolation
  {
    x <- ts(approx(idx, x[idx], tt, rule=2)$y)
  }
  # Otherwise a seasonal series
  # Estimate seasonal component robustly
  # Then add to linear interpolation of seasonally adjusted series
  else
  {
    # Fit Fourier series for seasonality and a cubic polynomial for the trend,
    #just to get something reasonable to start with
    K <- min(trunc(freq/2),3)
    X <- cbind(fourier(x,K),poly(tt,degree=3))
    fit <- lm(x ~ X, na.action=na.exclude)
    pred <- predict(fit, newdata =data.frame(X))
    x[missng] <- pred[missng]
    # Now re-do it with stl to get better results
    fit <- stl(x,s.window=11,robust=TRUE)
    # Interpolate seasonally adjusted values
    sa <- seasadj(fit)
    sa <- approx(idx,sa[idx],1:n, rule=2)$y
    # Replace original missing values
    x[missng] <- sa[missng] + fit$time.series[missng,"seasonal"]
  }
  
  # Backtransform if required
  if(!is.null(lambda))
    x <- InvBoxCox(x, lambda=lambda)
  
  # Ensure time series characteristics not lost
  tsp(x) <- tspx
  return(x)
}

# Function to identify outliers and replace them with better values
# Missing values replaced as well if replace.missing=TRUE

tsclean <- function(x, replace.missing=TRUE, lambda = NULL)
{
  outliers <- tsoutliers(x, lambda = lambda)
  x[outliers$index] <- outliers$replacements
  if(replace.missing)
    x <- na.interp(x, lambda = lambda)
  return(x)
}

stl_resid <- function(x)
{
  n <- length(x)
  freq <- frequency(x)
  
  if(freq > 1 & n > 2*freq)
  {
    fit <- stl(x, s.window="periodic", robust=TRUE)
    # Check if seasonality is sufficient to use these results
    rem <- fit$time.series[,"remainder"]
    detrend <- rem + fit$time.series[,"seasonal"]
    strength <- 1 - var(rem) / var(detrend)
    if(strength >= 0.05)
      x <- seasadj(fit)
  }
  # Use super-smoother on the (seasonally adjusted) data
  tt <- 1:n
  mod <- supsmu(tt,x)
  resid <- (x - mod$y) / sd(mod$y)
  resid
}

# Function to identify time series outlieres
tsoutliers <- function(x, iterate=2, 
                       lambda=NULL, 
                       iqr_factor = 3,
                       fitted_values = NULL,
                       index_for_inspection = 1)
{
  # Identify and fill missing values
  index_distance <- NULL
  missng <- is.na(x)
  nmiss <- sum(missng)
  if(nmiss > 0L)
    xx <- na.interp(x, lambda=lambda)
  else
    xx <- x
  
  #Transform if requested
  if(!is.null(lambda))
    xx <- BoxCox(xx, lambda = lambda)
  
  if (is.null(fitted_values))
  {
    resid <- stl_resid(xx)
  } else {
    resid <- (xx - fitted_values) / sd(fitted_values)
  }
  
  # Make sure missing values are not interpeted as outliers
  if(nmiss > 0L)
    resid[missng] <- NA
  
  # Limits of acceptable residuals
  resid.q <- quantile(resid, prob=c(0.25,0.75), na.rm=TRUE)
  iqr <- diff(resid.q)
  limits <- resid.q + iqr_factor*iqr*c(-1,1)
  
  # Find residuals outside limits
  if((limits[2]-limits[1]) > 1e-14)
    outliers <- which((resid < limits[1]) | (resid > limits[2]))
  else
    outliers <- numeric(0)
  
  # Replace all missing values including outliers
  x[outliers] <- NA
  x <- na.interp(x, lambda=lambda)
  
  # Do no more than 2 iterations regardless of the value of iterate
  if(iterate > 1)
  {
    tmp <- tsoutliers(x, 
                      iterate=1, 
                      lambda=lambda, 
                      fitted_values = fitted_values,
                      iqr_factor = iqr_factor,
                      index_for_inspection = index_for_inspection)
    if(length(tmp$index) > 0) # Found some more
    {
      index_distance <- tmp$index_distance
      outliers <- sort(c(outliers,tmp$index))
      x[outliers] <- NA
      x <- na.interp(x, lambda=lambda)
    }
  }
  
  if (is.null(index_distance))
  {
    index_distance <- resid[index_for_inspection]
  }
  
  # Return outlier indexes and replacements
  return(list(index=outliers, 
              replacements=x[outliers], 
              residuals = resid[outliers],
              index_distance = index_distance))
}
