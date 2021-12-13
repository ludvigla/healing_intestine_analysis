ifthenelse <- function(t, y, n) {if(t) {y} else {n} }

smth.gaussian <- function (
  x,
  windowLength = 10,
  alpha = 0.1,
  tails = TRUE
){

  # Check Numeric Arguments
  if(!is.numeric(x) | !is.numeric(alpha)){stop("argument 'x' and 'alpha' must be numeric", call. = FALSE)}

  # Hidden Gaussian Window Function
  makeWindow  = function(w, a){
    hw  = abs(w/2.0) #halfwidth
    e   = exp(1)     #eulers number
    a   = abs(a)     #alpha
    ret = sapply(c(0:(w - 1)), function(x){
      n = x - as.integer(hw)
      k = -0.5*(a*n/hw)^2
      e^k
    })
    ret
  }

  w = makeWindow(windowLength, alpha[1])

  sizeW = length(w)
  sizeD = length(x)

  w = w/sum(w)

  hkwL = as.integer(sizeW/2)
  hkwR = sizeW - hkwL

  ret  = sapply(c(1:sizeD),function(i){
    ix.d = c((i - hkwL):(i + hkwR - 1))
    ix.w = which(ix.d %in% 1:sizeD)
    ix.d = ix.d[ix.w]
    W.nm = ifthenelse(length(ix.w) != sizeW, w[ix.w]/sum(w[ix.w]), w)
    D.nm = x[ix.d]
    as.numeric(D.nm %*% W.nm)
  })
  if(!tails){ret[c(1:hkwL, (sizeD - hkwR + 1):sizeD)] = NA}
  return(ret)
}


finite.differences <- function(x, y) {
  if (length(x) != length(y)) {
    stop('x and y vectors must have equal length')
  }

  n <- length(x)

  # Initialize a vector of length n to enter the derivative approximations
  fdx <- vector(length = n)

  # Iterate through the values using the forward differencing method
  for (i in 2:n) {
    fdx[i-1] <- (y[i-1] - y[i]) / (x[i-1] - x[i])
  }

  # For the last value, since we are unable to perform the forward differencing method
  # as only the first n values are known, we use the backward differencing approach
  # instead. Note this will essentially give the same value as the last iteration
  # in the forward differencing method, but it is used as an approximation as we
  # don't have any more information
  fdx[n] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])

  return(fdx)
}
