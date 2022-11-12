#' Compute impulse responses
#'
#' Take the draws of parameter matrix and covariance matrix and compute impulse
#' responses for a predefined number of horizon.
#'
#' @param bh A draw of the parameter matrix.
#' @param swish A draw of the covariance matrix.
#' @param nn A vector containing the amount of variables in the system, number
#' of lags and prediction horizon.
#'
#' @return A matrix with impulse responses
#' 
#' @references Code based on compute_impulses function of package VARsignR 
#' by Christian Danne (2015)


compute_impulses <-
  function(bh, swish, nn) {
    # Extract the number of variables in the system
    nvar <- nn[1]
    # Extract the numner of lags in the system
    lags <- nn[2]
    # Define step length to compute the impulse responses
    imstep <- nn[3]
    # Add one to each variable and lags to take care of intercept and start at 
    # time period zero
    ll <- lags + 1
    n1 <- nvar + 1
    nl <- nvar * lags
    # Obtain A matrix by transpoing parameter matrix B
    Ah <- t(bh)
    # Define the matrix to save impulse responses to
    imf <- matrix(nrow = imstep, ncol = nvar * nvar)
    # Define matrix to capture the covariance matrices
    M <- matrix(nrow = nvar * imstep, ncol = nvar)
    # Supply defined covaraine matrix to the M matrix transposed
    M[1:nvar, ] <- t(swish)
    # Define a temporary M
    Mtem <- M[1:nvar, ]
    # Define starting row for the impulse response
    imf[1, ] <- t(as.vector(Mtem))
    # Finally compute all the impulse responses
    ims2 <- imstep - 1
    ims1 <- min(c(ims2, lags))
    t <- 1
    while (t <= ims1) {
      nt <- nvar * t
      ntt <- nvar * (t + 1)
      tt <- t + 1
      Mtem <- Ah[, 1:nt] %*% M[1:nt, ]
      M[n1:ntt, ] <- M[1:nt, ]
      M[1:nvar, ] <- Mtem
      imf[tt, ] <- t(as.vector(Mtem))
      t <- t + 1
    }
    #
    for (t in ll:ims2) {
      nt <- nvar * t
      ntt <- nvar * (t + 1)
      tt <- t + 1
      Mtem <- Ah[, 1:nl] %*% M[1:nl, ]
      M[n1:ntt, ] <- M[1:nt, ]
      M[1:nvar, ] <- Mtem
      imf[tt, ] <- t(as.vector(Mtem))
    }
    # return the impulse responses
    imf
  }
