#' Ridge regression that takes into account colinear regression variables
#' @param form a linear model formula
#' @param data a dataset
#' @param lambda regularization parameter
#' @return The coefficients of the ridge regression
#' @example
#' ridge_regression(mpg ~ ., mtcars,lambda = lambda_vals)


# The code below used BIS 557 Lecture Notes from Michael Kane as reference

ridge_regression <- function(form, data, lambda = 0) {
  rownames(data) <- NULL
  X <- model.matrix(form, data)
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
  ret <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
  attributes(ret)$formula <- form
  class(ret) <- c(class(ret), "ridge_regression")
  model<-list()
  model$coefficients <- ret
  return(model)
}

#' Compute ridge regression vector
#' @param form a linear model formula
#' @param data a dataset
#' @param lambda A sequence of penalty terms
#' @return A matrix of regression vectors with ncol(X) columns and length(lambda) rows.
#' @example
#' lm_ridge_lambda (Sepal.Length ~ ., iris,lambda = 0)

# The code below used code from the book 'A computational approach to statistical learning' as reference
lm_ridge_lambda <- function(form, data, lambda=0){
  rownames(data) <- NULL
  # Apply 5-fold Cross Validation
  # Randomly shuffle the data
  data<-data[sample(nrow(data)),]
  # Create 5 equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=5,labels=FALSE)
  # Perform 5 fold cross validation
  mse<-NULL
  for(i in 1:5){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    X_test<-model.matrix(form, testData)
    y_test<-data[[as.character(form)[2]]][as.numeric(rownames(X_test))]
    X <- model.matrix(form, trainData)
    y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
    svd_obj <- svd(X)
    U <- svd_obj$u
    V <- svd_obj$v
    svals <- svd_obj$d
    k <- length(lambda)
    ridge_beta <- matrix(NA_real_, nrow = k, ncol = ncol(X))
    for (j in seq_len(k)){
      D <- diag(svals / (svals^2 + lambda[j]))
      ridge_beta[j,] <- V %*% D %*% t(U) %*% y
    }
    y_hat <- tcrossprod(X_test, ridge_beta)
    mse_new <- apply((y_hat - as.vector(y_test))^2, 2, mean)
    mse=cbind(mse,mse_new)
  }
  mse_mean=rowMeans(mse)
  optimal=lambda[which.min(mse_mean)]
  optimal
}

