library(testthat)
library(homework2)
library(MASS)


#test_check("homework2")

# Test 1
context("Test the output of homework 2.")

test_that("You ridge_regression() function works in a case.", {
  data(iris)
  # My method
  fit_ridge_model <- ridge_regression(Sepal.Length ~ ., iris,lambda = 0)
  # lm.ridge
  fit_ridge <- lm.ridge(Sepal.Length ~ ., iris,lambda = 0)
  expect_equivalent(as.matrix(coef(fit_ridge)),fit_ridge_model$coefficients,
                    tolerance = 0.001)
})


# Test 2
test_that("You lm_ridge_lambda () function works in a case.", {
  data(iris)
  data=iris
  form=Sepal.Length ~.
  #Set lambda values
  lambda_vals <- seq(0, nrow(mtcars)*2, length.out = 500)
  # My method + 5-fold cross validation
  lambda1 <- lm_ridge_lambda (form, data,lambda= lambda_vals)

  # glmnet + 5 fold cross validation
  rownames(data) <- NULL
  X <- model.matrix(form, data)
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
  test2 <- cv.glmnet(X, Y, alpha = 0, nfolds=5,lambda = lambda_vals)
  lambda2<-test2$lambda.min
  expect_equivalent(lambda1,lambda2,tolerance = 1)
})

