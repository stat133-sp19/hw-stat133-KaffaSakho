
library("testthat")
source("R/workout03.R")


test_that("check_prob handles a number between 0 and 1 included",{
  prob1 <- 0.7
  prob2 <- 2 
  
  expect_true(check_prob(prob1))
  expect_length(prob1,1)
  expect_error(check_prob(prob2))
})

test_that("check_trials handles non-negative integers", {
  trials1 <- -0.5
  trials2 <- 4
  
  expect_true(check_trials(trials2))
  expect_length(trials2, 1)
  expect_error(trials1)
})

test_that("check_success handles vector of non-negative integers less than trials", {
  success1 <- 2:5
  success2 <- -4
  success3 <- 9
  trials <- 6
  
  expect_true(check_success(success1,trials))
  expect_length(check_success(success1,trials), 1)
  expect_error(check_success(success2,trials))
  expect_error(check_success(success3,trials))
  
})

test_that("aux_mean works with normal input", {
  trials1 <- 10
  prob <- 0.3 
  mean <- trials1 * prob
  
  expect_equal(aux_mean(trials1,prob), mean)
  
})

test_that("aux_variance works with normal input", {
  trials1 <- 10
  prob <- 0.3 
  var <- trials*prob*(1-prob)
  
  expect_equal(aux_variance(trials1,prob), var)
})

test_that("aux_mode works with normal input", {
  trials1 <- 10
  prob <- 0.3 
  mod <- floor((trials*prob) + prob)
  
  expect_equal(aux_mode(trials1,prob), mod)
})

test_that("aux_skewness works with normal input", {
  trials1 <- 10
  prob <- 0.3 
  skew <- (1-2*prob)/sqrt(trials*prob*(1-prob))
  
  expect_equal(aux_skewness(trials1,prob), skew)
})

test_that("aux_kurtosis works with normal input", {
  trials1 <- 10
  prob <- 0.3 
  kurt <- (1-6*prob*(1-prob))/(trials*prob*(1-prob))
  
  expect_equal(aux_kurtosis(trials1,prob), kurt)
})

test_that("bin_choose handles correct input values and works with normal inputs", {
  n1 <- 2
  k1 <- 3
  n2 <- -2
  k2 <- 1
  n3 <- 5
  k3 <- 1:3
  comb <- factorial(n1)/(factorial(k2)*factorial(n1-k2))
  
    expect_equal(bin_choose(n1, k2), comb)
    expect_error(bin_choose(n1,k1))
    expect_error(bin_choose(n2,k2))
    expect_length(bin_choose(n3,k3), length(k3))
    
})

test_that("bin_probability handles correct input values and works with normal inputs", {
  n1 <- 2
  k1 <- 3
  p1 <- 0.7
  n2 <- -2
  k2 <- 1
  p2 <- 1
  n3 <- 5
  k3 <- 1:3
  p3 <- 0.7
  proba <- bin_choose(n3,k2)*(p1**k2)*((1-p1)**(n3-k2))  #can u use another function in the test
  
  expect_equal(bin_probability(n3,k2,p1), proba)
  expect_error(bin_probability(n2,k1,p1))
  expect_length(bin_probability(n3,k3,p3), length(k3))
  
})

test_that("bin_distribution handles correct input values and works with normal inputs", {
  p1 <- 0.7
  n2 <- -2
  n3 <- 5
  p3 <- 0.42
 
  expect_is(bin_distribution(n3,p3), "bindis")
  expect_type(bin_distribution(n3,p3), "list")
  expect_error(bin_distribution(n2,p1))
 
})

test_that("bin_cumulative handles correct input values and works with normal inputs",{
  p1 <- 0.7
  n2 <- -2
  n3 <- 5
  p3 <- 0.42
  
  expect_is(bin_cumulative(n3,p3), "bincum")
  expect_type(bin_cumulative(n3,p3), "list")
  expect_error(bin_cumulative(n2,p1))
})


