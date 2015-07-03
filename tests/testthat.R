library(testthat)
library(Gini)
 
context("Gini calculations")

## All equal, expected gini = 0
da <- as.data.frame(cbind(region = rep(1,20), income = rep(1, 20)))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0)
  })


## All equal, expected gini = 0
da <- as.data.frame(cbind(region = rep(1,20), income = rep(10, 20)))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0)
})


## Half takes all,  expected gini = 0.5
da <- as.data.frame(cbind(region = rep(1,20), income = c(rep(0, 10) ,rep(10, 10))))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0.5)
})

## Half takes all
## https://en.wikipedia.org/wiki/Gini_coefficient
da <- as.data.frame(cbind(region = rep(1,10), income = c(rep(0, 5) ,rep(10, 5))))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0.5)
})


da <- as.data.frame(cbind(region = rep(1,20), income = c(rep(0, 10) ,rep(1, 10))))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0.5)
})

da <- as.data.frame(cbind(region = rep(1,20), income = c( rep(1, 10), rep(0, 10))))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0.5)
})

da <- as.data.frame(cbind(region = rep(1,10000), income = c( rep(1, 5000), rep(0, 5000))))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0.5)
})

## One takes all,  expected gini = 1 - 1/N = 0.95
da <- as.data.frame(cbind(region = rep(1,20), income = c( rep(1, 1), rep(0, 19))))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0.95)
})

## One takes all  expected gini = 1 - 1/N = 0.9
da <- as.data.frame(cbind(region = rep(1,10), income = c( rep(1, 1), rep(0, 9))))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0.9)
})

## One takes all, expected gini = 1 - 1/N = 1-1/20000 0 0.99995
da <- as.data.frame(cbind(region = rep(1,20000), income = c( rep(1, 1), rep(0, 19999))))
test_that("Gini gives the right answer", {
  expect_equal(as.numeric(huvud(da)["gini"]) , 0.99995)
})



# test_check("Gini")
