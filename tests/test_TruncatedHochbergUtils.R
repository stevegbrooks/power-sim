context('TruncatedHochbergUtils.R')

#global inputs
alpha <- 0.025
gamma <- 0.5

#truncHoch.pe
g1      <- c(.01, .001,   1,    1, .02, .01, .02, .01, 1)
g2      <- c(  1,    1, .01, .001, .02, .02, .01, .01, 1)
expected <-c(  1,    1,   2,    2,   0,   1,   2,   3, 0)
context('testing truncHoch.pe functionality - path coverage')
test_that('truncHoch.pe returns correct values', {
  expect_equivalent(truncHoch.pe(g1, g2, alpha, gamma), expected)
})

#truncHoch.cope - 1 
g1      <- list('pe1' = c(.01, .001,   1,    1, .02, .01, .02, .01, 1))
g2      <- list('pe1' = c(  1,    1, .01, .001, .02, .02, .01, .01, 1))
expected <-             c(  1,    1,   2,    2,   0,   1,   2,   3, 0)
context('testing truncHoch.cope functionality - path coverage')
test_that('truncHoch.cope returns correct values', {
  expect_equivalent(truncHoch.cope(g1, g2, alpha, gamma), expected)
})

#truncHoch.cope - 2
g1      <- list('pe1' = c(  1, .001,    1, .001, .001, .001,    1,    1, .001, .001),
                'pe2' = c(  1,    1, .001, .001, .001, .001, .001,    1,    1, .001))
g2      <- list('pe1' = c(  1,    1, .001,    1,    1, .001, .001, .001, .001, .001),
                'pe2' = c(  1, .001,    1, .001,    1,    1, .001, .001, .001, .001))
expected <-             c(  0,    0,    0,    1,    1,    1,    2,    2,    2,    3)
context('testing truncHoch.cope functionality - path coverage with co-primary')
test_that('truncHoch.cope returns correct values', {
  expect_equivalent(truncHoch.cope(g1, g2, alpha, gamma), expected)
})

#truncHoch.se - last = F
g1       <- list('se1' = c(1, .001, 1,    1, .02, .01, .02, .01, 1))
g2       <- list('se1' = c(1,    1, 1, .001, .02, .02, .01, .01, 1))
prev     <-              c(1,    1, 2,    2,   3,   3,   3,   3, 0)
expected <-              c(0,    1, 0,    2,   0,   1,   2,   3, 0)
context('testing truncHoch.se functionality when last = F - path coverage')
test_that('truncHoch.pe returns correct values when last = F', {
  expect_equivalent(truncHoch.se(g1, g2, prev, alpha, gamma), expected)
})

#truncHoch.se - last = T
g1       <- list('se1' = c(1, .001, 1,    1, .03, .01, .03, .01, 1))
g2       <- list('se1' = c(1,    1, 1, .001, .03, .03, .01, .01, 1))
prev     <-              c(1,    1, 2,    2,   3,   3,   3,   3, 0)
expected <-              c(0,    1, 0,    2,   0,   1,   2,   3, 0)
context('testing truncHoch.se functionality when last = T - path coverage')
test_that('truncHoch.pe returns correct values when last = T', {
  expect_equivalent(truncHoch.se(g1, g2, prev, alpha, gamma, last = T), expected)
})