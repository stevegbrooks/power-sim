context('TruncatedHochberg.R')

#global inputs
alpha <- 0.025
gamma <- 0.5

#truncHoch with only one primary endpoint
g1      <-        list('pe1' = c(  1, .001,    1, .001, .001, .001, .001, .001, .001, .001),
                       'se1' = c(  1, .001,    1, .001,    1, .001,    1, .001, .001, .001),
                       'se2' = c(  1, .001,    1, .001,    1, .001,    1,    1, .001,    1))
g2      <-        list('pe1' = c(  1,    1, .001, .001, .001, .001, .001, .001, .001, .001),
                       'se1' = c(  1,    1, .001, .001,    1,    1, .001, .001, .001, .001),
                       'se2' = c(  1,    1, .001, .001,    1,    1, .001,    1,    1, .001))
expected <-         data.frame(c(  0,    1,    2,    3,    3,    3,    3,    3,    3,    3),
                               c(  0,    1,    2,    3,    0,    1,    2,    3,    3,    3),
                               c(  0,    1,    2,    3,    0,    1,    2,    0,    1,    2),
                               fix.empty.names = F)
context('testing truncHoch functionality - path coverage')
test_that('truncHoch returns correct values', {
  expect_equivalent(truncHoch(g1, g2, prev = NA, alpha, gamma), expected)
})

#trunchHoch with co-primary endpoints
g1      <- list('pe1' = c(  1, .001,    1, .001, .001, .001,    1,    1, .001, .001, .001, .001, .001),
                'pe2' = c(  1,    1, .001, .001, .001, .001, .001,    1,    1, .001, .001, .001, .001),
                'se1' = c(  1, .001,    1, .001,    1, .001,    1, .001, .001, .001,    1,    1, .001))
g2      <- list('pe1' = c(  1,    1, .001,    1,    1, .001, .001, .001, .001, .001, .001, .001, .001),
                'pe2' = c(  1, .001,    1, .001,    1,    1, .001, .001, .001, .001, .001, .001, .001),
                'se1' = c(  1,    1, .001,    1, .001, .001, .001, .001,    1,    1, .001,    1, .001))
expected <-  data.frame(c(  0,    0,    0,    1,    1,    1,    2,    2,    2,    3,    3,    3,    3),
                        c(  0,    0,    0,    1,    0,    1,    2,    2,    0,    1,    2,    0,    3),
                        fix.empty.names = F)
context('testing truncHoch functionality - path coverage with co-primary')
test_that('truncHoch returns correct values', {
  expect_equivalent(truncHoch(g1, g2, prev = NA, alpha, gamma), expected)
})

#trunchHoch with co-primary endpoints and testing the last endpoint feature where gamma gets set to 1
g1      <- list('pe1' = c(  1, .001,    1, .001, .001, .001,    1,    1, .001,     .001,   .001,     .001, .001),
                'pe2' = c(  1,    1, .001, .001, .001, .001, .001,    1,    1,     .001,   .001,     .001, .001),
                'se1' = c(  1, .001,    1, .025,    1, .025,    1, .025, .025,  alpha/2,      1,alpha+.01,alpha))
g2      <- list('pe1' = c(  1,    1, .001,    1,    1, .001, .001, .001, .001,     .001,   .001,     .001, .001),
                'pe2' = c(  1, .001,    1, .001,    1,    1, .001, .001, .001,     .001,   .001,     .001, .001),
                'se1' = c(  1,    1, .001,    1, .001, .025, .025, .025,    1,alpha+.01,alpha/2,alpha+.01,alpha))
expected <-  data.frame(c(  0,    0,    0,    1,    1,    1,    2,    2,    2,    3,    3,    3,    3),
                        c(  0,    0,    0,    0,    0,    0,    0,    0,    0,    1,    2,    0,    3),
                        fix.empty.names = F)
context('testing truncHoch functionality - path coverage with co-primary and last gamma = 1')
test_that('truncHoch returns correct values', {
  expect_equivalent(truncHoch(g1, g2, prev = NA, alpha, gamma), expected)
})