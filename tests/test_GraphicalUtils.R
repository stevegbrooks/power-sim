context('GraphicalUtils.R')

#global inputs
alpha0 <- 0.025
gamma <- 0.5
alpha1 <- (alpha0 * (1-gamma))/2
#graphical.se1
primary <- c(  1,    1,   2,    2,   3,   3,   3,   3, 0)
g1      <- c(.01, .001,   1,    1, .02, .01, .02, .01, 1)
g2      <- c(  1,    1, .01, .001, .02, .02, .01, .01, 1)
expected <-c(  0,    1,   0,    2,   0,   1,   2,   3, 0)
context('testing graphical.se1 functionality - path coverage')
test_that('graphical.se1 returns correct values', {
  expect_equivalent(graphical.se1(g1, g2, primary, alpha0, alpha1), expected)
})

#graphical.se2
primary  <- c(  3,   3,   1,    1,   3,   3,   1,    1,   3,   3,   3,   3, 1, 0)
prev     <- c(  1,   1,   1,    1,   2,   2,   2,    2,   3,   3,   3,   3, 3, 0)
g1  <- list(c(.02, .01, .01, .001,   1,   1,   1,    1, .02, .01, .02, .01, 1, 1))
g2  <- list(c(  1,   1,   1,    1, .02, .01, .01, .001, .02, .02, .01, .01, 1, 1))
expected <- c(  0,   1,   0,    1,   0,   2,   0,    2,   0,   1,   2,   3, 0, 0)
context('testing graphical.se2 functionality - path coverage')
test_that('graphical.se2 returns correct values', {
  expect_equivalent(graphical.se2(g1, g2, primary, prev, alpha0, alpha1), expected)
})

#graphical.update.se1
toUpdate <- c(0,   1,   1,   2,   2,  3)
g1       <- c(1, .03, .01,   1,   1,  1)
g2       <- c(1,   1,   1, .03, .01,  1)
expected <- c(0,   0,   1,   0,   2, NA)
context('testing graphical.update.se1 functionality - path coverage')
test_that('graphical.update.se1 returns correct values', {
  expect_equivalent(graphical.update.se1(g1, g2, toUpdate, alpha0), expected)
})

#graphical.update.se2
toUpdate <- c(0,  2,   1,   1,  1,   2,   2)
prev     <- c(0,  1,   1,   1,  2,   2,   2)
g1  <- list(c(1,  1, .03, .01,  1,   1,   1))
g2  <- list(c(1,  1,   1,   1,  1, .03, .01))
expected <- c(0, NA,   0,   1, NA,   0,   2)
context('testing graphical.update.se2 functionality - path coverage')
test_that('graphical.update.se2 returns correct values', {
  expect_equivalent(graphical.update.se2(g1, g2, toUpdate, prev, alpha0), expected)
})

#graphical.recur.update
toUpdate <- c(0,  2,   1,   1,  1,   2,   2)
prev     <- c(0,  1,   1,   1,  2,   2,   2)
g1  <- list(c(1,  1, .03, .01,  1,   1,   1))
g2  <- list(c(1,  1,   1,   1,  1, .03, .01))
expected <- c(0, NA,   0,   1, NA,   0,   2)
context('testing graphical.recur.update functionality - path coverage')
test_that('graphical.recur.update returns correct values', {
  expect_equivalent(graphical.update.recur(g1, g2, toUpdate, prev, alpha0), expected)
})