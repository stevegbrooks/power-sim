context('Graphical.R')

#global inputs
alpha0 <- 0.025
gamma <- 0.5
alpha1 <- (alpha0 * (1-gamma))/2

#graphical - 2 secondaries
primary <-             c(  1,    1,   2,    2,   3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3, 0)

g1    <-  list('pe1' = c(  1, .001,   1,    1,   1, .001,    1, .001, .001, .001, .001, .001, .001, .001, .001, .001, 1),
               'se1' = c(  1, .001,   1,    1,   1, .001,    1, .001,    1, .001,    1, .001, .001, .001, .001, .001, 1),
               'se2' = c(  1, .001,   1,    1,   1, .001,    1, .001,    1, .001,    1, .001,    1, .001,    1, .001, 1))

g2    <-  list('pe1' = c(  1,    1,   1, .001,   1,    1, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, 1),
               'se1' = c(  1,    1,   1, .001,   1,    1, .001, .001,    1,    1, .001, .001, .001, .001, .001, .001, 1),
               'se2' = c(  1,    1,   1, .001,   1,    1, .001, .001,    1,    1, .001, .001,    1,    1, .001, .001, 1))

expected <- data.frame(c(  0,    1,   0,    2,   0,    1,    2,    3,    0,    1,    2,    3,    3,    3,    3,    3, 0),
                       c(  0,    1,   0,    2,   0,    1,    2,    3,    0,    1,    2,    3,    0,    1,    2,    3, 0))
context('testing graphical functionality - path coverage')
test_that('graphical returns correct values', {
  expect_equivalent(graphical(g1 = g1[-which(grepl('pe', names(g1)))], 
                              g2 = g2[-which(grepl('pe', names(g2)))], 
                              primary = primary, 
                              prev = NA, 
                              alpha0 = alpha0, 
                              alpha1 = alpha1), 
                    expected)
})

#graphical - 1 secondary
primary <-             c(  1,    1,   2,    2,   3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3, 0)

g1    <-  list('pe1' = c(  1, .001,   1,    1,   1, .001,    1, .001, .001, .001, .001, .001, .001, .001, .001, .001, 1),
               'se1' = c(  1, .001,   1,    1,   1, .001,    1, .001,    1, .001,    1, .001, .001, .001, .001, .001, 1))

g2    <-  list('pe1' = c(  1,    1,   1, .001,   1,    1, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, 1),
               'se1' = c(  1,    1,   1, .001,   1,    1, .001, .001,    1,    1, .001, .001, .001, .001, .001, .001, 1))

expected <- data.frame(c(  0,    1,   0,    2,   0,    1,    2,    3,    0,    1,    2,    3,    3,    3,    3,    3, 0))
context('testing graphical functionality - path coverage with only 1 secondary')
test_that('graphical returns correct values', {
  expect_equivalent(graphical(g1 = g1[-which(grepl('pe', names(g1)))],
                              g2 = g2[-which(grepl('pe', names(g2)))],
                              primary = primary,
                              prev = NA,
                              alpha0 = alpha0,
                              alpha1 = alpha1),
                    expected)
})

#graphical.update
results.gr <- data.frame(c(  0,    1,   2,    3,   3,    3,     3,    3,     3,     3,     3,     3,    3,     3,    3),
                         c(  0,    1,   2,    3,   3,    1,     1,    1,     2,     2,     2,     3,    3,     3,    3),
                         c(  0,    1,   2,    3,   0,    1,     1,    1,     2,     2,     2,     1,    1,     2,    2),
                         fix.empty.names = F)

g1      <-  list('pe1' = c(  1,    1,   1,    1,   1,    1,     1,     1,    1,     1,     1,     1,    1,     1,    1),
                 'se2' = c(  1,    1,   1,    1,   1,    1,     1,     1,    1,alpha0,alpha0,     1,    1,alpha0,    1),
                 'se3' = c(  1,    1,   1,    1,   1,    1,     1,     1,    1,alpha0,     1,     1,    1,alpha0,    1))

g2      <-  list('pe1' = c(  1,    1,   1,    1,   1,    1,     1,     1,    1,     1,     1,     1,    1,     1,    1),
                 'se2' = c(  1,    1,   1,    1,   1,    1,alpha0,alpha0,    1,     1,     1,alpha0,    1,     1,    1),
                 'se3' = c(  1,    1,   1,    1,   1,    1,alpha0,     1,    1,     1,     1,alpha0,    1,     1,    1))

expected <-   data.frame(c(  0,    1,   2,    3,   3,    3,     3,     3,    3,     3,     3,     3,    3,     3,    3),
                         c(  0,    1,   2,    3,   3,    1,     3,     3,    2,     3,     3,     3,    3,     3,    3),
                         c(  0,    1,   2,    3,   0,    1,     3,     1,    2,     3,     2,     3,    1,     3,    2),
                         fix.empty.names = F)

context('testing graphical.update functionality - path coverage')
test_that('graphical.update returns correct values', {
  expect_equivalent(graphical.update(g1 = g1[-which(grepl('pe', names(g1)))], 
                                     g2 = g2[-which(grepl('pe', names(g2)))], 
                                     results.gr = results.gr, 
                                     alpha0 = alpha0), 
                    expected)
})

#graphical.update.alt
results.gr <- data.frame(c(         3,          3,          3,          3,     3,     3,    3,     3),
                         c(         1,          1,          1,          3,     3,     3,    3,     3),
                         c(         1,          1,          1,          1,     1,     3,    3,     3),
                         c(         1,          1,          0,          1,     1,     1,    1,     1),
                         fix.empty.names = F)

g1      <-  list('pe1' = c(         0,          0,          0,          1,     1,     1,    1,     1),
                 'ks1' = c(  alpha0/2,   alpha0/2,   alpha0/2,          1,     1,     1,    1,     1),
                 'ks2' = c(  alpha0/2,   alpha0/2,   alpha0/2,          1,     1,     1,    1,     1),
                 'se1' = c(  alpha0/2,   alpha0/4,     alpha0,          1,     1,     1,    1,     1)
                 )

g2      <-  list('pe1' = c(         0,          0,          0,          1,     1,     1,    1,     1),
                 'ks1' = c(alpha0*3/4, alpha0*3/4, alpha0*3/4,          1,     1,     1,    1,     1),
                 'ks2' = c(alpha0*3/4, alpha0*3/4, alpha0*3/4,          1,     1,     1,    1,     1),
                 'se1' = c(alpha0*3/4, alpha0*3/4, alpha0*3/4,          1,     1,     1,    1,     1)
                 )

expected <-   data.frame(c(         3,          3,          3,          3,     3,     3,    3,     3),
                         c(         3,          3,          3,          3,     3,     3,    3,     3),
                         c(         3,          3,          3,          3,     2,     3,    1,     3),
                         c(         3,          3,          3,          3,     2,     3,    1,     3),
                         fix.empty.names = F)

context('testing graphical.update functionality - path coverage')
test_that('graphical.update returns correct values', {
  expect_equivalent(graphical.update(g1 = g1[-which(grepl('pe', names(g1)))],
                                     g2 = g2[-which(grepl('pe', names(g2)))],
                                     results.gr = results.gr,
                                     alpha0 = alpha0),
                    expected)
})

