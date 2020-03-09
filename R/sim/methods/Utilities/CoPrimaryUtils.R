peTest.classical <- function(primaryResults) {
  unlist(lapply(seq(primaryResults[[1]]), 
                function(x) {
                  if_else(primaryResults[[1]][x] == primaryResults[[2]][x], #if the result is the same for both
                          primaryResults[[1]][x], #then arbitrarily return the first result
                          if_else(primaryResults[[1]][x] == 3 
                                  | primaryResults[[2]][x] == 3, #if not, check if either are 'both sig'
                                  min(primaryResults[[1]][x], primaryResults[[2]][x]), #if so, then return the most conservative result (the min of the two results)
                                  0)) #if not, return 'none sig'
                }))
}