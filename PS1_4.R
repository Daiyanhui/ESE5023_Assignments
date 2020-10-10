Least_moves <- function(m) {
  n <- 0
  while(m != 1) {
    if(m %% 2 == 0) {
      m <- m/2
      n <- n+1
    } else {
      m <- m-1
      n <- n+1
    }
  }
  print(n)
}
Least_moves(100)
