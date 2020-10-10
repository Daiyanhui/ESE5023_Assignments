Pascal_triangle <- function(k) {
  data_k_line <- array(NA,k)
  data_k_line[1] <- 1
  m <- 1
  for (i in 2:k) {
    m <- m*((k-i+1)/(i-1))
    data_k_line[i] <- m
  }
  print(data_k_line)
}
Pascal_triangle(100)
Pascal_triangle(200)
