Pascal_triangle <- function(k) {
  data_k_line <- array(NA,k)
  data_k_line[1] <- 1
  m <- 1
  for (i in 2:k) {
    #应加个判断，当k=1时如何处理？事实上，当k为1时你的代码无法输出正确的结果
    m <- m*((k-i+1)/(i-1))
    data_k_line[i] <- m
  }
  print(data_k_line)
}
Pascal_triangle(100)
Pascal_triangle(200)
