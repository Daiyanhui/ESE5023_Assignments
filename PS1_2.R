M1 <- matrix(sample(0:50,50),nrow = 5,ncol = 10)
M2 <- matrix(sample(0:50,50),nrow = 10,ncol = 5)
M3 <- matrix(nrow=5,ncol=5)
Matrix_multip <- function(m1,m2) {
  c=M1%*%M2
  
  for (i in 1:5){
    for (j in 1:5){
      m <- 1
      sum <- 0
      for (m in 1:10){
        
        sum=M1[i,m]*M2[m,j]+sum
        M3[i,j]=sum
      }
    }
  }

  print(c)
  print(M3)
}
Matrix_multip(M1,M2)


