Print_values <- function(a,b,c) {
  if (a > b && b > c) {
    print (c(a,b,c))
  }
  if (a > b && b <= c && a > c) {
    print (c(a,c,b))
  }
  if (a > b && b <= c && a <= c) {
    print (c(c,a,b))
  }
  if (a <= b && b <= c) {
    print (c(c,b,a))
  }
  if (a <= b && b > c && a <= c) {
    print (c(c,a,b))
  }
  if (a <= b && b > c && a > c) {
    print (c(a,c,b))
  }
}
Print_values(6,8,7)

#建议采用 if(){} else(){}结构，逻辑关系更清楚
