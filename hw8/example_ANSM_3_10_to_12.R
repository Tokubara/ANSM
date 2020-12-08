# Example 3.10
d <- c(0.6, 0.8, 1.1, 1.2, 1.4, 1.7, 1.8, 1.9, 2.2, 2.4, 2.5, 2.9, 3.1, 3.4, 3.4, 3.9, 4.4, 4.9, 5.2, 5.9)
ecdf_d <- ecdf(d)
ks.test(d,punif, 0, 6)
eg11_F<-function(x) {
  if(x>0 && x<=3) {
    return(x/5)
  } else if(x>3&x<=6) {
    return(0.2+4*x/30)
  } else if(x<=0) {
    return(0)
  } else {
    return(1)
  }
}
ks.test(d,eg11_F)
max(abs(ecdf_d(d) - eg11_F(d)), abs(ecdf_d(d) - 1 / length(d) - eg11_F(d)))
max(abs(ecdf_d(d) - punif(d, 0, 6)), abs(ecdf_d(d) - 1 / length(d) - punif(d, 0, 6)))

# Example 3.12
d <- c(11, 13, 14, 22, 29, 30, 41, 41, 52, 55, 56, 59, 65, 65, 66, 74, 74, 75, 77, 81, 82, 82, 82, 82, 83, 85, 85, 87, 87, 88)
mean(d)
sd(d)
std_d<-(d-mean(d))/sd(d)
sample_size=length(d) # 30
ks.test(std_d, pnorm)
