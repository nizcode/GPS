#Calculating HMLD
a <- 9.9166746
v <- 1.3805567
metabolic <- function(a){
  alpha <- atan(9.8/a)*(180/pi)
  ES <- tan(((90-alpha)/180)*pi)
  EM <- (sqrt(a**2+9.8**2)/9.8)
  EC <- ((155.4*(ES**5))-(30.4*ES**4)-(43.4*ES**3)+(46.3*ES**2)+(19.5*ES)+3.6)*EM
  return(EC)
}
  




