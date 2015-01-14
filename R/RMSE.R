# Team: ZeaPol   
# Team Members: Roeland de Koning / Barbara Sienkiewicz    
# Date: 12/01/2015       
# Exercise 7

#Alternate Function to Caluclate the RMSE between x and y as Standard Function does not Function

RMSE <- function(x,y) {
  z <- (x - y)^2
  w <- cellStats(z, stat = 'mean',na.rm = T)
  return (sqrt(w)) 
}
