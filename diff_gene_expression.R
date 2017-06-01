var_test <- function(GDS){
  normal   <- GDS[1:7]
  abnormal <- GDS[8:14]
  result   <- var.test(normal, abnormal)
  return(result$p.value)
}

# equal variance
GDS_t_test1 <- function(GDS){
  normal   <- GDS[1:7]
  abnormal <- GDS[8:14]  
  result <- t.test(normal, abnormal, var.equal = TRUE)
  return(result$p.value)
}  

# with no equal variance
GDS_t_test2 <- function(GDS){
  normal   <- GDS[1:7]
  abnormal <- GDS[8:14]  
  result <- t.test(normal, abnormal)
  return(result$p.value)
}  

var_result <- apply(GDS1, 1, var_test)
ifelse(var_result > 0.05, a <- apply(GDS1,1,GDS_t_test1),
       a <- apply(GDS1,1,GDS_t_test2))


diff_test <- function(GDS){
    normal   <- GDS[1:7]
    abnormal <- GDS[8:14]
    result   <- var.test(normal, abnormal)
    if(result$p.value > 0.05){
      t_result <- t.test(normal, abnormal, var.equal = TRUE)
      return(t_result$p.value)
    }
    else{
      t_result <- t.test(normal, abnormal)
      return(t_result$p.value)
    }
  }