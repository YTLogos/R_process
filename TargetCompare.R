target_compare <- function(target_file,untarget_file,results_file)
{
  #数据导入
  target <- read.table(target_file,header = TRUE)
  untarget <- read.table(untarget_file,header = TRUE)
  
  #文本输出重定向
  sink(results_file,split=TRUE)
  
  #提取数据长度
  trow <- nrow(target)
  urow <- nrow(untarget)
  
  #以Target analysis 结果为基准，去比对Untarget analysis 
  #结果中是否有Target analysis 结果。比对的原则是m/z 
  #在±10 ppm，RT 在±0.3 min中内的，则认为是一个峰。
  #这样的峰如果出现在Untarget analysis 结果中，
  #则挑选出来（连同ID号）。
  
  for(i in 1:trow){
    for(j in 1:urow){
      y <- abs(target[i,2] - untarget[j,2])
      z <- abs(target[i,3] - untarget[j,3])
      
      if(y <= 10 & z <= 0.3)
        print(cbind(target[i,],untarget[j,]))
        write.table(cbind(target[i,],untarget[j,]),'write_result.txt',append = TRUE)
        
    }
  }
  return(TRUE)
}