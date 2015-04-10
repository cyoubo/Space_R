
PrintWithTitle<-function(title,object)
{
  print(paste("---------------",title,"------------------"));
  print(object);
}
#=============================================================
#函数名称：Cal_distance_from_Point
#函数说明：计算ohters点到center的欧式距离
#参数说明：center 中心点c(3),others待计算距离点matrix(n,3)
#=============================================================
Cal_distance_from_Point<-function(center,others)
{
  result=rep(NA,dim(others)[1]);
  for(i in c(1:dim(others)[1]))
  {
    result[i]=sqrt((others[i,1]-center[1])^2+(others[i,2]-center[2])^2+(others[i,3]-center[3])^2);  
  }
  return(result);
}