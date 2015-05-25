
PrintWithTitle<-function(title,object)
{
  print(paste("---------------",title,"------------------"),quote=F);
  print(object,quote=F);
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
#=============================================================
#函数名称：Cal_distance_from_Point
#函数说明：计算像控点之间的夹角
#参数说明：
#=============================================================
Cal_distance_angle_imagepoint<-function(p1,p2,p3,israd=F)
{
  v12=c(p2[1]-p1[1],p2[2]-p1[2]);
  v13=c(p3[1]-p1[1],p3[2]-p1[2]);
  v23=c(p3[1]-p2[1],p3[2]-p2[2]);

  mod12=sqrt(v12[1]^2+v12[2]^2);
  mod13=sqrt(v13[1]^2+v13[2]^2);
  mod23=sqrt(v23[1]^2+v23[2]^2);

  a213=acos(v12%*%v13/(mod12*mod13));
  a123=acos(-v12%*%v23/(mod12*mod23));
  a132=acos(-v13%*%-v23/(mod13*mod23));
  
  if(israd==T)
  {
    return (c(a213,a123,a132));
  }
  else
  {
    return (c(a213*180/pi,a123*180/pi,a132*180/pi));
  }
}
Cal_distance_angle_imagepoint2<-function(imagepoints,israd=F)
{
  p1=as.vector(imagepoints[1,]);
  p2=as.vector(imagepoints[2,]);
  p3=as.vector(imagepoints[3,]);
  
  return (Cal_distance_angle_imagepoint(p1,p2,p3,israd)); 
}