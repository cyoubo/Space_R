#=========================================================
#函数集合说明：
#该R文件，主要用于有关坐标变化的函数计算
#=========================================================

#=========================================================
#=========================================================
#函数名称：CoordinateSwitch_CollinearityEquation
#函数说明：基于共线方程完成物方空间坐标与像平面坐标的转换
#参数说明：Fouce 相机焦距 Cx，Cy主点位移
#          ResCoor 待转换点,RMatrix 旋转矩阵,TMatrix 平移矩阵
#          lambda 缩放系数 默认为1
#=========================================================

CoordinateSwitch_CollinearityEquation<-function(Fouce,Cx,Cy,ResCoor,RMatrix,TMatrix,lambda=1)
{
  #1.合法性检验
  temp=CoordinateSwitch_Bursa7Parameters(ResCoor,RMatrix,TMatrix,lambda);
  if(temp==0){return (0);}
  if(Fouce<0)
  {
    cat("the value of Fouce is invaild");
    return (0);
  }
  #2.共线投影计算
  ProjectionMatrix=Matrix(c(Fouce,0,Cx,0,Fouce,Cy,0,0,1));
  temp=ProjectionMatrix%*%temp;
  
  return ((temp/temp[,3])[,c(1,2)]);
  
}
#=========================================================
#=========================================================
#函数名称：CoordinateSwitch_Bursa7Parameters
#函数说明：基于布尔莎七参数模型完成空间坐标的转换
#参数说明：ResCoor 待转换点,RMatrix 旋转矩阵,TMatrix 平移矩阵
#          lambda 缩放系数 默认为1
#=========================================================
CoordinateSwitch_Bursa7Parameters<-function(ResCoor,RMatrix,TMatrix,lambda=1)
{
  #1.合法性检验
  if(dim(ResCoor)[2]!=3)
  {
    cat("the dim of Resource Coordinata is invaild");
    return (0);
  }
  if(dim(RMatrix)[1]!=3|dim(RMatrix)[2]!=3)
  {
    cat("the dim of RMatrix is invaild");
    return (0);
  }
  if(dim(TMatrix)[1]!=3)
  {
    cat("the dim of TMatrix is invaild");
    return (0);
  }
  #2.坐标变换运算
  return (lambda*RMatrix%*%ResCoor-TMatrix);
  
}