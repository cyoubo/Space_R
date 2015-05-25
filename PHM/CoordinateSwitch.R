#=========================================================
#函数集合说明：
#该R文件，主要用于有关坐标变化的函数计算
#=========================================================

#---------------------------------------------------------
#=========================================================
#函数名称：CoordinateSwitch_CollinearityEquation
#函数说明：基于共线方程完成物方空间坐标与像平面坐标的转换
#参数说明：Intrinsc 内方位元素矩阵
#          ResCoor 待转换点,RMatrix 旋转矩阵,TMatrix 平移矩阵
#          lambda 缩放系数 默认为1
#=========================================================
CoordinateSwitch_CollinearityEquation<-function(ResCoor,Intrinsc,RMatrix,TMatrix,lambda=1,filename="")
{
  
  ######################################################################
  path=paste("E://collinear",filename,".txt");
  FileHelper_Data_Txtoutputting(path,"ResCoor",ResCoor);
  FileHelper_Data_TxtAppendoutputting(path,"RMatrix",RMatrix);
  FileHelper_Data_TxtAppendoutputting(path,"RMatrix is cross",(t(RMatrix)-solve(RMatrix)));
  ######################################################################
  
  #1.7参数变化
  temp=CoordinateSwitch_Bursa7Parameters(ResCoor,RMatrix,TMatrix,lambda);
  
  ######################################################################
  FileHelper_Data_TxtAppendoutputting(path,"imageSpace",t(temp));
  ######################################################################
  
  #2.合法性检验
  CoordinateSwitch_IsValid2(temp,Intrinsc);
  #2.共线投影计算
  temp=Intrinsc%*%temp;
  #将z值消去
  result=rbind(temp[c(1),]/(-temp[c(3),]),temp[c(2),]/(-temp[c(3),]))
  
  ######################################################################
  FileHelper_Data_TxtAppendoutputting(path,"imagepoint",t(result));
  ######################################################################
  
  return (t(result));
}
#---------------------------------------------------------
#=========================================================
#函数名称：CoordinateSwitch_Bursa7Parameters
#函数说明：基于布尔莎七参数模型完成空间坐标的转换
#参数说明：ResCoor 待转换点(列数为3),RMatrix 旋转矩阵,TMatrix 平移矩阵
#          lambda 缩放系数 默认为1
#=========================================================
CoordinateSwitch_Bursa7Parameters<-function(ResCoor,RMatrix,TMatrix,lambda=1)
{
  #1.合法性检验
  if(CoordinateSwitch_IsValid(ResCoor,RMatrix,TMatrix)==0)
    return (0);
  ResCoor=t(ResCoor);
  #3.根据中心坐标与待转换点的个数，生成对应长度的平移矩阵
  TMatrix=t(matrix(rep(TMatrix,dim(ResCoor)[2]),dim(ResCoor)[2],3,byrow=T));
  #4.坐标变换运算
  return (lambda*RMatrix%*%(ResCoor-TMatrix));
}
#---------------------------------------------------------
#=========================================================
#函数名称：CoordinateSwitch_CreateIntrinscMatrix
#函数说明：构建内方位元素矩阵
#参数说明：Fouce 相机焦距 Cx，Cy主点位移
#=========================================================
CoordinateSwitch_CreateIntrinscMatrix<-function(Fouce,Cx,Cy)
{
  return (matrix(c(Fouce,0,Cx,0,Fouce,Cy,0,0,1),3,3,byrow=T));
}
#---------------------------------------------------------
#=========================================================
#函数名称：CoordinateSwitch_CreateRotateMatrix
#函数说明：构建旋转矩阵
#参数说明：arpha,kappa,grama 旋转角度，以角度形式表示
#=========================================================
CoordinateSwitch_CreateRotateMatrix<-function(phi,omiga,kappa,israd=F)
{
  if(israd==F)
  {
    p=phi*pi/180;k=kappa*pi/180;w=omiga*pi/180;
  }
  else
  {
    p=phi;k=kappa;w=omiga;
  }  
  a1= cos(p)*cos(k)-sin(p)*sin(w)*sin(k);
  a2=-cos(p)*sin(k)-sin(p)*sin(w)*cos(k);
  a3=-sin(p)*cos(w);
  b1= cos(w)*sin(k);
  b2= cos(w)*cos(k);
  b3=-sin(w);
  c1= sin(p)*cos(k)+cos(p)*sin(w)*sin(k);
  c2=-sin(p)*sin(k)+cos(p)*sin(w)*cos(k);
  c3= cos(p)*cos(w);
  return (matrix(c(a1,b1,c1,a2,b2,c2,a3,b3,c3),3,3,byrow=T));
}
#---------------------------------------------------------
#=========================================================
#函数名称：CoordinateSwitch_CreateTranslateMatrix
#函数说明：构建平移矩阵
#参数说明：X0,Y0,Z0 平移常数
#=========================================================
CoordinateSwitch_CreateTranslateMatrix<-function(X0,Y0,Z0)
{
  return (matrix(c(X0,Y0,Z0),3,1,byrow=T));
}
#---------------------------------------------------------
#=========================================================
#函数名称：CoordinateSwitch_ResolveRotateMatrix
#函数说明：通过旋转矩阵反解出旋转角度
#参数说明：Rmatrix 待反解的旋转矩阵
#=========================================================
CoordinateSwitch_ResolveRotateMatrix<-function(Rmatrix)
{
  p=atan2(Rmatrix[1,3],Rmatrix[3,3]);
  w=asin(-Rmatrix[2,3]);
  k=atan2(Rmatrix[2,1],Rmatrix[2,2]);
  return (list(Phi=p,W=w,Kappa=k));
}
#---------------------------------------------------------
#=========================================================
#函数名称：CoordinateSwitch_IsValid
#函数说明：检验传入参数的合法性
#参数说明：ResCoor 待转换点,RMatrix 旋转矩阵,TMatrix 平移矩阵
#=========================================================
CoordinateSwitch_IsValid<-function(ResCoor,RMatrix,TMatrix)
{
  if(dim(ResCoor)[1]!=3)#若行数不为3，则判断写入的点可能是按照(x,y,z)*n行读入的
  {
    if(dim(ResCoor)[2]!=3)
    {
      cat("the dim of Resource Coordinata is invaild");
      return (0);
    }
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
  return (1);
}
#---------------------------------------------------------
#=========================================================
#函数名称：CoordinateSwitch_IsValid2
#函数说明：检验传入参数的合法性
#参数说明：TempPoints 7参数转化后的点,Intrinsc 内方位元素矩阵
#=========================================================
CoordinateSwitch_IsValid2<-function(TempPoints,Intrinsc)
{
  if(!is.matrix(TempPoints))
  {
    cat("Bursa7Parameters solove failed");
    return (0);
  }
  if(Intrinsc[1,1]<0|Intrinsc[2,2]<0)
  {
    cat("the value of Fouce is invaild");
    return (0);
  }
  return (1);
}
#-----------------------------------------------------------------------------
#=============================================================================
#函数名称：CoordinateSwitch_Screan_To_PhotoCoordinateSystem
#函数说明：完成屏幕坐标到像平面坐标的转换
#参数说明：ImageHeight 图像高度,ImageWidth 图像宽度,Points 按行分布的(x,y)坐标
#=============================================================================
CoordinateSwitch_Screan_To_PhotoCoordinateSystem<-function(ImageHeight,ImageWidth,Points)
{
  TranslateMatrix=matrix(c(1,0,0,0,1,0,-ImageWidth/2.0,-ImageHeight/2.0,1),3,3,byrow=T);
  MirrorMatrix=matrix(c(1,0,0,0,-1,0,0,0,1),3,3,byrow=T);
  M=TranslateMatrix%*%MirrorMatrix;
  result=CoordinateSwitch_2DPointCrossMatrix(Points,M);
  return(result);
 
}
#-----------------------------------------------------------------------------
#=============================================================================
#函数名称：CoordinateSwitch_Screan_To_PhotoCoordinateSystem
#函数说明：完成像平面坐标到屏幕坐标的转换
#参数说明：ImageHeight 图像高度,ImageWidth 图像宽度,Points 按行分布的(x,y)或(x,y,1)坐标
#=============================================================================
CoordinateSwitch_Photo_To_ScreanCoordinateSystem<-function(ImageHeight,ImageWidth,Points)
{
  TranslateMatrix=matrix(c(1,0,0,0,1,0,ImageWidth/2.0,ImageHeight/2.0,1),3,3,byrow=T);
  MirrorMatrix=matrix(c(1,0,0,0,-1,0,0,0,1),3,3,byrow=T);
  M=MirrorMatrix%*%TranslateMatrix;
  result=CoordinateSwitch_2DPointCrossMatrix(Points,M);
  return(result[,c(1,2)]);
}
#-----------------------------------------------------------------------------
#=============================================================================
#函数名称：CoordinateSwitch_2DPointCrossMatrix
#函数说明：完成二维点的矩阵变化运算
#参数说明：Points 按行分布的(x,y)或(x,y,1)坐标，M 3*3 变化矩阵
#=============================================================================
CoordinateSwitch_2DPointCrossMatrix<-function(Points,M)
{
  size=dim(Points);
  if(size[2]==2)
  {
    one=rep(1,size[1]);
    Points=cbind(Points,one);
    return (Points%*%M);
  }
  else if(size[2]==3)
  {
    return (Points%*%M);
  }
  else
  {
    return (-1);
  }
}