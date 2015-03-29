Test1<-function()
{
  R=CoordinateSwitch_CreateRotateMatrix(-0.003926,-0.067556,0.002075,T);
  
  dis=matrix(c(39795.08,27476.75,7572.81),3,1);
  
  
  landpoint=matrix(c(36589.41,25273.32,2195.17,
                     37631.08,31324.51, 728.69,
                     40426.54,30319.81, 757.31,
                     39100.97,24934.98,2386.50),4,3,byrow=T);
  
  pixepoint=matrix(c(-86.15,-68.99,
                     -53.40,82.21,
                     10.46,64.43,
                     -14.78,-76.63),4,2,byrow=T);
   
  Intrinsic=CoordinateSwitch_CreateIntrinscMatrix(153.24,0,0);
  
  CoordinateSwitch_CollinearityEquation(landpoint,Intrinsic,R,dis);
}

Test2<-function()
{
  #构建旋转矩阵
  R=CoordinateSwitch_CreateRotateMatrix(0.002778,0,0,T);
  PrintWithTitle("R",R);
  #构建内方位元素矩阵
  Intrinsic=CoordinateSwitch_CreateIntrinscMatrix(153.24,0,0);
  PrintWithTitle("Intrinsic",Intrinsic);
  
  #设置地面点
  landpoint=matrix(c(40589,26273,2195,
                     38589,26273, 728,
                     38589,28273, 757,
                     40589,28273,2386),4,3,byrow=T);
  #设置中心
  dis=matrix(c(39795,27477,7573),3,1);
  #7参数坐标转化
  #result=CoordinateSwitch_Bursa7Parameters(landpoint,R,dis);
  #PrintWithTitle("Result",result);

  result=CoordinateSwitch_CollinearityEquation(landpoint,Intrinsic,R,dis);
  PrintWithTitle("Result",result);
}