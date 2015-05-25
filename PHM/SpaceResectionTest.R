#=============================================================
#函数集合说明�?
#该R文件，主要用于空间后方交会直接解的测�?
#=============================================================

#=============================================================
#函数说明：获取测试数�?1
#该数据来自于官云兰文献《》中数据1
#
#=============================================================
SpaceResectionTest_Data1<-function()
{
  photopoint=matrix(c(-86.15,-68.99,
                      -53.40,82.20,
                      10.46,64.43,
                      -14.78,-76.63),4,2,byrow=T);
  
  landspoint=matrix(c(36589.41,25273.32,2195.17,
                      37631.08,31324.51, 728.69,
                      40426.54,30319.81, 757.31,
                      39100.97,24934.98, 2386.50),4,3,byrow=T);
  
  Rmatrix=CoordinateSwitch_CreateRotateMatrix(-0.003926,0.002075,-0.067556,T);
  
  Intrinsic=CoordinateSwitch_CreateIntrinscMatrix(153.24,0,0);
  
  Tvector=matrix(c(39795.08,27476.75,7572.81),3,1);
  
  return (list(Intrinsic,Rmatrix,Tvector,photopoint,landspoint));
  
}

#=============================================================
#函数说明：获取测试数�?1
#该数据来自于官云兰文献《》中数据1
#但提供人工修改旋转角�?
#=============================================================
SpaceResectionTest_Data1_Userattitude<-function(phi,omiga,kappa,israd=T)
{
  
  
  landspoint=matrix(c(36589.41,25273.32,2195.17,
                      37631.08,31324.51, 728.69,
                      40426.54,30319.81, 757.31,
                      39100.97,24934.98, 2386.50),4,3,byrow=T);
  
  Rmatrix=CoordinateSwitch_CreateRotateMatrix(phi,omiga,kappa,israd);
  
  Intrinsic=CoordinateSwitch_CreateIntrinscMatrix(153.24,0,0);
  
  Tvector=matrix(c(39795.08,27476.75,7572.81),3,1);
  
  photopoint=CoordinateSwitch_CollinearityEquation(landspoint,Intrinsic,Rmatrix,Tvector);
  
  return (list(Intrinsic,Rmatrix,Tvector,photopoint,landspoint));
  
}


#=============================================================
#函数说明：获取测试数�?1
#该数据来自于官云兰文献《》中数据2
#
#=============================================================
SpaceResectionTest_Data2<-function()
{
#   photopoint=matrix(c( 22.1893,-34.2927,
#                       -27.4380,-26.9674,
#                       -27.5530, 17.9049,
#                       23.0317,23.5064),4,2,byrow=T);
  
  photopoint=matrix(c( -27.4380,-26.9674,
                       -27.5530, 17.9049,
                       23.0317,23.5064),3,2,byrow=T);
  
  landspoint=matrix(c(41589.0,25273.0,2195.0,
                      37589.0,26273.0, 728.0,
                      38589.0,27273.0, 757.0,
                      40589.0,28273.0,2386.0),4,3,byrow=T);
  
  Rmatrix=CoordinateSwitch_CreateRotateMatrix(0.002778,0.004,0,T);
  
  Intrinsic=CoordinateSwitch_CreateIntrinscMatrix(153.24,0,0);
  
  Tvector=matrix(c(39795,27477,7573),3,1);
  
  return (list(Intrinsic,Rmatrix,Tvector,photopoint,landspoint));
}

#====================================================================
#测试代码1�?
#   实现模拟数据的共线方程地面控制点与像平面坐标系转化测�?
#====================================================================
Test_CollinearityEquation<-function()
{
  #数据1的测�?
  data1=SpaceResectionTest_Data1();
  result=CoordinateSwitch_CollinearityEquation(data1[[5]],data1[[1]],data1[[2]],data1[[3]]);
  PrintWithTitle("Data1_result",result);
  #数据2的测�?
  data2=SpaceResectionTest_Data2();
  result=CoordinateSwitch_CollinearityEquation(data2[[5]],data2[[1]],data2[[2]],data2[[3]]);
  PrintWithTitle("Data2_result",result);
}

#====================================================================
#测试代码2�?
#   实现模拟数据1的摄影中心与地面点距离的计算
#====================================================================
Test_DistanceFromCenter_data1<-function()
{
  #像平面精度为2位时，即默认手工输入数据
  data1=SpaceResectionTest_Data1();
  result=SpaceResection_DistanceChange(data1[[4]],data1[[5]],data1[[1]]);
  PrintWithTitle("Data1_result_handinput",result);
  #像平面精度为4位时，即通过共线方程计算所得数�?
  photopoints=CoordinateSwitch_CollinearityEquation(data1[[5]],data1[[1]],data1[[2]],data1[[3]]);
  result=SpaceResection_DistanceChange(photopoints,data1[[5]],data1[[1]]);
  PrintWithTitle("Data1_result_Collinearityinput",result);
  #利用已知外方位线元素（即摄影中心坐标）计算摄影中心到地面点的理论�?
  result=Cal_distance_from_Point(data1[[3]],data1[[5]]);
  PrintWithTitle("Data1_result_resovle",result);  
}

#====================================================================
#测试代码3�?
#   实现模拟数据2的摄影中心与地面点距离的计算
#====================================================================
Test_DistanceFromCenter_data2<-function()
{
  #像平面精度为2位时，即默认手工输入数据
  data2=SpaceResectionTest_Data2();
  result=SpaceResection_DistanceChange(data2[[4]],data2[[5]],data2[[1]]);
  PrintWithTitle("data2_result_handinput",result);
  #像平面精度为4位时，即通过共线方程计算所得数�?
  photopoints=CoordinateSwitch_CollinearityEquation(data2[[5]],data2[[1]],data2[[2]],data2[[3]]);
  result=SpaceResection_DistanceChange(photopoints,data2[[5]],data2[[1]]);
  PrintWithTitle("data2_result_Collinearityinput",result);
  #利用已知外方位线元素（即摄影中心坐标）计算摄影中心到地面点的理论�?
  result=Cal_distance_from_Point(data2[[3]],data2[[5]]);
  PrintWithTitle("data2_result_resovle",result);    
}

#====================================================================
#测试代码4�?
#   实现模拟数据1的空间后方交会解�?
#====================================================================

Test_Quaternion_RotateMatrix_data1<-function()
{
  #像平面精度为2位时，即默认手工输入数据
  #获得数据
  data1=SpaceResectionTest_Data2();
  #PrintWithTitle("data1",data1);
  #距离计算
  #dis=SpaceResection_DistanceChange(data1[[4]],data1[[5]],data1[[1]]);
  #PrintWithTitle("dis",dis);
  #求解旋转参数
  #result=SpaceResection_Quaternion_RotateMatrix(data1[[4]],data1[[5]],dis$Dis,153.24);
  #PrintWithTitle("data1_Quaternion_RotateMatrix",result);
  
  #像平面精度为4位时，即通过共线方程计算所得数�?
  PrintWithTitle("data1",data1);
  R=CoordinateSwitch_CreateRotateMatrix(0,0,0);
  photopoints=CoordinateSwitch_CollinearityEquation(data1[[5]],data1[[1]],data1[[2]],data1[[3]],filename="ddd");
  dis=SpaceResection_DistanceChange_test(photopoints,data1[[5]],data1[[1]]);
  PrintWithTitle("dis",dis);
  result=SpaceResection_Quaternion_RotateMatrix(photopoints,data1[[5]],dis$Dis,153.24);
  PrintWithTitle("data1_Quaternion_RotateMatrix",result);
}
#====================================================================
#测试代码4�?
#   实现基于模拟数据1的空间后方交会解�?
#   可以实现自定姿态角�?
#====================================================================
Test_Quaternion_RotateMatrix_User<-function(phi,omiga,kappa) 
{
  #获得自定义角度一套数�?
  data1=SpaceResectionTest_Data1_Userattitude(phi,omiga,kappa);
  #距离计算
  dis=SpaceResection_DistanceChange(data1[[4]],data1[[5]],data1[[1]]);
  PrintWithTitle("dis",dis);
  #求解旋转参数
  result=SpaceResection_Quaternion_RotateMatrix(data1[[4]],data1[[5]],dis$Dis,153.24);
  PrintWithTitle("data1_Quaternion_RotateMatrix",result);
  
  PrintWithTitle("refrence",c(phi,omiga,kappa));
}