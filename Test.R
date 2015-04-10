#====================================================================
#测试代码1：
#   实现模拟数据的共线方程地面控制点与像平面坐标系转化测试
#====================================================================
Test2<-function()
{
  #构建旋转矩阵
  R1=CoordinateSwitch_CreateRotateMatrix(0.002778,0,0,T);
  R2=CoordinateSwitch_CreateRotateMatrix(-0.003926,0.002075,-0.067556,T);
  #构建内方位元素矩阵
  Intrinsic=CoordinateSwitch_CreateIntrinscMatrix(153.24,0,0);
  #设置地面点
  landpoint1=matrix(c(40589,26273,2195,
                     38589,26273, 728,
                     38589,28273, 757,
                     40589,28273,2386),4,3,byrow=T);
  landpoint2=matrix(c(36589.41,25273.32,2195.17,
                     37631.08,31324.51, 728.69,
                     40426.54,30319.81, 757.31,
                     39100.97,24934.98,2386.50),4,3,byrow=T);
  #设置中心
  dis1=matrix(c(39795,27477,7573),3,1);
  dis2=matrix(c(39795.08,27476.75,7572.81),3,1);
  #算例
  result1=CoordinateSwitch_CollinearityEquation(landpoint1,Intrinsic,R1,dis1);
  PrintWithTitle("Result",result1);
  result2=CoordinateSwitch_CollinearityEquation(landpoint2,Intrinsic,R2,dis2);
  PrintWithTitle("Result",result2);
}

#====================================================================
#测试代码2：
#   实现模拟数据的屏幕坐标与像平面坐标的相互转化
#====================================================================
Test3<-function()
{
  Points=matrix(c(0,0,
                  1,1,
                  0,1),3,2,byrow=T);
  PrintWithTitle("Points",Points);
  
  reuslt=CoordinateSwitch_Screan_To_PhotoCoordinateSystem(12,10,Points);
  PrintWithTitle("reuslt",reuslt);
  
  result=CoordinateSwitch_Photo_To_ScreanCoordinateSystem(12,10,reuslt);
  PrintWithTitle("result",result);
}

Test4<-function()
{
  landpoint2=matrix(c(36589.41,25273.32,2195.17,
                      37631.08,31324.51, 728.69,
                      40426.54,30319.81, 757.31),3,3,byrow=T);
  R2=CoordinateSwitch_CreateRotateMatrix(-0.003926,0.002075,-0.067556,T);

 
  dis2=matrix(c(39795.08,27476.75,7572.81),3,1);
  Intrinsic=CoordinateSwitch_CreateIntrinscMatrix(153.24,0,0);
  
#   PhotoSpacePoint=matrix(c(779.0569,-1204.0000,-5380.1850,
#                            -1225.011,-1204.000,-6841.623,
#                            -1224.930,796.000,-6812.623,
#                            779.5875,796.0000,-5189.1857
#                            ),4,3,byrow=T);
  
  #7参数变换获得变化后的三维坐标
  PhotoSpacePoint=CoordinateSwitch_Bursa7Parameters(landpoint2,R2,dis2);
  PrintWithTitle("PhotoSpacePoint",t(PhotoSpacePoint));
  #参数反解，检验解算的结果是否与预设值一直
  Result=Quaternion_Direction_RotateMatrix(landpoint2,t(PhotoSpacePoint));
  
  PrintWithTitle("Result",Result);
  PrintWithTitle("R2",R2);
  PrintWithTitle("dev:",t(R2)-Result$R);
}
#====================================================================
#测试代码2：
#   实现共线方程的正算与后方交会求解旋转矩阵
#   R2,landpoint2,dis2,photopoint2均来自《基于单位四元数的空间后方交会解算_官云兰》
#   landpoint2,photopoint2均来自<改进的空间后方交会直接解法_姚吉利>
#====================================================================
Test5<-function()
{
  Intrinsic=CoordinateSwitch_CreateIntrinscMatrix(153.24,0,0);
  
  R2=CoordinateSwitch_CreateRotateMatrix(-0.003926,0.002075,-0.067556,T);
  
  landpoint2=matrix(c(36589.41,25273.32,2195.17,
                      37631.08,31324.51, 728.69,
                      40426.54,30319.81, 757.31),3,3,byrow=T);
  
  landpoint3=matrix(c(36589.41,25273.32,2195.17,
                      37631.08,31324.51, 728.69,
                      39100.97,24934.98, 2386.50),3,3,byrow=T);
  
  dis2=matrix(c(39795.08,27476.75,7572.81),3,1);
  
  photopoint2=matrix(c(-86.15118,-68.98761,
                      -53.40691,82.20985,
                      10.46671,64.42773),3,2,byrow=T);
  photopoint3=matrix(c(-92.7514,-62.9368,
                       -47.6810,85.5143,
                       -21.7751,-76.0586),3,2,byrow=T);
  
  #计算距离改化R2
  temp=SpcaeResection_DistanceChange(photopoint2,landpoint2,Intrinsic);
  PrintWithTitle("R2 dis",temp$Dis);
  #计算距离理论值
  Cal_distance_from_Point=Cal_distance_from_Point(dis2,landpoint2);
  PrintWithTitle("Cal_distance_from_Point",Cal_distance_from_Point);
  #利用四元数完成后方交会
  result2=SpaceResection_Quaternion_SolveCenterCoordinate(photopoint2,landpoint2,temp$Dis,Intrinsic[1,1]);
  PrintWithTitle("Angel 2",result2$Angel);
  
  #计算距离改化R3
  temp=SpcaeResection_DistanceChange(photopoint3,landpoint3,Intrinsic);
  PrintWithTitle("R3 dis",temp$Dis);
  #利用四元数完成后方交会
  result2=SpaceResection_Quaternion_SolveCenterCoordinate(photopoint3,landpoint3,temp$Dis,Intrinsic[1,1]);
  PrintWithTitle("Angel 3",result2$Angel);
}

Test6<-function()
{
  center=c(39795.08,27476.75,7572.81);
  landpoint2=matrix(c(36589.41,25273.32,2195.17,
                      37631.08,31324.51, 728.69,
                      40426.54,30319.81, 757.31),3,3,byrow=T);
  result=Cal_distance_from_Point(center,landpoint2);
  
  PrintWithTitle("result",result);
}
#====================================================================
#测试代码2：
#   实现后方交会中的距离改算
#   R1,landpoint1,dis1,photopoint1
#   R2,landpoint2,dis2,photopoint2均来自《基于单位四元数的空间后方交会解算_官云兰》
#   landpoint2,photopoint2均来自<改进的空间后方交会直接解法_姚吉利>
#====================================================================
Test7<-function()
{
  Intrinsic=CoordinateSwitch_CreateIntrinscMatrix(153.24,0,0);
  
  R1=CoordinateSwitch_CreateRotateMatrix(0.002778,0,0,T);
  R2=CoordinateSwitch_CreateRotateMatrix(-0.003926,0.002075,-0.067556,T);
  R3=CoordinateSwitch_CreateRotateMatrix(0.0029,0.00582,0.011636,T);
  
  landpoint1=matrix(c(40589.0,26273.0,2195.0,
                      38589.0,26273.0, 728.0,
                      38589.0,28273.0, 757.0),3,3,byrow=T);
  
  landpoint2=matrix(c(36589.41,25273.32,2195.17,
                      37631.08,31324.51, 728.69,
                      40426.54,30319.81, 757.31),3,3,byrow=T);
  
  landpoint3=matrix(c(36589.41,25273.32,2195.17,
                      37631.08,31324.51, 728.69,
                      39100.97,24934.98, 2386.50),3,3,byrow=T);
  
  dis1=matrix(c(39795,27477,7573),3,1);
  dis2=matrix(c(39795.08,27476.75,7572.81),3,1);
  
  photopoint1=matrix(c( 22.1893,-34.2927,
                       -27.4380,-26.9674,
                       -27.5530, 17.9049),3,2,byrow=T);
  
  photopoint2=matrix(c(-86.15,-68.99,
                       -53.40,82.21,
                        10.46,64.43),3,2,byrow=T);
  
  photopoint3=matrix(c(-92.7514,-62.9368,
                       -47.6810, 85.5143,
                       -21.7751,-76.0586),3,2,byrow=T);
  
  print("============================================================")
  
  #result=CoordinateSwitch_CollinearityEquation(landpoint2,Intrinsic,R3,dis2);
  #PrintWithTitle("result",result);
  SpcaeResection_DistanceChange=SpcaeResection_DistanceChange(photopoint3,landpoint3,Intrinsic);
  PrintWithTitle("SpcaeResection_DistanceChange",SpcaeResection_DistanceChange$Dis);
  #Cal_distance_from_Point=Cal_distance_from_Point(dis2,landpoint2);
  #PrintWithTitle("Cal_distance_from_Point",Cal_distance_from_Point);
  
}