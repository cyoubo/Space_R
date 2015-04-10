#=============================================================
#函数集合说明：
#该R文件，主要用于空间后方交会直接解的算法编写
#
#=============================================================

#=============================================================
#函数名称：SpcaeResection_DistanceChange
#函数说明：实现空间后方交会直接解的距离改化
#参数说明：PhotoCoor 像平面点,LandCoor 地面点
#          Intrinsc 内方位元素矩阵，EPS 迭代精度
#=============================================================
SpcaeResection_DistanceChange<-function(PhotoCoor,LandCoor,Intrinsic,EPS=0.1)
{
  fx=Intrinsic[1,1];fy=Intrinsic[2,2];cx=Intrinsic[1,3];cy=Intrinsic[2,3];
  A=rep(1,3);B=rep(1,3);
  for(i in c(1:3))
  {
    tanA=(PhotoCoor[i,1]-cx)/fx;
    tanB=(PhotoCoor[i,2]-cy)/sqrt(fy^2+(PhotoCoor[i,1]-cx)^2);
    A[i]=atan(tanA);
    B[i]=atan(tanB);
  }
  cosPHI12=sin(B[1])*sin(B[2])+cos(B[1])*cos(B[2])*cos(A[2]-A[1]);
  cosPHI23=sin(B[2])*sin(B[3])+cos(B[2])*cos(B[3])*cos(A[3]-A[2]);
  cosPHI31=sin(B[1])*sin(B[3])+cos(B[1])*cos(B[3])*cos(A[1]-A[3]);
  
  S12=sqrt((LandCoor[1,1]-LandCoor[2,1])^2+(LandCoor[1,2]-LandCoor[2,2])^2+(LandCoor[1,3]-LandCoor[2,3])^2);
  S23=sqrt((LandCoor[2,1]-LandCoor[3,1])^2+(LandCoor[2,2]-LandCoor[3,2])^2+(LandCoor[2,3]-LandCoor[3,3])^2);
  S31=sqrt((LandCoor[1,1]-LandCoor[3,1])^2+(LandCoor[1,2]-LandCoor[3,2])^2+(LandCoor[1,3]-LandCoor[3,3])^2);

  #求解方程并返回S1,s2,s3
  F12=0.5/(1-cosPHI12);F23=0.5/(1-cosPHI23);F31=0.5/(1-cosPHI31);
 # F12=0.5/(2*(sin(acos(cosPHI12)/2))^2);F23=0.5/(2*(sin(acos(cosPHI23)/2))^2);F31=0.5/(2*(sin(acos(cosPHI31)/2))^2);
  G12=cosPHI12*F12;    G23=cosPHI23*F23;    G31=cosPHI31*F31;
  A1=matrix(c(1,-1,1,1,1,-1,-1,1,1),3,3,byrow=3);
  a=matrix(c(F12*S12^2,F23*S23^2,F31*S31^2),3,1);
  #计算初值
  init=x=A1%*%a;#x为3*1矩阵，分别表示[S1^2,S2^2,S3^2];
  #设置限差与终止条件
  tolerance=10000;count=0;
  #开始迭代
  while(count<100&&tolerance>EPS)
  {
    #PrintWithTitle("cout",count);
    #PrintWithTitle("x",x);
    #当距离为负数时则跳出循环
    if(x[1,1]<0||x[2,1]<0||x[3,1]<0) 
       break;
    #计算新的距离值
    s1=sqrt(x[1,1]);s2=sqrt(x[2,1]);s3=sqrt(x[3,1]);
    #PrintWithTitle("s",c(s1,s2,s3));
    #计算迭代方程更新量
    b =matrix(c(-G12*(s1-s2)^2,-G23*(s2-s3)^2,-G31*(s3-s1)^2),3,1);
    #计算新方程数
    temp=init+A1%*%b;
    #计算差异
    des=abs(x-temp);
    #当限差小于指定值时，跳出循环
    if(des[which.max(des)]<EPS) break;
    #计数器加1
    count=count+1; 
    #更新距离值
    x=temp;
  }
  
  return(list(A=A,B=B,Dis=as.vector(sqrt(x))));
}

SpaceResection_SolveCenterCoordinate<-function(Dis,LandCoor,A,B)
{
  CosA=rep(NA,3);CosB=rep(NA,3);SinA=rep(NA,3);SinB=rep(NA,3)
  for(i in c(1:3))
  {
    CosA[i]=cos(A[i]);SinA[i]=sin(A[i]);
    CosB[i]=cos(B[i]);SinB[i]=sin(B[i]);
  }
  convalue=matrix(c(Dis[1]*CosA[1]*CosB[1]-LandCoor[1,1],Dis[1]*CosA[1]*CosB[1]-LandCoor[1,2],
                    Dis[1]*SinB[1]-LandCoor[1,3],
                    Dis[2]*CosA[2]*CosB[2]-LandCoor[2,1],Dis[2]*CosA[2]*CosB[2]-LandCoor[2,2],
                    Dis[3]*SinB[3]-LandCoor[3,3]),6,1);
 
  temp1=c(0,Dis[1]*SinB[1]+LandCoor[1,3],-1*Dis[1]*SinA[1]*CosB[1]-LandCoor[1,2],1,0,0);
  print(temp1);
  temp2=c(-1*Dis[1]*SinB[1]-LandCoor[1,3],0,Dis[1]*CosA[1]*CosB[1]+LandCoor[1,1],0,1,0);
  print(temp2);
  temp3=c(Dis[1]*SinA[1]*CosB[1]+LandCoor[1,2],-1*Dis[1]*CosA[1]*CosB[1]-LandCoor[1,1],0,0,0,1);
  print(temp3);
  temp4=c(0,Dis[2]*SinB[2]+LandCoor[2,3],Dis[2]*-SinA[2]*CosB[2]-LandCoor[2,2],1,0,0);
  print(temp4);
  temp5=c(Dis[2]*-SinB[2]-LandCoor[2,3],0,Dis[2]*CosA[2]*CosB[2]+LandCoor[2,1],0,1,0);
  print(temp5);
  temp6=c(Dis[3]*SinA[3]*CosB[3]+LandCoor[3,2],Dis[3]*-CosA[3]*CosB[3]-LandCoor[3,1],0,0,0,1);
  print(temp6);
  cof  =rbind(temp1,temp2,temp3,temp4,temp5,temp6);
  
  resolution=solve(cof,convalue);
  
  
  a=resolution[1];b=resolution[2];c=resolution[3];
  convalue2=resolution[4:7];
  
  temp=1.0/(a^2+b^2+c^2+1);
  
  cof2=matrix(c(1+a^2,a*b+c,a*c-b,a*b-c,1+b^2,b*c+a,a*c+b,b*c-a,1+c^2),3,3,byrow=T);
  
  result=c(temp*cof2,convalue2);
  
  return (result);
}

SpaceResection_Quaternion_SolveCenterCoordinate<-function(ImagePoint,LandPoint,Dis,f)
{
  Lumbda=rep(NA,3);
  ImageSpcaePoints=matrix(c(rep(NA,9)),3,3);
  for(i in c(1:3))
  {
    Lumbda[i]=Dis[i]/sqrt(ImagePoint[i,1]^2+ImagePoint[i,2]^2+f^2)
    ImageSpcaePoints[i,1]=Lumbda[i]*ImagePoint[i,1];
    ImageSpcaePoints[i,2]=Lumbda[i]*ImagePoint[i,2];
    ImageSpcaePoints[i,3]=-1*Lumbda[i]*f;
  }
  return (Quaternion_Direction_RotateMatrix(LandPoint,ImageSpcaePoints));
  
}