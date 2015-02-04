#=============================================================
#函数集合说明：
#该R文件，主要用于空间后方交会直接解的算法编写
#
#=============================================================

#=============================================================
#
#
#
#=============================================================
SpcaeResection_DistanceChange(PhotoCoor,LandCoor,f,cx,cy)
{
  for(i in c(1:3))
  {
    tanA=(PhotoCoor[i][1]-cx)/f;
    tanB=(PhotoCoor[i][2]-cy)/sqrt(f^2+((PhotoCoor[i][1]-cx)^2));
    A[i]=atan(tanA);
    B[i]=atan(tanB);
  }
  cosPHI12=sin(B[1])*sin(B[2])+cos(B[1])*cos(B[2])*cos(A[2]-A[1]);
  cosPHI23=sin(B[2])*sin(B[3])+cos(B[2])*cos(B[3])*cos(A[3]-A[2]);
  cosPHI13=sin(B[1])*sin(B[3])+cos(B[1])*cos(B[3])*cos(A[3]-A[1]);
  dis12=sqrt((LandCoor[1][1]-LandCoor[2][1])^2+(LandCoor[1][2]-LandCoor[2][2])^2+(LandCoor[1][3]-LandCoor[2][3])^2);
  dis23=sqrt((LandCoor[2][1]-LandCoor[3][1])^2+(LandCoor[2][2]-LandCoor[3][2])^2+(LandCoor[2][3]-LandCoor[3][3])^2);
  dis13=sqrt((LandCoor[1][1]-LandCoor[3][1])^2+(LandCoor[1][2]-LandCoor[3][2])^2+(LandCoor[1][3]-LandCoor[3][3])^2);

  #求解方程并返回S1,s2,s3

}

SpaceResection_SolveCenterCoor(Dis,LandCoor,A,B)
{
  for(i in c(1:3))
  {
    CosA[i]=cos(A[i]);SinA[i]=sin(A[i]);
    CosB[i]=cos(B[i]);SinB[i]=sin(B[i]);
  }
  convalue=matrix(c(Dis[1]*CosA[1]*CosB[1]-LandCoor[1][1],Dis[1]*CosA[1]*CosB[1]-LandCoor[1][2],
                    Dis[1]*SinB[1]-LandCoor[1][3],
                    Dis[2]*CosA[2]*CosB[2]-LandCoor[2][1],Dis[2]*CosA[2]*CosB[2]-LandCoor[2][2],
                    Dis[3]*SinB[3]-LandCoor[3][3],),6,1);
 
  temp1=c(0,Dis[1]*SinB[1]+LandCoor[1][3],-Dis[1]*SinA[1]CosB[1]-LandCoor[1][2],1,0,0);
  tmep2=c(-Dis[1]*SinB[1]-LandCoor[1][3],0,Dis[1]*CosA[1]CosB[1]+LandCoor[1][1],0,1,0);
  tmep3=c(Dis[1]*SinA[1]CosB[1]+LandCoor[1][2],-Dis[1]*CosA[1]CosB[1]-LandCoor[1][1],0,0,1);
  temp4=c(0,Dis[2]*SinB[2]+LandCoor[2][3],-Dis[2]*SinA[2]CosB[2]-LandCoor[2][2],1,0,0);
  tmep5=c(-Dis[2]*SinB[2]-LandCoor[2][3],0,Dis[2]*CosA[2]CosB[2]+LandCoor[2][1],0,1,0);
  tmep6=c(Dis[3]*SinA[3]CosB[3]+LandCoor[3][2],-Dis[3]*CosA[3]CosB[3]-LandCoor[3][1],0,0,1);
  cof=rbind(temp1,temp2,temp3,temp4,temp5,temp6);
  
  resolution=slove(cof,convalue);
  
  a=resolution[1];b=resolution[2];c=resolution[3];
  convalue2=resolution[4:7];
  
  temp=1/(a^2+b^2+c^2+1);
  
  cof2=matrix(c(1+a^2,a*b+c,a*c-b,a*b-c,1+b^2,b*c+a,a*c+b,b*c-a,1+c^2),3,3,byrow=T);
  
  result=(temp*cof2,convalue2);
  
  return result;
}