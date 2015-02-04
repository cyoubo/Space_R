#=========================================================
#函数集合说明：
#该R文件，主要用于有关四元数应用的函数计算
#=========================================================


#=========================================================
#=========================================================
#函数名称：Quaternion_Direction_RotateMatrix
#函数说明：用于完成利用空间四元数求解旋转矩阵
#参数说明：SrcCoor 目标坐标 ResCoor 待转换坐标
#=========================================================
Quaternion_Direction_RotateMatrix<-function(SrcCoor,ResCoor)
{
  #1.数据检验
  if(dim(SrcCoor)[1]!=3|dim(ResCoor)[1]!=3|dim(SrcCoor)[2]!=dim(ResCoor)[2])
  {
    cat("the dim of SrcCoor or ResCoor is invaild");
    return (0);
  }
  #2.重心化坐标
  SrcMean=apply(SrcCoor,2,mean);
  P=t(apply(SrcCoor,1,function(x){x-SrcMean}));#重心化后的坐标
  ResMean=apply(ResCoor,2,mean);
  S=t(apply(ResCoor,1,function(x){x-ResMean}));
  #3.构建N矩阵
  N1=c( S[,1]*P[,1]+S[,2]*P[,2]+S[,3]*P[,3],S[,2]*P[,3]-S[,3]*P[,2],S[,3]*P[,1]-S[,1]*P[,3],S[,1]*P[,2]-S[,2]*P[,1]);
  N2=c(S[,2]*P[,3]-S[,3]*P[,2], S[,1]*P[,1]-S[,2]*P[,2]-S[,3]*P[,3],S[,1]*P[,2]+S[,1]*P[,2],S[,1]*P[,3]+S[,3]*P[,1]);
  N3=c(S[,3]*P[,1]-S[,1]*P[,3],S[,2]*P[,1]+S[,1]*P[,2],-S[,1]*P[,1]+S[,2]*P[,2]-S[,3]*P[,3],S[,3]*P[,2]+S[,2]*P[,3]);
  N4=c(S[,1]*P[,2]-S[,2]*P[,1],S[,1]*P[,3]+S[,3]*P[,1],S[,3]*P[,2]+S[,2]*P[,3],-S[,1]*P[,1]-S[,2]*P[,2]+S[,3]*P[,3]);
  N1=apply(N1,2,sum);N2=apply(N2,2,sum);N3=apply(N3,2,sum);N4=apply(N4,2,sum);
  N=rbind(N1,N2,N3,N4);
  #4.求解最大特征向量
  En=eigen(N);
  V=En$vector[,which.max(En$values)];
  #5.组成矩阵
  R1=c(1-2*(V[3]^2+V[4]^2),2*(V[2]*V[3]-V[1]*V[4]),2*(V[1]*V[3]+V[2]*V[4]));
  R2=c(2*(V[2]*V[3]+V[1]*V[4]),1-2*(V[2]^2+V[4]^2),2*(V[4]*V[3]-V[2]*V[1]));
  R3=c(2*(V[2]*V[4]-V[1]*V[3]),2*(V[4]*V[3]+V[1]*V[2]),1-2*(V[2]^2+V[3]^2));
  R=rbind(R1,R2,R3);
  #6.计算倾角
  Angel=c(atan2(R[1,3]*-1,R[3,3]),asin(-R[2,3]),atan2(R[2,1],R[2,2]));
  
  return (list(R,Angel));
  
}