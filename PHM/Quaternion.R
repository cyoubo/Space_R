#=========================================================
#函数集合说明：
#该R文件，主要用于有关四元数应用的函数计算
#=========================================================


#=========================================================
#=========================================================
#函数名称：Quaternion_Direction_RotateMatrix
#函数说明：用于完成利用空间四元数求解旋转矩阵（直接解）
#参数说明：SrcCoor 目标坐标(列数为3) ResCoor 待转换坐标(列数为3)
#=========================================================
Quaternion_Direction_RotateMatrix<-function(SrcCoor,ResCoor)
{
  ########test##############
  #PrintWithTitle("SrcCoor in Quaternion_Direction_RotateMatrix",SrcCoor);
  #PrintWithTitle("ResCoor in Quaternion_Direction_RotateMatrix",ResCoor)
  #################################
  #1.数据检验
  if(dim(SrcCoor)[2]!=3|dim(ResCoor)[2]!=3|dim(SrcCoor)[1]!=dim(ResCoor)[1])
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
  P=t(P);#转置方便计算内积
  sxx = P[1,]%*%S[,1]; sxy = P[1,]%*%S[,2]; sxz = P[1,]%*%S[,3];
  syy = P[2,]%*%S[,2]; syx = P[2,]%*%S[,1]; syz = P[2,]%*%S[,3]; 
  szz = P[3,]%*%S[,3]; szx = P[3,]%*%S[,1]; szy = P[3,]%*%S[,2];
  N=matrix(c(sxx+syy+szz,syz-szy,szx-sxz, sxy-syx,
             syz-szy,sxx-syy-szz,sxy+syx, szx+sxz,
             szx-sxz,sxy+syx,-sxx+syy-szz,syz+szy,
             sxy-syx,szx+sxz,syz+szy,-sxx-syy+szz
            ),4,4,byrow=T);
  #4.求解最大特征向量
  En=eigen(N);
  V=En$vector[,which.max(En$values)];
  #5.组成矩阵
  R1=c(1-2*(V[3]^2+V[4]^2),2*(V[2]*V[3]-V[1]*V[4]),2*(V[1]*V[3]+V[2]*V[4]));
  R2=c(2*(V[2]*V[3]+V[1]*V[4]),1-2*(V[2]^2+V[4]^2),2*(V[4]*V[3]-V[2]*V[1]));
  R3=c(2*(V[2]*V[4]-V[1]*V[3]),2*(V[4]*V[3]+V[1]*V[2]),1-2*(V[2]^2+V[3]^2));
  R=cbind(R1,R2,R3);
  #6.计算倾角
  phi=atan2(-R[1,3],R[3,3]);
  omiga=asin(-R[2,3]);
  kappa=atan2(R[2,1],R[2,2]);
  return (list(R=R,Angel=c(phi,omiga,kappa)));
}