#=====================================================
#Ԥ����ɫ��ָ�������
#=====================================================

#====================================================
#�������ƣ�������ɢ����ɫ����
#����������ָ��������ɢ��ɫ�����ĸ���
#ǰ��������ֻ�����ɡ�2-8������ɫ����
#====================================================
ColorHelper_DefaultDisCreateColor<-function(count)
{
  if(count>9|count<1)
  {
    cat("the length of colour vector is invail");
    return (0);
  }
  else if (count==2)
  {
    return (ColorHelper_TwoColour());
  }
  else if (count==3)
  {
    return (ColorHelper_ThreeColour());
  }
  else if (count==4)
  {
    return (ColorHelper_FourColour());
  }
  else if (count==5)
  {
    return (ColorHelper_FiveColour());
  }
  else if (count==6)
  {
    return (ColorHelper_SixColour());
  }
  else if (count==7)
  {
    return (ColorHelper_SevenColour());
  }
  else 
  {
    return (ColorHelper_EightColour());
  }
}  
#------------DefaultDisCreateColor���ڲ�����---�ɵ���ʹ��---------
ColorHelper_TwoColour<-function(type=1)
{
  if(type==1)
  {
    return(c("#0072B2","#D55E00"));
  }
  else if(type==2)
  {
    return(c("#FF7F00","#1874CD"));
  }
  else
  {
    return(c("#575757","#C1CDCD"));  
  }
}
ColorHelper_ThreeColour<-function()
{
  return(c("#0072B2","#D55E00","#009E73"))
}
ColorHelper_FourColour<-function()
{
  return(c("#0072B2","#D55E00","#009E73","#E69F00"))
}
ColorHelper_FiveColour<-function()
{
  return(c("#0072B2","#D55E00","#009E73","#E69F00","#F0E442"))
}
ColorHelper_ColorHelper_SixColour<-function()
{
  return(c("#0072B2","#D55E00","#009E73","#E69F00","#F0E442","#CC79A7"))
}
ColorHelper_SevenColour<-function()
{
  return(c("#0072B2","#D55E00","#009E73","#E69F00","#F0E442","#CC79A7","#56B4E9"))
}
ColorHelper_EightColour<-function()
{
  return(c("#0072B2","#D55E00","#009E73","#E69F00","#F0E442","#CC79A7","#56B4E9","#999999"))
}
#------------DefaultDisCreateColor���ڲ�����------------

#====================================================
#�������ƣ�����ggplot��������ɫ�ľ��
#����������ָ������������ɫ�Ľ�����
#1 ��ɫ 2 ��ɫ 3 ��ɫ 4 ��ɫ 5 ��ɫ 6 �ڰ�
#ǰ����������Ҫ��ggplot�װ���֧��
#====================================================
ColorHelper_DefaultContinuousColor<-function(colourtype=1)
{
  
  library(ggplot2);
  
  if(colourtype==1)#��ɫϵ
  {
     return (scale_color_gradient(low="#191970",high="#00EEEE"));
  }
  else if(colourtype==2)#��ɫϵ
  {
    return (scale_color_gradient(low="#8B1A1A",high="#EE2C2C"));
  }
  else if(colourtype==3)#��ɫϵ
  {
    return (scale_color_gradient(low="#458B00",high="#7FFF00"));
  }
  else if(colourtype==4)#��ɫϵ
  {
    return (scale_color_gradient(low="#EEC900",high="#FFF68F"));
  }
  else if(colourtype==5)#��ɫϵ
  {
    return (scale_color_gradient(low="#D2691E",high="#FFC125"));
  }
  else if(colourtype==6)#��ɫϵ
  {
    return (scale_color_gradient(low="#0A0A0A",high="#FFF0F5"));
  }
  else
  {
    cat("the colourtype is unvail");
    return (scale_color_gradient(low="#191970",high="#00EEEE"));
  }
}

#====================================================
#�������ƣ�����ggplot������ײɫ��ɫ�ľ��
#����������
#1.ָ������������ɫ�Ľ�������  1 ���� 2 ���� 3 �̻� 4 ���� 5 ���� 6 �ڰ�
#2.��ֵ��ֵ
#3.��ֵ��ɫ��Ĭ��Ϊ��ɫ
#ǰ����������Ҫ��ggplot�װ���֧��
#====================================================
ColorHelper_DefaultContinuousMiddleColor<-function(colourtype=1,midvalue,midcolour="white")
{
  library(ggplot2);
  
  if(colourtype==1)#����
  {
    return (scale_color_gradient2(low="#FF0000",high="#1C86EE",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==2)#����
  {
    return (scale_color_gradient2(low="#FF7F00",high="#1C86EE",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==3)#�̻�
  {
    return (scale_color_gradient2(low="#66CD00",high="#FFD700",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==4)#����
  {
    return (scale_color_gradient2(low="#8A2BE2",high="#7CFC00",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==5)#����
  {
    return (scale_color_gradient2(low="#00B2EE",high="#FFFF00",mid=midcolour,midpoint=midvalue));
  }
  else
  {
    cat("the colourtype is unvail");
    return (scale_color_gradient(low="#191970",high="#00EEEE"));
  }
}

#====================================================
#�������ƣ�����ggplot���������ľ��
#����������ָ�������������Ľ�����
#1 ��ɫ 2 ��ɫ 3 ��ɫ 4 ��ɫ 5 ��ɫ 6 �ڰ�
#ǰ����������Ҫ��ggplot�װ���֧��
#====================================================
ColorHelper_DefaultContinuousfill<-function(colourtype=1)
{
  
  library(ggplot2);
  
  if(colourtype==1)#��ɫϵ
  {
    return (scale_fill_gradient(low="#191970",high="#00EEEE"));
  }
  else if(colourtype==2)#��ɫϵ
  {
    return (scale_fill_gradient(low="#8B1A1A",high="#EE2C2C"));
  }
  else if(colourtype==3)#��ɫϵ
  {
    return (scale_fill_gradient(low="#458B00",high="#7FFF00"));
  }
  else if(colourtype==4)#��ɫϵ
  {
    return (scale_fill_gradient(low="#EEC900",high="#FFF68F"));
  }
  else if(colourtype==5)#��ɫϵ
  {
    return (scale_fill_gradient(low="#D2691E",high="#FFC125"));
  }
  else if(colourtype==6)#��ɫϵ
  {
    return (scale_fill_gradient(low="#0A0A0A",high="#FFF0F5"));
  }
  else
  {
    cat("the colourtype is unvail");
    return (scale_fill_gradient(low="#191970",high="#00EEEE"));
  }
}

#====================================================
#�������ƣ�����ggplot������ײɫ���ľ��
#����������
#1.ָ�������������Ľ�������  1 ���� 2 ���� 3 �̻� 4 ���� 5 ���� 6 �ڰ�
#2.��ֵ��ֵ
#3.��ֵ��ɫ��Ĭ��Ϊ��ɫ
#ǰ����������Ҫ��ggplot�װ���֧��
#====================================================
ColorHelper_DefaultContinuousMiddlefill<-function(colourtype=1,midvalue,midcolour="white")
{
  library(ggplot2);
  
  if(colourtype==1)#����
  {
    return (scale_fill_gradient2(low="#FF0000",high="#1C86EE",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==2)#����
  {
    return (scale_fill_gradient2(low="#FF7F00",high="#1C86EE",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==3)#�̻�
  {
    return (scale_fill_gradient2(low="#66CD00",high="#FFD700",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==4)#����
  {
    return (scale_fill_gradient2(low="#8A2BE2",high="#7CFC00",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==5)#����
  {
    return (scale_fill_gradient2(low="#00B2EE",high="#FFFF00",mid=midcolour,midpoint=midvalue));
  }
  else
  {
    cat("the colourtype is unvail");
    return (scale_fill_gradient(low="#191970",high="#00EEEE"));
  }
}

#=====================================================
#Ԥ�����ʽ���߶���ʽ��ָ�������
#=====================================================

#==========================================================
#�������ƣ����س��õĵ�������������
#����������ָ����Ҫ���صĵ���ʽ��
#ǰ��������ֻ�ṩ����10�ֵ���ʽ
#==========================================================
ColorHelper_Defaultshape<-function(count)
{
  shapecollection=c(15,18,16,17,13,12,10,9,4,2);
  
  if(count>length(shapecollection))
    return(shapecollection[1:length(shapecollection)])
  else
    return(shapecollection[1:count])
}

#==========================================================
#�������ƣ����س����߶���������
#����������
#1.��Ҫ�����������������Ĭ��Ϊֱ��
#2.��Ҫ���Ƴ����ߵ������������
#3.��Ż���Ϊ�ϵ�������������
#==========================================================
ColorHelper_DefalutLineType<-function(length,dashedindex=NULL,doshindex=NULL)
{
  result=rep(1,length);
  
  if(length(dashedindex)!=0)
  {
    for(i in dashedindex)
      result[i]=2;
  }  
  if(length(doshindex)!=0)    
  {
    for(i in doshindex)
      result[i]=3;
  }
  print(result);
  return (result);
}