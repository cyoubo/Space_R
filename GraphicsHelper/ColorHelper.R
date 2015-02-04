#=====================================================
#预设颜色的指令函数集合
#=====================================================

#====================================================
#函数名称：生成离散的颜色向量
#函数参数：指定生成离散样色向量的个数
#前置条件：只能生成【2-8】种颜色向量
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
#------------DefaultDisCreateColor的内部函数---可单独使用---------
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
#------------DefaultDisCreateColor的内部函数------------

#====================================================
#函数名称：生成ggplot中连续颜色的句柄
#函数参数：指定生成连续颜色的渐变句柄
#1 蓝色 2 红色 3 绿色 4 黄色 5 橙色 6 黑白
#前置条件：需要有ggplot底包做支持
#====================================================
ColorHelper_DefaultContinuousColor<-function(colourtype=1)
{
  
  library(ggplot2);
  
  if(colourtype==1)#蓝色系
  {
     return (scale_color_gradient(low="#191970",high="#00EEEE"));
  }
  else if(colourtype==2)#红色系
  {
    return (scale_color_gradient(low="#8B1A1A",high="#EE2C2C"));
  }
  else if(colourtype==3)#绿色系
  {
    return (scale_color_gradient(low="#458B00",high="#7FFF00"));
  }
  else if(colourtype==4)#黄色系
  {
    return (scale_color_gradient(low="#EEC900",high="#FFF68F"));
  }
  else if(colourtype==5)#橙色系
  {
    return (scale_color_gradient(low="#D2691E",high="#FFC125"));
  }
  else if(colourtype==6)#黑色系
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
#函数名称：生成ggplot中连续撞色颜色的句柄
#函数参数：
#1.指定生成连续颜色的渐变类型  1 红蓝 2 橙蓝 3 绿黄 4 紫绿 5 黄蓝 6 黑白
#2.中值数值
#3.中值颜色，默认为白色
#前置条件：需要有ggplot底包做支持
#====================================================
ColorHelper_DefaultContinuousMiddleColor<-function(colourtype=1,midvalue,midcolour="white")
{
  library(ggplot2);
  
  if(colourtype==1)#红蓝
  {
    return (scale_color_gradient2(low="#FF0000",high="#1C86EE",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==2)#橙蓝
  {
    return (scale_color_gradient2(low="#FF7F00",high="#1C86EE",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==3)#绿黄
  {
    return (scale_color_gradient2(low="#66CD00",high="#FFD700",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==4)#紫绿
  {
    return (scale_color_gradient2(low="#8A2BE2",high="#7CFC00",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==5)#黄蓝
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
#函数名称：生成ggplot中连续填充的句柄
#函数参数：指定生成连续填充的渐变句柄
#1 蓝色 2 红色 3 绿色 4 黄色 5 橙色 6 黑白
#前置条件：需要有ggplot底包做支持
#====================================================
ColorHelper_DefaultContinuousfill<-function(colourtype=1)
{
  
  library(ggplot2);
  
  if(colourtype==1)#蓝色系
  {
    return (scale_fill_gradient(low="#191970",high="#00EEEE"));
  }
  else if(colourtype==2)#红色系
  {
    return (scale_fill_gradient(low="#8B1A1A",high="#EE2C2C"));
  }
  else if(colourtype==3)#绿色系
  {
    return (scale_fill_gradient(low="#458B00",high="#7FFF00"));
  }
  else if(colourtype==4)#黄色系
  {
    return (scale_fill_gradient(low="#EEC900",high="#FFF68F"));
  }
  else if(colourtype==5)#橙色系
  {
    return (scale_fill_gradient(low="#D2691E",high="#FFC125"));
  }
  else if(colourtype==6)#黑色系
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
#函数名称：生成ggplot中连续撞色填充的句柄
#函数参数：
#1.指定生成连续填充的渐变类型  1 红蓝 2 橙蓝 3 绿黄 4 紫绿 5 黄蓝 6 黑白
#2.中值数值
#3.中值颜色，默认为白色
#前置条件：需要有ggplot底包做支持
#====================================================
ColorHelper_DefaultContinuousMiddlefill<-function(colourtype=1,midvalue,midcolour="white")
{
  library(ggplot2);
  
  if(colourtype==1)#红蓝
  {
    return (scale_fill_gradient2(low="#FF0000",high="#1C86EE",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==2)#橙蓝
  {
    return (scale_fill_gradient2(low="#FF7F00",high="#1C86EE",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==3)#绿黄
  {
    return (scale_fill_gradient2(low="#66CD00",high="#FFD700",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==4)#紫绿
  {
    return (scale_fill_gradient2(low="#8A2BE2",high="#7CFC00",mid=midcolour,midpoint=midvalue));
  }
  else if(colourtype==5)#黄蓝
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
#预设点样式，线段样式的指令函数集合
#=====================================================

#==========================================================
#函数名称：返回常用的点类型索引数组
#函数参数：指定需要返回的点样式数
#前置条件：只提供常用10种点样式
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
#函数名称：返回常用线段类型数组
#函数参数：
#1.需要的总体的线条数，并默认为直线
#2.需要绘制成虚线的线条编号向量
#3.序号绘制为断点的线条编号向量
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
