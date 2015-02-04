#=============================================================
#用于完成指定theme以及与之相关的操作函数
#=============================================================

#=============================================================
#函数名称：对指定绘图句柄添加标题，x轴标签，y轴标签
#参数列表：
#1.待绘制的绘图句柄
#2.标题
#3.x轴标签
#4.y轴标签
#返回值：绘制后的绘图句柄
#=============================================================
ThemeHelper_AddTitle_xyLab_Legend<-function(GraphHandle,title,xLabel,yLabel,legendcolour="",legendfill="",lengedlinetype="",lengedshape="")
{
  GraphHandle<-GraphHandle+xlab(xLabel)+ylab(yLabel);
  if(title!="")
    GraphHandle<-GraphHandle+ggtitle(title);
  if(legendcolour!="")
    GraphHandle<-GraphHandle+labs(colour=legendcolour);
  if(legendfill!="")
    GraphHandle<-GraphHandle+labs(fill=legendfill);
  if(lengedlinetype!="")
    GraphHandle<-GraphHandle+labs(linetype=lengedlinetype);
  if(lengedshape!="")
    GraphHandle<-GraphHandle+labs(shape=lengedshape);
  return (GraphHandle);
}
#=============================================================
#函数名称：设置标题样式
#1.尺寸
#2.竖直偏移
#3.水平偏移
#4.姿态颜色
#=============================================================
ThemeHelper_TitleTheme<-function(m_size=15,m_vjust=0.5,m_hjust=0.5,m_colour="black")
{
  return(theme(plot.title=element_text(lineheight = 0.9,size=m_size,colour=m_colour,vjust=m_vjust,hjust=m_hjust)))
}
#=============================================================
#函数名称：设置X轴标签样式
#1.尺寸
#2.竖直偏移
#3.水平偏移
#4.姿态颜色
#=============================================================
ThemeHelper_XLabelTheme<-function(m_size=15,m_vjust=0.5,m_hjust=0.5,m_colour="black")
{
  return(theme(axis.title.x=element_text(lineheight = 0.9,size=m_size,colour=m_colour,vjust=m_vjust,hjust=m_hjust)))
}
#=============================================================
#函数名称：设置y轴标签样式
#1.尺寸
#2.竖直偏移
#3.水平偏移
#4.姿态颜色
#=============================================================
ThemeHelper_YLabelTheme<-function(m_size=15,m_vjust=0.5,m_hjust=0.5,m_colour="black")
{
  return(theme(axis.title.y=element_text(lineheight = 0.9,size=m_size,colour=m_colour,vjust=m_vjust,hjust=m_hjust,angle=0)))
}
#=============================================================
#函数名称：设置x轴坐标轴样式
#1.尺寸
#2.姿态颜色
#=============================================================
ThemeHelper_XAxisTheme<-function(m_size=10,m_colour="black")
{
  return(theme(axis.text.x=element_text(size=m_size,colour=m_colour,lineheight = 0.9)))
}
#=============================================================
#函数名称：设置y轴坐标轴样式
#1.尺寸
#2.姿态颜色
#=============================================================
ThemeHelper_YAxisTheme<-function(m_size=10,m_colour="black")
{
  return(theme(axis.text.y=element_text(size=m_size,colour=m_colour,lineheight = 0.9)))
}
#=============================================================
#函数名称：设置xy轴坐标轴样式
#1.尺寸
#2.姿态颜色
#=============================================================
ThemeHelper_XYAxisTheme<-function(m_size=10,m_colour="black")
{
  return(theme(axis.text=element_text(size=m_size,colour=m_colour,lineheight = 0.9)))
}
#=============================================================
#函数名称：设置xy轴坐标轴样式
#=============================================================
ThemeHelper_PanelTheme<-function()
{
  return(theme(panel.background=element_blank(),
               panel.border=element_rect(fill=rgb(0,0,0,0),colour="black",size=0.5),
               panel.grid.major.y=element_line(colour="black",linetype=3),
               panel.grid.major.x=element_blank(),
               panel.grid.minor=element_blank()
               ));
}
#=============================================================
#函数名称：设置xy轴的变化范围
#=============================================================
ThemeHelper_AddXYAxisContinuousLimit<-function(GraphHandle,xlimt,ylimt)
{
  GraphHandle<-GraphHandle+scale_x_continuous(limits=xlimt)+scale_y_continuous(limits=ylimt);
  return(GraphHandle);
}
#==================================== =========================
#函数名称：设置x轴的变化范围
#=============================================================
ThemeHelper_AddXAxisContinuousLimit<-function(GraphHandle,xlimt)
{
  GraphHandle<-GraphHandle+scale_x_continuous(limits=xlimt);
  return(GraphHandle);
}
#=============================================================
#函数名称：设置xy轴的变化范围
#=============================================================
ThemeHelper_AddYAxisContinuousLimit<-function(GraphHandle,ylimt)
{
  GraphHandle<-GraphHandle+scale_y_continuous(limits=ylimt);
  return(GraphHandle);
}
#=============================================================
#函数名称：设置设置图例位置
#=============================================================
ThemeHelper_LegendTheme<-function(titlesize=15,textsize=12,m_postion=c())
{
  return(theme(legend.background=element_rect(fill="white",colour="black"),
               legend.title=element_text(size=titlesize,colour="black",lineheight = 1.1,face="bold"),
               legend.text=element_text(size=textsize,colour="black",lineheight = 1.1,face="bold"),
               legend.key=element_rect(size=5,colour=rgb(0,0,0,0),fill=rgb(0,0,0,0))
               ))
}
#=============================================================
#函数名称：设置绘图的统一默认样式
#=============================================================
ThemeHelper_DefalutTheme<-function()
{
  return(theme(panel.background=element_blank(),
               panel.border=element_blank(),
               panel.grid.major.y=element_line(colour="black",linetype=3),
               panel.grid.major.x=element_blank(),
               panel.grid.minor=element_blank(),
               axis.line=element_line(colour="black",size=1),
               axis.text=element_text(lineheight = 1.2,size=12,colour="black",face="italic"),
               axis.title.x=element_text(lineheight = 1.1,size=15,colour="black",vjust=0.5,hjust=0.5,face="plain"),
               axis.title.y=element_text(lineheight = 1.1,size=15,colour="black",vjust=0.5,hjust=0.5,angle=0,face="plain"),
               plot.title=element_text(lineheight = 3,size=20,colour="black",vjust=0.5,hjust=0.5,face="plain"),
               legend.background=element_rect(fill="white",colour="black"),
               legend.title=element_text(size=15,colour="black",lineheight = 1.2,face="plain"),
               legend.text=element_text(size=12,colour="black",lineheight = 1.2,face="plain"),
               legend.key=element_rect(size=5,colour=rgb(0,0,0,0),fill=rgb(0,0,0,0))
  ));
}