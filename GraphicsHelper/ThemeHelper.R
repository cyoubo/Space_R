#=============================================================
#�������ָ��theme�Լ���֮��صĲ�������
#=============================================================

#=============================================================
#�������ƣ���ָ����ͼ������ӱ��⣬x���ǩ��y���ǩ
#�����б���
#1.�����ƵĻ�ͼ���
#2.����
#3.x���ǩ
#4.y���ǩ
#����ֵ�����ƺ�Ļ�ͼ���
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
#�������ƣ����ñ�����ʽ
#1.�ߴ�
#2.��ֱƫ��
#3.ˮƽƫ��
#4.��̬��ɫ
#=============================================================
ThemeHelper_TitleTheme<-function(m_size=15,m_vjust=0.5,m_hjust=0.5,m_colour="black")
{
  return(theme(plot.title=element_text(lineheight = 0.9,size=m_size,colour=m_colour,vjust=m_vjust,hjust=m_hjust)))
}
#=============================================================
#�������ƣ�����X���ǩ��ʽ
#1.�ߴ�
#2.��ֱƫ��
#3.ˮƽƫ��
#4.��̬��ɫ
#=============================================================
ThemeHelper_XLabelTheme<-function(m_size=15,m_vjust=0.5,m_hjust=0.5,m_colour="black")
{
  return(theme(axis.title.x=element_text(lineheight = 0.9,size=m_size,colour=m_colour,vjust=m_vjust,hjust=m_hjust)))
}
#=============================================================
#�������ƣ�����y���ǩ��ʽ
#1.�ߴ�
#2.��ֱƫ��
#3.ˮƽƫ��
#4.��̬��ɫ
#=============================================================
ThemeHelper_YLabelTheme<-function(m_size=15,m_vjust=0.5,m_hjust=0.5,m_colour="black")
{
  return(theme(axis.title.y=element_text(lineheight = 0.9,size=m_size,colour=m_colour,vjust=m_vjust,hjust=m_hjust,angle=0)))
}
#=============================================================
#�������ƣ�����x����������ʽ
#1.�ߴ�
#2.��̬��ɫ
#=============================================================
ThemeHelper_XAxisTheme<-function(m_size=10,m_colour="black")
{
  return(theme(axis.text.x=element_text(size=m_size,colour=m_colour,lineheight = 0.9)))
}
#=============================================================
#�������ƣ�����y����������ʽ
#1.�ߴ�
#2.��̬��ɫ
#=============================================================
ThemeHelper_YAxisTheme<-function(m_size=10,m_colour="black")
{
  return(theme(axis.text.y=element_text(size=m_size,colour=m_colour,lineheight = 0.9)))
}
#=============================================================
#�������ƣ�����xy����������ʽ
#1.�ߴ�
#2.��̬��ɫ
#=============================================================
ThemeHelper_XYAxisTheme<-function(m_size=10,m_colour="black")
{
  return(theme(axis.text=element_text(size=m_size,colour=m_colour,lineheight = 0.9)))
}
#=============================================================
#�������ƣ�����xy����������ʽ
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
#�������ƣ�����xy��ı仯��Χ
#=============================================================
ThemeHelper_AddXYAxisContinuousLimit<-function(GraphHandle,xlimt,ylimt)
{
  GraphHandle<-GraphHandle+scale_x_continuous(limits=xlimt)+scale_y_continuous(limits=ylimt);
  return(GraphHandle);
}
#==================================== =========================
#�������ƣ�����x��ı仯��Χ
#=============================================================
ThemeHelper_AddXAxisContinuousLimit<-function(GraphHandle,xlimt)
{
  GraphHandle<-GraphHandle+scale_x_continuous(limits=xlimt);
  return(GraphHandle);
}
#=============================================================
#�������ƣ�����xy��ı仯��Χ
#=============================================================
ThemeHelper_AddYAxisContinuousLimit<-function(GraphHandle,ylimt)
{
  GraphHandle<-GraphHandle+scale_y_continuous(limits=ylimt);
  return(GraphHandle);
}
#=============================================================
#�������ƣ���������ͼ��λ��
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
#�������ƣ����û�ͼ��ͳһĬ����ʽ
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