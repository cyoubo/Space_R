#===================================================
#用于完成文件、文件夹操作的工具类
#===================================================

#---------------------------------------------------
#===================================================
#函数名称：完成文件路径的路径、文件名以及后缀的分割
#函数参数：filepath 待分割的文件路径
#函数返回值：包括[Path,Filename,Ext]三个属性的列表
#===================================================
FileHelper_SplitPath<-function(filepath)
{
  temp=strsplit(filepath,"/",fixed=T);
  count=length(temp[[1]]);
  
  temp2=strsplit(temp[[1]][count],".",fixed=T);
  filename=temp2[[1]][1];
  ext=temp2[[1]][2];
  
  path=temp[[1]][1];
  for(str in temp[[1]][2:(count-1)])
  {
    path=paste(path,str,sep='/')
  }
  
  return(list(Path=path,Filename=filename,Ext=ext));
}

#---------------------------------------------------
#===================================================
#函数名称：完成文件路径的路径、文件名（包含后缀）分割
#函数参数：filepath 待分割的文件路径
#函数返回值：包括[Path,Filename]三个属性的列表
#===================================================
FileHelper_GetFilepath<-function(Path)
{
  temp=strsplit(Path,"/",fixed=T);
  count=length(temp[[1]]);
  
  path=temp[[1]][1];
  for(str in temp[[1]][2:(count-1)])
  {
    path=paste(path,str,sep='/')
  }
  return(list(Path=path,Filename=temp[[1]][count]));
}

#---------------------------------------------------
#===================================================
#函数名称：完成文件路径、文件名，后缀的组合
#函数参数：path文件路径,name文件名,ext后缀
#函数返回值：组合后的文件完整路径
#===================================================
FileHelper_CombinePath<-function(path,name,ext)
{
  temp=paste(name,ext,sep=".");
  return(file.path(path,temp));
}
#---------------------------------------------------
#===================================================
#函数名称：根据指定路径创建文件夹
#函数参数：path 待创建的文件夹路径（不以/结束）
#函数返回值：逻辑值，判断指定文件夹是否创建
#===================================================
FileHelper_CreateDir<-function(path)
{
  if(!file.exists(path))
  {
    paths=strsplit(path,"/",fixed=T)[[1]];
    temp=paths[1];
    for(i in 2:length(paths))
    {
      temp=file.path(temp,paths[i])
      if(!file.exists(temp))
        dir.create(temp);
    }
  }
  return(file.exists(path))
}
#---------------------------------------------------
#===================================================
#函数名称：根据指定过滤字符串遍历文件夹
#函数参数：path 待遍历的文件夹路径（不以/结束）
#          fitter 指定的过滤字符串
#函数返回值：过滤后的文件完整路径向量
#===================================================
FileHelper_FileFitter<-function(path,fitter)
{
  
  
  if(regexpr(".",fitter,fixed=T)[1]!=-1)
    return (list.files(path,full.name=T,pattern=paste("*.",fitter,sep="")))
  else if(regexpr(".",fitter,fixed=T)[1]==1)
    return (list.files(path,full.name=T,pattern=paste("*",fitter,sep="")))
  else
    return (list.files(path,full.name=T,pattern=fitter))
  
}
#---------------------------------------------------
#===================================================
#函数名称：将绘图句柄绘制为pdf文件
#函数参数：handle 待绘制的图形句柄
#          path 输出的pdf文件路径
#函数返回值：void
#===================================================
FileHelper_Graphics_PDFoutputting<-function(handle,path)
{
 # print(path);
  if(FileHelper_CreateDir(FileHelper_GetFilepath(path)[[1]])==T)
  {
    pdf(path,width=10,height=10)
    print(handle)
    dev.off();
  } 
}
#---------------------------------------------------
#===================================================
#函数名称：将绘图句柄绘制为png文件
#函数参数：handle 待绘制的图形句柄
#          path 输出的pdf文件路径
#          wid图像宽度 hei 图像高度 单位像素
#函数返回值：void
#===================================================
FileHelper_Graphics_PNGoutputting<-function(handle,path,wid=600,hei=338)
{
  if(FileHelper_CreateDir(FileHelper_GetFilepath(path)[[1]])==T)
  {
    png(path,width=wid,height=hei)
    print(handle)
    dev.off();
  } 
}
#---------------------------------------------------
#===================================================
#函数名称：将矩阵句柄输出为csv文件
#函数参数：mat 待输出的矩阵
#          path 输出的csv文件路径
#函数返回值：void
#===================================================
FileHelper_Matrix_Csvoutputting<-function(Mat,path)
{
  if(FileHelper_CreateDir(FileHelper_GetFilepath(path)[[1]])==T)
  {
   write.csv(mat,path);
  } 
}
#---------------------------------------------------
#===================================================
#函数名称：将数据输出为Txt文件
#函数参数：title 数据的标题
#          data 待输出的矩阵
#          path 输出的Txt文件路径
#函数返回值：void
#===================================================
FileHelper_Data_Txtoutputting<-function(title,data,path)
{
  if(FileHelper_CreateDir(FileHelper_GetFilepath(path)[[1]])==T)
  {
    sink(path);
    print(paste("=-=-=-=-=-=-=-=-=-=-=",title,"=-=-=-=-=-=-=-=-=-=-=");
    print(data); 
    sink();
  } 
}
#---------------------------------------------------
#===================================================
#函数名称：将数据输出为Txt文件(路径相同时，则采用追加模式)
#函数参数：title 数据的标题
#          data 待输出的矩阵
#          path 输出的Txt文件路径
#函数返回值：void
#===================================================
FileHelper_Data_TxtAppendoutputting<-function(title,data,path)
{
  if(FileHelper_CreateDir(FileHelper_GetFilepath(path)[[1]])==T)
  {
    sink(path,append=T);
    print(paste("=-=-=-=-=-=-=-=-=-=-=",title,"=-=-=-=-=-=-=-=-=-=-=");
    print(data); 
    sink();
  } 
}