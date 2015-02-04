#===================================================
#��������ļ����ļ��в����Ĺ�����
#===================================================

#---------------------------------------------------
#===================================================
#�������ƣ�����ļ�·����·�����ļ����Լ���׺�ķָ�
#����������filepath ���ָ���ļ�·��
#��������ֵ������[Path,Filename,Ext]�������Ե��б�
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
#�������ƣ�����ļ�·����·�����ļ�����������׺���ָ�
#����������filepath ���ָ���ļ�·��
#��������ֵ������[Path,Filename]�������Ե��б�
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
#�������ƣ�����ļ�·�����ļ�������׺�����
#����������path�ļ�·��,name�ļ���,ext��׺
#��������ֵ����Ϻ���ļ�����·��
#===================================================
FileHelper_CombinePath<-function(path,name,ext)
{
  temp=paste(name,ext,sep=".");
  return(file.path(path,temp));
}
#---------------------------------------------------
#===================================================
#�������ƣ�����ָ��·�������ļ���
#����������path ���������ļ���·��������/������
#��������ֵ���߼�ֵ���ж�ָ���ļ����Ƿ񴴽�
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
#�������ƣ�����ָ�������ַ��������ļ���
#����������path ���������ļ���·��������/������
#          fitter ָ���Ĺ����ַ���
#��������ֵ�����˺���ļ�����·������
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
#�������ƣ�����ͼ�������Ϊpdf�ļ�
#����������handle �����Ƶ�ͼ�ξ��
#          path �����pdf�ļ�·��
#��������ֵ��void
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
#�������ƣ�����ͼ�������Ϊpng�ļ�
#����������handle �����Ƶ�ͼ�ξ��
#          path �����pdf�ļ�·��
#          widͼ����� hei ͼ��߶� ��λ����
#��������ֵ��void
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
#�������ƣ������������Ϊcsv�ļ�
#����������mat ������ľ���
#          path �����csv�ļ�·��
#��������ֵ��void
#===================================================
FileHelper_Matrix_Csvoutputting<-function(Mat,path)
{
  if(FileHelper_CreateDir(FileHelper_GetFilepath(path)[[1]])==T)
  {
   write.csv(mat);
  } 
}
