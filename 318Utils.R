POI_318_update_pointtype<-function(path)
{
  Pois=read.csv(path);
  #Type_one=t,1,1);
  #Type_two=substr(t,2,3);
  #Type_three=substr(t,4,4);
  
  for(i in c(1:dim(Pois)[1]))
  #for(i in c(1:10))
  {
    t=Pois[i,10];
    Type_one=substr(t,1,1);
    
    print(i);
    
    if(Pois[i,2]=="")
      Pois[i,2]="未知";
    
    if(Type_one=="C")
    {
      Pois[i,7]="Food";
      Pois[i,8]="sh1";
    }
    else if(Type_one=="Z")
    {
      
      Pois[i,7]="Inn";
      Pois[i,8]="sh2";
    }
    else if(Type_one=="X")
    {
      Pois[i,7]="Transplate";
      Pois[i,8]="sh3";
    }
    else if(Type_one=="G")
    {
      Pois[i,7]="Shop";
      Pois[i,8]="sh4";
    }
    else if(Type_one=="L")
    {
      Pois[i,7]="Entertement";
      Pois[i,8]="sh5";
    }
    else if(Type_one=="F")
    {
      Pois[i,7]="Serve";
      Pois[i,8]="sh6";
    }
    else
    {
      Pois[i,7]="Other";
      Pois[i,8]="sh7";
    }
    
    write.csv(Pois,"H:\\basetable.csv",na="",row.names=F);
  }
}