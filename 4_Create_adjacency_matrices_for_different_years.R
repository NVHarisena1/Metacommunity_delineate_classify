##Calculate spatial distances in the past

l<-750
basepath<-paste0("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/Networks",l)
setwd(basepath)

#load("Basedata_28")

save1992[!is.finite(save1992)]<-NA
try1<-matrix(nrow = length(unique(wet2010@data$Id)), ncol= length(unique(wet2010@data$Id)))
rownames(try1)<-colnames(try1)<-unique(wet2010@data$Id)


system.time(for(m in 1:length(wet2010@data$Id)){
  k<-unlist(wet2010@data[wet2010@data$Id==m,"cat1992"])
  print(m)
  for(n in m:length(wet2010@data$Id)){ 
    l<-unlist(wet2010@data[wet2010@data$Id==n,"cat1992"])
    
  if(length(k)==1 && length(l)==1)  {  
    xx1<-save1992[k,l]
  } else if(length(k)==2 && length(l)==1){
    xx1<-xx2<-xx3<-xx4<-NA
    xx1<-save1992[k[1],l[1]]
    xx2<-save1992[k[2],l[1]]
    xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
  } else if(length(k)==1 && length(l)==2){
    xx1<-xx2<-xx3<-xx4<-NA
    xx1<-save1992[k[1],l[1]]
    xx2<-save1992[k[1],l[2]]
    xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
  }else if(length(k)==3 && length(l)==1){
    xx1<-xx2<-xx3<-xx4<-NA
    xx1<-save1992[k[1],l[1]]
    xx2<-save1992[k[2],l[1]]
    xx3<-save1992[k[3],l[1]]
    xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
  } else if(length(k)==1 && length(l)==3){
    xx1<-xx2<-xx3<-xx4<-NA
    xx1<-save1992[k[1],l[1]]
    xx2<-save1992[k[1],l[2]]
    xx3<-save1992[k[1],l[3]]
    xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
  } else if(length(k)==2 && length(l)==3){
    xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
    xx1<-save1992[k[1],l[1]]
    xx2<-save1992[k[1],l[2]]
    xx3<-save1992[k[1],l[3]]
    xx4<-save1992[k[2],l[1]]
    xx5<-save1992[k[2],l[2]]
    xx6<-save1992[k[2],l[3]]
    xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
  }else if(length(k)==3 && length(l)==2){
    xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
    xx1<-save1992[k[1],l[1]]
    xx2<-save1992[k[2],l[1]]
    xx3<-save1992[k[3],l[1]]
    xx4<-save1992[k[1],l[2]]
    xx5<-save1992[k[2],l[2]]
    xx6<-save1992[k[3],l[2]]
    xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
  }else if(length(k)==3 && length(l)==3){
    xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-xx7<-xx8<-xx9<-NA
    xx1<-save1992[k[1],l[1]]
    xx2<-save1992[k[1],l[2]]
    xx3<-save1992[k[1],l[3]]
    xx4<-save1992[k[2],l[1]]
    xx5<-save1992[k[2],l[2]]
    xx6<-save1992[k[2],l[3]]
    xx7<-save1992[k[3],l[1]]
    xx8<-save1992[k[3],l[2]]
    xx9<-save1992[k[3],l[3]]
    xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9),na.rm=TRUE)
  }
    
    try1[m,n]<-xx1
  }}
)
    
    
TNDclustdist_sptemp1992<-try1
colnames(TNDclustdist_sptemp1992)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp1992)<-unique(wet2010@data$Id)




save1978[!is.finite(save1978)]<-NA
try1<-matrix(nrow = length(unique(wet2010@data$Id)), ncol= length(unique(wet2010@data$Id)))
rownames(try1)<-colnames(try1)<-unique(wet2010@data$Id)


system.time(for(m in 1:length(wet2010@data$Id)){
  k<-unlist(wet2010@data[wet2010@data$Id==m,"cat1978"])
  print(m)
  for(n in m:length(wet2010@data$Id)){ 
    l<-unlist(wet2010@data[wet2010@data$Id==n,"cat1978"])
    
    if(length(k)==1 && length(l)==1)  {  
      xx1<-save1978[k,l]
    } else if(length(k)==2 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1978[k[1],l[1]]
      xx2<-save1978[k[2],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1978[k[1],l[1]]
      xx2<-save1978[k[1],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1978[k[1],l[1]]
      xx2<-save1978[k[2],l[1]]
      xx3<-save1978[k[3],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1978[k[1],l[1]]
      xx2<-save1978[k[1],l[2]]
      xx3<-save1978[k[1],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==2 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1978[k[1],l[1]]
      xx2<-save1978[k[1],l[2]]
      xx3<-save1978[k[1],l[3]]
      xx4<-save1978[k[2],l[1]]
      xx5<-save1978[k[2],l[2]]
      xx6<-save1978[k[2],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1978[k[1],l[1]]
      xx2<-save1978[k[2],l[1]]
      xx3<-save1978[k[3],l[1]]
      xx4<-save1978[k[1],l[2]]
      xx5<-save1978[k[2],l[2]]
      xx6<-save1978[k[3],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-xx7<-xx8<-xx9<-NA
      xx1<-save1978[k[1],l[1]]
      xx2<-save1978[k[1],l[2]]
      xx3<-save1978[k[1],l[3]]
      xx4<-save1978[k[2],l[1]]
      xx5<-save1978[k[2],l[2]]
      xx6<-save1978[k[2],l[3]]
      xx7<-save1978[k[3],l[1]]
      xx8<-save1978[k[3],l[2]]
      xx9<-save1978[k[3],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9),na.rm=TRUE)
    }
    
    try1[m,n]<-xx1
  }}
)

TNDclustdist_sptemp1978<-try1
colnames(TNDclustdist_sptemp1978)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp1978)<-unique(wet2010@data$Id)



save1970[!is.finite(save1970)]<-NA
try1<-matrix(nrow = length(unique(wet2010@data$Id)), ncol= length(unique(wet2010@data$Id)))
rownames(try1)<-colnames(try1)<-unique(wet2010@data$Id)


system.time(for(m in 1:length(wet2010@data$Id)){
  k<-unlist(wet2010@data[wet2010@data$Id==m,"cat1970"])
  print(m)
  for(n in m:length(wet2010@data$Id)){ 
    l<-unlist(wet2010@data[wet2010@data$Id==n,"cat1970"])
    
    if(length(k)==1 && length(l)==1)  {  
      xx1<-save1970[k,l]
    } else if(length(k)==2 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1970[k[1],l[1]]
      xx2<-save1970[k[2],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1970[k[1],l[1]]
      xx2<-save1970[k[1],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1970[k[1],l[1]]
      xx2<-save1970[k[2],l[1]]
      xx3<-save1970[k[3],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1970[k[1],l[1]]
      xx2<-save1970[k[1],l[2]]
      xx3<-save1970[k[1],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==2 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1970[k[1],l[1]]
      xx2<-save1970[k[1],l[2]]
      xx3<-save1970[k[1],l[3]]
      xx4<-save1970[k[2],l[1]]
      xx5<-save1970[k[2],l[2]]
      xx6<-save1970[k[2],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1970[k[1],l[1]]
      xx2<-save1970[k[2],l[1]]
      xx3<-save1970[k[3],l[1]]
      xx4<-save1970[k[1],l[2]]
      xx5<-save1970[k[2],l[2]]
      xx6<-save1970[k[3],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-xx7<-xx8<-xx9<-NA
      xx1<-save1970[k[1],l[1]]
      xx2<-save1970[k[1],l[2]]
      xx3<-save1970[k[1],l[3]]
      xx4<-save1970[k[2],l[1]]
      xx5<-save1970[k[2],l[2]]
      xx6<-save1970[k[2],l[3]]
      xx7<-save1970[k[3],l[1]]
      xx8<-save1970[k[3],l[2]]
      xx9<-save1970[k[3],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9),na.rm=TRUE)
    }
    
    try1[m,n]<-xx1
  }}
)

TNDclustdist_sptemp1970<-try1
colnames(TNDclustdist_sptemp1970)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp1970)<-unique(wet2010@data$Id)





save1959[!is.finite(save1959)]<-NA
try1<-matrix(nrow = length(unique(wet2010@data$Id)), ncol= length(unique(wet2010@data$Id)))
rownames(try1)<-colnames(try1)<-unique(wet2010@data$Id)


system.time(for(m in 1:length(wet2010@data$Id)){
  k<-unlist(wet2010@data[wet2010@data$Id==m,"cat1959"])
  print(m)
  for(n in m:length(wet2010@data$Id)){ 
    l<-unlist(wet2010@data[wet2010@data$Id==n,"cat1959"])
    
    if(length(k)==1 && length(l)==1)  {  
      xx1<-save1959[k,l]
    } else if(length(k)==2 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1959[k[1],l[1]]
      xx2<-save1959[k[2],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1959[k[1],l[1]]
      xx2<-save1959[k[1],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1959[k[1],l[1]]
      xx2<-save1959[k[2],l[1]]
      xx3<-save1959[k[3],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1959[k[1],l[1]]
      xx2<-save1959[k[1],l[2]]
      xx3<-save1959[k[1],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==2 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1959[k[1],l[1]]
      xx2<-save1959[k[1],l[2]]
      xx3<-save1959[k[1],l[3]]
      xx4<-save1959[k[2],l[1]]
      xx5<-save1959[k[2],l[2]]
      xx6<-save1959[k[2],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1959[k[1],l[1]]
      xx2<-save1959[k[2],l[1]]
      xx3<-save1959[k[3],l[1]]
      xx4<-save1959[k[1],l[2]]
      xx5<-save1959[k[2],l[2]]
      xx6<-save1959[k[3],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-xx7<-xx8<-xx9<-NA
      xx1<-save1959[k[1],l[1]]
      xx2<-save1959[k[1],l[2]]
      xx3<-save1959[k[1],l[3]]
      xx4<-save1959[k[2],l[1]]
      xx5<-save1959[k[2],l[2]]
      xx6<-save1959[k[2],l[3]]
      xx7<-save1959[k[3],l[1]]
      xx8<-save1959[k[3],l[2]]
      xx9<-save1959[k[3],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9),na.rm=TRUE)
    }
    
    try1[m,n]<-xx1
  }}
)

TNDclustdist_sptemp1959<-try1
colnames(TNDclustdist_sptemp1959)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp1959)<-unique(wet2010@data$Id)






save1949[!is.finite(save1949)]<-NA
try1<-matrix(nrow = length(unique(wet2010@data$Id)), ncol= length(unique(wet2010@data$Id)))
rownames(try1)<-colnames(try1)<-unique(wet2010@data$Id)


system.time(for(m in 1:length(wet2010@data$Id)){
  k<-unlist(wet2010@data[wet2010@data$Id==m,"cat1949"])
  print(m)
  for(n in m:length(wet2010@data$Id)){ 
    l<-unlist(wet2010@data[wet2010@data$Id==n,"cat1949"])
    
    if(length(k)==1 && length(l)==1)  {  
      xx1<-save1949[k,l]
    } else if(length(k)==2 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1949[k[1],l[1]]
      xx2<-save1949[k[2],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1949[k[1],l[1]]
      xx2<-save1949[k[1],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1949[k[1],l[1]]
      xx2<-save1949[k[2],l[1]]
      xx3<-save1949[k[3],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1949[k[1],l[1]]
      xx2<-save1949[k[1],l[2]]
      xx3<-save1949[k[1],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==2 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1949[k[1],l[1]]
      xx2<-save1949[k[1],l[2]]
      xx3<-save1949[k[1],l[3]]
      xx4<-save1949[k[2],l[1]]
      xx5<-save1949[k[2],l[2]]
      xx6<-save1949[k[2],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1949[k[1],l[1]]
      xx2<-save1949[k[2],l[1]]
      xx3<-save1949[k[3],l[1]]
      xx4<-save1949[k[1],l[2]]
      xx5<-save1949[k[2],l[2]]
      xx6<-save1949[k[3],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-xx7<-xx8<-xx9<-NA
      xx1<-save1949[k[1],l[1]]
      xx2<-save1949[k[1],l[2]]
      xx3<-save1949[k[1],l[3]]
      xx4<-save1949[k[2],l[1]]
      xx5<-save1949[k[2],l[2]]
      xx6<-save1949[k[2],l[3]]
      xx7<-save1949[k[3],l[1]]
      xx8<-save1949[k[3],l[2]]
      xx9<-save1949[k[3],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9),na.rm=TRUE)
    }
    
    try1[m,n]<-xx1
  }}
)

TNDclustdist_sptemp1949<-try1
colnames(TNDclustdist_sptemp1949)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp1949)<-unique(wet2010@data$Id)





save1933[!is.finite(save1933)]<-NA
try1<-matrix(nrow = length(unique(wet2010@data$Id)), ncol= length(unique(wet2010@data$Id)))
rownames(try1)<-colnames(try1)<-unique(wet2010@data$Id)


system.time(for(m in 1:length(wet2010@data$Id)){
  k<-unlist(wet2010@data[wet2010@data$Id==m,"cat1933"])
  print(m)
  for(n in m:length(wet2010@data$Id)){ 
    l<-unlist(wet2010@data[wet2010@data$Id==n,"cat1933"])
    
    if(length(k)==1 && length(l)==1)  {  
      xx1<-save1933[k,l]
    } else if(length(k)==2 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1933[k[1],l[1]]
      xx2<-save1933[k[2],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1933[k[1],l[1]]
      xx2<-save1933[k[1],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1933[k[1],l[1]]
      xx2<-save1933[k[2],l[1]]
      xx3<-save1933[k[3],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1933[k[1],l[1]]
      xx2<-save1933[k[1],l[2]]
      xx3<-save1933[k[1],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==2 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1933[k[1],l[1]]
      xx2<-save1933[k[1],l[2]]
      xx3<-save1933[k[1],l[3]]
      xx4<-save1933[k[2],l[1]]
      xx5<-save1933[k[2],l[2]]
      xx6<-save1933[k[2],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1933[k[1],l[1]]
      xx2<-save1933[k[2],l[1]]
      xx3<-save1933[k[3],l[1]]
      xx4<-save1933[k[1],l[2]]
      xx5<-save1933[k[2],l[2]]
      xx6<-save1933[k[3],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-xx7<-xx8<-xx9<-NA
      xx1<-save1933[k[1],l[1]]
      xx2<-save1933[k[1],l[2]]
      xx3<-save1933[k[1],l[3]]
      xx4<-save1933[k[2],l[1]]
      xx5<-save1933[k[2],l[2]]
      xx6<-save1933[k[2],l[3]]
      xx7<-save1933[k[3],l[1]]
      xx8<-save1933[k[3],l[2]]
      xx9<-save1933[k[3],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9),na.rm=TRUE)
    }
    
    try1[m,n]<-xx1
  }}
)

TNDclustdist_sptemp1933<-try1
colnames(TNDclustdist_sptemp1933)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp1933)<-unique(wet2010@data$Id)





save1918[!is.finite(save1918)]<-NA
try1<-matrix(nrow = length(unique(wet2010@data$Id)), ncol= length(unique(wet2010@data$Id)))
rownames(try1)<-colnames(try1)<-unique(wet2010@data$Id)


system.time(for(m in 1:length(wet2010@data$Id)){
  k<-unlist(wet2010@data[wet2010@data$Id==m,"cat1918"])
  print(m)
  for(n in m:length(wet2010@data$Id)){ 
    l<-unlist(wet2010@data[wet2010@data$Id==n,"cat1918"])
    
    if(length(k)==1 && length(l)==1)  {  
      xx1<-save1918[k,l]
    } else if(length(k)==2 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1918[k[1],l[1]]
      xx2<-save1918[k[2],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1918[k[1],l[1]]
      xx2<-save1918[k[1],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1918[k[1],l[1]]
      xx2<-save1918[k[2],l[1]]
      xx3<-save1918[k[3],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1918[k[1],l[1]]
      xx2<-save1918[k[1],l[2]]
      xx3<-save1918[k[1],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==2 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1918[k[1],l[1]]
      xx2<-save1918[k[1],l[2]]
      xx3<-save1918[k[1],l[3]]
      xx4<-save1918[k[2],l[1]]
      xx5<-save1918[k[2],l[2]]
      xx6<-save1918[k[2],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1918[k[1],l[1]]
      xx2<-save1918[k[2],l[1]]
      xx3<-save1918[k[3],l[1]]
      xx4<-save1918[k[1],l[2]]
      xx5<-save1918[k[2],l[2]]
      xx6<-save1918[k[3],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-xx7<-xx8<-xx9<-NA
      xx1<-save1918[k[1],l[1]]
      xx2<-save1918[k[1],l[2]]
      xx3<-save1918[k[1],l[3]]
      xx4<-save1918[k[2],l[1]]
      xx5<-save1918[k[2],l[2]]
      xx6<-save1918[k[2],l[3]]
      xx7<-save1918[k[3],l[1]]
      xx8<-save1918[k[3],l[2]]
      xx9<-save1918[k[3],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9),na.rm=TRUE)
    }
    
    try1[m,n]<-xx1
  }}
)

TNDclustdist_sptemp1918<-try1
colnames(TNDclustdist_sptemp1918)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp1918)<-unique(wet2010@data$Id)




save1899[!is.finite(save1899)]<-NA
try1<-matrix(nrow = length(unique(wet2010@data$Id)), ncol= length(unique(wet2010@data$Id)))
rownames(try1)<-colnames(try1)<-unique(wet2010@data$Id)


system.time(for(m in 1:length(wet2010@data$Id)){
  k<-unlist(wet2010@data[wet2010@data$Id==m,"cat1899"])
  print(m)
  for(n in m:length(wet2010@data$Id)){ 
    l<-unlist(wet2010@data[wet2010@data$Id==n,"cat1899"])
    
    if(length(k)==1 && length(l)==1)  {  
      xx1<-save1899[k,l]
    } else if(length(k)==2 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1899[k[1],l[1]]
      xx2<-save1899[k[2],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1899[k[1],l[1]]
      xx2<-save1899[k[1],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==1){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1899[k[1],l[1]]
      xx2<-save1899[k[2],l[1]]
      xx3<-save1899[k[3],l[1]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==1 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-NA
      xx1<-save1899[k[1],l[1]]
      xx2<-save1899[k[1],l[2]]
      xx3<-save1899[k[1],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4),na.rm=TRUE)
    } else if(length(k)==2 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1899[k[1],l[1]]
      xx2<-save1899[k[1],l[2]]
      xx3<-save1899[k[1],l[3]]
      xx4<-save1899[k[2],l[1]]
      xx5<-save1899[k[2],l[2]]
      xx6<-save1899[k[2],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==2){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-NA
      xx1<-save1899[k[1],l[1]]
      xx2<-save1899[k[2],l[1]]
      xx3<-save1899[k[3],l[1]]
      xx4<-save1899[k[1],l[2]]
      xx5<-save1899[k[2],l[2]]
      xx6<-save1899[k[3],l[2]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6),na.rm=TRUE)
    }else if(length(k)==3 && length(l)==3){
      xx1<-xx2<-xx3<-xx4<-xx5<-xx6<-xx7<-xx8<-xx9<-NA
      xx1<-save1899[k[1],l[1]]
      xx2<-save1899[k[1],l[2]]
      xx3<-save1899[k[1],l[3]]
      xx4<-save1899[k[2],l[1]]
      xx5<-save1899[k[2],l[2]]
      xx6<-save1899[k[2],l[3]]
      xx7<-save1899[k[3],l[1]]
      xx8<-save1899[k[3],l[2]]
      xx9<-save1899[k[3],l[3]]
      xx1<-min(c(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9),na.rm=TRUE)
    }
    
    try1[m,n]<-xx1
  }}
)

TNDclustdist_sptemp1899<-try1
colnames(TNDclustdist_sptemp1899)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp1899)<-unique(wet2010@data$Id)



##add basic values of patches with same ancestor patch

for(j in c(unique(wet2010@data$Id))){
  k<-unlist(wet2010@data[wet2010@data$Id==j,"cat1992"])
  ttmp<-wet2010@data[wet2010@data$cat1992 %in% c(k),]
  TNDclustdist_sptemp1992[c(ttmp$Id),c(ttmp$Id)]<-10
}

for(j in c(unique(wet2010@data$Id))){
  k<-unlist(wet2010@data[wet2010@data$Id==j,"cat1978"])
  ttmp<-wet2010@data[wet2010@data$cat1978 %in% c(k),]
  TNDclustdist_sptemp1978[c(ttmp$Id),c(ttmp$Id)]<-10
}

for(j in c(unique(wet2010@data$Id))){
  k<-unlist(wet2010@data[wet2010@data$Id==j,"cat1970"])
  ttmp<-wet2010@data[wet2010@data$cat1970 %in% c(k),]
  TNDclustdist_sptemp1970[c(ttmp$Id),c(ttmp$Id)]<-10
}

for(j in c(unique(wet2010@data$Id))){
  k<-unlist(wet2010@data[wet2010@data$Id==j,"cat1959"])
  ttmp<-wet2010@data[wet2010@data$cat1970 %in% c(k),]
  TNDclustdist_sptemp1959[c(ttmp$Id),c(ttmp$Id)]<-10
}


for(j in c(unique(wet2010@data$Id))){
  k<-unlist(wet2010@data[wet2010@data$Id==j,"cat1949"])
  ttmp<-wet2010@data[wet2010@data$cat1970 %in% c(k),]
  TNDclustdist_sptemp1949[c(ttmp$Id),c(ttmp$Id)]<-10
}


for(j in c(unique(wet2010@data$Id))){
  k<-unlist(wet2010@data[wet2010@data$Id==j,"cat1933"])
  ttmp<-wet2010@data[wet2010@data$cat1970 %in% c(k),]
  TNDclustdist_sptemp1933[c(ttmp$Id),c(ttmp$Id)]<-10
}


for(j in c(unique(wet2010@data$Id))){
  k<-unlist(wet2010@data[wet2010@data$Id==j,"cat1918"])
  ttmp<-wet2010@data[wet2010@data$cat1970 %in% c(k),]
  TNDclustdist_sptemp1918[c(ttmp$Id),c(ttmp$Id)]<-10
}


for(j in c(unique(wet2010@data$Id))){
  k<-unlist(wet2010@data[wet2010@data$Id==j,"cat1899"])
  ttmp<-wet2010@data[wet2010@data$cat1970 %in% c(k),]
  TNDclustdist_sptemp1899[c(ttmp$Id),c(ttmp$Id)]<-10
}


TNDclustdist_sptemp1992[!is.finite(TNDclustdist_sptemp1992)]<-NA
TNDclustdist_sptemp1978[!is.finite(TNDclustdist_sptemp1978)]<-NA
TNDclustdist_sptemp1970[!is.finite(TNDclustdist_sptemp1970)]<-NA
TNDclustdist_sptemp1959[!is.finite(TNDclustdist_sptemp1959)]<-NA
TNDclustdist_sptemp1949[!is.finite(TNDclustdist_sptemp1949)]<-NA
TNDclustdist_sptemp1933[!is.finite(TNDclustdist_sptemp1933)]<-NA
TNDclustdist_sptemp1918[!is.finite(TNDclustdist_sptemp1918)]<-NA
TNDclustdist_sptemp1899[!is.finite(TNDclustdist_sptemp1899)]<-NA



#basepath<-"C:/Users/nharisena/Documents/research/Working/temporal_networks/Cycle_2/odonate/disp2000"
#setwd(basepath)

base_patch<-rgdal::readOGR("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/base_patch/patches.shp")
#K<-over(wet2010,base_patch, byid = TRUE)
#crosstab<-cbind(wet2010@data$Id,K$Id)
#row.names(crosstab)<-crosstab[,1]
#colnames(crosstab)<-c("layid","baseid")

#test<-TNDclustdist_sptemp
#test<-cbind(test,row.names(TNDclustdist_sptemp))
#test<-as.data.frame(test)
#colnames(test)<-c(1:3509,"layid")
#colnames(TNDclustdist_sptemp1992)<-rownames(TNDclustdist_sptemp1992)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_sptemp1992),"baseid"]
#colnames(TNDclustdist_sptemp1978)<-rownames(TNDclustdist_sptemp1978)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_sptemp1978),"baseid"]
#colnames(TNDclustdist_sptemp1970)<-rownames(TNDclustdist_sptemp1970)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_sptemp1970),"baseid"]
#colnames(TNDclustdist_sptemp1959)<-rownames(TNDclustdist_sptemp1959)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_sptemp1959),"baseid"]
#colnames(TNDclustdist_sptemp1949)<-rownames(TNDclustdist_sptemp1949)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_sptemp1949),"baseid"]
#colnames(TNDclustdist_sptemp1933)<-rownames(TNDclustdist_sptemp1933)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_sptemp1933),"baseid"]
#colnames(TNDclustdist_sptemp1918)<-rownames(TNDclustdist_sptemp1918)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_sptemp1918),"baseid"]
#colnames(TNDclustdist_sptemp1899)<-rownames(TNDclustdist_sptemp1899)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_sptemp1899),"baseid"]

#current distance

#refids<-cbind(as.numeric(1:4660), as.numeric(crosstab[,2]))
#colnames(refids)<-c("ID1","BaseID")
##Add 2010 data
#ch2010link<-read.csv("ch2010link.csv")
ch2010link<- Network2010_rsc
colnames(ch2010link)<-c("ID1",	"ID2","rsc_dist")
ch2010link$ID1<-as.numeric(ch2010link$ID1)
ch2010link$ID2<-as.numeric(ch2010link$ID2)

ch2010link$ID1bckp<-ch2010link$ID1
ch2010link$ID2bckp<-ch2010link$ID2

#ch2010link$ID1<-sapply(1:length(ch2010link$ID1), function(i) {refids[refids[,1]==ch2010link$ID1bckp[i],2] })
#K<-merge(ch2010link,refids,by="ID1",sort=TRUE)
#ch2010link[order(ch2010link$ID1bckp),"ID1"]<-K$BaseID #NO ORDERING order(ch2010link$ID1bckp)

#colnames(refids)<-c("ID2","BaseID")
#K<-merge(ch2010link,refids,by="ID2",sort=TRUE)
#ch2010link[order(ch2010link$ID2bckp),"ID2"]<-K$BaseID

#Annyfunctions
dat2adj2<- function(i){
  dist2010[ch2010link[i,"ID1"],ch2010link[i,"ID2"]]<<-ch2010link[i,"rsc_dist"]
  dist2010[ch2010link[i,"ID2"],ch2010link[i,"ID1"]]<<-ch2010link[i,"rsc_dist"]
}


dist2010<-matrix(NA,nrow=length(base_patch@data$Id),ncol=length(base_patch@data$Id))
colnames(dist2010)<-base_patch@data$Id
rownames(dist2010)<-base_patch@data$Id
sapply(1:length(ch2010link$ID1), FUN=dat2adj2)

#order all distances
dist2010<-dist2010[order(as.numeric(rownames(dist2010))),order(as.numeric(colnames(dist2010)))]
TNDclustdist_sptemp1992<-TNDclustdist_sptemp1992[order(as.numeric(rownames(TNDclustdist_sptemp1992))),order(as.numeric(colnames(TNDclustdist_sptemp1992)))]
TNDclustdist_sptemp1978<-TNDclustdist_sptemp1978[order(as.numeric(rownames(TNDclustdist_sptemp1978))),order(as.numeric(colnames(TNDclustdist_sptemp1978)))]
TNDclustdist_sptemp1970<-TNDclustdist_sptemp1970[order(as.numeric(rownames(TNDclustdist_sptemp1970))),order(as.numeric(colnames(TNDclustdist_sptemp1970)))]
TNDclustdist_sptemp1959<-TNDclustdist_sptemp1959[order(as.numeric(rownames(TNDclustdist_sptemp1959))),order(as.numeric(colnames(TNDclustdist_sptemp1959)))]
TNDclustdist_sptemp1949<-TNDclustdist_sptemp1949[order(as.numeric(rownames(TNDclustdist_sptemp1949))),order(as.numeric(colnames(TNDclustdist_sptemp1949)))]
TNDclustdist_sptemp1933<-TNDclustdist_sptemp1933[order(as.numeric(rownames(TNDclustdist_sptemp1933))),order(as.numeric(colnames(TNDclustdist_sptemp1933)))]
TNDclustdist_sptemp1918<-TNDclustdist_sptemp1918[order(as.numeric(rownames(TNDclustdist_sptemp1918))),order(as.numeric(colnames(TNDclustdist_sptemp1918)))]
TNDclustdist_sptemp1899<-TNDclustdist_sptemp1899[order(as.numeric(rownames(TNDclustdist_sptemp1899))),order(as.numeric(colnames(TNDclustdist_sptemp1899)))]


## Combine all  distances


TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

l<-750
setwd(paste0("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/Networks",l))

TNDclustdist_sptemp<-dist2010
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1992,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1978,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1970,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1959,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1949,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1933,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1918,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1899,TNDclustdist_sptemp)


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_2010weight_all.csv"))



#colnames(TNDclustdist_sptemp)<-rownames(TNDclustdist_sptemp)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_sptemp),"baseid"]
#colnames(TNDclustdist_Timetemp)<-rownames(TNDclustdist_Timetemp)<-crosstab[crosstab[,"layid"]==row.names(TNDclustdist_Timetemp),"baseid"]


write.csv(dist2010,file=paste0("TNDmatrixspace2010.csv"))
write.csv(TNDclustdist_sptemp1992,file=paste0("TNDmatrixspace1992.csv"))
write.csv(TNDclustdist_sptemp1978,file=paste0("TNDmatrixspace1978.csv"))
write.csv(TNDclustdist_sptemp1970,file=paste0("TNDmatrixspace1970.csv"))
write.csv(TNDclustdist_sptemp1959,file=paste0("TNDmatrixspace1959.csv"))
write.csv(TNDclustdist_sptemp1949,file=paste0("TNDmatrixspace1949.csv"))
write.csv(TNDclustdist_sptemp1933,file=paste0("TNDmatrixspace1933.csv"))
write.csv(TNDclustdist_sptemp1918,file=paste0("TNDmatrixspace1918.csv"))
write.csv(TNDclustdist_sptemp1899,file=paste0("TNDmatrixspace1899.csv"))





## Combine all  distances
TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

TNDclustdist_sptemp<-dist2010
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1992,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1978,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1970,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1959,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1949,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1933,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1918,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1899,TNDclustdist_sptemp)


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_t1899.csv"))



TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

TNDclustdist_sptemp<-dist2010
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1992,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1978,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1970,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1959,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1949,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1933,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1918,TNDclustdist_sptemp)


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_t1918.csv"))


TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

TNDclustdist_sptemp<-dist2010
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1992,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1978,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1970,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1959,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1949,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1933,TNDclustdist_sptemp)


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_t1933.csv"))



TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

TNDclustdist_sptemp<-dist2010
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1992,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1978,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1970,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1959,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1949,TNDclustdist_sptemp)


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_t1949.csv"))


TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

TNDclustdist_sptemp<-dist2010
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1992,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1978,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1970,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1959,TNDclustdist_sptemp)


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_t1959.csv"))


TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

TNDclustdist_sptemp<-dist2010
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1992,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1978,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1970,TNDclustdist_sptemp)


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_t1970.csv"))

TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

TNDclustdist_sptemp<-dist2010
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1992,TNDclustdist_sptemp)
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1978,TNDclustdist_sptemp)


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_t1978.csv"))


TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

TNDclustdist_sptemp<-dist2010
TNDclustdist_sptemp<-ifelse(is.na(TNDclustdist_sptemp),TNDclustdist_sptemp1992,TNDclustdist_sptemp)


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_t1992.csv"))

TNDclustdist_sptemp<-matrix(NA, nrow=length(unique(wet2010@data$Id)),ncol=length(unique(wet2010@data$Id)))
colnames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)
rownames(TNDclustdist_sptemp)<-unique(wet2010@data$Id)

TNDclustdist_sptemp<-dist2010


write.csv(TNDclustdist_sptemp,file=paste0("Space_STN_t2010.csv"))




