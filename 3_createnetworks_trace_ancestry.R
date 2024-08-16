library(raster)
library(rgdal)
library(graph4lg)
library(vegan)
library("dplyr")
library(ade4)
library(rgeos)
library(spdep)
library(igraph)
library(usedist)
library(qgraph)
library(psych)

library(rlist)
library(terra)

#OPTIONAL
library(qgraph)

##set dispersal
disp<-1000

##Creating the networks using graph4lg

ch<-paste0(rep("wetland",9),c(1:9),".tif")
yr<-c("1899","1918","1933","1949","1959","1970","1978","1992","2010")

disp<-c(1000)
l<-1000
mm<-1
for( l in disp){
  
  basepath<-paste0("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/Networks",l)
  setwd(basepath)
  
  for (z in ch){
    
    gc()
    unlink("C:/Users/nharisena/Documents/research/Working/temp/*", recursive = T, force = T)
    
    proj_name <- paste0("Network", yr[mm])
    graphab_project(proj_name = proj_name,
                    raster = z,
                    habitat = 1)
    
    #Create cost df 
    cost_df<-as.data.frame(cbind(c(0:6000),c(1,1:6000)))
    colnames(cost_df)<-c("code","cost")
    
    
    txt1<-readLines(paste0("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/Networks",l,"/Network", yr[mm],"/Network",yr[mm],".xml") )
    txt1<-txt1[-c(10)]
    writeLines(txt1,paste0("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/Networks",l,"/Network", yr[mm],"/Network",yr[mm],".xml") )
    
    #with slope
    graphab_link(
      proj_name,
      distance = "euclid",
      cost = cost_df,
      name="Complt_euclid",
      topo = "complete",
      #alloc_ram = 8,
      proj_path = NULL,
      maxcost = l)
    
    
    
    #Create graph
    graphab_graph(
      proj_name,
      linkset = "Complt_euclid",
      name = "Gmxcst10000",
      thr = NULL,
      cost_conv = FALSE,
      proj_path = NULL,
      alloc_ram = NULL
    )
    
    graphab_modul(
      proj_name,
      graph="Gmxcst10000",
      dist=10000,
      prob = 0.05,
      beta = 1,
      nb = NULL,
      return = TRUE,
      proj_path = NULL,
      alloc_ram = NULL
    )
    
    mm<-mm+1
  }
  
}


## Load the network data and trace ancestory of all patches across 1899-2010

disp<-c(1000)
l<-1000

system.time(
  for(l in disp){
    
    basepath<-paste0("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/Networks",l)
    setwd(basepath)
    
    gc()
    #Loading cluster shapefile
    #ch2010vor<-rgdal::readOGR(paste0("Network2010/Gmxcst10000-voronoi.shp"))
    #ch1950vor<-rgdal::readOGR(paste0("Network2010/Gmxcst10000-voronoi.shp"))
    #ch1900vor<-rgdal::readOGR(paste0("Network2010/Gmxcst10000-voronoi.shp"))
    #ch1850vor<-rgdal::readOGR(paste0("Network2010/Gmxcst10000-voronoi.shp"))
    
    wet2010<-rgdal::readOGR(paste0("Network2010/patches.shp"))
    wet1992<-rgdal::readOGR(paste0("Network1992/patches.shp"))
    wet1978<-rgdal::readOGR(paste0("Network1978/patches.shp"))
    wet1970<-rgdal::readOGR(paste0("Network1970/patches.shp"))
    wet1959<-rgdal::readOGR(paste0("Network1959/patches.shp"))
    wet1949<-rgdal::readOGR(paste0("Network1949/patches.shp"))
    wet1933<-rgdal::readOGR(paste0("Network1933/patches.shp"))
    wet1918<-rgdal::readOGR(paste0("Network1918/patches.shp"))
    wet1899<-rgdal::readOGR(paste0("Network1899/patches.shp"))
  
    
    wet2010@data$Tcode<-NA
    wet1992@data$Tcode<-NA
    wet1978@data$Tcode<-NA
    wet1970@data$Tcode<-NA
    wet1959@data$Tcode<-NA
    wet1949@data$Tcode<-NA
    wet1933@data$Tcode<-NA
    wet1918@data$Tcode<-NA
    wet1899@data$Tcode<-NA
    
    
    
    #Identify history of patches
    K<-gIntersects( wet1899, wet1918, byid = TRUE)
    rownames(K)<-c(1:length(wet1918))
    colnames(K)<-c(1:length(wet1899))
    vec<-list()
    vec2<-list()
    for(i in 1:length(wet1918)){
      vec<-list()
      for (j in 1:length(wet1899)){
        #c<-0
        if(K[i,j]==TRUE){
          #c=c+1
          #if(c>1){print(c)}
          p<-subset(wet1899,Id==j)
          
          #p<-subset(wet1899,Id==j)
          vec<- unique(c(unlist(vec),p@data$Id))
          #print(paste0(i,",",j,",",vec))
        }
      }
      if(length(vec)==0){vec<-NA}
      vec2<-c(vec2,paste0(paste0(vec, sep = "_", collapse = ''),"_ch_"))
      #print(paste0(i,",",vec2))
      #print(vec2)
    }
    
    wet1918@data$Tcode<-unlist(vec2)
    
    
    K<-gIntersects( wet1918, wet1933, byid = TRUE)
    rownames(K)<-c(1:length(wet1933))
    colnames(K)<-c(1:length(wet1918))
    vec<-list()
    vec2<-list()
    for(i in 1:length(wet1933)){
      vec<-list()
      xxx<-list()
      for (j in 1:length(wet1918)){
        
        if(K[i,j]==TRUE){
          p<-subset(wet1918,Id==j)
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx<-c(xxx,xx)
          vec<- unique(c(unlist(vec),p@data$Id))
        }
      }
      xxx<-c(unique(unlist(xxx)))
      if(length(vec)==0){vec<-NA}
      wet1933@data[i,"Tcode"]<-paste0(paste0(xxx, sep = "_", collapse = ''),"ch_",paste0(vec, sep = "_", collapse = ''),"ch_")
    }
    
    
    K<-gIntersects( wet1933, wet1949, byid = TRUE)
    rownames(K)<-c(1:length(wet1949))
    colnames(K)<-c(1:length(wet1933))
    vec<-list()
    vec2<-list()
    for(i in 1:length(wet1949)){
      vec<-list()
      xxx<-list()
      xxxx<-list()
      #c=0
      for (j in 1:length(wet1933)){
        if(K[i,j]==TRUE){ 
          p<-subset(wet1933,Id==j)
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[1]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx<-c(xxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[2]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxxx<-c(xxxx,xx)
          
          vec<- unique(c(unlist(vec),p@data$Id))
        }
      }
      xxx<-c(unique(unlist(xxx)))
      xxxx<-c(unique(unlist(xxxx)))
      if(length(vec)==0){vec<-NA}
      wet1949@data[i,"Tcode"]<-paste0(paste0(xxx, sep = "_", collapse = ''),"ch_",paste0(xxxx, sep = "_", collapse = ''),"ch_",paste0(vec, sep = "_", collapse = ''),"ch_")
    }
    
    
    K<-gIntersects( wet1949, wet1959, byid = TRUE)
    rownames(K)<-c(1:length(wet1959))
    colnames(K)<-c(1:length(wet1949))
    vec<-list()
    vec2<-list()
    for(i in 1:length(wet1959)){
      vec<-list()
      xxx<-list()
      xxxx<-list()
      xxx4<-list()
      #c=0
      for (j in 1:length(wet1949)){
        if(K[i,j]==TRUE){
          p<-subset(wet1949,Id==j)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[1]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx<-c(xxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[2]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxxx<-c(xxxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[3]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx4<-c(xxx4,xx)
          
          vec<- unique(c(unlist(vec),p@data$Id))
          
        }
      }
      xxx<-c(unique(unlist(xxx)))
      xxxx<-c(unique(unlist(xxxx)))
      xxx4<-c(unique(unlist(xxx4)))
      if(length(vec)==0){vec<-NA}
      wet1959@data[i,"Tcode"]<-paste0(paste0(xxx, sep = "_", collapse = ''),"ch_",paste0(xxxx, sep = "_", collapse = ''),"ch_",paste0(xxx4, sep = "_", collapse = ''),"ch_",paste0(vec, sep = "_", collapse = ''),"ch_")
    }
    
    
    K<-gIntersects( wet1959, wet1970, byid = TRUE)
    rownames(K)<-c(1:length(wet1970))
    colnames(K)<-c(1:length(wet1959))
    vec<-list()
    vec2<-list()
    for(i in 1:length(wet1970)){
      vec<-list()
      xxx<-list()
      xxxx<-list()
      xxx4<-list()
      xxx5<-list()
      for (j in 1:length(wet1959)){
        if(K[i,j]==TRUE){
          p<-subset(wet1959,Id==j)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[1]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx<-c(xxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[2]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxxx<-c(xxxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[3]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx4<-c(xxx4,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[4]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx5<-c(xxx5,xx)
          
          vec<- unique(c(unlist(vec),p@data$Id))
        }
      }
      xxx<-c(unique(unlist(xxx)))
      xxxx<-c(unique(unlist(xxxx)))
      xxx4<-c(unique(unlist(xxx4)))
      xxx5<-c(unique(unlist(xxx5)))
      if(length(vec)==0){vec<-NA}
      wet1970@data[i,"Tcode"]<-paste0(paste0(xxx, sep = "_", collapse = ''),"ch_",paste0(xxxx, sep = "_", collapse = ''),"ch_",paste0(xxx4, sep = "_", collapse = ''),"ch_",paste0(xxx5, sep = "_", collapse = ''),"ch_",paste0(vec, sep = "_", collapse = ''),"ch_")
    }
    
    
    K<-gIntersects( wet1970, wet1978, byid = TRUE)
    rownames(K)<-c(1:length(wet1978))
    colnames(K)<-c(1:length(wet1970))
    vec<-list()
    vec2<-list()
    for(i in 1:length(wet1978)){
      vec<-list()
      xxx<-list()
      xxxx<-list()
      xxx4<-list()
      xxx5<-list()
      xxx6<-list()
      for (j in 1:length(wet1970)){
        if(K[i,j]==TRUE){
          p<-subset(wet1970,Id==j)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[1]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx<-c(xxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[2]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxxx<-c(xxxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[3]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx4<-c(xxx4,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[4]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx5<-c(xxx5,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[5]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx6<-c(xxx6,xx)
          
          vec<- unique(c(unlist(vec),p@data$Id))
        }
      }
      xxx<-c(unique(unlist(xxx)))
      xxxx<-c(unique(unlist(xxxx)))
      xxx4<-c(unique(unlist(xxx4)))
      xxx5<-c(unique(unlist(xxx5)))
      xxx6<-c(unique(unlist(xxx6)))
      if(length(vec)==0){vec<-NA}
      wet1978@data[i,"Tcode"]<-paste0(paste0(xxx, sep = "_", collapse = ''),"ch_",paste0(xxxx, sep = "_", collapse = ''),"ch_",paste0(xxx4, sep = "_", collapse = ''),"ch_",paste0(xxx5, sep = "_", collapse = ''),"ch_",paste0(xxx6, sep = "_", collapse = ''),"ch_",paste0(vec, sep = "_", collapse = ''),"ch_")
      
    }
    
    
    
    K<-gIntersects( wet1978, wet1992, byid = TRUE)
    rownames(K)<-c(1:length(wet1992))
    colnames(K)<-c(1:length(wet1978))
    vec<-list()
    vec2<-list()
    for(i in 1:length(wet1992)){
      vec<-list()
      xxx<-list()
      xxxx<-list()
      xxx4<-list()
      xxx5<-list()
      xxx6<-list()
      xxx7<-list()
      
      
      for (j in 1:length(wet1978)){
        if(K[i,j]==TRUE){
          p<-subset(wet1978,Id==j)
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[1]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx<-c(xxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[2]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxxx<-c(xxxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[3]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx4<-c(xxx4,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[4]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx5<-c(xxx5,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[5]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx6<-c(xxx6,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[6]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx7<-c(xxx7,xx)
          
          vec<- unique(c(unlist(vec),p@data$Id))
        }
      }
      xxx<-c(unique(unlist(xxx)))
      xxxx<-c(unique(unlist(xxxx)))
      xxx4<-c(unique(unlist(xxx4)))
      xxx5<-c(unique(unlist(xxx5)))
      xxx6<-c(unique(unlist(xxx6)))
      xxx7<-c(unique(unlist(xxx7)))
      
      if(length(vec)==0){vec<-NA}
      wet1992@data[i,"Tcode"]<-paste0(paste0(xxx, sep = "_", collapse = ''),"ch_",paste0(xxxx, sep = "_", collapse = ''),"ch_",paste0(xxx4, sep = "_", collapse = ''),"ch_",paste0(xxx5, sep = "_", collapse = ''),"ch_",paste0(xxx6, sep = "_", collapse = ''),"ch_",paste0(xxx7, sep = "_", collapse = ''),"ch_",paste0(vec, sep = "_", collapse = ''),"ch_")
      
    }
    
    
    
    
    
    
    K<-gIntersects( wet1992, wet2010, byid = TRUE)
    rownames(K)<-c(1:length(wet2010))
    colnames(K)<-c(1:length(wet1992))
    vec<-list()
    vec2<-list()
    for(i in 1:length(wet2010)){
      vec<-list()
      xxx<-list()
      xxxx<-list()
      xxx4<-list()
      xxx5<-list()
      xxx6<-list()
      xxx7<-list()
      xxx8<-list()
      
      for (j in 1:length(wet1992)){
        if(K[i,j]==TRUE){
          
          p<-subset(wet1992,Id==j)
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[1]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx<-c(xxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[2]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxxx<-c(xxxx,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[3]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx4<-c(xxx4,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[4]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx5<-c(xxx5,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[5]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx6<-c(xxx6,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[6]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx7<-c(xxx7,xx)
          
          xx<-unlist(strsplit(p@data$Tcode,"_ch_"))[7]
          xx<-unlist(strsplit(xx,"_"))
          xx<-sapply(1:length(xx), function(i) {if(!is.na(as.numeric(xx[i]))) {xx[i]} } )
          xx<-unlist(xx[!vapply(xx, is.null, logical(1))])
          xxx8<-c(xxx8,xx)
          
          vec<- unique(c(unlist(vec),p@data$Id))
        }
      }
      xxxx<-c(unique(unlist(xxxx)))
      xxx4<-c(unique(unlist(xxx4)))
      xxx5<-c(unique(unlist(xxx5)))
      xxx6<-c(unique(unlist(xxx6)))
      xxx7<-c(unique(unlist(xxx7)))
      xxx8<-c(unique(unlist(xxx8)))
      
      if(length(vec)==0){vec<-NA}
      wet2010@data[i,"Tcode"]<-paste0(paste0(xxx, sep = "_", collapse = ''),"ch_",paste0(xxxx, sep = "_", collapse = ''),"ch_",paste0(xxx4, sep = "_", collapse = ''),"ch_",paste0(xxx5, sep = "_", collapse = ''),"ch_",paste0(xxx6, sep = "_", collapse = ''),"ch_",paste0(xxx7, sep = "_", collapse = ''),"ch_",paste0(xxx8, sep = "_", collapse = ''),"ch_",paste0(vec, sep = "_", collapse = ''),"ch_")
    }
    
    
    wet2010@data$cat1992<-list(c(NA))
    wet2010@data$cat1978<-list(c(NA))
    wet2010@data$cat1970<-list(c(NA))
    wet2010@data$cat1959<-list(c(NA))
    wet2010@data$cat1949<-list(c(NA))
    wet2010@data$cat1933<-list(c(NA))
    wet2010@data$cat1918<-list(c(NA))
    wet2010@data$cat1899<-list(c(NA))
    
    
    c<-0
    ##Identify patches of similar ancestory
    for(i in 1:length(wet2010@data$Id)){
      z<-subset(wet2010,Id==i)
      anc<-strsplit(z@data$Tcode,split="_ch_")
      anc<-unlist(anc)
      print(anc)
      #anc<-sort(unique(anc[!anc %in% c("NA")]))
      #wet2010@data$yrsplit[i]<-150-length(anc[[1]])*50
      #wet2010@data$numsplit[i]<-length(anc[[1]])
      
      if(length(anc)>8){c=c+1}
      
      
      wet2010@data$cat1992[i]<-strsplit(anc[8],"_")
      
      wet2010@data$cat1978[i]<-strsplit(anc[7],"_")
      
      wet2010@data$cat1970[i]<-strsplit(anc[6],"_")
      
      wet2010@data$cat1959[i]<-strsplit(anc[5],"_")
      
      wet2010@data$cat1949[i]<-strsplit(anc[4],"_")
      
      wet2010@data$cat1933[i]<-strsplit(anc[3],"_")
      
      wet2010@data$cat1918[i]<-strsplit(anc[2],"_")
      
      wet2010@data$cat1899[i]<-strsplit(anc[1],"_")
      
      
    }
     
    
    
  ## Calculate distances for all networks  
    
    mm<-1
    for (z in ch){
      
      gc()
      unlink("C:/Users/nharisena/Documents/research/Working/temp/*", recursive = T, force = T)
      
      proj_name <- paste0("Network", yr[mm])
      
      y<-graphab_to_igraph(
        proj_name,
        linkset="Complt_euclid",
        nodes = "patches",
        weight = "euclid",
        proj_path = NULL,
        fig = FALSE,
        crds = FALSE
      )
      
      #E(y)$weight<-scales::rescale(E(y)$weight,to = c(1, 7))
      assign(paste0(proj_name,"_graph"),y)
      mm<-mm+1
    }
    
    
    ##### Get stepping-stone distances
    
    save2010<-distances(
      graph=Network2010_graph,
      v = V(Network2010_graph),
      to = V(Network2010_graph),
      mode = c("all"),
      weights = NULL)
    
    g  <- graph.adjacency(save2010,weighted=TRUE,mode="undirected")
    df <- get.data.frame(g)
    Network2010_rsc<-df[is.finite(df$weight),]
    
    save1992<-distances(
      graph=Network1992_graph,
      v = V(Network1992_graph),
      to = V(Network1992_graph),
      mode = c("all"),
      weights = NULL)
    
    g  <- graph.adjacency(save1992,weighted=TRUE)
    df <- get.data.frame(g)
    Network1992_rsc<-df[is.finite(df$weight),]
    
    save1978<-distances(
      graph=Network1978_graph,
      v = V(Network1978_graph),
      to = V(Network1978_graph),
      mode = c("all"),
      weights = NULL)
    
    g  <- graph.adjacency(save1978,weighted=TRUE)
    df <- get.data.frame(g)
    Network1978_rsc<-df[is.finite(df$weight),]
    
    save1970<-distances(
      graph=Network1970_graph,
      v = V(Network1970_graph),
      to = V(Network1970_graph),
      mode = c("all"),
      weights = NULL)
    
    g  <- graph.adjacency(save1970,weighted=TRUE)
    df <- get.data.frame(g)
    Network1970_rsc<-df[is.finite(df$weight),]
    
    
    save1959<-distances(
      graph=Network1959_graph,
      v = V(Network1959_graph),
      to = V(Network1959_graph),
      mode = c("all"),
      weights = NULL)
    
    g  <- graph.adjacency(save1959,weighted=TRUE)
    df <- get.data.frame(g)
    Network1959_rsc<-df[is.finite(df$weight),]
    
    
    
    save1949<-distances(
      graph=Network1949_graph,
      v = V(Network1949_graph),
      to = V(Network1949_graph),
      mode = c("all"),
      weights = NULL)
    
    g  <- graph.adjacency(save1949,weighted=TRUE)
    df <- get.data.frame(g)
    Network1949_rsc<-df[is.finite(df$weight),]
    
    
    save1933<-distances(
      graph=Network1933_graph,
      v = V(Network1933_graph),
      to = V(Network1933_graph),
      mode = c("all"),
      weights = NULL)
    
    g  <- graph.adjacency(save1933,weighted=TRUE)
    df <- get.data.frame(g)
    Network1933_rsc<-df[is.finite(df$weight),]
    
    
    
    save1918<-distances(
      graph=Network1918_graph,
      v = V(Network1918_graph),
      to = V(Network1918_graph),
      mode = c("all"),
      weights = NULL)
    
    g  <- graph.adjacency(save1918,weighted=TRUE)
    df <- get.data.frame(g)
    Network1918_rsc<-df[is.finite(df$weight),]
    
    
    
    save1899<-distances(
      graph=Network1899_graph,
      v = V(Network1899_graph),
      to = V(Network1899_graph),
      mode = c("all"),
      weights = NULL)
    
    g  <- graph.adjacency(save1899,weighted=TRUE)
    df <- get.data.frame(g)
    Network1899_rsc<-df[is.finite(df$weight),]
    
    rm(df)
    rm(g)
    
    ch1899link<- Network1899_rsc
    ch1918link<- Network1918_rsc
    ch1933link<- Network1933_rsc
    ch1949link<- Network1949_rsc
    ch1959link<- Network1959_rsc
    ch1970link<- Network1970_rsc
    ch1978link<- Network1978_rsc
    ch1992link<- Network1992_rsc
    ch2010link<- Network2010_rsc
    
    ch1899link$rsc_dist<- Network1899_rsc$weight
    ch1918link$rsc_dist<- Network1918_rsc$weight
    ch1933link$rsc_dist<- Network1933_rsc$weight
    ch1949link$rsc_dist<- Network1949_rsc$weight
    ch1959link$rsc_dist<- Network1959_rsc$weight
    ch1970link$rsc_dist<- Network1970_rsc$weight
    ch1978link$rsc_dist<- Network1978_rsc$weight
    ch1992link$rsc_dist<- Network1992_rsc$weight
    ch2010link$rsc_dist<- Network2010_rsc$weight
    
    colnames(ch1899link)<-colnames(ch1918link)<-colnames(ch1933link)<-colnames(ch1949link)<-colnames(ch1959link)<-colnames(ch1970link)<-colnames(ch1978link)<-colnames(ch1992link)<-colnames(ch2010link)<-c("ID1","ID2","Dist","rsc_dist")
    
    
    #Get cluster ids also
    
    vor_shp<-rgdal::readOGR(paste0("Network1899/Gmxcst10000-voronoi.shp"))
    raster::crs(vor_shp)<-raster::crs(wet2010)
    patchshp1<-over(wet2010,vor_shp,returnList = FALSE)
    wet2010@data$cluster1899<-patchshp1$Id
    
    vor_shp<-rgdal::readOGR(paste0("Network1918/Gmxcst10000-voronoi.shp"))
    patchshp1<-over(wet2010,vor_shp,returnList = FALSE)
    wet2010@data$cluster1918<-patchshp1$Id
    
    vor_shp<-rgdal::readOGR(paste0("Network1933/Gmxcst10000-voronoi.shp"))
    patchshp1<-over(wet2010,vor_shp,returnList = FALSE)
    wet2010@data$cluster1933<-patchshp1$Id
    
    vor_shp<-rgdal::readOGR(paste0("Network1949/Gmxcst10000-voronoi.shp"))
    patchshp1<-over(wet2010,vor_shp,returnList = FALSE)
    wet2010@data$cluster1949<-patchshp1$Id
    
    vor_shp<-rgdal::readOGR(paste0("Network1959/Gmxcst10000-voronoi.shp"))
    patchshp1<-over(wet2010,vor_shp,returnList = FALSE)
    wet2010@data$cluster1959<-patchshp1$Id
    
    vor_shp<-rgdal::readOGR(paste0("Network1970/Gmxcst10000-voronoi.shp"))
    patchshp1<-over(wet2010,vor_shp,returnList = FALSE)
    wet2010@data$cluster1970<-patchshp1$Id
    
    vor_shp<-rgdal::readOGR(paste0("Network1978/Gmxcst10000-voronoi.shp"))
    patchshp1<-over(wet2010,vor_shp,returnList = FALSE)
    wet2010@data$cluster1978<-patchshp1$Id
    
    vor_shp<-rgdal::readOGR(paste0("Network1992/Gmxcst10000-voronoi.shp"))
    patchshp1<-over(wet2010,vor_shp,returnList = FALSE)
    wet2010@data$cluster1992<-patchshp1$Id
    
    vor_shp<-rgdal::readOGR(paste0("Network2010/Gmxcst10000-voronoi.shp"))
    patchshp1<-over(wet2010,vor_shp,returnList = FALSE)
    wet2010@data$cluster2010<-patchshp1$Id
    
    ch2010vorcomp<-wet2010@data[complete.cases(wet2010@data),]
    ch1992vorcomp<-wet1992@data[complete.cases(wet1992@data),]
    ch1978vorcomp<-wet1978@data[complete.cases(wet1978@data),]
    ch1970vorcomp<-wet1970@data[complete.cases(wet1970@data),]
    ch1959vorcomp<-wet1959@data[complete.cases(wet1959@data),]
    ch1949vorcomp<-wet1949@data[complete.cases(wet1949@data),]
    ch1933vorcomp<-wet1933@data[complete.cases(wet1933@data),]
    ch1918vorcomp<-wet1918@data[complete.cases(wet1918@data),]
    ch1899vorcomp<-wet1899@data[complete.cases(wet1899@data),]
    
    
    #basepath<-"C:/Users/nharisena/Documents/research/Working/temporal_networks/Cycle_2/odonate/disp2000"
    #setwd(basepath)
    
    save.image("Basedata_oct25th")
    
  })

  