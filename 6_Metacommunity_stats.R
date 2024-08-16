library(lwgeom)
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
library(Hmisc)
library(reshape2)
library(rlist)
library(dplyr)
library(ggplot2)
library(reshape2)
library(betapart)

disp<-c(500)
l<-500

basepath<-paste0("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/Networks",l,"_2")
setwd(basepath)
ch2010patch1base<-rgdal::readOGR("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/base_patch/patches.shp")
ch2010patch<-ch2010patch1base


#Load species data
amph_pts<-rgdal::readOGR(paste0("C:/Users/nharisena/Documents/research/Working/temporal_networks/odonate/originaldata.shp")) #orig_onlymoor #snap30_onlymoor.shp #snap2_500m #snap3_50m

##Add overlapping patch code to species data points
crs(amph_pts)<-crs(ch2010patch1base)
k<-as.data.frame(over(amph_pts,ch2010patch1base,returnList = FALSE))
amph_pts@data$patchID<-k$Id
Vlidate_amph<-data.frame(matrix(nrow=length(ch2010patch1base$Id),ncol=length(unique(amph_pts@data$SPECIES))))
colnames(Vlidate_amph)<-c(unique(amph_pts@data$SPECIES))
Vlidate_amph[,c(1:length(unique(amph_pts@data$SPECIES)))]<-NA
amph_pts_df<-as.data.frame(amph_pts@data)
amph_pts_df<-amph_pts_df[complete.cases(amph_pts_df),]
for(i in 1:length(amph_pts_df$patchID)){
  subset<-amph_pts_df[amph_pts_df$patchID==i,c("SPECIES","patchID")]
  for(j in unique(subset$SPECIES) ){
    Vlidate_amph[i,j]<-1
  }
}
#Calculate species richness
Vlidate_amph<-subset(Vlidate_amph, rowSums(is.na(Vlidate_amph[,c(1:ncol(Vlidate_amph))]))!= ncol(Vlidate_amph[,c(1:ncol(Vlidate_amph))]))
Vlidate_amph[is.na(Vlidate_amph)]<-0
Vlidate_amph$SR<-rowSums(Vlidate_amph)
#Calculate beta diversity
jdist<-vegdist(Vlidate_amph[,c(1:length(Vlidate_amph)-1)],method="jaccard",na.rm = TRUE)
jdist2<-as.matrix(jdist)
plot(density(jdist2))


XX<-get(paste0("Current",l))
diag(XX)<-0
maxs <- max(unlist(XX),na.rm=TRUE)
mins <- rep(0,ncol(XX))
CurrentRsc<-scale(XX, center = mins, scale = maxs - mins)

cordat<-data.frame()

wts<-expand.grid(x1=c(0,0.3,0.5,0.7,1),x2=c(0,0.3,0.5,0.7,1))
wts$sum<-wts$x1 + wts$x2
wts<-wts[wts$sum==1,]

#for(i in 1:length(wts$x1)){
tt<-wts[1,]
nam<-paste0("TNDST500_adj",tt$x1,tt$x2)
STN<-get(nam)

year<-c("1899","1918","1933","1949","1959","1970","1978","1992","2010")# 
mspastar<-list()

for(yr in year ){
vor_shp<-rgdal::readOGR(paste0("Network",yr,"/Gmxcst10000-voronoi.shp"))
vor_ptch<-rgdal::readOGR(paste0("Network",yr,"/patches.shp"))
vor_shp$area<-area(vor_shp)
vor_shp$perim<-st_perimeter(st_as_sf(vor_shp))
vor_shp$shpfact<-(vor_shp$perim*vor_shp$perim)/(4*3.14*vor_shp$area)
patchshp1<-over(ch2010patch,vor_shp,returnList = FALSE)
ch2010patch@data$cluster<-patchshp1$Id

ch2010vorcomp<-ch2010patch

patchshp1<-over(vor_ptch,vor_shp,returnList = FALSE)
vor_ptch@data$cluster<-patchshp1$Id

vor_shp@data$parea<-NA
for(i in c(unique(vor_ptch@data$cluster))){
  ttmp<-vor_ptch[vor_ptch@data$cluster==i,]
  vor_shp[vor_shp@data$Id==i, "parea"]<-sum(ttmp@data$Area)
}

mspastar<-rbind(mspastar,cbind(mean(vor_shp@data$parea,na.rm=TRUE),paste0("MC",yr)))


vor_shp@data$Gamm<-NA
vor_shp@data$Beta<-NA
vor_shp@data$P_Area<-NA
vor_shp@data$P_Patches<-NA
vor_shp@data$cnet_diam<-NA
vor_shp@data$stn_diam<-NA
vor_shp@data$Avgdeg_cnet<-NA
vor_shp@data$Avgdeg_stn<-NA
vor_shp@data$MX_BCTN<-NA
vor_shp@data$MX_BSTN<-NA
vor_shp@data$Avg_BCTN<-NA
vor_shp@data$Avg_BSTN<-NA


for(i in c(unique(ch2010vorcomp$cluster))){
  #print(i)
  
  if(length(ch2010vorcomp[ch2010vorcomp$cluster==i,"Id"])>0){
    
    ttmp<-ch2010vorcomp[ch2010vorcomp$cluster==i,]
    
    xx<-subset(ch2010patch1base, Id %in% c(ttmp$Id) )
    
    jdistdiv<-Vlidate_amph[row.names(Vlidate_amph) %in% c(ttmp$Id),c(1:(ncol(Vlidate_amph)-1))]
    x<-melt(jdistdiv)[melt(jdistdiv)$value==1,]
    gamadiv<-length(c(unique(as.character(x$variable))))
    vor_shp@data[vor_shp$Id==i,"Gamm"]<-gamadiv
    
    jdisttt<-jdist2
    jdisttt[lower.tri(jdisttt)]<-0
    jdistdiv<-jdisttt[row.names(jdisttt) %in% c(ttmp$Id),row.names(jdisttt) %in% c(ttmp$Id)]
    vor_shp@data[vor_shp$Id==i,"Beta"]<-mean(jdistdiv[jdistdiv!=0])
    
    vor_shp@data[vor_shp$Id==i,"P_Area"]<-sum(xx@data$Area)
    
    vor_shp@data[vor_shp$Id==i,"P_Patches"]<-length(xx@data$Area)
    
    if(length(ttmp$Id)>1){
      
      NET_2010<-CurrentRsc[rownames(CurrentRsc) %in% c(ttmp$Id), colnames(CurrentRsc) %in% c(ttmp$Id) ]
      NET_2010[NET_2010==0]<-NA
      #vor_shp@data[vor_shp$Id==i,"cnet_diam"]<-max(NET_2010,na.rm=TRUE)
      #plot(density(NET_2010,na.rm=TRUE))
      vor_shp@data[vor_shp$Id==i,"cnet_diam"]<-mean(NET_2010,na.rm=TRUE)
      
      NET_STN<-STN[rownames(STN) %in% c(ttmp$Id), colnames(STN) %in% c(ttmp$Id) ]
      NET_STN[NET_STN==0]<-NA
      #vor_shp@data[vor_shp$Id==i,"stn_diam"]<-max(NET_STN,na.rm=TRUE)
      vor_shp@data[vor_shp$Id==i,"stn_diam"]<-mean(NET_STN,na.rm=TRUE)
      #plot(density(NET_STN,na.rm=TRUE))
      
    }
    
  }
  
}

assign(paste0("MC",yr),vor_shp)

}

##Kmeans clustering for MC
yr<-1899

dic2cols<-cbind(get(paste0("MC",yr))@data$parea,get(paste0("MC",yr))@data$Id,get(paste0("MC",yr))@data$Beta,get(paste0("MC",yr))@data$Gamm,get(paste0("MC",yr))@data$Betamulti)
#dic2cols<-scale(dic2cols)
dic2cols<-dic2cols[complete.cases(dic2cols),]
dic2cols<-as.data.frame(dic2cols)
colnames(dic2cols)<-c("Area","Id","Beta","Gamma","betam")

scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

dic2cols$Beta<-scale_values(dic2cols$Beta)
#dic2cols$betam<-scale_values(dic2cols$betam)

dic2cols$Gamma<-scale_values(c(dic2cols$Gamma))
colnames(dic2cols)<-c("Area","Id","Beta","Gamma","betam")
#dic2cols2<-dic2cols[,c("betam","Gamma")]
dic2cols2<-dic2cols[,c("Beta","Gamma")]

#set.seed(00008)
km.out <- kmeans(dic2cols2, centers = 3)

dic2cols$cluster_id <- factor(km.out$cluster)
ggplot(dic2cols, aes(Beta, Gamma, color = cluster_id)) +
  geom_point() +
  xlab("Beta") +
  ylab("Gamma")+theme_bw()+scale_color_brewer(palette="Dark2")

MC_2<-MC[MC@data$Id %in% dic2cols$Id,]

MC_2@data$kmclust<-dic2cols$cluster_id
MC_2<-vect(MC_2)

library(terra)
writeVector(MC_2,filename= "MC_kmeans21899_zurichtest11.shp", filetype="ESRI Shapefile")
