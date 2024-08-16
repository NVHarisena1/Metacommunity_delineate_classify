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
library(ggpubr)
library(wesanderson)
library(betapart)
library(adespatial)
#OPTIONAL
library(qgraph)
library(terra)

#load("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle4/networks/disp500/STNtables/Steppingstones/fullresult.Rdata")

l<-1000
#Load base patches
ch2010patch1base<-rgdal::readOGR("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/base_patch/patches.shp")

#Load species data
amph_pts<-rgdal::readOGR(paste0("C:/Users/nharisena/Documents/research/Working/temporal_networks/odonate/odonate_zurich.shp")) #snap2_500m #snap3_50m

##Add overlapping patch code to species data points
crs(amph_pts)<-crs(ch2010patch1base)
k<-as.data.frame(over(amph_pts,ch2010patch1base,returnList = FALSE))
amph_pts@data$patchID<-k$Id
Vlidate_amph<-data.frame(matrix(nrow=length(ch2010patch1base$Id),ncol=length(unique(amph_pts@data$SPECIES))))
colnames(Vlidate_amph)<-c(unique(amph_pts@data$SPECIES))
Vlidate_amph[,c(1:length(unique(amph_pts@data$SPECIES)))]<-NA
amph_pts_df<-as.data.frame(amph_pts@data)
amph_pts_df<-amph_pts_df[complete.cases(amph_pts_df),]
#for(i in 1:length(amph_pts_df$patchID)){
for(i in c(unique(amph_pts_df$patchID))){
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

wts<-expand.grid(x1=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),x2=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
wts$sum<-wts$x1 + wts$x2
wts<-wts[wts$sum==1,]

assign(paste0("Current",l),dist2010)

XX<-get(paste0("Current",l))

g_TND_current <- graph.adjacency(XX,weighted=TRUE,mode="undirected")
df_TND_current <- get.data.frame(g_TND_current)
df_TND_current$code<-paste0(df_TND_current$from,"_",df_TND_current$to)

gg<-gCentroid(ch2010patch1base,byid=TRUE)
rownames(gg@coords)<-ch2010patch1base$Id
g_gg<-dist(gg@coords)
g_eucliddist<-graph.adjacency(as.matrix(g_gg),weighted=TRUE,mode="undirected")
g_eucliddist <- get.data.frame(g_eucliddist)
g_eucliddist$code<-paste0(g_eucliddist$from,"_",g_eucliddist$to)
g_eucliddist1<-g_eucliddist[g_eucliddist$weight<=15000,]

##Load spatio-temporal matrices

kappadatfull<-data.frame()
corfull_nounconnect<-data.frame()
boxplotdat<-data.frame()

for (kkk in c("1899","1918","1933","1949","1959","1970","1978","1992","2010")){#"1850","1900","1950",
 # for (lll in c("2010")){#"1850","1900","1950",

disp<-c(1000)
l<-1000

basepath<-paste0("C:/Users/nharisena/Documents/research/Working/25thnovupdate/cycle5_resub/Networks",l)
setwd(basepath)
TNDclustdist_sptemp1<-read.csv(paste0("Space_STN_t",kkk,".csv"))
TNDclustdist_sptemp1<-TNDclustdist_sptemp1[,(2:length(TNDclustdist_sptemp1))]
TNDclustdist_sptemp1<-as.matrix(TNDclustdist_sptemp1)
diag(TNDclustdist_sptemp1)<-0
TNDclustdist_sptemp1[!is.finite(TNDclustdist_sptemp1)]<-NA
rownames(TNDclustdist_sptemp1)<-colnames(TNDclustdist_sptemp1)<-c(1:length(ch2010patch1base@data$Id))
diag(TNDclustdist_sptemp1)<-0
assign(paste0("TNDspace_noscale",l),TNDclustdist_sptemp1)

g_TND_space <- graph.adjacency(get(paste0("TNDspace_noscale",l)),weighted=TRUE,mode="undirected")

assign(paste0("TNDspace_noscale_rsc",l),get(paste0("TNDspace_noscale",l)))

g_jdist <- graph.adjacency(as.matrix(jdist2),weighted=TRUE,mode="undirected")
df_jdist <- get.data.frame(g_jdist)
df_jdist$code<-paste0(df_jdist$from,"_",df_jdist$to)

tempspace<-get(paste0("TNDspace_noscale_rsc",l))

tempspace[is.na(tempspace)]<-0


tempadj<-tempspace
class(tempadj)<-"numeric"
tempadj[tempadj==0]<-NA
diag(tempadj)<-0
assign(paste0("TNDST500_adj",kkk),tempadj)

g1<-graph.adjacency(tempadj,weighted=TRUE,mode="undirected")
df_TND_st <- get.data.frame(g1)
df_TND_st$code<-paste0(df_TND_st$from,"_",df_TND_st$to)

#plot(density(df_TND_st[!is.na(df_TND_st$weight),"weight"]))

assign(paste0("TNDST500_df",kkk),df_TND_st)

}

aovdat<-list() #anova calculation
datafull<-data.frame()
for(kkk in c("1899","1918","1933","1949","1959","1970","1978","1992","2010")){
  df<-get (paste0("TNDST500_df",kkk))
  plot(density(df$weight,na.rm=TRUE))
  dflow<-df[!is.na(df$weight),]#df[df$weight<=zz[3],] ##median(df$weight,na.rm=TRUE)
  dflow<-dflow[complete.cases(dflow),]
  
  dfNA<-df[df$code %in% c(g_eucliddist1$code),]
  dfNA<-dfNA[is.na(dfNA$weight),]
  
  data1<-cbind(df_jdist[df_jdist$code%in% dflow$code,"weight"],rep("Low",length(df_jdist[df_jdist$code%in% dflow$code,"weight"])),rep(paste0("Network",kkk),length(df_jdist[df_jdist$code%in% dflow$code,"weight"])))
  data4<-cbind(df_jdist[df_jdist$code%in% dfNA$code,"weight"],rep("High",length(df_jdist[df_jdist$code%in% dfNA$code,"weight"])),rep(paste0("Network",kkk),length(df_jdist[df_jdist$code%in% dfNA$code,"weight"])))
  datafull<-rbind(datafull,data1,data4)

  data<-as.data.frame(rbind(data1[,c(1,2)],data4[,c(1,2)]))
  dat<-summary(aov(data$V1~data$V2, data=data))[[1]][["Pr(>F)"]][1]
  aovdat<-c(aovdat,dat)
}


##add current network
df<-df_TND_current
dflow<-df[!is.na(df$weight),]#df[df$weight<=zz[3],]
dflow<-dflow[complete.cases(dflow),]
dfNA<-df[df$code %in% c(g_eucliddist1$code),]
dfNA<-dfNA[is.na(dfNA$weight),]

data1<-cbind(df_jdist[df_jdist$code%in% dflow$code,"weight"],rep("Low",length(df_jdist[df_jdist$code%in% dflow$code,"weight"])),rep("currentnet",length(df_jdist[df_jdist$code%in% dflow$code,"weight"])))
data4<-cbind(df_jdist[df_jdist$code%in% dfNA$code,"weight"],rep("High",length(df_jdist[df_jdist$code%in% dfNA$code,"weight"])),rep("currentnet",length(df_jdist[df_jdist$code%in% dfNA$code,"weight"])))
datafull<-rbind(datafull,data1,data4)


##add ws
df<-TNDST500_dfwc
dflow<-df[!is.na(df$weight),]#df[df$weight<=zz[3],]
dflow<-dflow[complete.cases(dflow),]
dfNA<-df[df$code %in% c(g_eucliddist1$code),]
dfNA<-dfNA[is.na(dfNA$weight),]

data1<-cbind(df_jdist[df_jdist$code%in% dflow$code,"weight"],rep("Low",length(df_jdist[df_jdist$code%in% dflow$code,"weight"])),rep("WS",length(df_jdist[df_jdist$code%in% dflow$code,"weight"])))
data4<-cbind(df_jdist[df_jdist$code%in% dfNA$code,"weight"],rep("High",length(df_jdist[df_jdist$code%in% dfNA$code,"weight"])),rep("WS",length(df_jdist[df_jdist$code%in% dfNA$code,"weight"])))
datafull<-rbind(datafull,data1,data4)

data<-as.data.frame(rbind(data1[,c(1,2)],data4[,c(1,2)]))
dat<-summary(aov(data$V1~data$V2, data=data))[[1]][["Pr(>F)"]][1]
aovdat<-c(aovdat,dat)


aovfull<-rbind(aovfull,aovdat)

colnames(aovfull)<-c(sapply(1:length(wts$x1), FUN=stnetnam),"currentnet")

stnetnam<-function(i){
  tt<-wts[i,]
  nam<-paste0("TNDST500_df",tt$x1,tt$x2)
  return(nam)
}

datawithcurrord<-datafull
datawithcurrord$V2<-as.factor(datawithcurrord$V2)
datawithcurrord$V2 <- factor(datawithcurrord$V2, levels = c("Low","High"))#,"Unconnected"))

g1<-ggplot(as.data.frame(datawithcurrord),aes(x=V3,y=as.numeric(V1),fill=V2))+geom_boxplot(aes(alpha=0.1),position=position_dodge(0.9))+ geom_violin(aes(alpha=0.1),position=position_dodge(0.9))+
 scale_fill_manual(values=c("darkgrey","lightblue"),labels=c("Low network distance","High network distance"))+ theme_bw() +theme(axis.text.x = element_text(angle = 45, vjust = 0.1, hjust=1))+ ylab("Beta diversity")+ theme(axis.title.x = element_blank(),axis.text.x=element_blank())+ theme(legend.position="bottom") # +scale_x_discrete(limits=paste0("Network",year))

 


##Kappa test


datawithcurr1<-datafull
kappavalsfull<-data.frame()

confusion_matfull<-data.frame()
cnfmat_details<-data.frame()

for(kkk in c("1899","1918","1933","1949","1959","1970","1978","1992","2010")){

cohentest<-cbind(datawithcurr1[datawithcurr1$V3 %in% paste0("Network",kkk),c("V1","V2")])
                 

colnames(cohentest)<- c("BD",paste0("Network",kkk))

cohentest$Truedist<-NA
cohentest[cohentest$BD<=median(jdist2),"Truedist"]<-"Low"
cohentest[cohentest$BD>median(jdist2),"Truedist"]<-"High"


t<-cohen.kappa(cohentest[,2:length(cohentest)])
low<-t$confid[,1]["weighted kappa"]
mean<-t$confid[,2]["weighted kappa"]
high<-t$confid[,3]["weighted kappa"]


confusion_mat = as.matrix(table(STN_Values = cohentest[,2], betadiv_Values = cohentest[,3])) 
print(confusion_mat)
confx<-cbind(confusion_mat,rep(paste0("Network",kkk),length(confusion_mat[,1])))
confusion_matfull<-rbind(confusion_matfull,confx)
if(length(confusion_mat[,1])>=2){
  acc<-(confusion_mat[1,1]+confusion_mat[2,2])/sum(confusion_mat)
  prec_high<-confusion_mat[1,1]/sum(confusion_mat[1,])
  prec_low<-confusion_mat[2,2]/sum(confusion_mat[2,])
  sens_high<-confusion_mat[1,1]/sum(confusion_mat[,1])
  sens_low<-confusion_mat[2,2]/sum(confusion_mat[,2])
} else {
  acc<- prec_high<-  prec_low<-  sens_high<- sens_low<-NA
}

cnfmat_details<-rbind(cnfmat_details,cbind(acc,1-acc,prec_high,prec_low,sens_high,sens_low,paste0("Network",kkk)))


kappavals<-c(paste0("Network",kkk),low,mean,high,"Group")
kappavalsfull<-rbind(kappavalsfull,kappavals)

}

cohentest<-cbind(datawithcurr1[datawithcurr1$V3 %in% "currentnet",c("V1","V2")])
colnames(cohentest)<- c("BD","currentnet")
cohentest$Truedist<-NA
cohentest[cohentest$BD<=median(jdist2),"Truedist"]<-"Low"
cohentest[cohentest$BD>median(jdist2) ,"Truedist"]<-"High"
t<-cohen.kappa(cohentest[,2:length(cohentest)])
low<-t$confid[,1]["weighted kappa"]
mean<-t$confid[,2]["weighted kappa"]
high<-t$confid[,3]["weighted kappa"]


confusion_mat = as.matrix(table(STN_Values = cohentest[,2], betadiv_Values = cohentest[,3])) 
confx<-cbind(confusion_mat,rep("currentnet",length(confusion_mat[,1])))
confusion_matfull<-rbind(confusion_matfull,confx)
if(length(confusion_mat[,1])>=2){
  acc<-(confusion_mat[1,1]+confusion_mat[2,2])/sum(confusion_mat)
  prec_high<-confusion_mat[1,1]/sum(confusion_mat[1,])
  prec_low<-confusion_mat[2,2]/sum(confusion_mat[2,])
  sens_high<-confusion_mat[1,1]/sum(confusion_mat[,1])
  sens_low<-confusion_mat[2,2]/sum(confusion_mat[,2])
} else {
  acc<- prec_high<-  prec_low<-  sens_high<- sens_low<-NA
}

cnfmat_details<-rbind(cnfmat_details,cbind(acc,1-acc,prec_high,prec_low,sens_high,sens_low,"currentnet"))


kappavals<-c("currentnet",low,mean,high,"Group")
kappavalsfull<-rbind(kappavalsfull,kappavals)


cohentest<-cbind(datawithcurr1[datawithcurr1$V3 %in% "WS",c("V1","V2")])
colnames(cohentest)<- c("BD","WS")
cohentest$Truedist<-NA
cohentest[cohentest$BD<=median(jdist2),"Truedist"]<-"Low"
cohentest[cohentest$BD>median(jdist2) ,"Truedist"]<-"High"
t<-cohen.kappa(cohentest[,2:length(cohentest)])
low<-t$confid[,1]["weighted kappa"]
mean<-t$confid[,2]["weighted kappa"]
high<-t$confid[,3]["weighted kappa"]


confusion_mat = as.matrix(table(STN_Values = cohentest[,2], betadiv_Values = cohentest[,3])) 
#print(confusion_mat)
confx<-cbind(confusion_mat,rep("WS",length(confusion_mat[,1])))
confusion_matfull<-rbind(confusion_matfull,confx)
if(length(confusion_mat[,1])>=2){
  acc<-(confusion_mat[1,1]+confusion_mat[2,2])/sum(confusion_mat)
  prec_high<-confusion_mat[1,1]/sum(confusion_mat[1,])
  prec_low<-confusion_mat[2,2]/sum(confusion_mat[2,])
  sens_high<-confusion_mat[1,1]/sum(confusion_mat[,1])
  sens_low<-confusion_mat[2,2]/sum(confusion_mat[,2])
} else {
  acc<- prec_high<-  prec_low<-  sens_high<- sens_low<-NA
}

cnfmat_details<-rbind(cnfmat_details,cbind(acc,1-acc,prec_high,prec_low,sens_high,sens_low,"WS"))
colnames(cnfmat_details)<-c("Acc","Misclass","Prec_high","Prec_low","Sens_high","Sens_low","STNnet")


kappavals<-c("WS",low,mean,high,"Group")
kappavalsfull<-rbind(kappavalsfull,kappavals)


colnames(kappavalsfull)<-c("STNnet","Lower","Estimate","High","Group")


cnfmelt<-rbind(cbind(cnfmat_details$Prec_high,"Precision to 'High beta diversity"),
               cbind(cnfmat_details$Prec_low,"Precision to Low beta diversity"),
               cbind(cnfmat_details$Acc,"Accuracy"),
               cbind(kappavalsfull$Estimate,"Cohens Kappa"))

cnfmelt<-as.data.frame(cnfmelt)
cnfmelt$STN<-rep(c(cnfmat_details$STNnet),4)
cnfmelt$lower<-c(cnfmat_details$Prec_high,cnfmat_details$Prec_low,cnfmat_details$Acc,kappavalsfull$Lower)
cnfmelt$upper<-c(cnfmat_details$Prec_high,cnfmat_details$Prec_low,cnfmat_details$Acc,kappavalsfull$High)
cnfmelt1<-cnfmelt[!cnfmelt$STN %in% "currentnet",]

g2<-ggplot(as.data.frame(cnfmelt1),aes(x=STN,y=as.numeric(V1),group=V2,colour=V2))+geom_errorbar(aes(ymin=as.numeric(lower), ymax=as.numeric(upper)), width=0.5,
                                                                                                 position=position_dodge(0.05),colour="black")+geom_point(size=2)+geom_line(size=0.2)+ 
  theme_bw() +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ylim(c(-0.1,1)) +
  scale_color_brewer(palette="Set1")+ ylab("Metric estimates")+ theme(legend.position="bottom")#scale_x_discrete(limits=paste0("Network",year)) #scale_colour_grey()#scale_colour_manual(values=wes_palette(n=4, name="Grandbudapest"))  +geom_ribbon(aes(ymin=as.numeric(lower), ymax=as.numeric(upper)),alpha=0.3,fill='grey')

out<-ggarrange(g1,g2, 
          ncol = 1, nrow = 2,  align = "v",
          widths = c(1, 1),heights = c(0.6, 1),
          common.legend = FALSE)#+scale_x_discrete(limits=c("currentnet",sapply(1:length(wts$x1), FUN=stnetnam )))+ theme(axis.text.x = element_text(angle = 45))

print(out)
