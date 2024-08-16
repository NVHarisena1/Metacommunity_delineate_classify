  
  corslt<-data.frame(matrix(NA, nrow=12, ncol=5))
  colnames(corslt)<-c("Beta_stat","Beta_pval","Gamma_stat","Gamma_pval","Network")
  corslt$Metric<-c(rep("Patch_area",2),rep("Patch_number",2),rep("Network_diameter",2))
  corslt$Network<-rep(c("1899","2010"),6)
  
  #install.packages("ggpubr")
  library(ggpubr)
  library("gridExtra")
  
  checkdatstn<-MC1899@data[complete.cases(MC1899@data$Beta),]
  
  checkdat<-MC2010@data[complete.cases(MC2010@data$Beta),]
  
  scatdat<-cbind(checkdat$Beta,checkdat$cnet_diam,"spatial_net")
  scatdat<-rbind(scatdat,cbind(checkdatstn$Beta,checkdatstn$stn_diam,"STN"))
  #scatdat<-rbind(scatdat,cbind(checkdatws$Beta,checkdatws$cnet_diam,"WS"))
  
  scatdat<-as.data.frame(scatdat)
  colnames(scatdat)<-c("Beta","Mean_shortest_distance","Group")
  scatdat$Beta<-as.numeric(scatdat$Beta)
  scatdat$Mean_shortest_distance<-log(as.numeric(scatdat$Mean_shortest_distance))
  
  #corslt<-rbind(corslt,c(scatdat))
  
  # Scatter plot colored by groups ("Species")
  formula <- y ~ poly(x, 2, raw = TRUE)
  
  sp1 <- ggscatter(scatdat, x = "Beta", y = "Mean_shortest_distance",
                   color = "Group", palette = "jco",size = 3, alpha = 0.6, add = "reg.line",title=paste("Beta",nam))+ border()# +stat_regline_equation(
  # formula = formula)+ stat_smooth(aes(fill = Group, color = Group), method = "lm", formula = formula,se=FALSE) 
  
  cordat<-scatdat[scatdat$Group %in% "spatial_net",]
  ans<-cor.test(cordat$Beta,cordat$Mean_shortest_distance)
  
  corslt[corslt$Metric %in% c("Network_diameter") & corslt$Network %in% "2010","Beta_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Network_diameter") & corslt$Network %in% "2010","Beta_pval"]<-ans$p.value
  
  cordat<-scatdat[scatdat$Group %in% "STN",]
  ans<-cor.test(cordat$Beta,cordat$Mean_shortest_distance)
  
  corslt[corslt$Metric %in% c("Network_diameter") & corslt$Network %in% "1899","Beta_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Network_diameter") & corslt$Network %in% "1899","Beta_pval"]<-ans$p.value
  
  
  scatdat<-cbind(checkdat$Gamm,checkdat$cnet_diam,"spatial_net")
  scatdat<-rbind(scatdat,cbind(checkdatstn$Gamm,checkdatstn$stn_diam,"STN"))
  #scatdat<-rbind(scatdat,cbind(checkdatws$Gamm,checkdatws$cnet_diam,"WS"))
  
  scatdat<-as.data.frame(scatdat)
  colnames(scatdat)<-c("Gamma","Mean_shortest_distance","Group")
  scatdat$Gamma<-as.numeric(scatdat$Gamma)
  scatdat$Mean_shortest_distance<-log(as.numeric(scatdat$Mean_shortest_distance))
  
  paste(nam,"GAMMA","STN",Stnp,"SPNET",spnet)
  cordat<-rbind(cordat,paste(nam,"GAMMA","STN",Stnp,"SPNET",spnet))
  
  formula <- y ~ poly(x, 3, raw = TRUE)
  sp2 <- ggscatter(scatdat, x = "Gamma", y = "Mean_shortest_distance",
                   color = "Group", palette = "jco",size = 3, alpha = 0.6,title=paste("Gamma",nam),add = "reg.line")+ border() +scale_colour_manual(values=c("navyblue","darkgrey")) #+stat_reg_line(aes(fill = Group, color = Group), method = "lm", formula = formula,se=FALSE) 
  
  #+stat_regline_equation(formula = formula)+
  
  cordat<-scatdat[scatdat$Group %in% "spatial_net",]
  ans<-cor.test(cordat$Gamma,cordat$Mean_shortest_distance)
  
  corslt[corslt$Metric %in% c("Network_diameter") & corslt$Network %in% "2010","Gamma_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Network_diameter") & corslt$Network %in% "2010","Gamma_pval"]<-ans$p.value
  
  cordat<-scatdat[scatdat$Group %in% "STN",]
  ans<-cor.test(cordat$Gamma,cordat$Mean_shortest_distance)
  
  corslt[corslt$Metric %in% c("Network_diameter") & corslt$Network %in% "1899","Gamma_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Network_diameter") & corslt$Network %in% "1899","Gamma_pval"]<-ans$p.value
  
  
  
  print(ggarrange(sp1,sp2,
                  ncol = 2, nrow = 1,  align = "hv", 
                  widths = c(1, 1), heights = c(1, 2),
                  common.legend = TRUE))
  
  
  
  
  #install.packages("ggpubr")
  #library(ggpubr)
  #library("gridExtra")
  
  checkdatstn<-MC1899@data[complete.cases(MC1899@data$Beta),]
  #checkdatstn$parea<-as.numeric(rescale(checkdatstn$parea))
  checkdat<-MC2010@data[complete.cases(MC2010@data$Beta),]
  #checkdat$parea<-as.numeric(rescale(checkdat$parea))
  
  scatdat<-cbind(checkdat$Beta,checkdat$parea,"spatial_net")
  scatdat<-rbind(scatdat,cbind(checkdatstn$Beta,checkdatstn$parea,"STN"))
  #scatdat<-rbind(scatdat,cbind(checkdatws$Beta,checkdatws$cnet_diam,"WS"))
  
  scatdat<-as.data.frame(scatdat)
  colnames(scatdat)<-c("Beta","Patch_area","Group")
  scatdat$Beta<-as.numeric(scatdat$Beta)
  scatdat$Patch_area<-log(as.numeric(scatdat$Patch_area))
  
  # Scatter plot colored by groups ("Species")
  formula <- y ~ poly(x, 2, raw = TRUE)
  
  sp1 <- ggscatter(scatdat, x = "Beta", y = "Patch_area",
                   color = "Group", palette = "jco",size = 3, alpha = 0.6, add = "reg.line",title=paste("Beta",nam))+ border()# +stat_regline_equation(
  # formula = formula)+ stat_smooth(aes(fill = Group, color = Group), method = "lm", formula = formula,se=FALSE) 
  
  cordat<-scatdat[scatdat$Group %in% "spatial_net",]
  ans<-cor.test(cordat$Beta,log(cordat$Patch_area))
  
  corslt[corslt$Metric %in% c("Patch_area") & corslt$Network %in% "2010","Beta_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Patch_area") & corslt$Network %in% "2010","Beta_pval"]<-ans$p.value
  
  cordat<-scatdat[scatdat$Group %in% "STN",]
  ans<-cor.test(cordat$Beta,log(cordat$Patch_area))
  
  corslt[corslt$Metric %in% c("Patch_area") & corslt$Network %in% "1899","Beta_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Patch_area") & corslt$Network %in% "1899","Beta_pval"]<-ans$p.value
  
  
  scatdat<-cbind(checkdat$Gamm,checkdat$parea,"spatial_net")
  scatdat<-rbind(scatdat,cbind(checkdatstn$Gamm,checkdatstn$parea,"STN"))
  #scatdat<-rbind(scatdat,cbind(checkdatws$Gamm,checkdatws$cnet_diam,"WS"))
  
  scatdat<-as.data.frame(scatdat)
  colnames(scatdat)<-c("Gamma","Patch_area","Group")
  scatdat$Gamma<-as.numeric(scatdat$Gamma)
  scatdat$Patch_area<-log(as.numeric(scatdat$Patch_area))
  
  paste(nam,"GAMMA","STN",Stnp,"SPNET",spnet)
  cordat<-rbind(cordat,paste(nam,"GAMMA","STN",Stnp,"SPNET",spnet))
  
  formula <- y ~ poly(x, 3, raw = TRUE)
  sp2 <- ggscatter(scatdat, x = "Gamma", y = "Patch_area",
                   color = "Group",size = 3, alpha = 0.6,title=paste("Gamma",nam),add = "reg.line")+ border() +scale_colour_manual(values=c("navyblue","darkgrey"))#+stat_reg_line(aes(fill = Group, color = Group), method = "lm", formula = formula,se=FALSE) 
  
  #+stat_regline_equation(formula = formula)+
  
  
  cordat<-scatdat[scatdat$Group %in% "spatial_net",]
  ans<-cor.test(cordat$Gamma,cordat$Patch_area)
  
  corslt[corslt$Metric %in% c("Patch_area") & corslt$Network %in% "2010","Gamma_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Patch_area") & corslt$Network %in% "2010","Gamma_pval"]<-ans$p.value
  
  cordat<-scatdat[scatdat$Group %in% "STN",]
  ans<-cor.test(cordat$Gamma,cordat$Patch_area)
  
  corslt[corslt$Metric %in% c("Patch_area") & corslt$Network %in% "1899","Gamma_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Patch_area") & corslt$Network %in% "1899","Gamma_pval"]<-ans$p.value
  
  
  
  print(ggarrange(sp1,sp2,
                  ncol = 2, nrow = 1,  align = "hv", 
                  widths = c(1, 1), heights = c(1, 2),
                  common.legend = TRUE))
  
  
  
  checkdatstn<-MC1899@data[complete.cases(MC1899@data$Beta),]
 # checkdatstn$P_Patches<-as.numeric(rescale(checkdatstn$P_Patches))
  checkdat<-MC2010@data[complete.cases(MC2010@data$Beta),]
  #checkdat$P_Patches<-as.numeric(rescale(checkdat$P_Patches))
  
  scatdat<-cbind(checkdat$Beta,checkdat$P_Patches,"spatial_net")
  scatdat<-rbind(scatdat,cbind(checkdatstn$Beta,checkdatstn$P_Patches,"STN"))
  #scatdat<-rbind(scatdat,cbind(checkdatws$Beta,checkdatws$cnet_diam,"WS"))
  
  scatdat<-as.data.frame(scatdat)
  colnames(scatdat)<-c("Beta","P_Patches","Group")
  scatdat$Beta<-as.numeric(scatdat$Beta)
  scatdat$P_Patches<-log(as.numeric(scatdat$P_Patches))
  
  # Scatter plot colored by groups ("Species")
  formula <- y ~ poly(x, 2, raw = TRUE)
  
  sp1 <- ggscatter(scatdat, x = "Beta", y = "P_Patches",
                   color = "Group", palette = "jco",size = 3, alpha = 0.6, add = "reg.line",title=paste("Beta",nam))+ border()# +stat_regline_equation(
  # formula = formula)+ stat_smooth(aes(fill = Group, color = Group), method = "lm", formula = formula,se=FALSE) 
  
  
  cordat<-scatdat[scatdat$Group %in% "spatial_net",]
  ans<-cor.test(cordat$Beta,cordat$P_Patches)
  
  corslt[corslt$Metric %in% c("Patch_number") & corslt$Network %in% "2010","Beta_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Patch_number") & corslt$Network %in% "2010","Beta_pval"]<-ans$p.value
  
  cordat<-scatdat[scatdat$Group %in% "STN",]
  ans<-cor.test(cordat$Beta,cordat$P_Patches)
  
  corslt[corslt$Metric %in% c("Patch_number") & corslt$Network %in% "1899","Beta_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Patch_number") & corslt$Network %in% "1899","Beta_pval"]<-ans$p.value
  
  
  scatdat<-cbind(checkdat$Gamm,checkdat$P_Patches,"spatial_net")
  scatdat<-rbind(scatdat,cbind(checkdatstn$Gamm,checkdatstn$P_Patches,"STN"))
  #scatdat<-rbind(scatdat,cbind(checkdatws$Gamm,checkdatws$cnet_diam,"WS"))
  
  scatdat<-as.data.frame(scatdat)
  colnames(scatdat)<-c("Gamma","P_Patches","Group")
  scatdat$Gamma<-as.numeric(scatdat$Gamma)
  scatdat$P_Patches<-log(as.numeric(scatdat$P_Patches))
  
  paste(nam,"GAMMA","STN",Stnp,"SPNET",spnet)
  cordat<-rbind(cordat,paste(nam,"GAMMA","STN",Stnp,"SPNET",spnet))
  
  formula <- y ~ poly(x, 3, raw = TRUE)
  sp2 <- ggscatter(scatdat, x = "Gamma", y = "P_Patches",
                   color = "Group", palette = "jco",size = 3, alpha = 0.6,title=paste("Gamma",nam),add = "reg.line")+ border() +scale_colour_manual(values=c("navyblue","darkgrey")) #+stat_reg_line(aes(fill = Group, color = Group), method = "lm", formula = formula,se=FALSE) 
  
  
  
  cordat<-scatdat[scatdat$Group %in% "spatial_net",]
  ans<-cor.test(cordat$Gamma,cordat$P_Patches)
  
  corslt[corslt$Metric %in% c("Patch_number") & corslt$Network %in% "2010","Gamma_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Patch_number") & corslt$Network %in% "2010","Gamma_pval"]<-ans$p.value
  
  cordat<-scatdat[scatdat$Group %in% "STN",]
  ans<-cor.test(cordat$Gamma,cordat$P_Patches)
  
  corslt[corslt$Metric %in% c("Patch_number") & corslt$Network %in% "1899","Gamma_stat"]<-ans$statistic
  corslt[corslt$Metric %in% c("Patch_number") & corslt$Network %in% "1899","Gamma_pval"]<-ans$p.value
  
  
  #+stat_regline_equation(formula = formula)+
  
  

print(ggarrange(sp1,sp2,
                ncol = 2, nrow = 1,  align = "hv", 
                widths = c(1, 1), heights = c(1, 2),
                common.legend = TRUE))



write.csv(corslt,"Network_stats.csv")
