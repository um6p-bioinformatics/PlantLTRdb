# Libraries
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
require(gridExtra)
require(scales)
library(grid)
library(ggpubr)
library(tidyverse)
library(ggjoy)
library(scales)

rm(list = ls())
source("ztheme.R")
source("plot_functions.r")

All_data<-read.csv("LENGTH-Good.csv",sep="\t")
colnames(All_data)<-c("Species","Chr","Type","Start","End","Length")
#colnames(All_data)<-c("Chr","Start","End","Type","MG","Species","TimeType","TimeRate")

#select time data by two

#When Work on time
#All_data<-All_data[All_data$TimeType=="TimeK",]
#All_data<-All_data[All_data$TimeRate==1.5,]

All.sp<-unique(All_data$Species)

#Species 
#All.sp.sub<-All.sp[1:2]   # select some species or ALL for test
All.sp.sub<-All.sp  # select some species or ALL for test

LTR_var<-"Length"
#LTR_var<-"MG"
#LTR_var.name<-"insertion age"
LTR_var.name<-"Length"
plotdata<-All_data

plot.var.boxplot<-function(plotdata,Type,mycolor,var.to.plot,Position)
{
  pp<-plotdata[plotdata$Type==Type,]
  bx<-ggplot(pp,aes_string("Species",var.to.plot))+
    geom_boxplot(aes(fill=Type), fill=mycolor,width=0.8, position = position_dodge(width=0.5))+ #geom_violin
    scale_x_discrete(limits = Position)+
    guides(fill="none",color="none")+
    labs(title="",x="",y="")+
    coord_flip()+
    z_theme()+#theme(axis.text.y = element_text(size=6))
    theme(axis.text.y=element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1,size = 20,color = paste("dark",mycolor,sep = "")))
}

plot.var.chart<-function(plotdata,Lmax,step,var.to.plot,Position)
{
  figure_A<-ggplot(plotdata,aes_string(y="Species",x=var.to.plot))+
    geom_joy(scale=4, aes(fill=Species), alpha=3/4)+
    scale_x_continuous(breaks=seq(0,Lmax,step))+
    scale_y_discrete(limits = Position)+
    guides(fill="none",color="none")+
    labs(title="",
         y="Species",
         x=var.to.plot)+
    z_theme()+
    theme(axis.title.x  =element_blank(),axis.text.y = element_text(size=20,color = "black"),axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1,size = 20,color = "black"))
}

#Calculate Stats

##Species STATs
#Species STATs
mat = matrix(ncol = 0, nrow = 5)
Species.stats<-as.data.frame(mat)
rownames(Species.stats)<-c("Min","First Quartile","Median","Third Quartile","Maximum")
for (variable in All.sp.sub) {
  bstat<-plot_LTR.boxplot.stat(plotdata,variable,LTR_var)
  bstat<-as.data.frame(bstat)
  colnames(bstat)<-c(variable)
  Species.stats<-cbind(Species.stats,bstat)
  dev.off()
}


#All Species
bstat<-plot_LTR.boxplot.stat(plotdata,"Species",LTR_var)
bstat<-as.data.frame(bstat)
colnames(bstat)<-c("ALL_Species")
Species.stats2<-cbind(Species.stats,as.data.frame(bstat) ) #in order to not add it to all
write.table(Species.stats2,paste(LTR_var,"-stats.tsv",sep=""),sep="\t")


###LTR Types stats
LTR.names<-unique(plotdata$Type)
mat = matrix(ncol = 0, nrow = 5)
LTR.stats<-as.data.frame(mat)
rn<-0
for (sp in All.sp.sub) {
  for (ltr in LTR.names) {
    select_species<-plotdata[plotdata$Species==sp,]
    select_ltr<-select_species[select_species$Type==ltr,]
    if (nrow(select_ltr)==0) {
      next
    }
    bstat<-plot_LTR.boxplot.stat(select_ltr,"Species",LTR_var)
    bstat<-as.data.frame(bstat)
    colnames(bstat)<-paste("LTR_STATS")
    bstat$Species<-sp
    bstat$LTR_Types<-ltr
    bstat$STAT_Cat<-c("Min","First Quartile","Median","Third Quartile","Maximum")
    rownames(bstat)<-seq(rn,rn+4)
    LTR.stats<-rbind(LTR.stats,bstat)
    rn<-rn+5
    dev.off()
  }
}
write.table(LTR.stats,paste(LTR_var,"-All-stats.tsv",sep=""),sep="\t")


#Ordering Species According to Mean

##Ordering Median Table
MedianTable<-Species.stats["Median",]
MedianTable<-t(MedianTable)
MedianTable<-as.data.frame(MedianTable)
MedianTable$Species<-rownames(MedianTable)

#order Median Table according to Median
Species.order<-order(MedianTable$Median)
MedianTable<-MedianTable[Species.order,]
#ordering vector in file
Position <- MedianTable$Species

textoffcet<-1
figure_G<-ggplot(MedianTable)+
  geom_bar(aes(x = Species, y = Median,fill=Species), stat = 'identity',width = 0.8,  position = position_dodge(0))+
  scale_x_discrete(limits = Position)+
  geom_text(aes(x = Species, y = Median+textoffcet,label=round(Median,digits = 3)), vjust=0.3, size=3.5)+
  guides(fill="none",color="none")+
  labs(title="",x="",y="")+
  coord_flip()+
  z_theme()+#theme(axis.text.y = element_text(size=6))
  theme(axis.text.y=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1,size = 10))

# Joyplot for probly
Lmax<-max(plotdata[[LTR_var]])
Lmin<-min(plotdata[[LTR_var]])

####plotdata[plotdata$Type=="Copia",]
Types<-unique(plotdata$Type)
#"Gypsy"   "unknown" "Copia" 

stepUnit<-Lmax/20
stepUnit<-stepUnit + (1000-stepUnit %% 1000)

figure_A<-plot.var.chart(plotdata,Lmax,stepUnit,LTR_var,Position)
figure_B<-plot.var.boxplot(plotdata,"Gypsy","red",LTR_var,Position)
figure_C<-plot.var.boxplot(plotdata,"Copia","blue",LTR_var,Position)
#figure_D<-plot.var.boxplot(plotdata,"unknown","green",LTR_var,Position)


#First Plot
ggarrange(figure_A, figure_B,figure_C,heights=c(1,1,1),widths = c(15,8,8), align="v",
                          labels=c(paste("LTR",LTR_var.name,"density plot (A)"), "Gypsy (B)","Copia (C)"),ncol = 3)
ggsave(paste(LTR_var.name,"_1.pdf",sep=""), height=40, width=30, dpi=300) #cairo-png #, type="pdf"


ggarrange(figure_A, figure_G,heights=c(10, 10),widths = c(5,3), align="v",
          labels=c(paste("LTR",LTR_var.name,"density plot (A)"), "Length (G)"),ncol = 2)
ggsave(paste(LTR_var.name,"_2.pdf",sep=""), height=40, width=30, dpi=300) #cairo-png #, type="pdf"

