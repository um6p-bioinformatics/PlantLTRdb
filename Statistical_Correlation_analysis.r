#!/usr/bin/env Rscript
library("ggpubr")
library("ggExtra")

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop()
}

merge.all <- function(x, ..., by = "row.names") {
  L <- list(...)
  for (i in seq_along(L)) {
    x <- merge(x, L[[i]], by = by)
    rownames(x) <- x$Row.names
    x$Row.names <- NULL
  }
  return(x)
}

LTRLen<-"Length-stats.tsv"
MYScore<-"MG-stats.tsv"
GenomeInfo<-"Genome_INFO.csv"

LTRs<-"LENGTH.tsv"

LTRLen.table=read.table(LTRLen,header = T,row.names = 1,sep="\t")
LTRs.table=read.table(LTRs,sep="\t")
MYScore.table=read.table(MYScore,header = T,row.names = 1,sep="\t")
GenomeInfo.table=read.table(GenomeInfo,row.names = 1,sep="\t")

LTRs.count<-as.data.frame(table(LTRs.table$V1))
colnames(LTRs.count)<-c("species","ltrcount")
rownames(LTRs.count)<-LTRs.count$species

#species is the first column
colnames(GenomeInfo.table)<-c("family","taxid","assemblylevel","coverage","acc","genomesize","chrn")

DATA<-merge.all(LTRLen.table,MYScore.table,GenomeInfo.table,LTRs.count)
DATA[is.na(DATA)]<-0 
colnames(DATA)

DATA$Species <- rownames(DATA)
DATA$Species.abb<-abbreviate(DATA$Species)
DATA$Species
#LTR.genome.ratio
#Total.No..of.LTR.RT
#Total.length..Mb.
#Length.Median
#Gypsy.inside.Gene
#Copia.inside.Gene
#Unknown.LTR.Gene
#genes.in.1kb.area
#Genes.in.10kb.area
#Gypsy.inside.Pseudogene
#Copia.inside.Pseudogene
#Unknown.LTR.Pseudogene
#TimeK.Min
#Genome.size
#TimeK.Maximum
#TimeK.Median
colnames(DATA)
sp<-ggscatter(DATA, x = "genomesize", y = "Length.Median", 
          add = "reg.line", conf.int = TRUE, color = "genomesize",size = "genomesize",
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Genome size (Mb)", ylab = "LTR Length (bp)",label = "Species.abb")
sp+ stat_cor( show.legend = FALSE) +
  scale_size(range = c(0.5, 5)) + gradient_color(c("blue", "green", "red"))+ labs(colour = "Genome Size",size= "Genome Size")+
  theme(legend.text  = element_text(angle = 90,hjust = 0.8))+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))
  
#ggMarginal(sp, type = "density")
# Change marginal plot type
#ggMarginal(sp, type = "boxplot")

ggsave("Correlation_GenomeSize_LTRLength.pdf", height=10, width=10, dpi=300) #cairo-png #, type="pdf"

X<-"genomesize"
Y<-"MG.Median"
X.name<-sub("[.]"," ",x = X)
X.unit<-"(Mbp)"
Y.name<-sub("[.]"," ",x = Y)
Y.unit<-"(Generation)"

sp<-ggscatter(DATA, x = X, y = Y, 
              add = "reg.line", conf.int = TRUE, color = X,size = X,
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Genome size (Mb)", ylab = paste("Insertion time",Y.unit),label = "Species.abb")
sp+ stat_cor( show.legend = FALSE) +
  scale_size(range = c(0.5, 5)) + gradient_color(c("blue", "green", "red"))+ labs(colour = "Genome Size",size= "Genome Size")+
  theme(legend.text  = element_text(angle = 90,hjust = 0.8))+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

ggsave(paste("Correlation",X.name,Y.name,"LTRLength.pdf",sep="_"), height=10, width=10, dpi=300) #cairo-png #, type="pdf"

X<-"genomesize"
Y<-"ltrcount"
X.name<-sub("[.]"," ",x = X)
X.unit<-"(Mbp)"
Y.name<-sub("[.]"," ",x = Y)
Y.unit<-""

sp<-ggscatter(DATA, x = X, y = Y, 
              add = "reg.line", conf.int = TRUE, color = X,size = X,
              cor.coef = TRUE, cor.method = "pearson",
              xlab = paste("Genome size (Mbp)"), ylab = paste("Total LTRs"),label = "Species.abb")
sp+ stat_cor( show.legend = FALSE) +
  scale_size(range = c(0.5, 5)) + gradient_color(c("blue", "green", "red"))+ labs(colour = "Genome Size",size= "Genome Size")+
  theme(legend.text  = element_text(angle = 90,hjust = 0.8))+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

ggsave(paste("Correlation",X.name,Y.name,".pdf",sep="_"), height=10, width=10, dpi=300) #cairo-png #, type="pdf"
