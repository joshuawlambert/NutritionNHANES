#Gather NHANES

library(nhanesA)
library(plyr)
DIQ<-rbind.fill(nhanes(nh_table = "DIQ"),
                nhanes(nh_table = "DIQ_B"),nhanes(nh_table = "DIQ_C"),
                nhanes(nh_table = "DIQ_D"),nhanes(nh_table = "DIQ_E"),
                nhanes(nh_table = "DIQ_F"),nhanes(nh_table = "DIQ_G"),
                nhanes(nh_table = "DIQ_H"),nhanes(nh_table = "DIQ_I"))
                
ALQ<-rbind.fill(nhanes(nh_table = "ALQ"),
                nhanes(nh_table = "ALQ_B"),nhanes(nh_table = "ALQ_C"),
                nhanes(nh_table = "ALQ_D"),nhanes(nh_table = "ALQ_E"),
                nhanes(nh_table = "ALQ_F"),nhanes(nh_table = "ALQ_G"),
                nhanes(nh_table = "ALQ_H"),nhanes(nh_table = "ALQ_I"))

DEMO<-rbind.fill(nhanes(nh_table = "DEMO"),
                 nhanes(nh_table = "DEMO_B"),nhanes(nh_table = "DEMO_C"),
                 nhanes(nh_table = "DEMO_D"),nhanes(nh_table = "DEMO_E"),
                 nhanes(nh_table = "DEMO_F"),nhanes(nh_table = "DEMO_G"),
                 nhanes(nh_table = "DEMO_H"),nhanes(nh_table = "DEMO_I"))

L06VIT<-rbind.fill(nhanes(nh_table = "L06VIT"),
                nhanes(nh_table = "L06VIT_B"),nhanes(nh_table = "L06VIT_C"),
                nhanes(nh_table = "L06VIT_D"),nhanes(nh_table = "L06VIT_E"),
                nhanes(nh_table = "L06VIT_F"),nhanes(nh_table = "L06VIT_G"),
                nhanes(nh_table = "L06VIT_H"),nhanes(nh_table = "L06VIT_I"))

VITAEC<-rbind.fill(nhanes(nh_table = "VITAEC"),
                   nhanes(nh_table = "VITAEC_B"),nhanes(nh_table = "VITAEC_C"),
                   nhanes(nh_table = "VITAEC_D"),nhanes(nh_table = "VITAEC_E"),
                   nhanes(nh_table = "VITAEC_F"),nhanes(nh_table = "VITAEC_G"),
                   nhanes(nh_table = "VITAEC_H"),nhanes(nh_table = "VITAEC_I"))

B12<-rbind.fill(nhanes(nh_table = "B12"),
                   nhanes(nh_table = "B12_B"),nhanes(nh_table = "B12_C"),
                   nhanes(nh_table = "B12_D"),nhanes(nh_table = "B12_E"),
                   nhanes(nh_table = "B12_F"),nhanes(nh_table = "B12_G"),
                   nhanes(nh_table = "B12_H"),nhanes(nh_table = "B12_I"))

B6<-rbind.fill(nhanes(nh_table = "B6"),
                nhanes(nh_table = "B6_B"),nhanes(nh_table = "B6_C"),
                nhanes(nh_table = "B6_D"),nhanes(nh_table = "B6_E"),
                nhanes(nh_table = "B6_F"),nhanes(nh_table = "B6_G"),
                nhanes(nh_table = "B6_H"),nhanes(nh_table = "B6_I"))

VIC<-rbind.fill(nhanes(nh_table = "VIC"),
                nhanes(nh_table = "VIC_B"),nhanes(nh_table = "VIC_C"),
                nhanes(nh_table = "VIC_D"),nhanes(nh_table = "VIC_E"),
                nhanes(nh_table = "VIC_F"),nhanes(nh_table = "VIC_G"),
                nhanes(nh_table = "VIC_H"),nhanes(nh_table = "VIC_I"))

VID<-rbind.fill(nhanes(nh_table = "VID"),
                nhanes(nh_table = "VID_B"),nhanes(nh_table = "VID_C"),
                nhanes(nh_table = "VID_D"),nhanes(nh_table = "VID_E"),
                nhanes(nh_table = "VID_F"),nhanes(nh_table = "VID_G"),
                nhanes(nh_table = "VID_H"),nhanes(nh_table = "VID_I"))

DSQTOT<-rbind.fill(nhanes(nh_table = "DSQTOT"),
                   nhanes(nh_table = "DSQTOT_B"),nhanes(nh_table = "DSQTOT_C"),
                   nhanes(nh_table = "DSQTOT_D"),nhanes(nh_table = "DSQTOT_E"),
                   nhanes(nh_table = "DSQTOT_F"),nhanes(nh_table = "DSQTOT_G"),
                   nhanes(nh_table = "DSQTOT_H"),nhanes(nh_table = "DSQTOT_I"))

m1<-merge(DEMO,DIQ, by="SEQN",all = TRUE)
m2<-merge(m1,ALQ, by="SEQN",all = TRUE)
m3<-merge(m2,L06VIT, by="SEQN",all = TRUE)
m4<-merge(m3,VITAEC, by="SEQN",all = TRUE)
m5<-merge(m4,B12, by="SEQN",all = TRUE)
#m6<-merge(m5,B6, by="SEQN")
m7<-merge(m5,VIC, by="SEQN",all = TRUE)
m8<-merge(m7,VID, by="SEQN",all = TRUE)
m9<-merge(m8,DSQTOT, by="SEQN",all = TRUE)


sum.na<-function(var1,var2){
  tmp<-NULL
  for(i in 1:length(var1)){
    if(is.na(var1[i]) & is.na(var2[i])){tmp<-c(tmp,NA)
    } else tmp<-c(tmp,ifelse(is.na(var1[i]),yes = 0,no = var1[i])+ifelse(is.na(var2[i]),yes = 0,no = var2[i]))
  }
  return(tmp)
}

labs<-unlist(lapply(m9, function(x) attributes(x)$label))
labs[grep(pattern = '.x',x = tolower(labs))]
  
