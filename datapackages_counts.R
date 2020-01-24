#gather data and load packages
library(nhanesA)
library(plyr)
library(rFSA)
library(sjPlot)
library(sjmisc)

#BMI

BMI<-rbind.fill(nhanes(nh_table = "BMX_D"),
                nhanes(nh_table = "BMX_C"),nhanes(nh_table = "BMX_B"))
#nutrients
Vit1<-nhanes(nh_table = "L06VIT_B")
Vit2<-nhanes(nh_table = "L45VIT_C") #Only year LBXLUT included
colnames(Vit2)[2]<-"LBXVIE"
Vit3<-nhanes(nh_table = "VITAEC_D")
Vit<-rbind.fill(Vit1,Vit2,Vit3)

#demographics
demo<-rbind.fill(nhanes(nh_table = "DEMO_D"),
                 nhanes(nh_table = "DEMO_C"),nhanes(nh_table = "DEMO_B"))

#The diabetes section (variable name prefix DIQ) provides personal interview data on diabetes, prediabetes, use of insulin 
#or oral hypoglycemic medications, and diabetic retinopathy. It also provides self-reported information on awareness of risk 
#factors for diabetes, general knowledge of diabetic complications, and medical or personal cares associated with diabetes.
DIQ<-rbind.fill(nhanes(nh_table = "DIQ_B"),nhanes(nh_table = "DIQ_C"),
                nhanes(nh_table = "DIQ_D"))

#health survey
HSQ<-rbind.fill(nhanes(nh_table = "HSQ_B"),nhanes(nh_table = "HSQ_C"),
                nhanes(nh_table = "HSQ_D"))




m1<-merge(Vit,demo,by="SEQN",all = TRUE)
m2<-merge(m1,DIQ,by="SEQN",all = TRUE)
m3<-merge(m2,HSQ,by="SEQN",all = TRUE)
m3<-merge(m3,BMI,by="SEQN",all=TRUE)

m3$diab<-rep(NA,length(m3$DIQ010))
m3$diab[which(m3$DIQ010==1)]<-"yes"
m3$diab[which(m3$DIQ010==2)]<-"no"
m3$diab<-factor(m3$diab)

labs<-unlist(lapply(m3, function(x) attributes(x)$label))
labs[grep(pattern = '.x',x = tolower(labs))]

m3$flu<-rep(NA,length(m3$HSQ520))
m3$flu[which(m3$HSQ520==1)]<-"yes"
m3$flu[which(m3$HSQ520==2)]<-"no"
m3$flu<-factor(m3$flu)
m3$flu<-relevel(m3$flu,ref="no")

nutr<-unique(c(grep(pattern = "lbx",x = tolower(colnames(m3))),grep(pattern = "lbd",x = tolower(colnames(m3)))))
adjust4<-c('RIDAGEYR','RIAGENDR','RIDRETH1','DMDEDUC','INDFMPIR')

m3$gender<-rep(NA,length(m3$HSQ520))
m3$gender[which(m3$RIAGENDR==1)]<-"male"
m3$gender[which(m3$RIAGENDR==2)]<-"female"
m3$gender<-factor(m3$gender)
m3$gender<-relevel(m3$gender,ref="male")

m3$edu<-rep(NA,length(m3$HSQ520))
m3$edu[which(m3$DMDEDUC==1|m3$DMDEDUC2==1|m3$DMDEDUC2==2)]<-"LTHS"
m3$edu[which(m3$DMDEDUC==2|m3$DMDEDUC2==3)]<-"HS"
m3$edu[which(m3$DMDEDUC==3|m3$DMDEDUC2==4|m3$DMDEDUC2==5)]<-"GTHS"
m3$edu<-factor(m3$edu)
m3$edu<-relevel(m3$edu,ref="GTHS")

m3$race<-rep(NA,length(m3$HSQ520))
m3$race[which(m3$RIDRETH1==1|m3$RIDRETH1==2)]<-"MA_OH"
m3$race[which(m3$RIDRETH1==3)]<-"White"
m3$race[which(m3$RIDRETH1==4)]<-"Black"
m3$race[which(m3$RIDRETH1==5)]<-"Other"
m3$race<-factor(m3$race)
m3$race<-relevel(m3$race,ref="White")

m3$ses<-factor(ifelse(m3$INDFMPIR>1,yes = "Above1",no = "Below1"))
m3$ses<-relevel(m3$ses,ref="Above1")

#####chest cold
m3$cc<-rep(NA,length(m3$HSQ500))
m3$cc[which(m3$HSQ500==1)]<-"yes"
m3$cc[which(m3$HSQ500==2)]<-"no"
m3$cc<-factor(m3$cc)
m3$cc<-relevel(m3$cc,ref="no")

m3$mh<-m3$HSQ480
m3$mh[which(m3$mh==99)]<-NA
# m3$mh[which(m3$mh==0)]<-0
# m3$mh[which(m3$mh>0)]<-1


m3$ph<-m3$HSQ470
m3$ph[which(m3$ph==99)]<-NA
# m3$ph[which(m3$ph==0)]<-0
# m3$ph[which(m3$ph>0)]<-1
