library(epiDisplay)
library(tidyverse)
library(plyr)
library(ggplot2)
library(readxl)
library(qqman)
library(patchwork)
library(grid)
library(gridExtra)
library(ggrepel)
library(openxlsx)


# load dataset
mydata <- readRDS("mydata.rds")
dim(mydata)
colnames(mydata)
head(mydata)

# standalization -- continuous variables
mydata[c(6,7,20,22,28:37)] <- lapply(mydata[c(6,7,20,22,28:37)], function(x) c(scale(x,center = T,scale = T)))
aa<-mydata

################################### 1. overall BC #########################################################################################################
# 1.1 overall BC unadjusted
Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0("outcome_status ~ ",x))
    glm1<-glm(FML,data=aa,family = binomial)
    glm2<-summary(glm1)
    OR<-exp(coef(glm1))
    SE<-glm2$coefficients[,2]
    CI5<-exp(coef(glm1)-1.96*SE)
    CI95<-exp(coef(glm1)+1.96*SE)
    P<-glm2$coefficients[,4]
    Uni_glm_model <- data.frame("variables"=x,"Pval" = P,"OR" = OR , "L95" = CI5, "U95" = CI95 )[-1,]
    return(Uni_glm_model)
  }  

variable.names<- colnames(aa)[c(2:37,39:42,45:47)]

Uni_glm <- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm
dim(Uni_glm) #57 5

variable.names
names<- glm(outcome_status~ave_total_household_income+ breastfed_as_baby+ maternal_smoking+family_history+ recruitment_age+tdi+own_or_rent+accommodation_type+number_in_household+
              employment+education+ethnic+num_moderate_activity+num_vigorous_activity+sleep_duration+insomnia+alcohol_drinker+smoking_status+waist_circumference+BMI+                      
              menarche_age+menopause_status+live_births_num+stillbirth_status+oc_status+HRT_status+ap_Nitrogen_oxides+ap_pm10+ap_pm2.5+
              np_ave_24.hour_sound_level+bio_triglycerides+bio_hba1c+bio_glucose+bio_HDL+bio_SHBG+bio_IGF1+Cardiovascular+Hypertension+
              Diabetes+Respiratory+PC1+PC2+PC3,
            data=aa,
            family = binomial)
name<-data.frame(summary(names)$aliased)
rownames(Uni_glm)<-rownames(name)[-1]
Uni_glm <- Uni_glm[,-1]

write.csv(Uni_glm,"overall_unadjusted.csv")

# 1.2 overall BC adjusted by age and BMI
Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0("outcome_status ~ recruitment_age + BMI + ",x))
    glm1<-glm(FML,data=aa,family = binomial)
    glm2<-summary(glm1)
    OR<-exp(coef(glm1))
    SE<-glm2$coefficients[,2]
    CI5<-exp(coef(glm1)-1.96*SE)
    CI95<-exp(coef(glm1)+1.96*SE)
    P<-glm2$coefficients[,4]
    Uni_glm_model <- data.frame("variables"=x,"Pval" = P,"OR" = OR , "L95" = CI5, "U95" = CI95 )[-c(1:5),]
    return(Uni_glm_model)
  }  

variable.names<- colnames(aa)[c(2:5,7:20,22:37,39:42,45:47)];variable.names 

Uni_glm <- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm
dim(Uni_glm) #53 5

variable.names
names<- glm(outcome_status~ave_total_household_income+ breastfed_as_baby+ maternal_smoking+family_history+ tdi+own_or_rent+accommodation_type+number_in_household+
              employment+education+ethnic+num_moderate_activity+num_vigorous_activity+sleep_duration+insomnia+alcohol_drinker+smoking_status+waist_circumference+                      
              menarche_age+menopause_status+live_births_num+stillbirth_status+oc_status+HRT_status+ap_Nitrogen_oxides+ap_pm10+ap_pm2.5+
              np_ave_24.hour_sound_level+bio_triglycerides+bio_hba1c+bio_glucose+bio_HDL+bio_SHBG+bio_IGF1+Cardiovascular+Hypertension+
              Diabetes+Respiratory+PC1+PC2+PC3,
            data=aa,
            family = binomial)
name<-data.frame(summary(names)$aliased)
rownames(Uni_glm)<-rownames(name)[-1]
Uni_glm <- Uni_glm[,-1];Uni_glm

write.csv(Uni_glm,"overall_adjusted_age_BMI.csv")


################################### 2. bc_inner  #########################################################################################################
# 2.1 bc_inner unadjusted
Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0("bc_inner ~ ",x))
    glm1<-glm(FML,data=aa,family = binomial)
    glm2<-summary(glm1)
    OR<-exp(coef(glm1))
    SE<-glm2$coefficients[,2]
    CI5<-exp(coef(glm1)-1.96*SE)
    CI95<-exp(coef(glm1)+1.96*SE)
    P<-glm2$coefficients[,4]
    Uni_glm_model <- data.frame("variables"=x,"Pval" = P,"OR" = OR , "L95" = CI5, "U95" = CI95 )[-1,]
    return(Uni_glm_model)
  }  

variable.names<- colnames(aa)[c(2:37,39:42,45:47)]

Uni_glm <- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm
dim(Uni_glm) #57 5

variable.names
names<- glm(bc_inner~ave_total_household_income+ breastfed_as_baby+ maternal_smoking+family_history+ recruitment_age+tdi+own_or_rent+accommodation_type+number_in_household+
              employment+education+ethnic+num_moderate_activity+num_vigorous_activity+sleep_duration+insomnia+alcohol_drinker+smoking_status+waist_circumference+BMI+                      
              menarche_age+menopause_status+live_births_num+stillbirth_status+oc_status+HRT_status+ap_Nitrogen_oxides+ap_pm10+ap_pm2.5+
              np_ave_24.hour_sound_level+bio_triglycerides+bio_hba1c+bio_glucose+bio_HDL+bio_SHBG+bio_IGF1+Cardiovascular+Hypertension+
              Diabetes+Respiratory+PC1+PC2+PC3,
            data=aa,
            family = binomial)
name<-data.frame(summary(names)$aliased)
rownames(Uni_glm)<-rownames(name)[-1]
Uni_glm <- Uni_glm[,-1]

write.csv(Uni_glm,"inner_unadjusted.csv")

# 2.2 bc_inner adjusted age and BMI
Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0("bc_inner ~ recruitment_age + BMI + ",x))
    glm1<-glm(FML,data=aa,family = binomial)
    glm2<-summary(glm1)
    OR<-exp(coef(glm1))
    SE<-glm2$coefficients[,2]
    CI5<-exp(coef(glm1)-1.96*SE)
    CI95<-exp(coef(glm1)+1.96*SE)
    P<-glm2$coefficients[,4]
    Uni_glm_model <- data.frame("variables"=x,"Pval" = P,"OR" = OR , "L95" = CI5, "U95" = CI95 )[-c(1:5),]
    return(Uni_glm_model)
  }  

variable.names<- colnames(aa)[c(2:5,7:20,22:37,39:42,45:47)];variable.names 

Uni_glm <- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm
dim(Uni_glm) #53 5

variable.names
names<- glm(bc_inner~ave_total_household_income+ breastfed_as_baby+ maternal_smoking+family_history+ tdi+own_or_rent+accommodation_type+number_in_household+
              employment+education+ethnic+num_moderate_activity+num_vigorous_activity+sleep_duration+insomnia+alcohol_drinker+smoking_status+waist_circumference+                      
              menarche_age+menopause_status+live_births_num+stillbirth_status+oc_status+HRT_status+ap_Nitrogen_oxides+ap_pm10+ap_pm2.5+
              np_ave_24.hour_sound_level+bio_triglycerides+bio_hba1c+bio_glucose+bio_HDL+bio_SHBG+bio_IGF1+Cardiovascular+Hypertension+
              Diabetes+Respiratory+PC1+PC2+PC3,
            data=aa,
            family = binomial)
name<-data.frame(summary(names)$aliased)
rownames(Uni_glm)<-rownames(name)[-1]
Uni_glm <- Uni_glm[,-1];Uni_glm

write.csv(Uni_glm,"inner_adjusted_age_BMI.csv")


################################### 3. bc_outer  #########################################################################################################
# 3.1 bc_outer unadjusted
Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0("bc_outer ~ ",x))
    glm1<-glm(FML,data=aa,family = binomial)
    glm2<-summary(glm1)
    OR<-exp(coef(glm1))
    SE<-glm2$coefficients[,2]
    CI5<-exp(coef(glm1)-1.96*SE)
    CI95<-exp(coef(glm1)+1.96*SE)
    P<-glm2$coefficients[,4]
    Uni_glm_model <- data.frame("variables"=x,"Pval" = P,"OR" = OR , "L95" = CI5, "U95" = CI95 )[-1,]
    return(Uni_glm_model)
  }  

variable.names<- colnames(aa)[c(2:37,39:42,45:47)]

Uni_glm <- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm
dim(Uni_glm) #57 5

variable.names
names<- glm(bc_outer~ave_total_household_income+ breastfed_as_baby+ maternal_smoking+family_history+ recruitment_age+tdi+own_or_rent+accommodation_type+number_in_household+
              employment+education+ethnic+num_moderate_activity+num_vigorous_activity+sleep_duration+insomnia+alcohol_drinker+smoking_status+waist_circumference+BMI+                      
              menarche_age+menopause_status+live_births_num+stillbirth_status+oc_status+HRT_status+ap_Nitrogen_oxides+ap_pm10+ap_pm2.5+
              np_ave_24.hour_sound_level+bio_triglycerides+bio_hba1c+bio_glucose+bio_HDL+bio_SHBG+bio_IGF1+Cardiovascular+Hypertension+
              Diabetes+Respiratory+PC1+PC2+PC3,
            data=aa,
            family = binomial)
name<-data.frame(summary(names)$aliased)
rownames(Uni_glm)<-rownames(name)[-1]
Uni_glm <- Uni_glm[,-1]

write.csv(Uni_glm,"outer_unadjusted.csv")

# 3.2 bc_outer adjusted age and BMI
Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0("bc_outer ~ recruitment_age + BMI + ",x))
    glm1<-glm(FML,data=aa,family = binomial)
    glm2<-summary(glm1)
    OR<-exp(coef(glm1))
    SE<-glm2$coefficients[,2]
    CI5<-exp(coef(glm1)-1.96*SE)
    CI95<-exp(coef(glm1)+1.96*SE)
    P<-glm2$coefficients[,4]
    Uni_glm_model <- data.frame("variables"=x,"Pval" = P,"OR" = OR , "L95" = CI5, "U95" = CI95 )[-c(1:5),]
    return(Uni_glm_model)
  }  

variable.names<- colnames(aa)[c(2:5,7:20,22:37,39:42,45:47)];variable.names 

Uni_glm <- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm
dim(Uni_glm) #53 5

variable.names
names<- glm(bc_outer~ave_total_household_income+ breastfed_as_baby+ maternal_smoking+family_history+ tdi+own_or_rent+accommodation_type+number_in_household+
              employment+education+ethnic+num_moderate_activity+num_vigorous_activity+sleep_duration+insomnia+alcohol_drinker+smoking_status+waist_circumference+                      
              menarche_age+menopause_status+live_births_num+stillbirth_status+oc_status+HRT_status+ap_Nitrogen_oxides+ap_pm10+ap_pm2.5+
              np_ave_24.hour_sound_level+bio_triglycerides+bio_hba1c+bio_glucose+bio_HDL+bio_SHBG+bio_IGF1+Cardiovascular+Hypertension+
              Diabetes+Respiratory+PC1+PC2+PC3,
            data=aa,
            family = binomial)
name<-data.frame(summary(names)$aliased)
rownames(Uni_glm)<-rownames(name)[-1]
Uni_glm <- Uni_glm[,-1];Uni_glm

write.csv(Uni_glm,"outer_adjusted_age_BMI.csv")


##############overall BC forest plot#############################################################################################################
overall_OR_forestplot <-  data.frame(read_excel("overall_OR_forestplot.xlsx"))

fp1 = ggplot(data=overall_OR_forestplot,
             aes(x = variables,y = OR, ymin = L95, ymax = U95, shape=model ))+
  ggtitle("A  Overall Breast Cancer")+
  geom_pointrange(aes(col=group))+
  geom_hline(aes(fill=variables),yintercept =1, linetype=2)+
  xlab('variables')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=L95, ymax=U95,col=group),width=0.2,cex=1)+ 
  facet_grid(cols = vars(group), rows = vars(model), scales = "free")+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  scale_y_log10(breaks=c(0.5,1,2))+
  geom_text_repel(aes(label=round(OR, 2)), size = 3)+
  ggsave("overall_BC_forest.png", width = 1280/72, height = 800/72, dpi = 300)


######inner BC forest plot#######################################################################################################################
inner_OR_forestplot <- read_excel("inner_OR_forestplot.xlsx")

fp2 = ggplot(data=inner_OR_forestplot,
             aes(x = variables,y = OR, ymin = L95, ymax = U95, shape=model ))+
  ggtitle("B  Inner Quadrant Breast Cancer")+
  geom_pointrange(aes(col=group))+
  geom_hline(aes(fill=variables),yintercept =1, linetype=2)+
  xlab('variables')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=L95, ymax=U95,col=group),width=0.2,cex=1)+ 
  facet_grid(cols = vars(group), rows = vars(model), scales = "free")+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  scale_y_log10(breaks=c(0.5,1,2))+
  geom_text_repel(aes(label=round(OR, 2)), size = 3)+
  ggsave("inner_BC_forest.png", width = 1280/72, height = 800/72, dpi = 300)


######outer BC forest plot###################################################################################################################
outer_OR_forestplot <- read_excel("outer_OR_forestplot.xlsx")
fp3 = ggplot(data=outer_OR_forestplot,
             aes(x = variables,y = OR, ymin = L95, ymax = U95, shape=model ))+
  ggtitle("C  Outer Quadrant Breast Cancer")+
  geom_pointrange(aes(col=group))+
  geom_hline(aes(fill=variables),yintercept =1, linetype=2)+
  xlab('variables')+ ylab("Odds Ratio (95% Confidence Interval)")+
  geom_errorbar(aes(ymin=L95, ymax=U95,col=group),width=0.2,cex=1)+ 
  facet_grid(cols = vars(group), rows = vars(model), scales = "free")+
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.x=element_text(angle = 90, hjust = 0.5, vjust = 0.5,face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  scale_y_log10(breaks=c(0.5,1,2))+
  geom_text_repel(aes(label=round(OR, 2)), size = 3)+
  ggsave("outer_BC_forest.png", width = 1280/72, height = 800/72, dpi = 300)


##############overall BC Manhattan plot#################################################

# calcualte bonferroni
bonf1 <- 0.05/length(unique(overall_pval_manhanttan$variables))
bonf_log1 <- -log10(bonf1)

#  import data and takes p values from results
overall_pval_manhanttan <- read.xlsx("overall_pval_manhanttan.xlsx")
View(overall_pval_manhanttan)
overall_pval_manhanttan$pval<-as.numeric(overall_pval_manhanttan$pval)
overall_pval_manhanttan$model<-as.factor(overall_pval_manhanttan$model)
# Manhattan plot 
p_overall_BC<-ggplot(overall_pval_manhanttan, aes(y = -log10(pval), x = variables)) +   
  geom_point(aes(color=model),size = 3,alpha = 1/1.5)+ 
  geom_hline(yintercept = bonf_log1, color="red", linetype="dashed")+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle = 75, hjust = 1))+
  xlab("Predictors")+
  ylab("-log10(P value)")+
  ggtitle("A  Overall Breast Cancer")+
  facet_grid(.~group, scales = "free", switch = "x")+
  scale_x_discrete(labels=c("Ethnicity-Black"=expression(bold(Ethnicity-Black)), 
                            "Ethnicity-Others"=expression(bold(Ethnicity-Others)),
                            "Family BC history-Yes"=expression(bold(Family_BC_history-Yes)),
                            "Hypertension-Yes"=expression(bold(Hypertension-Yes))))+
  ggsave("overall_BC_Manhattan.png",
         width = 1280/72, height = 800/72, dpi = 300)


##############inner BC Manhattan plot#################################################
#  import inner data and takes p values from results
inner_pval_manhanttan <- read.xlsx("plot_manhanttan_pval/inner/inner_pval_manhanttan.xlsx")
View(inner_pval_manhanttan)
inner_pval_manhanttan$pval<-as.numeric(inner_pval_manhanttan$pval)
inner_pval_manhanttan$model<-as.factor(inner_pval_manhanttan$model)
# Manhattan plot 
p_inner_BC<-ggplot(inner_pval_manhanttan, aes(y = -log10(pval), x = variables)) +   
  geom_point(aes(color=model),size = 3,alpha = 1/1.5)+ 
  geom_hline(yintercept = bonf_log1, color="red", linetype="dashed")+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle = 75, hjust = 1))+
  xlab("Predictors")+
  ylab("-log10(P value)")+
  ggtitle("B  Inner Quadrant Breast Cancer")+
  facet_grid(.~group, scales = "free", switch = "x")+
  scale_x_discrete(labels=c("Family BC history-Yes"=expression(bold(Family_BC_history-Yes)),
                            "Hypertension-Yes"=expression(bold(Hypertension-Yes))))+
  ggsave("plots/inner_BC_Manhattan.png",
         width = 1280/72, height = 800/72, dpi = 300)


##############outer BC Manhattan plot#################################################
#  import outer data and takes p values from results
outer_pval_manhanttan <- read.xlsx("outer_pval_manhanttan.xlsx")
View(outer_pval_manhanttan)
outer_pval_manhanttan$pval<-as.numeric(outer_pval_manhanttan$pval)
outer_pval_manhanttan$model<-as.factor(outer_pval_manhanttan$model)
# Manhattan plot 
p_outer_BC<-ggplot(outer_pval_manhanttan, aes(y = -log10(pval), x = variables)) +   
  geom_point(aes(color=model),size = 3,alpha = 1/1.5)+ 
  geom_hline(yintercept = bonf_log1, color="red", linetype="dashed")+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle = 75, hjust = 1))+
  xlab("Predictors")+
  ylab("-log10(P value)")+
  ggtitle("C  Outer Quadrant Breast Cancer")+
  facet_grid(.~group, scales = "free", switch = "x")+
  scale_x_discrete(labels=c("Family BC history-Yes"=expression(bold(Family_BC_history-Yes)),
                            "Hypertension-Yes"=expression(bold(Hypertension-Yes))))+
  ggsave("outer_BC_Manhattan.png",
         width = 1280/72, height = 800/72, dpi = 300)




