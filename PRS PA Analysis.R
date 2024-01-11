## Copy and Pasted R code for master dataset

## Sports Activity
#libraries
library(readxl)
library(readr)

#setWD
setwd("C:/Users/qyuan25/OneDrive - Emory University/Benson Temp File/ABCD/prs physical")


data<-read_xlsx("abcd_master_10_17_16.xlsx")#---------------------read final master dataset with CBCL
# 11868 sample size
SP_swim<-read_xlsx("C:/Users/qyuan25/OneDrive - Emory University/Benson Temp File/ABCD/Sports Recategorized for Swim 1218.xlsx")# ------------------read recategorized swim sports
# 11859 sample size


#check data columns

colnames(data)
library(dplyr)

data_subset<-data%>%
  select(1:36, # demographics
         57:131,
         132:531,
         532:555,
         672:688)
colnames(data_subset)

colnames(SP_swim)
merged_data <- left_join(data_subset, SP_swim, by = "id", suffix = c("", "_recoded"))

colnames(merged_data)

# merge close friends 
# import datawholewithpRS

dataPRS<-read_csv("C:/Users/qyuan25/OneDrive - Emory University/Shared Documents - SOM Ku Lab/General/ABCD Data/main csv file and variables/datawholewithpRS.csv")

colnames(dataPRS)

#select friends and closefriends
selected_dataPRS<-dataPRS%>%
  select(src_subject_id,friends, closefriends)

CF_data<-left_join(merged_data,selected_dataPRS,by=c("id"="src_subject_id") )

colnames(CF_data)
#check CF_data
#check sum_TP_mhour sum_SP_mhour sum_NSP_mhour recoded and sum_NSPS_mhour
selected_merged_data <- merged_data %>%
  select(id, sum_TP_mhour, sum_SP_mhour, sum_NSP_mhour, sum_TP_mhour_recoded, 
         sum_SP_mhour_recoded, sum_NSP_mhour_recoded, sum_NSPS_mhour)

summary(selected_merged_data$sum_TP_mhour)
summary(selected_merged_data$sum_TP_mhour_recoded)

summary(selected_merged_data$sum_SP_mhour)
summary(selected_merged_data$sum_SP_mhour_recoded) # no swimming

summary(selected_merged_data$sum_NSP_mhour)
summary(selected_merged_data$sum_NSP_mhour_recoded) # the same 

summary(selected_merged_data$sum_NSPS_mhour) # non-social added swimming
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0000  0.1731  0.3791  0.5192 14.5385       9 

# export CF_data out
library(writexl)
write_xlsx(CF_data,"data_1_3_24_CF.xlsx") # master dataset that has all variables needed and all participants


# create a subset that has no missing values for designated variables
CF_data<-read_excel("data_1_3_24_CF.xlsx")

colnames(CF_data)

CF_data_NA<-CF_data%>%
  select(id, eventname, site, family, age, sex, race_ethnicity, White_NonHispanic,p_edu_college,fh_psychosis,reshist_addr1_popdensity,
         Persis_dis_di2,Persis_dis_di3,Persistently_dis_tri1,Persistently_dis_tri2,EU_ancestry,SCZ_autocs,
         PC1_EU, PC2_EU, PC3_EU, PC4_EU, PC5_EU, PC6_EU, PC7_EU, PC8_EU, PC9_EU, PC10_EU, 
         PC11_EU, PC12_EU, PC13_EU, PC14_EU, PC15_EU, PC16_EU, PC17_EU, PC18_EU, PC19_EU, PC20_EU, 
         sum_TP_perwk, sum_TP_tspent, sum_TP_mhour, sum_SP_perwk, sum_SP_tspent, sum_SP_mhour, sum_NSP_perwk, sum_NSP_tspent, sum_NSP_mhour,
         bmi, poverty_line, INR, poverty_t, 
         cbcl_scr_dsm5_depress_r, sum_TP_mhour_recoded, sum_SP_mhour_recoded, sum_NSP_mhour_recoded, sum_NSPS_mhour, 
         friends, closefriends)%>%
  filter(complete.cases(id, eventname, site, family, age, sex, race_ethnicity, White_NonHispanic,p_edu_college,fh_psychosis,
                        Persis_dis_di2,Persis_dis_di3,Persistently_dis_tri1,Persistently_dis_tri2,SCZ_autocs,
                        sum_TP_perwk, sum_TP_tspent, sum_TP_mhour, sum_SP_perwk, sum_SP_tspent, sum_SP_mhour, sum_NSP_perwk, sum_NSP_tspent, sum_NSP_mhour,
                        bmi, INR, poverty_t, 
                        sum_TP_mhour_recoded, sum_SP_mhour_recoded, sum_NSP_mhour_recoded, sum_NSPS_mhour, 
                        friends, closefriends))


#8519
# Filter only White Hispanic with EU ancestry
data<-CF_data
data$fh_psych<-ifelse(data$fh_psychosis>=1,1,0)
dataEU <- data[(!is.na(data$EU_ancestry) & data$EU_ancestry == 1) & 
                 (!is.na(data$White_NonHispanic) & data$White_NonHispanic == 1), ] #5567



summary(dataEU$demo_comb_income_v2.x,useNA="ifany")


#Get rid of NAs for all variables for analysis
library(dplyr)

sub = dataEU%>%dplyr::select(
                             id, site, family, age, sex,p_edu_college,fh_psych,
                             Persistently_dis_tri1,Persis_dis_di2,EU_ancestry,SCZ_autocs,
                             PC1_EU, PC2_EU, PC3_EU, PC4_EU, PC5_EU, PC6_EU, PC7_EU, PC8_EU, PC9_EU, PC10_EU, 
                             PC11_EU, PC12_EU, PC13_EU, PC14_EU, PC15_EU, PC16_EU, PC17_EU, PC18_EU, PC19_EU, PC20_EU, 
                             sum_TP_mhour, sum_SP_mhour, sum_NSP_mhour,
                             bmi, poverty_line, INR, poverty_t, 
                              sum_TP_mhour_recoded, sum_SP_mhour_recoded, sum_NSP_mhour_recoded, sum_NSPS_mhour, 
                            # friends, closefriends
                             
)%>%na.omit() 
#4737


# Show the sex table
table(sub$sex)
dataEU1<-sub%>%filter(sex!=3) #4736
dataEU1$sex<-ifelse(dataEU1$sex==2,0,1) #recode 2 to 0

summary(dataEU1$SCZ_autocs,useNA="ifany")
summary(dataEU1$sum_SP_mhour,useNA="ifany")

table(dataEU1$family)

# Assuming you have your data in a vector called dataEU1$family
family_table <- table(dataEU1$family)

# Count how many have count 1 (unique family IDs)
count_unique <- sum(family_table == 1)

# Count how many have count 2 or more (duplicate family IDs)
count_duplicate <- sum(family_table >= 2)

# Print the results
cat("Number of unique family IDs:", count_unique, "\n")
cat("Number of duplicate family IDs:", count_duplicate, "\n")
#Z-scored all continuous variables
#age
dataEU1$agez<-scale(as.numeric(dataEU1$age),center=T,scale=T)
dataEU1$INRz<-scale(as.numeric(dataEU1$INR),center=T, scale=T)

summary(dataEU1$sum_TP_mhour)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.2885  0.6346  0.8029  1.1154  6.8077 

summary(dataEU1$sum_TP_mhour_recoded)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.2885  0.6346  0.8029  1.1154  6.8077 

dataEU1$TPz<-scale(as.numeric(dataEU1$sum_TP_mhour_recoded),center=T,scale=T)

dataEU1$SPz<-scale(as.numeric(dataEU1$sum_SP_mhour_recoded),center=T,scale=T)
dataEU1$NSPz<-scale(as.numeric(dataEU1$sum_NSP_mhour_recoded),center=T,scale=T)
dataEU1$NSPSz<-scale(as.numeric(dataEU1$sum_NSPS_mhour),center=T,scale=T)

dataEU1$SCZ_autocs_z<-scale(as.numeric(dataEU1$SCZ_autocs),center=T,scale=T)
dataEU1$BMIz<-scale(as.numeric(dataEU1$bmi),center=T,scale=T)
#dataEU1$CBCL_dep_z<-scale(as.numeric(dataEU1$cbcl_scr_dsm5_depress_r),center=T,scale=T)
summary(dataEU1$BMIz)


library(writexl)
write_xlsx(dataEU1,"dataEU_01_03_52.xlsx")# save for SPSS analysis


########################################################################### Start this for any additional analyses. 

dataEU1<-read_xlsx("dataEU_01_03_52.xlsx")
summary(dataEU1$fh_psych,useNA="ifany")
hist(data$fh_psychosis)
hist(dataEU$fh_psychosis) #

#table 1
colnames(dataEU1)
library(tableone)
allvars <- c("age","sex","p_edu_college","INR","poverty_t","fh_psychosis","bmi","SCZ_autocs",
             "sum_TP_mhour_recoded","sum_NSP_mhour_recoded","sum_SP_mhour_recoded","sum_NSPS_mhour")

catvars <- c("sex","p_edu_college","poverty_t","fh_psychosis")
non<-c("age","INR","bmi","SCZ_autocs","sum_TP_mhour_recoded","sum_NSP_mhour_recoded","sum_SP_mhour_recoded","sum_NSPS_mhour")

tab2 <- CreateTableOne(vars = allvars , data = dataEU1,factorVars = catvars)
# Set options to display numbers without scientific notation
options(scipen = 999) # Set a high value for scipen to avoid scientific notation
options(digits=2)
t1a<-print(tab2,nonnormal = non, showAllLevels = TRUE,formatOptions = list(big.mark = ",",digits=2))
library(tableone)
?CreateTableOne

# Calculate percentages with desired decimal places
dataEU1$sex <- paste0(dataEU1$sex, " (", sprintf("%.2f", prop.table(table(dataEU1$sex)) * 100), ")")
dataEU1$p_edu_college <- paste0(dataEU1$p_edu_college, " (", sprintf("%.2f", prop.table(table(dataEU1$p_edu_college)) * 100), ")")
dataEU1$poverty_t <- paste0(dataEU1$poverty_t, " (", sprintf("%.2f", prop.table(table(dataEU1$poverty_t)) * 100), ")")
dataEU1$fh_psych <- paste0(dataEU1$fh_psych, " (", sprintf("%.2f", prop.table(table(dataEU1$fh_psych)) * 100), ")")

# Create the table
tab2 <- CreateTableOne(vars = allvars, data = dataEU1, factorVars = catvars, nonnormal = non, showAllLevels = TRUE)



#stable 1
datas<-data[data$id %in% dataEU$id,] #Only including EUropean data for stable 1

datas$excluded<-ifelse(datas$id %in% dataEU1$id,0,1)

table(datas$excluded,useNA="ifany")
datae<-datas[datas$excluded==1,]

allvars <- c("age","sex","p_edu_college","INR","poverty_t","fh_psychosis","bmi","SCZ_autocs",
             "sum_TP_mhour_recoded","sum_NSP_mhour_recoded","sum_SP_mhour_recoded","sum_NSPS_mhour")

catvars <- c("sex","p_edu_college","poverty_t","fh_psychosis")
non<-c("age","INR","bmi","SCZ_autocs","sum_TP_mhour_recoded","sum_NSP_mhour_recoded","sum_SP_mhour_recoded","sum_NSPS_mhour")
stab2 <- CreateTableOne(vars = allvars , strata = "excluded", data = datas,factorVars = catvars)
st1<-print(stab2,nonnormal = non, showAllLevels = TRUE, formatOptions = list(big.mark = ","))


#table 3
colnames(dataEU1)
data1<-dataEU[dataEU$id %in% dataEU1$id,] #Create data1 which has no missing value

# Compare None vs. Transient
None_Tran<-subset(dataEU1,dataEU1$Persistently_dis_tri1<=1) #3888   #4066


None_Persis<-subset(dataEU1,dataEU1$Persistently_dis_tri1!=1) #3634   #3806

# 
# None_Tran$Transient <- ifelse(rowSums(data1[, c("PQBC_distress_di.0", "PQBC_distress_di.1", "PQBC_distress_di.2", "PQBC_distress_di.3")]) >= 2, 2,
#                           ifelse(rowSums(data1[, c("PQBC_distress_di.0", "PQBC_distress_di.1", "PQBC_distress_di.2", "PQBC_distress_di.3")]) == 1, 1, 0))

# table(data1$Transient,useNA="ifany")


allvars <- c("age","sex","p_edu_college","INR","poverty_t","fh_psychosis","bmi","SCZ_autocs",
             "sum_TP_mhour_recoded","sum_NSP_mhour_recoded","sum_SP_mhour_recoded","sum_NSPS_mhour")

catvars <- c("sex","p_edu_college","poverty_t","fh_psychosis")
non<-c("age","INR","bmi","SCZ_autocs","sum_TP_mhour_recoded","sum_NSP_mhour_recoded","sum_SP_mhour_recoded","sum_NSPS_mhour")

stab2 <- CreateTableOne(vars = allvars , strata = "Persistently_dis_tri1", data = None_Tran,factorVars = catvars)
?CreateTableOne
st1<-print(stab2,nonnormal = non, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write_xlsx(dataEU1,"dataEU_8_31_13.xlsx")

# Box plot for PRS and Persistently TRI

# boxplot(dataEU1$SCZ_autocs_z~dataEU1$Persistently_dis_tri1,main="Boxplot for PLE groups",
#         xlab="PRS for schizophrenia (Z-score)",ylab="PLE groups",names=c("None","Transient","Persistent"),
#         ylim=c(-1,1),
#         col="#7ccba2",border="#045275",outline=FALSE,horizontal=TRUE)
# 
# boxplot(dataEU1$Persistently_dis_tri1 ~ dataEU1$SCZ_autocs_z, 
#         main = "Boxplot for PLE groups",
#         ylab = "PLE groups",
#         xlab = "PRS for schizophrenia (Z-score)",
#         names = c("None", "Transient", "Persistent"),
#         #xlim = c(-1, 1),  # Adjust the limits as needed
#         col = "#7ccba2",
#         border = "#045275",
#         outline = FALSE,
#         horizontal = TRUE)  # Use horizontal = TRUE to swap axes


#ggplot boxplot violin plot
library(ggplot2)
library(dplyr)
sample_size=dataEU1%>%group_by(Persistently_dis_tri1)%>%summarize(num=n())

dataEU1$Persistently_dis_tri1<-as.factor(dataEU1$Persistently_dis_tri1)
#plot
new_labels <- c("None", "Transiently", "Persistently")
dataEU1$Persistently_dis_tri <- factor(dataEU1$Persistently_dis_tri1, levels = c(0, 1, 2), labels = new_labels)

p <- ggplot(dataEU1, aes(x = Persistently_dis_tri, y = SCZ_autocs_z, fill = Persistently_dis_tri)) +
  geom_violin(trim = FALSE, scale = "width", width = 0.6) +
  geom_boxplot(width = 0.2, position = position_dodge(0.75)) +  # Include geom_boxplot
  scale_fill_brewer(palette = "RdBu") +
  theme_minimal() +
  #ylim(c(-1, 1)) +
  labs(title = "Violin Plot with Boxplot of PRS for Schizophrenia in 3 PLE groups",
       x = "Distressing PLE", y = "PRS-SCZ (z-scored)", fill = "Distressing PLE")

install.packages('Cairo')
library('Cairo')
CairoWin()
p <- ggplot(dataEU1, aes(x = Persistently_dis_tri, y = SCZ_autocs_z, fill = Persistently_dis_tri)) +
  geom_violin(trim = FALSE, scale = "width", width = 0.6, color = NA) +
  geom_boxplot(aes(fill = Persistently_dis_tri), width = 0.2, position = position_dodge(0.75), color = "#666699") +
  geom_point(stat = "summary", fun = "mean", shape = 1, position = position_dodge(0.75), color = "#666699", size = 2.5) +  # Add open circles for mean
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(
    title = "Violin Plot with Boxplot of PRS for Schizophrenia in 3 PLE groups",
    x = "Distressing PLE", y = "PRS-SCZ (z-scored)", fill = "Distressing PLE"
  )

print(p)

ggsave(p, filename = 'violinfig1.png', dpi = 300, type = 'cairo',
       width = 8, height = 8, units = 'in')
p



#Box plot fo PA,SA,NSA and Persistently TRI
boxplot(dataEU1$sum_NSP_mhour~dataEU1$Persistently_dis_tri1,main="Boxplot for PLE groups",
        xlab="PLE groups",ylab="PRS for schizophrenia",names=c("None","Transient","Persistent"),
        col="#7ccba2",border="#045275",outline=FALSE)

#ggplot for NSP,SP, TP (multiple variables) sum_NSP_mhour~Persistently_dis_tri1, sum_SP_mhour~Persistently_dis_tri1
colnames(dataEU1)


# longer_data%>%ggplot(aes(x=PLEgroups, y = mhour,color=PAgroups))+
#   geom_boxplot()+
#   facet_wrap(vars(PAgroups),ncol=3)+
#   labs(x="PLE groups",y="Mean hours per week")
# #change this num to categorical factor 0-> None, 1-> Transient, 2-> Persistent
# str(longer_data$Persistently_dis_tri1)

category_labels <- c("None", "Transiently", "Persistently")
PA_labels<-c("IS","TS","PA")
library(tidyr)
library(forcats)
longer_data <- dataEU1 %>%
  pivot_longer(c( sum_TP_mhour,sum_NSPS_mhour, sum_SP_mhour_recoded), names_to = "PAgroups", values_to = "mhour") %>%
  mutate(Persistently_dis_tri = factor(Persistently_dis_tri1, levels = 0:2, labels = category_labels))%>%
  mutate(PAgroups = case_when(
    PAgroups == "sum_NSPS_mhour" ~ PA_labels[1],
    PAgroups == "sum_SP_mhour_recoded" ~ PA_labels[2],
    PAgroups == "sum_TP_mhour" ~ PA_labels[3]
  ))%>%
  mutate(PAgroups = fct_relevel(PAgroups, "PA", "TS", "IS"))%>%
  mutate(Persistently_dis_tri = fct_relevel(Persistently_dis_tri,  "None", "Transient","Persistent"))


table(longer_data$PAgroups)
# 
# NSA   PA   SA 
# 4507 4507 4507 #I want the order to be PA, SA, and then NSA


ggplot(longer_data, aes(x = mhour, y = Persistently_dis_tri, fill = Persistently_dis_tri)) +
  geom_violin(trim=FALSE)+
  geom_boxplot(outlier.shape = NA) +
 # xlim(c(0, 1.5)) +  # Adjust the limits for the y-axis if needed
  facet_grid(PAgroups ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Mean hours per week", y = "Distressful PLE Types") +
  labs(fill = "Distressful PLE Types")

category_labels <- c("None", "Transiently", "Persistently")
PA_labels <- c("NSA", "SA", "PA")

##--------------This boxplot works!!!#########


g<-ggplot(longer_data, aes(x = Persistently_dis_tri, y = mhour, fill = Persistently_dis_tri)) +
  geom_violin(trim = FALSE,color=NA) +
  geom_boxplot(outlier.shape = NA,width=0.2,color="#666699") +
 ylim(c(0, 3)) +  # Adjust the limits for the y-axis if needed
  facet_grid(. ~ PAgroups, scales = "free_x", space = "free_x") +  # Swap x and y in facet_grid
  labs(x = "Distressing PLE", y = "Mean hours per week") +  # Swap x and y labels
  geom_point(stat = "summary", fun = "mean", shape = 1, position = position_dodge(0.75), color = "#666699", size = 2.5) +  # Add open circles for mean
  labs(fill = "Distressing PLE") +
  scale_fill_brewer(palette = "Set3")+
  theme_bw()
g
ggsave(g, filename = 'violinfig2.png', dpi = 300, type = 'cairo',
       width = 8, height = 6, units = 'in')

library(forcats)

ggplot(longer_data, aes(x = mhour, y = Persistently_dis_tri1, fill = Persistently_dis_tri1)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = fill_colors) +  # Specify the fill colors
  xlim(c(0, 1.5)) +  # Adjust the limits for the y-axis if needed
  facet_grid(PAgroups ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Mean hours per week", y = "Distressful PLE Types") +
  labs(fill = "Distressful PLE Types")

table(dataEU1$Persistently_dis_tri1)


# Create subgroups based on Persistenyly Distress TRI, None, Transient, Persistent
####------------Interaction Plots with simple Slopes-------------------------------
None_Tran<-subset(dataEU1,dataEU1$Persistently_dis_tri1<=1) #3888   #4066
None_Persis<-subset(dataEU1,dataEU1$Persistently_dis_tri1!=1) #3634   #3806
None<-subset(dataEU1,dataEU1$Persistently_dis_tri1==0) #3015.   #3136

#Create Persistently_di for all participants
#but 0 = none or Tran
# 1 = Persistent


#Logistic regression for None Tran,----------------------------------------------------------------
#interaction term with PA (TP)
summary(None_Tran$Persistently_dis_tri1)
library(interactionR)
library(interactions)
library(survival)
library(readr)
library(mediation)
library(lme4)
library(lmerTest)
str(None_Tran)
None_Tran$site <- as.factor(None_Tran$site)

model<-glmer(Persistently_dis_tri1~
               TPz*SCZ_autocs_z+
               agez+sex+p_edu_college+BMIz+
               INRz+fh_psych+
               +
               PC1_EU+PC2_EU+PC3_EU+PC4_EU+PC5_EU+PC6_EU+PC7_EU+PC8_EU+PC9_EU+PC10_EU+
               PC11_EU+PC12_EU+PC13_EU+PC14_EU+PC15_EU+PC16_EU+PC17_EU+PC18_EU+PC19_EU+PC20_EU+
               (1|site)+ (1|family), 
             data = None_Tran,family = binomial)
summary(model)

#interaction plot!!! for PA( TP)
p<-interact_plot(model, pred = SCZ_autocs_z, modx = TPz, interval = TRUE, x.label = 'PRS for schizophrenia', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
                 legend.main = 'Physical Activity')

print(p)


sim_slopes(model, pred = SCZ_autocs_z, modx = TPz, interval = TRUE, x.label = 'Schizophrenia PRS', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
           legend.main = 'Physical Activity')

#None Tran TS
model<-glmer(Persistently_dis_tri1~
               SPz*SCZ_autocs_z+
               agez+sex+p_edu_college+BMIz+
               INRz+fh_psych+
               
               PC1_EU+PC2_EU+PC3_EU+PC4_EU+PC5_EU+PC6_EU+PC7_EU+PC8_EU+PC9_EU+PC10_EU+
               PC11_EU+PC12_EU+PC13_EU+PC14_EU+PC15_EU+PC16_EU+PC17_EU+PC18_EU+PC19_EU+PC20_EU+
               (1|site)+ (1|family), 
             data = None_Tran,family = binomial)
summary(model)
summ(model)
#interaction plot!!! for SA( SP)
p<-interact_plot(model, pred = SCZ_autocs_z, modx = SPz, interval = TRUE, x.label = 'PRS for schizophrenia', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
                 legend.main = 'Social Physical Activity')
sim_slopes(model, pred = SCZ_autocs_z, modx = SPz, interval = TRUE, x.label = 'Schizophrenia PRS', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
           legend.main = 'Physical Activity')
print(p)
#Non Tran NSA
model<-glmer(Persistently_dis_tri1~
               NSPz*SCZ_autocs_z+
               agez+sex+p_edu_college+BMIz+
               INRz+fh_psych+
               PC1_EU+PC2_EU+PC3_EU+PC4_EU+PC5_EU+PC6_EU+PC7_EU+PC8_EU+PC9_EU+PC10_EU+
               PC11_EU+PC12_EU+PC13_EU+PC14_EU+PC15_EU+PC16_EU+PC17_EU+PC18_EU+PC19_EU+PC20_EU+
               (1|site)+ (1|family), 
             data = None_Tran,family = binomial)
summary(model)
#interaction plot!!! for NSA( NSP)
p<-interact_plot(model, pred = SCZ_autocs_z, modx = NSPz, interval = TRUE, x.label = 'PRS for schizophrenia', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
                 legend.main = 'NSA')


print(p)
sim_slopes(model, pred = SCZ_autocs_z, modx = NSPz, interval = TRUE, x.label = 'Schizophrenia PRS', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
           legend.main = 'Physical Activity')

# Non_Persistent -----------------------------------------------------------------------------------
control <- glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000))

model<-glmer(Persis_dis_di2~
               TPz*SCZ_autocs_z+
               agez+sex+p_edu_college+BMIz+
               INRz+fh_psych+
               PC1_EU+PC2_EU+PC3_EU+PC4_EU+PC5_EU+PC6_EU+PC7_EU+PC8_EU+PC9_EU+PC10_EU+
               PC11_EU+PC12_EU+PC13_EU+PC14_EU+PC15_EU+PC16_EU+PC17_EU+PC18_EU+PC19_EU+PC20_EU+
               (1|site)+ (1|family), 
             data = None_Persis,family = binomial,control = control)
summary(model)

#interaction plot!!! for PA( TP)
p<-interact_plot(model, pred = SCZ_autocs_z, modx = TPz, interval = TRUE, x.label = 'PRS-SCZ', y.label = 'Log of Persistently Distressing PLE',
                 legend.main = 'Physical Activity')

print(p)


sim_slopes(model, pred = SCZ_autocs_z, modx = TPz, interval = TRUE, x.label = 'Schizophrenia PRS', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
           legend.main = 'Physical Activity')

#None Persis SA
model<-glmer(Persis_dis_di2~
               SPz*SCZ_autocs_z+
               agez+sex+p_edu_college+BMIz+
               INRz+fh_psych+
               
               PC1_EU+PC2_EU+PC3_EU+PC4_EU+PC5_EU+PC6_EU+PC7_EU+PC8_EU+PC9_EU+PC10_EU+
               PC11_EU+PC12_EU+PC13_EU+PC14_EU+PC15_EU+PC16_EU+PC17_EU+PC18_EU+PC19_EU+PC20_EU+
               (1|site)+ (1|family), 
             data = None_Persis,family = binomial,control=control)
summary(model)
summ(model)
#interaction plot!!! for SA( SP) #############################################################8/31/2023
p<-interact_plot(model, pred = SCZ_autocs_z, modx = SPz, interval = TRUE, x.label = 'PRS-SCZ', y.label = 'Log of Persistently Distressing Psychotic-Like Experiences',
                 legend.main = 'Team-based Sports')

ggsave(p, filename = 'fig3.png', dpi = 300, type = 'cairo',
       width = 8, height = 8, units = 'in')

sim_slopes(model, pred = SCZ_autocs_z, modx = SPz, interval = TRUE, x.label = 'PRS-SCZ', y.label = 'Log of Persistently Distressing Psychotic-Like Experiences',
           legend.main = 'Team-based Sports')
print(p)
#Non Persistent NSA####################################### RUN SIMPLE SLOPE ##############
model<-glmer(Persis_dis_di2~
               NSPz*SCZ_autocs_z+
               agez+sex+p_edu_college+BMIz+
               INRz+fh_psych+
               PC1_EU+PC2_EU+PC3_EU+PC4_EU+PC5_EU+PC6_EU+PC7_EU+PC8_EU+PC9_EU+PC10_EU+
               PC11_EU+PC12_EU+PC13_EU+PC14_EU+PC15_EU+PC16_EU+PC17_EU+PC18_EU+PC19_EU+PC20_EU+
               (1|site)+ (1|family), 
             data = None_Persis,family = binomial, control=control)
summary(model)
#interaction plot!!! for NSA( NSP)
p<-interact_plot(model, pred = SCZ_autocs_z, modx = NSPz, interval = TRUE, x.label = 'PRS for schizophrenia', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
                 legend.main = 'Individual Sports')
summary(None_Persis$NSPz)
summary(dataEU1$NSPz)
dataEU1$sum

print(p)
sim_slopes(model, pred = SCZ_autocs_z, modx = NSPz, interval = TRUE, x.label = 'PRS-SCZ', y.label = 'Log of Persistently Distressing Psychotic-Like Experiences',
           legend.main = 'Individual Sports')

# NSA for the whole group considering tran and none as 0 
model<-glmer(Persis_dis_di2~
               sum_NSP_mhour*SCZ_autocs_z+
               agez+sex+p_edu_college+BMIz+
               INRz+fh_psych+
               PC1_EU+PC2_EU+PC3_EU+PC4_EU+PC5_EU+PC6_EU+PC7_EU+PC8_EU+PC9_EU+PC10_EU+
               PC11_EU+PC12_EU+PC13_EU+PC14_EU+PC15_EU+PC16_EU+PC17_EU+PC18_EU+PC19_EU+PC20_EU+
               (1|site)+ (1|family), 
             data = None_Persis,family = binomial,control=control)
summary(model)
#interaction plot!!! for NSA( NSP)
p<-interact_plot(model, pred = SCZ_autocs_z, modx = NSPz, interval = TRUE, x.label = 'PRS-SCZ', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
                 legend.main = 'NSA')


print(p)
sim_slopes(model, pred = SCZ_autocs_z, modx = sum_NSP_mhour, interval = TRUE, x.label = 'Schizophrenia PRS', y.label = 'Log of Persistent Distressing Psychotic-Like Experiences',
           legend.main = 'Physical Activity')





#Multivariable # Weekly Act that Requires Teamwork
dataEU1$Persis_dis_di2<-as.numeric(dataEU1$Persis_dis_di2)
model<-glmer(Persis_dis_di2~
               agez+sex+p_edu_college+
               reshist_addr1_adi_pov_z+reshist_addr1_popdensity_z+week_sp_RTz+
               SCZ_autocs_z+
               PC1_EU+PC2_EU+PC3_EU+PC4_EU+PC5_EU+PC6_EU+PC7_EU+PC8_EU+PC9_EU+PC10_EU+
               PC11_EU+PC12_EU+PC13_EU+PC14_EU+PC15_EU+PC16_EU+PC17_EU+PC18_EU+PC19_EU+PC20_EU+
               (1|site), data = dataEU1,family = binomial)
summary(model)
str(dataEU1)

# Check Family occurences
summary(dataEU1$family,useNA="ifany") # 3024 + 716 *2 + 16*3 + 1 * 5 = 4059

counts <- table(dataEU1$family)
count_1_entry <- sum(counts == 1)
count_2_entries <- sum(counts == 2)
count_3_entries <- sum(counts == 3)
count_4_entries <- sum(counts == 4)
count_5_entries <- sum(counts == 5)
cat("Number of entries with 1 occurrence:", count_1_entry, "\n")
cat("Number of entries with 2 occurrences:", count_2_entries, "\n")
cat("Number of entries with 3 occurrences:", count_3_entries, "\n")
cat("Number of entries with 4 occurrences:", count_4_entries, "\n")
cat("Number of entries with 4 occurrences:", count_5_entries, "\n")

#Check Site
table(dataEU1$site,useNA="ifany")
