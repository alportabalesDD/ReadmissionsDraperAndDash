options(warn=-1)

list.of.packages <- c("here", "FNN","data.table", "tibble","bit64","icd", "janitor","lubridate","hms","tidyr","stringr","openxlsx","forcats","RcppRoll","dplyr","readr", "caret","partykit","xts","tidyverse","rpart","viridisLite","e1071","cluster")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


library(here)
library(readr)
library(caret)
library(partykit)
library(xts)
library(tidyverse)
library(rpart)
library(viridisLite)
library(e1071)
library(cluster)
library(data.table)
library(FNN)

setwd("C:/Users/Pochátil/Desktop/Stanadalone Readmissions")



Base <- read_csv("./Inpatients_Readmissions_Updated.csv", 
                                              col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"), 
                                                               Consultant = col_skip(), `Days to Readmission` = col_double(), 
                                                               DaystoReadmission = col_number(), 
                                                               DaystoReadmissions = col_number(), 
                                                               `Discharge Date` = col_date(format = "%m/%d/%Y"), 
                                                               `Discharge Method` = col_skip(), 
                                                               `Discharge Method Code` = col_double(), 
                                                               `Admission Date` = col_date(format = "%m/%d/%Y"),
                                                               `Procedure Date` = col_date(format = "%m/%d/%Y"),
                                                               InpatientAdmissionsDays = col_number(), 
                                                               `Length of Stay` = col_number(), 
                                                               `Local Hospital Code` = col_skip(),
                                                               `Age on Arrival` = col_number(), 
                                                               `Patient Class` = col_character(), 
                                                               `Patient ID` = col_skip(), `Primary Diagnosis Code` = col_character(), 
                                                               `Purchaser Code` = col_skip(), `Spell Number` = col_skip()))

Base$Readmissions <-ifelse(Base$DaystoReadmission>30,1,0)
Base$Readmissions <- ifelse(is.na(Base$DaystoReadmission),0,Base$Readmissions)
Base$Readmissions <- ifelse(is.na(Base$Readmissions),0,Base$Readmissions)

names(Base)<-make.names(names(Base))

Base$Patient.ID<- NULL
Base$DaystoReadmission<- NULL
Base$Days.to.Readmission<- NULL
Base$Readmission.Part<- NULL
Base$Readmission.Part.Exclusions<- NULL
Base$Readmission.Spells<-NULL
Base$Readmission.Spells.Exclusions<-NULL
Base$Readmitted.Same.Consultant<-ifelse(is.na(Base$Readmitted.Same.Consultant),0,Base$Readmitted.Same.Consultant)
Base$Readmitted.Same.Consultant.Exclusions<-ifelse(is.na(Base$Readmitted.Same.Consultant.Exclusions),0,Base$Readmitted.Same.Consultant.Exclusions)
Base$Readmitted.Same.Diagnosis.Group<-ifelse(is.na(Base$Readmitted.Same.Diagnosis.Group),0,Base$Readmitted.Same.Diagnosis.Group)
Base$Readmitted.Same.Specialty<-ifelse(is.na(Base$Readmitted.Same.Specialty),0,Base$Readmitted.Same.Specialty)
Base$Readmitted.Same.Diagnosis.Group.Exclusions<-ifelse(is.na(Base$Readmitted.Same.Diagnosis.Group.Exclusions),0,Base$Readmitted.Same.Diagnosis.Group.Exclusions)
Base$Readmitted.Same.HRG<-ifelse(is.na(Base$Readmitted.Same.HRG),0,Base$Readmitted.Same.HRG)
Base$Readmitted.Same.HRG.Exclusions<-ifelse(is.na(Base$Readmitted.Same.HRG.Exclusions),0,Base$Readmitted.Same.HRG.Exclusions)
Base$Readmitted.Same.Primary.Diagnosis<-ifelse(is.na(Base$Readmitted.Same.Primary.Diagnosis),0,Base$Readmitted.Same.Primary.Diagnosis)
Base$Readmitted.Same.Primary.Diagnosis.Exclusions<-ifelse(is.na(Base$Readmitted.Same.Primary.Diagnosis.Exclusions),0,Base$Readmitted.Same.Primary.Diagnosis.Exclusions)
Base$Readmitted.Same.Specialty.Exclusions<-ifelse(is.na(Base$Readmitted.Same.Specialty.Exclusions),0,Base$Readmitted.Same.Specialty.Exclusions)
Base$Readmitted.Same.Ward<-ifelse(is.na(Base$Readmitted.Same.Ward),0,Base$Readmitted.Same.Ward)
Base$Readmitted.Same.Ward.Exclusions<-ifelse(is.na(Base$Readmitted.Same.Ward.Exclusions),0,Base$Readmitted.Same.Ward.Exclusions)
Base$Readmitted.Within.30.Days<-ifelse(is.na(Base$Readmitted.Within.30.Days),0,Base$Readmitted.Within.30.Days)
Base$Readmitted.Within.30.Days.Exclusions<-NULL
Base<-Base[, -grep("Parent", colnames(Base))]
Base<-Base[, -grep("Exclude", colnames(Base))]
Base$Death.on.Readmission <- NULL
Base$Patient.Readmitted<-NULL
Base$Patient.Readmitted.Exclusions<-NULL

Base$fk_PersonGenderCode<-ifelse(is.na(Base$fk_PersonGenderCode),0,Base$fk_PersonGenderCode)

Base$From.Care.Home <- ifelse(Base$From.Care.Home=="Yes",1,0)

Base$Last.Episode.In.Spell.Indicator <- NULL
Base$Local.Specialty.Code<-NULL
Base$Admission.Method<- NULL

Base$Patient.Class <- NULL
Base$Patient.Name<-NULL

Base$InpatientAdmissionsDays <-NULL

Base$Length.of.Stay<- ifelse(is.na(Base$Length.of.Stay),0,Base$Length.of.Stay)

Base$TH.Procedure.Code<- NULL
Base$WL.Procedure.Code<- NULL
Base$WL.Procedure.Date<- NULL
Base$ReadmitFlag<-NULL
Base$Adult.or.Child<-NULL
Base$DaystoReadmissions<-NULL
Base$IPAdmissionDays<-NULL
Base$Days.to.Readmission.Exclusions <- NULL
Base$IPFlag <-NULL
Base$Spell.HRG<-NULL


Base <- Base[!is.na(Base),]

#Clean the datasets

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))


#
Proc_codes<-read_csv("./Theatres/Theatres.ref_ProcedureCode.csv")
Specialty_codes<-read_csv("./Theatres/Theatres.ref_SpecialtyCode.csv")
Specialty_codes$`Service Area`<-NULL
Theatre_Operations<-read_csv("./Data/TheatreOperations.csv")
setnames(Theatre_Operations,"TheatreOperations.Theatre Session ID", "fk_TheatreSessions")
Theatre_Sessions<-read_csv("./Theatres/Theatres.TheatreSessions.csv")
Theatre_Sessions$fk_TheatreSessions<-as.character(Theatre_Sessions$fk_TheatreSessions)
Theatre_Sessions<-read_csv("./Theatres/Theatres.TheatreSessions.csv")
Surgeon_Codes<-read_csv("./Theatres/Theatres.ref_SurgeonCode.csv")
IntendedManagement_Codes<-read_csv("./Theatres/Theatres.ref_IntendedManagementCode.csv")

#####

names(Theatre_Operations)<- substring(names(Theatre_Operations),19)



# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(openxlsx)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(bit64)
library(icd)

setnames(Base,"Procedure.Code","fk_ProcedureCode")

setnames(Base, "Consultant.Treatment.Function.Code", "fk_SpecialtyCode")

Proc_codes$fk_ProcedureCode<-gsub("[[:punct:]]", "", Proc_codes$fk_ProcedureCode)

Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="A","Nervous System",NA)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="B","Endocrine System and Breast",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="C","Eye",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="D","Ear",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="E","Respiratory Tract",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="F","Mouth",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="G","Upper Digestive System",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="H","Lower Digestive System",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="J","Other Abdominal Organs, Principally Digestive",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="K","Heart",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="L","Arteries and Veins",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="M","Urinary",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="N","Male Genital Organs",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="P","Lower Female Genital Tract",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="Q","Upper Female Genital Tract",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="R","Female Genital Tract Associated with Pregnancy, Childbirth and the Puerperium",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="S","Skin",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="T","Soft Tissue",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="U","Diagnostic Imaging, Testing and Rehabilitation",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="V","Bones and Joints of Skull and Spine",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="W","Other Bones and Joints",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="X","Miscellaneous Operations",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="Y","Subsidiary Classification of Methods of Operation",Proc_codes$SimplifiedCode)
Proc_codes$SimplifiedCode <- ifelse(substr(Proc_codes$fk_ProcedureCode, 1, 1)=="Z","Subsidiary Classification of Sites of Operation",Proc_codes$SimplifiedCode)

Base<- left_join(Base, Proc_codes)
Base<-left_join(Base,Specialty_codes)
Base<-left_join(Base,Surgeon_Codes)
Base<-left_join(Base,IntendedManagement_Codes)

Base$fk_MainSpecialtyCode<-NULL
Base$fk_SpecialtyCode<-NULL
Base$fk_ProcedureCode<-NULL
Base$fk_SurgeonCode<-NULL
Base$fk_IntendedManagementCode<-NULL
Base$fk_TreatmentSpecialtyCode<-NULL
Base$fk_MainSpecialtyCode<-NULL
Base$Main.Specialty.Code<-NULL

Base$Procedure<-ifelse(is.na(Base$Procedure), "No Procedure", Base$Procedure)
Base$SimplifiedCode<-ifelse(is.na(Base$SimplifiedCode), "No SimplifiedCode", Base$SimplifiedCode)
Base$Specialty<-ifelse(is.na(Base$Specialty), "No Specialty", Base$Specialty)
Base$Surgeon<-ifelse(is.na(Base$Surgeon), "No Surgeon", Base$Surgeon)
Base$Division<-ifelse(is.na(Base$Division), "No Division", Base$Division)
Base$`Intended Management`<-ifelse(is.na(Base$`Intended Management`), "No Intended Management", Base$`Intended Management`)
Base$Age.Type<-NULL
Base$Operation.Date<-NULL
Base$Diagnosis.Group<-NULL
Base$Diagnosis.Group.Num<-NULL
Base$Episode.Number <- NULL


Base$DaysFromProcedureToDischarge<- (as.numeric(as.POSIXct(Base$Procedure.Date)) - as.numeric(as.POSIXct(Base$Discharge.Date)))/86400
Base$DaysFromAdmissionToProcedure<- (as.numeric(as.POSIXct(Base$Admission.Date)) - as.numeric(as.POSIXct(Base$Procedure.Date)))/86400
Base<-left_join(Base,Base %>%
      group_by(Pat_ID_Key) %>%
      summarise(NumberofReadmissions=sum(Flag=="RA")))
Base$DayoftheWeek <- weekdays(as.Date(Base$Admission.Date))

Base$Admission.Date<-NULL
Base$Discharge.Date<-NULL
Base$Procedure.Date<-NULL

names(Base)<-make.names(names(Base))

library(readr)
IP <- data.table(cbind(Base$Pat_ID_Key,Base$Primary.Diagnosis.Code))



#IP<- IP[complete.cases(IP),]
visits <- IP


visits$`Local Specialty Code`<-NULL
visits$`Spell Number`<- NULL
visits$V2<- as.icd10cm(visits$V2)
visits$`Discharge Method Code`<-NULL
visits$`Ward Code at Episode End`<-NULL
visits$HRG<-NULL
visits$`Discharge Date`<-NULL
visits$`Local Hospital Code`<-NULL
visits$`Episode Number`<-NULL
visits$`Date of Birth`<-NULL

visits <- visits %>%
  gather(., key ="Diagnosis", value="value", "V2")

visits<-unique(visits)

tst <- comorbid_charlson(visits, return_df = TRUE,
                         return_binary = TRUE,
                         short_code=TRUE,
                         icd_name="value")

#data.table::setnames(tst, "ActivityID", "PatientID")
tst<- unique(tst)
setnames(tst, "V1","Pat_ID_Key")
tst<-aggregate(. ~ Pat_ID_Key, tst, sum)
tst<- cbind(tst[,1],apply(tst[,2:18], 2, function(x) ifelse(x > 1, 1, x)))
tst<- data.frame(tst)
setnames(tst, "V1","Pat_ID_Key")


Base<- left_join(Base,tst,by="Pat_ID_Key")



Base$Specialty<- ifelse(is.na(Base$Specialty),0,Base$Specialty)
Base$Division<- ifelse(is.na(Base$Division),0,Base$Division)
Base$Procedure <- ifelse(is.na(Base$Procedure),0,Base$Procedure)



Base$Readmissions <- ifelse(is.na(Base$Readmissions),0,Base$Readmissions)



save(Base, file="Readmissions_Training.rda")



Base[c("MI", "CHF","PVD","Stroke","Dementia","Pulmonary","Rheumatic","PUD","LiverMild","DM","DMcx","Paralysis","Renal","Cancer","LiverSevere","Mets","HIV")][is.na(Base[c("MI", "CHF","PVD","Stroke","Dementia","Pulmonary","Rheumatic","PUD","LiverMild","DM","DMcx","Paralysis","Renal","Cancer","LiverSevere","Mets","HIV")])] <- 0



trainingAD <- Base
trainingAD<- trainingAD[,names(trainingAD) %in% names(testingAD)]


library(dplyr)


trainingAD <- data.frame(trainingAD)


### Naive Bayes (More sensitive to 1s)
fit <-naiveBayes(factor(Readmissions) ~ .  , data=trainingAD)
predicted <- predict(fit, testingAD, type = "raw")
predicted<- data.frame(predicted)



training<-trainingAD
testing<-testingAD

training$Length.of.Stay.0.3 <-ifelse(trainingAD$Length.of.Stay<=3,1,0)
training$Length.of.Stay.4.7<-ifelse(trainingAD$Length.of.Stay>3 & trainingAD$Length.of.Stay<=7,1,0)
training$Length.of.Stay.8.14<-ifelse(trainingAD$Length.of.Stay>7 & trainingAD$Length.of.Stay<=14,1,0)
training$Length.of.Stay.15.21<-ifelse(trainingAD$Length.of.Stay>14 & trainingAD$Length.of.Stay<=21,1,0)
training$Length.of.Stay.22.plus<-ifelse(trainingAD$Length.of.Stay>=22,1,0)

testing$Length.of.Stay.0.3 <-ifelse(testingAD$Length.of.Stay<=3,1,0)
testing$Length.of.Stay.4.7<-ifelse(testingAD$Length.of.Stay>3 & testingAD$Length.of.Stay<=7,1,0)
testing$Length.of.Stay.8.14<-ifelse(testingAD$Length.of.Stay>7 & testingAD$Length.of.Stay<=14,1,0)
testing$Length.of.Stay.15.21<-ifelse(testingAD$Length.of.Stay>14 & testingAD$Length.of.Stay<=21,1,0)
testing$Length.of.Stay.22.plus<-ifelse(testingAD$Length.of.Stay>=22,1,0)

training$fk_PersonGenderCode.Other<-ifelse(trainingAD$fk_PersonGenderCode!=1 & trainingAD$fk_PersonGenderCode!=2,1,0)
training$fk_PersonGenderCode.Male <- ifelse(trainingAD$fk_PersonGenderCode==1,1,0)
training$fk_PersonGenderCode.Female <- ifelse(trainingAD$fk_PersonGenderCode==2,1,0)

testing$fk_PersonGenderCode.Other<-ifelse(testingAD$fk_PersonGenderCode!=1 & testingAD$fk_PersonGenderCode!=2,1,0)
testing$fk_PersonGenderCode.Male <- ifelse(testingAD$fk_PersonGenderCode==1,1,0)
testing$fk_PersonGenderCode.Female <- ifelse(testingAD$fk_PersonGenderCode==2,1,0)

training$Admission.Method.Full.Elective<- ifelse(trainingAD$Admission.Method.Full=="Elective",1,0)
training$Admission.Method.Full.Emergency <- ifelse(trainingAD$Admission.Method.Full=="Emergency",1,0)
training$Admission.Method.Full.Maternity <- ifelse(trainingAD$Admission.Method.Full=="Maternity",1,0) 
training$Admission.Method.Full.Other <- ifelse(trainingAD$Admission.Method.Full=="Other",1,0)


testing$Admission.Method.Full.Elective<- ifelse(testingAD$Admission.Method.Full=="Elective",1,0)
testing$Admission.Method.Full.Emergency <- ifelse(testingAD$Admission.Method.Full=="Emergency",1,0)
testing$Admission.Method.Full.Maternity <- ifelse(testingAD$Admission.Method.Full=="Maternity",1,0) 
testing$Admission.Method.Full.Other <- ifelse(testingAD$Admission.Method.Full=="Other",1,0)

training$Discharge.Method.Code.1 <- ifelse(trainingAD$Discharge.Method.Code == 1,1,0)
training$Discharge.Method.Code.2 <- ifelse(trainingAD$Discharge.Method.Code == 2,1,0)
training$Discharge.Method.Code.4 <- ifelse(trainingAD$Discharge.Method.Code == 4,1,0)
training$Discharge.Method.Code.5 <- ifelse(trainingAD$Discharge.Method.Code == 5,1,0)

testing$Discharge.Method.Code.1 <- ifelse(testingAD$Discharge.Method.Code == 1,1,0)
testing$Discharge.Method.Code.2 <- ifelse(testingAD$Discharge.Method.Code == 2,1,0)
testing$Discharge.Method.Code.4 <- ifelse(testingAD$Discharge.Method.Code == 4,1,0)
testing$Discharge.Method.Code.5 <- ifelse(testingAD$Discharge.Method.Code == 5,1,0)

training$Patient.Classification.Code.1<- ifelse(trainingAD$Patient.Classification.Code == 1,1,0)
training$Patient.Classification.Code.2<- ifelse(trainingAD$Patient.Classification.Code == 2,1,0)
training$Patient.Classification.Code.5<- ifelse(trainingAD$Patient.Classification.Code == 5,1,0)
training$Patient.Classification.Code.8<- ifelse(trainingAD$Patient.Classification.Code == 8,1,0)

testing$Patient.Classification.Code.1<- ifelse(testingAD$Patient.Classification.Code == 1,1,0)
testing$Patient.Classification.Code.2<- ifelse(testingAD$Patient.Classification.Code == 2,1,0)
testing$Patient.Classification.Code.5<- ifelse(testingAD$Patient.Classification.Code == 5,1,0)
testing$Patient.Classification.Code.8<- ifelse(testingAD$Patient.Classification.Code == 8,1,0)

training$Source.of.Admission.19 <- ifelse(trainingAD$Source.of.Admission == 19,1,0)
training$Source.of.Admission.29 <- ifelse(trainingAD$Source.of.Admission == 29,1,0)
training$Source.of.Admission.51 <- ifelse(trainingAD$Source.of.Admission == 51,1,0)
training$Source.of.Admission.52 <- ifelse(trainingAD$Source.of.Admission == 52,1,0)
training$Source.of.Admission.54 <- ifelse(trainingAD$Source.of.Admission == 54,1,0)
training$Source.of.Admission.79 <- ifelse(trainingAD$Source.of.Admission == 79,1,0)
training$Source.of.Admission.85 <- ifelse(trainingAD$Source.of.Admission == 85,1,0)
training$Source.of.Admission.87 <- ifelse(trainingAD$Source.of.Admission == 87,1,0)

testing$Source.of.Admission.19 <- ifelse(testingAD$Source.of.Admission == 19,1,0)
testing$Source.of.Admission.29 <- ifelse(testingAD$Source.of.Admission == 29,1,0)
testing$Source.of.Admission.51 <- ifelse(testingAD$Source.of.Admission == 51,1,0)
testing$Source.of.Admission.52 <- ifelse(testingAD$Source.of.Admission == 52,1,0)
testing$Source.of.Admission.54 <- ifelse(testingAD$Source.of.Admission == 54,1,0)
testing$Source.of.Admission.79 <- ifelse(testingAD$Source.of.Admission == 79,1,0)
testing$Source.of.Admission.85 <- ifelse(testingAD$Source.of.Admission == 85,1,0)
testing$Source.of.Admission.87 <- ifelse(testingAD$Source.of.Admission == 87,1,0)

training$From.Care.Home.No <- ifelse(trainingAD$From.Care.Home == 0,1,0)
training$From.Care.Home.Yes <- ifelse(trainingAD$From.Care.Home == 1,1,0)

testing$From.Care.Home.No <- ifelse(testingAD$From.Care.Home == 0,1,0)
testing$From.Care.Home.Yes <- ifelse(testingAD$From.Care.Home == 1,1,0)


training$Number.of.Diagnosis.Codes.0.3 <- ifelse(trainingAD$Number.of.Diagnosis.Codes>=0 & trainingAD$Number.of.Diagnosis.Codes<=3,1,0)
training$Number.of.Diagnosis.Codes.4.7 <- ifelse(trainingAD$Number.of.Diagnosis.Codes>=4 & trainingAD$Number.of.Diagnosis.Codes<=7,1,0)
training$Number.of.Diagnosis.Codes.8.11 <- ifelse(trainingAD$Number.of.Diagnosis.Codes>=8 & trainingAD$Number.of.Diagnosis.Codes<=11,1,0)
training$Number.of.Diagnosis.Codes.12.plus <- ifelse(trainingAD$Number.of.Diagnosis.Codes>=12,1,0)

testing$Number.of.Diagnosis.Codes.0.3 <- ifelse(testingAD$Number.of.Diagnosis.Codes>=0 & testingAD$Number.of.Diagnosis.Codes<=3,1,0)
testing$Number.of.Diagnosis.Codes.4.7 <- ifelse(testingAD$Number.of.Diagnosis.Codes>=4 & testingAD$Number.of.Diagnosis.Codes<=7,1,0)
testing$Number.of.Diagnosis.Codes.8.11 <- ifelse(testingAD$Number.of.Diagnosis.Codes>=8 & testingAD$Number.of.Diagnosis.Codes<=11,1,0)
testing$Number.of.Diagnosis.Codes.12.plus <- ifelse(testingAD$Number.of.Diagnosis.Codes>=12,1,0)


training$Intended.Management.Day.Case <- ifelse(trainingAD$Intended.Management=="DAY CASE",1,0)
training$Intended.Management.Inpatient <- ifelse(trainingAD$Intended.Management=="INPATIENT",1,0)
training$Intended.Management.None <- ifelse(trainingAD$Intended.Management=="No Intended Management",1,0)

testing$Intended.Management.Day.Case <- ifelse(testingAD$Intended.Management=="DAY CASE",1,0)
testing$Intended.Management.Inpatient <- ifelse(testingAD$Intended.Management=="INPATIENT",1,0)
testing$Intended.Management.None <- ifelse(testingAD$Intended.Management=="No Intended Management",1,0)

training$DaysFromAdmissionToProcedure.Unknown <- ifelse(is.na(trainingAD$DaysFromAdmissionToProcedure),1,0)
training$DaysFromAdmissionToProcedure.0.3 <- ifelse(trainingAD$DaysFromAdmissionToProcedure<=3,1,0)
training$DaysFromAdmissionToProcedure.4.7 <- ifelse(trainingAD$DaysFromAdmissionToProcedure>=4 & trainingAD$DaysFromAdmissionToProcedure<=7,1,0)
training$DaysFromAdmissionToProcedure.8.11 <- ifelse(trainingAD$DaysFromAdmissionToProcedure>=8 & trainingAD$DaysFromAdmissionToProcedure<=11,1,0)
training$DaysFromAdmissionToProcedure.12.plus <- ifelse(trainingAD$DaysFromAdmissionToProcedure>=12,1,0)

testing$DaysFromAdmissionToProcedure.Unknown <- ifelse(is.na(testingAD$DaysFromAdmissionToProcedure),1,0)
testing$DaysFromAdmissionToProcedure.0.3 <- ifelse(testingAD$DaysFromAdmissionToProcedure<=3,1,0)
testing$DaysFromAdmissionToProcedure.4.7 <- ifelse(testingAD$DaysFromAdmissionToProcedure>=4 & testingAD$DaysFromAdmissionToProcedure<=7,1,0)
testing$DaysFromAdmissionToProcedure.8.11 <- ifelse(testingAD$DaysFromAdmissionToProcedure>=8 & testingAD$DaysFromAdmissionToProcedure<=11,1,0)
testing$DaysFromAdmissionToProcedure.12.plus <- ifelse(testingAD$DaysFromAdmissionToProcedure>=12,1,0)

training$DaysFromProcedureToDischarge.Unknown <- ifelse(is.na(trainingAD$DaysFromProcedureToDischarge),1,0)
training$DaysFromProcedureToDischarge.0.3 <- ifelse(trainingAD$DaysFromProcedureToDischarge<=3,1,0)
training$DaysFromProcedureToDischarge.4.7 <- ifelse(trainingAD$DaysFromProcedureToDischarge>=4 & trainingAD$DaysFromProcedureToDischarge<=7,1,0)
training$DaysFromProcedureToDischarge.8.11 <- ifelse(trainingAD$DaysFromProcedureToDischarge>=8 & trainingAD$DaysFromProcedureToDischarge<=11,1,0)
training$DaysFromProcedureToDischarge.12.plus <- ifelse(trainingAD$DaysFromProcedureToDischarge>=12,1,0)

testing$DaysFromProcedureToDischarge.Unknown <- ifelse(is.na(testingAD$DaysFromProcedureToDischarge),1,0)
testing$DaysFromProcedureToDischarge.0.3 <- ifelse(testingAD$DaysFromProcedureToDischarge<=3,1,0)
testing$DaysFromProcedureToDischarge.4.7 <- ifelse(testingAD$DaysFromProcedureToDischarge>=4 & testingAD$DaysFromProcedureToDischarge<=7,1,0)
testing$DaysFromProcedureToDischarge.8.11 <- ifelse(testingAD$DaysFromProcedureToDischarge>=8 & testingAD$DaysFromProcedureToDischarge<=11,1,0)
testing$DaysFromProcedureToDischarge.12.plus <- ifelse(testingAD$DaysFromProcedureToDischarge>=12,1,0)

training$DayoftheWeek.Weekday <- ifelse(trainingAD$DayoftheWeek!= "Saturday" & trainingAD$DayoftheWeek!= "Sunday",1,0)
training$DayoftheWeek.Weekend <- ifelse(trainingAD$DayoftheWeek== "Saturday" & trainingAD$DayoftheWeek== "Sunday",1,0)

testing$DayoftheWeek.Weekday <- ifelse(testingAD$DayoftheWeek!= "Saturday" & testingAD$DayoftheWeek!= "Sunday",1,0)
testing$DayoftheWeek.Weekend <- ifelse(testingAD$DayoftheWeek== "Saturday" & testingAD$DayoftheWeek== "Sunday",1,0)


training$Admission.Dtm.Unknown <- ifelse(is.na(trainingAD$Admission.Dtm),1,0)
training$Admission.Dtm.0.4hours<-ifelse(as.numeric(trainingAD$Admission.Dtm)*0.00027777777<=4,1,0)
training$Admission.Dtm.4.8hours<-ifelse(as.numeric(trainingAD$Admission.Dtm)*0.00027777777 >4 & as.numeric(trainingAD$Admission.Dtm)*0.00027777777<=8,1,0)
training$Admission.Dtm.8.12hours<-ifelse(as.numeric(trainingAD$Admission.Dtm)*0.00027777777 >8 & as.numeric(trainingAD$Admission.Dtm)*0.00027777777<=12,1,0)
training$Admission.Dtm.12.16hours<-ifelse(as.numeric(trainingAD$Admission.Dtm)*0.00027777777 >12 & as.numeric(trainingAD$Admission.Dtm)*0.00027777777<=16,1,0)
training$Admission.Dtm.16.20hours<-ifelse(as.numeric(trainingAD$Admission.Dtm)*0.00027777777 >16 & as.numeric(trainingAD$Admission.Dtm)*0.00027777777<=20,1,0)
training$Admission.Dtm.20.24hours<-ifelse(as.numeric(trainingAD$Admission.Dtm)*0.00027777777>20& as.numeric(trainingAD$Admission.Dtm)*0.00027777777<=24,1,0)
training$Admission.Dtm.0.4hours <- ifelse(is.na(training$Admission.Dtm.0.4hours),0,training$Admission.Dtm.0.4hours)
training$Admission.Dtm.4.8hours <- ifelse(is.na(training$Admission.Dtm.4.8hours),0,training$Admission.Dtm.4.8hours)
training$Admission.Dtm.8.12hours <- ifelse(is.na(training$Admission.Dtm.8.12hours),0,training$Admission.Dtm.8.12hours)
training$Admission.Dtm.12.16hours <- ifelse(is.na(training$Admission.Dtm.12.16hours),0,training$Admission.Dtm.12.16hours)
training$Admission.Dtm.16.20hours <- ifelse(is.na(training$Admission.Dtm.16.20hours),0,training$Admission.Dtm.16.20hours)
training$Admission.Dtm.20.24hours <- ifelse(is.na(training$Admission.Dtm.20.24hours),0,training$Admission.Dtm.20.24hours)

testing$Admission.Dtm.Unknown <- ifelse(is.na(testingAD$Admission.Dtm),1,0)
testing$Admission.Dtm.0.4hours<-ifelse(as.numeric(testingAD$Admission.Dtm)*0.00027777777<=4,1,0)
testing$Admission.Dtm.4.8hours<-ifelse(as.numeric(testingAD$Admission.Dtm)*0.00027777777 >4 & as.numeric(testingAD$Admission.Dtm)*0.00027777777<=8,1,0)
testing$Admission.Dtm.8.12hours<-ifelse(as.numeric(testingAD$Admission.Dtm)*0.00027777777 >8 & as.numeric(testingAD$Admission.Dtm)*0.00027777777<=12,1,0)
testing$Admission.Dtm.12.16hours<-ifelse(as.numeric(testingAD$Admission.Dtm)*0.00027777777 >12 & as.numeric(testingAD$Admission.Dtm)*0.00027777777<=16,1,0)
testing$Admission.Dtm.16.20hours<-ifelse(as.numeric(testingAD$Admission.Dtm)*0.00027777777 >16 & as.numeric(testingAD$Admission.Dtm)*0.00027777777<=20,1,0)
testing$Admission.Dtm.20.24hours<-ifelse(as.numeric(testingAD$Admission.Dtm)*0.00027777777>20& as.numeric(testingAD$Admission.Dtm)*0.00027777777<=24,1,0)
testing$Admission.Dtm.0.4hours <- ifelse(is.na(testing$Admission.Dtm.0.4hours),0,testing$Admission.Dtm.0.4hours)
testing$Admission.Dtm.4.8hours <- ifelse(is.na(testing$Admission.Dtm.4.8hours),0,testing$Admission.Dtm.4.8hours)
testing$Admission.Dtm.8.12hours <- ifelse(is.na(testing$Admission.Dtm.8.12hours),0,testing$Admission.Dtm.8.12hours)
testing$Admission.Dtm.12.16hours <- ifelse(is.na(testing$Admission.Dtm.12.16hours),0,testing$Admission.Dtm.12.16hours)
testing$Admission.Dtm.16.20hours <- ifelse(is.na(testing$Admission.Dtm.16.20hours),0,testing$Admission.Dtm.16.20hours)
testing$Admission.Dtm.20.24hours <- ifelse(is.na(testing$Admission.Dtm.20.24hours),0,testing$Admission.Dtm.20.24hours)


training$Age.on.Arrival.0.18<- ifelse(trainingAD$Age.on.Arrival <=18,1,0)
training$Age.on.Arrival.19.35<- ifelse(trainingAD$Age.on.Arrival>18 & trainingAD$Age.on.Arrival <=35,1,0)
training$Age.on.Arrival.36.50<- ifelse(trainingAD$Age.on.Arrival>35 & trainingAD$Age.on.Arrival <=50,1,0)
training$Age.on.Arrival.51.80<- ifelse(trainingAD$Age.on.Arrival>50 & trainingAD$Age.on.Arrival <=80,1,0)
training$Age.on.Arrival.80.90<- ifelse(trainingAD$Age.on.Arrival>80 & trainingAD$Age.on.Arrival <=90,1,0)
training$Age.on.Arrival.90.plus<- ifelse(trainingAD$Age.on.Arrival>90,1,0)

testing$Age.on.Arrival.0.18<- ifelse(testingAD$Age.on.Arrival <=18,1,0)
testing$Age.on.Arrival.19.35<- ifelse(testingAD$Age.on.Arrival>18 & testingAD$Age.on.Arrival <=35,1,0)
testing$Age.on.Arrival.36.50<- ifelse(testingAD$Age.on.Arrival>35 & testingAD$Age.on.Arrival <=50,1,0)
testing$Age.on.Arrival.51.80<- ifelse(testingAD$Age.on.Arrival>50 & testingAD$Age.on.Arrival <=80,1,0)
testing$Age.on.Arrival.80.90<- ifelse(testingAD$Age.on.Arrival>80 & testingAD$Age.on.Arrival <=90,1,0)
testing$Age.on.Arrival.90.plus<- ifelse(testingAD$Age.on.Arrival>90,1,0)

#training$Division.Unknown <- ifelse(trainingAD$Division == "0",1,0)
training$Division.ChildrensServices <- ifelse(trainingAD$Division == "Children's Services",1,0)
training$Division.Emergency <- ifelse(trainingAD$Division == "Emergency and Urgent Care",1,0)
training$Division.Medicine.Frailty.Networked <- ifelse(trainingAD$Division == "Medicine, Frailty and Networked Services",1,0)
training$Division.Outpatient.Prevention.LongTerm <- ifelse(trainingAD$Division == "Outpatient, Prevention and Long Term Conditions",1,0)
training$Division.Surgery <- ifelse(trainingAD$Division == "Surgery",1,0)
training$Division.Women.FamilyServices <- ifelse(trainingAD$Division == "Women and Family Services",1,0)

#testing$Division.Unknown <- ifelse(testingAD$Division == "0",1,0)
testing$Division.ChildrensServices <- ifelse(testingAD$Division == "Children's Services",1,0)
testing$Division.Emergency <- ifelse(testingAD$Division == "Emergency and Urgent Care",1,0)
testing$Division.Medicine.Frailty.Networked <- ifelse(testingAD$Division == "Medicine, Frailty and Networked Services",1,0)
testing$Division.Outpatient.Prevention.LongTerm <- ifelse(testingAD$Division == "Outpatient, Prevention and Long Term Conditions",1,0)
testing$Division.Surgery <- ifelse(testingAD$Division == "Surgery",1,0)
testing$Division.Women.FamilyServices <- ifelse(testingAD$Division == "Women and Family Services",1,0)


#training$Procedure.Unknown <- ifelse(trainingAD$SimplifiedCode == "No SimplifiedCode",1,0)
training$Procedure.Nervous<- ifelse(trainingAD$SimplifiedCode == "Nervous System",1,0)
training$Procedure.EndocrineAndBreast<- ifelse(trainingAD$SimplifiedCode == "Endocrine System and Breast",1,0)
training$Procedure.Eye<- ifelse(trainingAD$SimplifiedCode == "Eye",1,0)
training$Procedure.Ear<- ifelse(trainingAD$SimplifiedCode == "Ear",1,0)
training$Procedure.Respiratory<- ifelse(trainingAD$SimplifiedCode == "Respiratory Tract",1,0)
training$Procedure.Mouth <- ifelse(trainingAD$SimplifiedCode == "Mouth",1,0)
training$Procedure.UpperDigestive<- ifelse(trainingAD$SimplifiedCode == "Upper Digestive System",1,0)
training$Procedure.LowerDigestive<- ifelse(trainingAD$SimplifiedCode == "Lower Digestive System",1,0)
training$Procedure.Abdominal<- ifelse(trainingAD$SimplifiedCode == "Other Abdominal Organs, Principally Digestive",1,0)
training$Procedure.Heart<- ifelse(trainingAD$SimplifiedCode == "Heart",1,0)
training$Procedure.Arteries<- ifelse(trainingAD$SimplifiedCode == "Arteries and Veins",1,0)
training$Procedure.Urinary<- ifelse(trainingAD$SimplifiedCode == "Urinary",1,0)
training$Procedure.MaleGenitals<- ifelse(trainingAD$SimplifiedCode == "Male Genital Organs",1,0)
training$Procedure.LowerFemaleGenitals<- ifelse(trainingAD$SimplifiedCode == "Lower Female Genital Tract",1,0)
training$Procedure.UpperFemaleGenitals<- ifelse(trainingAD$SimplifiedCode == "Upper Female Genital Tract",1,0)
training$Procedure.FemaleGenitalPregnancyChildbirth<- ifelse(trainingAD$SimplifiedCode == "Female Genital Tract Associated with Pregnancy, Childbirth and the Puerperium",1,0)
training$Procedure.Skin <- ifelse(trainingAD$SimplifiedCode == "Skin",1,0)
training$Procedure.SoftTissue<- ifelse(trainingAD$SimplifiedCode == "Soft Tissue",1,0)
training$Procedure.DiagnosticImagingRehab<- ifelse(trainingAD$SimplifiedCode == "Diagnostic Imaging, Testing and Rehabilitation",1,0)
training$Procedure.BonesJoints<- ifelse(trainingAD$SimplifiedCode == "Bones and Joints of Skull and Spine",1,0)
training$Procedure.OtherBonesJoints<- ifelse(trainingAD$SimplifiedCode == "Other Bones and Joints",1,0)
training$Procedure.Miscellaneous<- ifelse(trainingAD$SimplifiedCode == "Miscellaneous Operations",1,0)
training$Procedure.SubsidiaryClassificationMethods <- ifelse(trainingAD$SimplifiedCode == "Subsidiary Classification of Methods of Operation",1,0)
training$Procedure.SubsidiaryClassificationSites <- ifelse(trainingAD$SimplifiedCode == "Subsidiary Classification of Sites of Operation",1,0)



#testing$Procedure.Unknown <- ifelse(testingAD$SimplifiedCode == "No SimplifiedCode",1,0)
testing$Procedure.Nervous<- ifelse(testingAD$SimplifiedCode == "Nervous System",1,0)
testing$Procedure.EndocrineAndBreast<- ifelse(testingAD$SimplifiedCode == "Endocrine System and Breast",1,0)
testing$Procedure.Eye<- ifelse(testingAD$SimplifiedCode == "Eye",1,0)
testing$Procedure.Ear<- ifelse(testingAD$SimplifiedCode == "Ear",1,0)
testing$Procedure.Respiratory<- ifelse(testingAD$SimplifiedCode == "Respiratory Tract",1,0)
testing$Procedure.Mouth <- ifelse(testingAD$SimplifiedCode == "Mouth",1,0)
testing$Procedure.UpperDigestive<- ifelse(testingAD$SimplifiedCode == "Upper Digestive System",1,0)
testing$Procedure.LowerDigestive<- ifelse(testingAD$SimplifiedCode == "Lower Digestive System",1,0)
testing$Procedure.Abdominal<- ifelse(testingAD$SimplifiedCode == "Other Abdominal Organs, Principally Digestive",1,0)
testing$Procedure.Heart<- ifelse(testingAD$SimplifiedCode == "Heart",1,0)
testing$Procedure.Arteries<- ifelse(testingAD$SimplifiedCode == "Arteries and Veins",1,0)
testing$Procedure.Urinary<- ifelse(testingAD$SimplifiedCode == "Urinary",1,0)
testing$Procedure.MaleGenitals<- ifelse(testingAD$SimplifiedCode == "Male Genital Organs",1,0)
testing$Procedure.LowerFemaleGenitals<- ifelse(testingAD$SimplifiedCode == "Lower Female Genital Tract",1,0)
testing$Procedure.UpperFemaleGenitals<- ifelse(testingAD$SimplifiedCode == "Upper Female Genital Tract",1,0)
testing$Procedure.FemaleGenitalPregnancyChildbirth<- ifelse(testingAD$SimplifiedCode == "Female Genital Tract Associated with Pregnancy, Childbirth and the Puerperium",1,0)
testing$Procedure.Skin <- ifelse(testingAD$SimplifiedCode == "Skin",1,0)
testing$Procedure.SoftTissue<- ifelse(testingAD$SimplifiedCode == "Soft Tissue",1,0)
testing$Procedure.DiagnosticImagingRehab<- ifelse(testingAD$SimplifiedCode == "Diagnostic Imaging, Testing and Rehabilitation",1,0)
testing$Procedure.BonesJoints<- ifelse(testingAD$SimplifiedCode == "Bones and Joints of Skull and Spine",1,0)
testing$Procedure.OtherBonesJoints<- ifelse(testingAD$SimplifiedCode == "Other Bones and Joints",1,0)
testing$Procedure.Miscellaneous<- ifelse(testingAD$SimplifiedCode == "Miscellaneous Operations",1,0)
testing$Procedure.SubsidiaryClassificationMethods <- ifelse(testingAD$SimplifiedCode == "Subsidiary Classification of Methods of Operation",1,0)
testing$Procedure.SubsidiaryClassificationSites <- ifelse(testingAD$SimplifiedCode == "Subsidiary Classification of Sites of Operation",1,0)


training$Specialty.AccidentEmergency<- ifelse(trainingAD$Specialty == "Accident & Emergency",1,0)
training$Specialty.Anaesthetics<- ifelse(trainingAD$Specialty == "Anaesthetics",1,0)
training$Specialty.Cardiology<- ifelse(trainingAD$Specialty == "Cardiology",1,0)
training$Specialty.Dental<- ifelse(trainingAD$Specialty == "Dental Medicine Specialties",1,0)
training$Specialty.ENT<- ifelse(trainingAD$Specialty == "ENT",1,0)
training$Specialty.Gastroenterology<- ifelse(trainingAD$Specialty == "Gastroenterology",1,0)
training$Specialty.Obstetrics<- ifelse(trainingAD$Specialty == "Obstetrics for patients using a hospital bed or Delivery Facilities",1,0)
training$Specialty.Ophthalmology<- ifelse(trainingAD$Specialty == "Ophthalmology",1,0)
training$Specialty.Paediatrics<- ifelse(trainingAD$Specialty == "Paediatrics",1,0)
training$Specialty.Trauma<- ifelse(trainingAD$Specialty == "Trauma & Orthopaedics",1,0)
training$Specialty.Urology<- ifelse(trainingAD$Specialty == "Urology",1,0)

testing$Specialty.AccidentEmergency<- ifelse(testingAD$Specialty == "Accident & Emergency",1,0)
testing$Specialty.Anaesthetics<- ifelse(testingAD$Specialty == "Anaesthetics",1,0)
testing$Specialty.Cardiology<- ifelse(testingAD$Specialty == "Cardiology",1,0)
testing$Specialty.Dental<- ifelse(testingAD$Specialty == "Dental Medicine Specialties",1,0)
testing$Specialty.ENT<- ifelse(testingAD$Specialty == "ENT",1,0)
testing$Specialty.Gastroenterology<- ifelse(testingAD$Specialty == "Gastroenterology",1,0)
testing$Specialty.Obstetrics<- ifelse(testingAD$Specialty == "Obstetrics for patients using a hospital bed or Delivery Facilities",1,0)
testing$Specialty.Ophthalmology<- ifelse(testingAD$Specialty == "Ophthalmology",1,0)
testing$Specialty.Paediatrics<- ifelse(testingAD$Specialty == "Paediatrics",1,0)
testing$Specialty.Trauma<- ifelse(testingAD$Specialty == "Trauma & Orthopaedics",1,0)
testing$Specialty.Urology<- ifelse(testingAD$Specialty == "Urology",1,0)


training<-data.frame(training)
train<-training[,c(32,42:ncol(training))]

testing<-data.frame(testing)
test<-testing[,c(32,42:ncol(training))]



library(mlbench)
library(gbm)
control <- trainControl(method="cv",
                        number=10,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)
caret_fit <- train(make.names(factor(Readmissions))~.,
                   data = train,
                   method = "rpart",
                   tuneLength = 10,
                   metric = "Spec",
                   #maximize = TRUE,
                   # control = rpart.control(minsplit = 2,
                   #                          cp = 0.0006),
                   na.action = na.exclude,
                   # Optimized cp value based on plotError in na.fail.default(list(`make.names(factor(Readmissions))` = c("X0",  :

                   # plot(caret_fit, xlim = c(0, 0.0001))
                   trControl=control)



save(caret_fit, file = "model_rpart.rda")
save(fit, file = "model_naiveBayes.rda")
