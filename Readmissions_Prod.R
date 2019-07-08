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

load("model_rpart.rda")
load("model_naiveBayes.rda")



Prod <- read_csv("./Inpatients_Readmissions_Prod.csv", 
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

Prod$Readmissions <-ifelse(Prod$DaystoReadmission>30,1,0)
Prod$Readmissions <- ifelse(is.na(Prod$DaystoReadmission),0,Prod$Readmissions)
Prod$Readmissions <- ifelse(is.na(Prod$Readmissions),0,Prod$Readmissions)

names(Prod)<-make.names(names(Prod))

Prod$Patient.ID<- NULL
Prod$DaystoReadmission<- NULL
Prod$Days.to.Readmission<- NULL
Prod$Readmission.Part<- NULL
Prod$Readmission.Part.Exclusions<- NULL
Prod$Readmission.Spells<-NULL
Prod$Readmission.Spells.Exclusions<-NULL
Prod$Readmitted.Same.Consultant<-ifelse(is.na(Prod$Readmitted.Same.Consultant),0,Prod$Readmitted.Same.Consultant)
Prod$Readmitted.Same.Consultant.Exclusions<-ifelse(is.na(Prod$Readmitted.Same.Consultant.Exclusions),0,Prod$Readmitted.Same.Consultant.Exclusions)
Prod$Readmitted.Same.Diagnosis.Group<-ifelse(is.na(Prod$Readmitted.Same.Diagnosis.Group),0,Prod$Readmitted.Same.Diagnosis.Group)
Prod$Readmitted.Same.Specialty<-ifelse(is.na(Prod$Readmitted.Same.Specialty),0,Prod$Readmitted.Same.Specialty)
Prod$Readmitted.Same.Diagnosis.Group.Exclusions<-ifelse(is.na(Prod$Readmitted.Same.Diagnosis.Group.Exclusions),0,Prod$Readmitted.Same.Diagnosis.Group.Exclusions)
Prod$Readmitted.Same.HRG<-ifelse(is.na(Prod$Readmitted.Same.HRG),0,Prod$Readmitted.Same.HRG)
Prod$Readmitted.Same.HRG.Exclusions<-ifelse(is.na(Prod$Readmitted.Same.HRG.Exclusions),0,Prod$Readmitted.Same.HRG.Exclusions)
Prod$Readmitted.Same.Primary.Diagnosis<-ifelse(is.na(Prod$Readmitted.Same.Primary.Diagnosis),0,Prod$Readmitted.Same.Primary.Diagnosis)
Prod$Readmitted.Same.Primary.Diagnosis.Exclusions<-ifelse(is.na(Prod$Readmitted.Same.Primary.Diagnosis.Exclusions),0,Prod$Readmitted.Same.Primary.Diagnosis.Exclusions)
Prod$Readmitted.Same.Specialty.Exclusions<-ifelse(is.na(Prod$Readmitted.Same.Specialty.Exclusions),0,Prod$Readmitted.Same.Specialty.Exclusions)
Prod$Readmitted.Same.Ward<-ifelse(is.na(Prod$Readmitted.Same.Ward),0,Prod$Readmitted.Same.Ward)
Prod$Readmitted.Same.Ward.Exclusions<-ifelse(is.na(Prod$Readmitted.Same.Ward.Exclusions),0,Prod$Readmitted.Same.Ward.Exclusions)
Prod$Readmitted.Within.30.Days<-ifelse(is.na(Prod$Readmitted.Within.30.Days),0,Prod$Readmitted.Within.30.Days)
Prod$Readmitted.Within.30.Days.Exclusions<-NULL
Prod<-Prod[, -grep("Parent", colnames(Prod))]
Prod<-Prod[, -grep("Exclude", colnames(Prod))]
Prod$Death.on.Readmission <- NULL
Prod$Patient.Readmitted<-NULL
Prod$Patient.Readmitted.Exclusions<-NULL

Prod$fk_PersonGenderCode<-ifelse(is.na(Prod$fk_PersonGenderCode),0,Prod$fk_PersonGenderCode)

Prod$From.Care.Home <- ifelse(Prod$From.Care.Home=="Yes",1,0)

Prod$Last.Episode.In.Spell.Indicator <- NULL
Prod$Local.Specialty.Code<-NULL
Prod$Admission.Method<- NULL

Prod$Patient.Class <- NULL
Prod$Patient.Name<-NULL

Prod$InpatientAdmissionsDays <-NULL

Prod$Length.of.Stay<- ifelse(is.na(Prod$Length.of.Stay),0,Prod$Length.of.Stay)

Prod$TH.Procedure.Code<- NULL
Prod$WL.Procedure.Code<- NULL
Prod$WL.Procedure.Date<- NULL
Prod$ReadmitFlag<-NULL
Prod$Adult.or.Child<-NULL
Prod$DaystoReadmissions<-NULL
Prod$IPAdmissionDays<-NULL
Prod$Days.to.Readmission.Exclusions <- NULL
Prod$IPFlag <-NULL
Prod$Spell.HRG<-NULL


Prod <- Prod[!is.na(Prod),]

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

setnames(Prod,"Procedure.Code","fk_ProcedureCode")

setnames(Prod, "Consultant.Treatment.Function.Code", "fk_SpecialtyCode")

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

Prod<- left_join(Prod, Proc_codes)
Prod<-left_join(Prod,Specialty_codes)
Prod<-left_join(Prod,Surgeon_Codes)
Prod<-left_join(Prod,IntendedManagement_Codes)

Prod$fk_MainSpecialtyCode<-NULL
Prod$fk_SpecialtyCode<-NULL
Prod$fk_ProcedureCode<-NULL
Prod$fk_SurgeonCode<-NULL
Prod$fk_IntendedManagementCode<-NULL
Prod$fk_TreatmentSpecialtyCode<-NULL
Prod$fk_MainSpecialtyCode<-NULL
Prod$Main.Specialty.Code<-NULL

Prod$Procedure<-ifelse(is.na(Prod$Procedure), "No Procedure", Prod$Procedure)
Prod$SimplifiedCode<-ifelse(is.na(Prod$SimplifiedCode), "No SimplifiedCode", Prod$SimplifiedCode)
Prod$Specialty<-ifelse(is.na(Prod$Specialty), "No Specialty", Prod$Specialty)
Prod$Surgeon<-ifelse(is.na(Prod$Surgeon), "No Surgeon", Prod$Surgeon)
Prod$Division<-ifelse(is.na(Prod$Division), "No Division", Prod$Division)
Prod$`Intended Management`<-ifelse(is.na(Prod$`Intended Management`), "No Intended Management", Prod$`Intended Management`)
Prod$Age.Type<-NULL
Prod$Operation.Date<-NULL
Prod$Diagnosis.Group<-NULL
Prod$Diagnosis.Group.Num<-NULL
Prod$Episode.Number <- NULL


Prod$DaysFromProcedureToDischarge<- (as.numeric(as.POSIXct(Prod$Procedure.Date)) - as.numeric(as.POSIXct(Prod$Discharge.Date)))/86400
Prod$DaysFromAdmissionToProcedure<- (as.numeric(as.POSIXct(Prod$Admission.Date)) - as.numeric(as.POSIXct(Prod$Procedure.Date)))/86400
Prod<-left_join(Prod,Prod %>%
                  group_by(Pat_ID_Key) %>%
                  summarise(NumberofReadmissions=sum(Flag=="RA")))
Prod$DayoftheWeek <- weekdays(as.Date(Prod$Admission.Date))

Prod$Admission.Date<-NULL
Prod$Discharge.Date<-NULL
Prod$Procedure.Date<-NULL

names(Prod)<-make.names(names(Prod))

library(readr)
IP <- data.table(cbind(Prod$Pat_ID_Key,Prod$Primary.Diagnosis.Code))



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

tst<- unique(tst)
setnames(tst, "V1","Pat_ID_Key")
tst<-aggregate(. ~ Pat_ID_Key, tst, sum)
tst<- cbind(tst[,1],apply(tst[,2:18], 2, function(x) ifelse(x > 1, 1, x)))
tst<- data.frame(tst)
setnames(tst, "V1","Pat_ID_Key")


Prod<- left_join(Prod,tst,by="Pat_ID_Key")



Prod$Specialty<- ifelse(is.na(Prod$Specialty),0,Prod$Specialty)
Prod$Division<- ifelse(is.na(Prod$Division),0,Prod$Division)
Prod$Procedure <- ifelse(is.na(Prod$Procedure),0,Prod$Procedure)




Prod$Readmissions <- ifelse(is.na(Prod$Readmissions),0,Prod$Readmissions)



load("Readmissions_Training.rda")

Prod[c("MI", "CHF","PVD","Stroke","Dementia","Pulmonary","Rheumatic","PUD","LiverMild","DM","DMcx","Paralysis","Renal","Cancer","LiverSevere","Mets","HIV")][is.na(Prod[c("MI", "CHF","PVD","Stroke","Dementia","Pulmonary","Rheumatic","PUD","LiverMild","DM","DMcx","Paralysis","Renal","Cancer","LiverSevere","Mets","HIV")])] <- 0


testingAD<-  Prod
testingAD<- testingAD[,names(testingAD) %in% names(trainingAD)]


library(dplyr)

testingAD <- data.frame(testingAD)

predicted <- predict(fit, testingAD, type = "raw")
predicted<- data.frame(predicted)




testing<-testingAD


testing$Length.of.Stay.0.3 <-ifelse(testingAD$Length.of.Stay<=3,1,0)
testing$Length.of.Stay.4.7<-ifelse(testingAD$Length.of.Stay>3 & testingAD$Length.of.Stay<=7,1,0)
testing$Length.of.Stay.8.14<-ifelse(testingAD$Length.of.Stay>7 & testingAD$Length.of.Stay<=14,1,0)
testing$Length.of.Stay.15.21<-ifelse(testingAD$Length.of.Stay>14 & testingAD$Length.of.Stay<=21,1,0)
testing$Length.of.Stay.22.plus<-ifelse(testingAD$Length.of.Stay>=22,1,0)


testing$fk_PersonGenderCode.Other<-ifelse(testingAD$fk_PersonGenderCode!=1 & testingAD$fk_PersonGenderCode!=2,1,0)
testing$fk_PersonGenderCode.Male <- ifelse(testingAD$fk_PersonGenderCode==1,1,0)
testing$fk_PersonGenderCode.Female <- ifelse(testingAD$fk_PersonGenderCode==2,1,0)


testing$Admission.Method.Full.Elective<- ifelse(testingAD$Admission.Method.Full=="Elective",1,0)
testing$Admission.Method.Full.Emergency <- ifelse(testingAD$Admission.Method.Full=="Emergency",1,0)
testing$Admission.Method.Full.Maternity <- ifelse(testingAD$Admission.Method.Full=="Maternity",1,0) 
testing$Admission.Method.Full.Other <- ifelse(testingAD$Admission.Method.Full=="Other",1,0)


testing$Discharge.Method.Code.1 <- ifelse(testingAD$Discharge.Method.Code == 1,1,0)
testing$Discharge.Method.Code.2 <- ifelse(testingAD$Discharge.Method.Code == 2,1,0)
testing$Discharge.Method.Code.4 <- ifelse(testingAD$Discharge.Method.Code == 4,1,0)
testing$Discharge.Method.Code.5 <- ifelse(testingAD$Discharge.Method.Code == 5,1,0)


testing$Patient.Classification.Code.1<- ifelse(testingAD$Patient.Classification.Code == 1,1,0)
testing$Patient.Classification.Code.2<- ifelse(testingAD$Patient.Classification.Code == 2,1,0)
testing$Patient.Classification.Code.5<- ifelse(testingAD$Patient.Classification.Code == 5,1,0)
testing$Patient.Classification.Code.8<- ifelse(testingAD$Patient.Classification.Code == 8,1,0)


testing$Source.of.Admission.19 <- ifelse(testingAD$Source.of.Admission == 19,1,0)
testing$Source.of.Admission.29 <- ifelse(testingAD$Source.of.Admission == 29,1,0)
testing$Source.of.Admission.51 <- ifelse(testingAD$Source.of.Admission == 51,1,0)
testing$Source.of.Admission.52 <- ifelse(testingAD$Source.of.Admission == 52,1,0)
testing$Source.of.Admission.54 <- ifelse(testingAD$Source.of.Admission == 54,1,0)
testing$Source.of.Admission.79 <- ifelse(testingAD$Source.of.Admission == 79,1,0)
testing$Source.of.Admission.85 <- ifelse(testingAD$Source.of.Admission == 85,1,0)
testing$Source.of.Admission.87 <- ifelse(testingAD$Source.of.Admission == 87,1,0)


testing$From.Care.Home.No <- ifelse(testingAD$From.Care.Home == 0,1,0)
testing$From.Care.Home.Yes <- ifelse(testingAD$From.Care.Home == 1,1,0)



testing$Number.of.Diagnosis.Codes.0.3 <- ifelse(testingAD$Number.of.Diagnosis.Codes>=0 & testingAD$Number.of.Diagnosis.Codes<=3,1,0)
testing$Number.of.Diagnosis.Codes.4.7 <- ifelse(testingAD$Number.of.Diagnosis.Codes>=4 & testingAD$Number.of.Diagnosis.Codes<=7,1,0)
testing$Number.of.Diagnosis.Codes.8.11 <- ifelse(testingAD$Number.of.Diagnosis.Codes>=8 & testingAD$Number.of.Diagnosis.Codes<=11,1,0)
testing$Number.of.Diagnosis.Codes.12.plus <- ifelse(testingAD$Number.of.Diagnosis.Codes>=12,1,0)


testing$Intended.Management.Day.Case <- ifelse(testingAD$Intended.Management=="DAY CASE",1,0)
testing$Intended.Management.Inpatient <- ifelse(testingAD$Intended.Management=="INPATIENT",1,0)
testing$Intended.Management.None <- ifelse(testingAD$Intended.Management=="No Intended Management",1,0)

testing$DaysFromAdmissionToProcedure.Unknown <- ifelse(is.na(testingAD$DaysFromAdmissionToProcedure),1,0)
testing$DaysFromAdmissionToProcedure.0.3 <- ifelse(testingAD$DaysFromAdmissionToProcedure<=3,1,0)
testing$DaysFromAdmissionToProcedure.4.7 <- ifelse(testingAD$DaysFromAdmissionToProcedure>=4 & testingAD$DaysFromAdmissionToProcedure<=7,1,0)
testing$DaysFromAdmissionToProcedure.8.11 <- ifelse(testingAD$DaysFromAdmissionToProcedure>=8 & testingAD$DaysFromAdmissionToProcedure<=11,1,0)
testing$DaysFromAdmissionToProcedure.12.plus <- ifelse(testingAD$DaysFromAdmissionToProcedure>=12,1,0)

testing$DaysFromProcedureToDischarge.Unknown <- ifelse(is.na(testingAD$DaysFromProcedureToDischarge),1,0)
testing$DaysFromProcedureToDischarge.0.3 <- ifelse(testingAD$DaysFromProcedureToDischarge<=3,1,0)
testing$DaysFromProcedureToDischarge.4.7 <- ifelse(testingAD$DaysFromProcedureToDischarge>=4 & testingAD$DaysFromProcedureToDischarge<=7,1,0)
testing$DaysFromProcedureToDischarge.8.11 <- ifelse(testingAD$DaysFromProcedureToDischarge>=8 & testingAD$DaysFromProcedureToDischarge<=11,1,0)
testing$DaysFromProcedureToDischarge.12.plus <- ifelse(testingAD$DaysFromProcedureToDischarge>=12,1,0)


testing$DayoftheWeek.Weekday <- ifelse(testingAD$DayoftheWeek!= "Saturday" & testingAD$DayoftheWeek!= "Sunday",1,0)
testing$DayoftheWeek.Weekend <- ifelse(testingAD$DayoftheWeek== "Saturday" & testingAD$DayoftheWeek== "Sunday",1,0)


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


testing$Age.on.Arrival.0.18<- ifelse(testingAD$Age.on.Arrival <=18,1,0)
testing$Age.on.Arrival.19.35<- ifelse(testingAD$Age.on.Arrival>18 & testingAD$Age.on.Arrival <=35,1,0)
testing$Age.on.Arrival.36.50<- ifelse(testingAD$Age.on.Arrival>35 & testingAD$Age.on.Arrival <=50,1,0)
testing$Age.on.Arrival.51.80<- ifelse(testingAD$Age.on.Arrival>50 & testingAD$Age.on.Arrival <=80,1,0)
testing$Age.on.Arrival.80.90<- ifelse(testingAD$Age.on.Arrival>80 & testingAD$Age.on.Arrival <=90,1,0)
testing$Age.on.Arrival.90.plus<- ifelse(testingAD$Age.on.Arrival>90,1,0)


#testing$Division.Unknown <- ifelse(testingAD$Division == "0",1,0)
testing$Division.ChildrensServices <- ifelse(testingAD$Division == "Children's Services",1,0)
testing$Division.Emergency <- ifelse(testingAD$Division == "Emergency and Urgent Care",1,0)
testing$Division.Medicine.Frailty.Networked <- ifelse(testingAD$Division == "Medicine, Frailty and Networked Services",1,0)
testing$Division.Outpatient.Prevention.LongTerm <- ifelse(testingAD$Division == "Outpatient, Prevention and Long Term Conditions",1,0)
testing$Division.Surgery <- ifelse(testingAD$Division == "Surgery",1,0)
testing$Division.Women.FamilyServices <- ifelse(testingAD$Division == "Women and Family Services",1,0)



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



testing<-data.frame(testing)
test<-testing[,c(32,42:ncol(training))]






prediction <- predicted

setnames(prediction, c("X0","X1"), c("0","1"))

a<-factor(testingAD$Readmissions)
t<-ifelse(prediction$`1`>0.5,1,0)
t<- factor(t, levels = c("0","1"))
a<-factor(a, levels = levels(t))
confusionMatrix(t,a)

predictedOutput <- cbind(testing, prediction)

save(predictedOutput, file= "predictedOutputNOImpVar.rda")



m<- data.table(cbind(predictedOutput$`0`,predictedOutput$`1`))

setnames(m, c("V1","V2"),c("X0","X1"))
# names(predictedOutput)<- make.names(names(predictedOutput))


variableList<-t(data.frame(varImp(caret_fit)$importance))
variableList <- data.table(variableList)
variableList<-variableList[, names(variableList) := lapply(.SD, as.character)]
variableList <- sort(variableList, decreasing = TRUE)


for(i in 1:length(names(variableList))){
  variableList[1,i]<- names(variableList[1,..i])
}


not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

variableList<-variableList %>%
  select_if(not_any_na)


m<-cbind(m, test)
# We generate an extra column with the likeliness of being readmitted
m <- data.frame(m)


m$VeryUnlikely<- ifelse(m$X1<0.2,1,0)
m$Unlikely <- ifelse(m$X1>=0.2 & m$X1<0.4,1,0)
m$Possible <- ifelse(m$X1>=0.4 & m$X1<0.6,1,0)
m$Likely <- ifelse(m$X1>=0.6 & m$X1<0.8,1,0)
m$VeryLikely<- ifelse(m$X1>0.8,1,0)


m$ImportantVariables <- ifelse((m$Possible ==1|m$Likely==1|m$VeryLikely==1|m$X1>0.4),
                               paste0(
                                 ifelse(eval(parse(text = paste("m", variableList[1,1], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,1], sep="$")))<10000,
                                        paste0(variableList[1,1]),ifelse(eval(parse(text = paste("m", variableList[1,1], sep="$")))>=1,paste0(variableList[1,1], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,2], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,2], sep="$")))<10000,
                                        paste0(variableList[1,2]),ifelse(eval(parse(text = paste("m", variableList[1,2], sep="$")))>=1,paste0(variableList[1,2], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,3], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,3], sep="$")))<10000,
                                        paste0(variableList[1,3]),ifelse(eval(parse(text = paste("m", variableList[1,3], sep="$")))>=1,paste0(variableList[1,3], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,4], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,4], sep="$")))<10000,
                                        paste0(variableList[1,4]),ifelse(eval(parse(text = paste("m", variableList[1,4], sep="$")))>=1,paste0(variableList[1,4], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,5], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,5], sep="$")))<10000,
                                        paste0(variableList[1,5]),ifelse(eval(parse(text = paste("m", variableList[1,5], sep="$")))>=1,paste0(variableList[1,5], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,6], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,6], sep="$")))<10000,
                                        paste0(variableList[1,6]),ifelse(eval(parse(text = paste("m", variableList[1,6], sep="$")))>=1,paste0(variableList[1,6], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,7], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,7], sep="$")))<10000,
                                        paste0(variableList[1,7]),ifelse(eval(parse(text = paste("m", variableList[1,7], sep="$")))>=1,paste0(variableList[1,7], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,8], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,8], sep="$")))<10000,
                                        paste0(variableList[1,8]),ifelse(eval(parse(text = paste("m", variableList[1,8], sep="$")))>=1,paste0(variableList[1,8], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,9], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,9], sep="$")))<10000,
                                        paste0(variableList[1,9]),ifelse(eval(parse(text = paste("m", variableList[1,9], sep="$")))>=1,paste0(variableList[1,9], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,10], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,10], sep="$")))<10000,
                                        paste0(variableList[1,10]),ifelse(eval(parse(text = paste("m", variableList[1,10], sep="$")))>=1,paste0(variableList[1,10], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,11], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,11], sep="$")))<10000,
                                        paste0(variableList[1,11]),ifelse(eval(parse(text = paste("m", variableList[1,11], sep="$")))>=1,paste0(variableList[1,11], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,12], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,12], sep="$")))<10000,
                                        paste0(variableList[1,12]),ifelse(eval(parse(text = paste("m", variableList[1,12], sep="$")))>=1,paste0(variableList[1,12], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,13], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,13], sep="$")))<10000,
                                        paste0(variableList[1,13]),ifelse(eval(parse(text = paste("m", variableList[1,13], sep="$")))>=1,paste0(variableList[1,13], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,14], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,14], sep="$")))<10000,
                                        paste0(variableList[1,14]),ifelse(eval(parse(text = paste("m", variableList[1,14], sep="$")))>=1,paste0(variableList[1,14], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,15], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,15], sep="$")))<10000,
                                        paste0(variableList[1,15]),ifelse(eval(parse(text = paste("m", variableList[1,15], sep="$")))>=1,paste0(variableList[1,15], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,16], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,16], sep="$")))<10000,
                                        paste0(variableList[1,16]),ifelse(eval(parse(text = paste("m", variableList[1,16], sep="$")))>=1,paste0(variableList[1,16], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,17], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,17], sep="$")))<10000,
                                        paste0(variableList[1,17]),ifelse(eval(parse(text = paste("m", variableList[1,17], sep="$")))>=1,paste0(variableList[1,17], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,18], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,18], sep="$")))<10000,
                                        paste0(variableList[1,18]),ifelse(eval(parse(text = paste("m", variableList[1,18], sep="$")))>=1,paste0(variableList[1,18], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,19], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,19], sep="$")))<10000,
                                        paste0(variableList[1,19]),ifelse(eval(parse(text = paste("m", variableList[1,19], sep="$")))>=1,paste0(variableList[1,19], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,20], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,20], sep="$")))<10000,
                                        paste0(variableList[1,20]),ifelse(eval(parse(text = paste("m", variableList[1,20], sep="$")))>=1,paste0(variableList[1,20], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,21], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,21], sep="$")))<10000,
                                        paste0(variableList[1,21]),ifelse(eval(parse(text = paste("m", variableList[1,21], sep="$")))>=1,paste0(variableList[1,21], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,22], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,22], sep="$")))<10000,
                                        paste0(variableList[1,22]),ifelse(eval(parse(text = paste("m", variableList[1,22], sep="$")))>=1,paste0(variableList[1,22], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,23], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,23], sep="$")))<10000,
                                        paste0(variableList[1,23]),ifelse(eval(parse(text = paste("m", variableList[1,23], sep="$")))>=1,paste0(variableList[1,23], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,24], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,24], sep="$")))<10000,
                                        paste0(variableList[1,24]),ifelse(eval(parse(text = paste("m", variableList[1,24], sep="$")))>=1,paste0(variableList[1,24], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,25], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,25], sep="$")))<10000,
                                        paste0(variableList[1,25]),ifelse(eval(parse(text = paste("m", variableList[1,25], sep="$")))>=1,paste0(variableList[1,25], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,26], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,26], sep="$")))<10000,
                                        paste0(variableList[1,26]),ifelse(eval(parse(text = paste("m", variableList[1,26], sep="$")))>=1,paste0(variableList[1,26], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,27], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,27], sep="$")))<10000,
                                        paste0(variableList[1,27]),ifelse(eval(parse(text = paste("m", variableList[1,27], sep="$")))>=1,paste0(variableList[1,27], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,28], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,28], sep="$")))<10000,
                                        paste0(variableList[1,28]),ifelse(eval(parse(text = paste("m", variableList[1,28], sep="$")))>=1,paste0(variableList[1,28], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,29], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,29], sep="$")))<10000,
                                        paste0(variableList[1,29]),ifelse(eval(parse(text = paste("m", variableList[1,29], sep="$")))>=1,paste0(variableList[1,29], ","),paste0(""))),
                                 ifelse(eval(parse(text = paste("m", variableList[1,30], sep="$")))>1000 & eval(parse(text = paste("m", variableList[1,30], sep="$")))<10000,
                                        paste0(variableList[1,30]),ifelse(eval(parse(text = paste("m", variableList[1,30], sep="$")))>=1,paste0(variableList[1,30], ","),paste0("")))
                               ),"")

# We create our importance score by amount of times a value appears in the ImportantVariables column and in what position

a<-str_split(m$ImportantVariables, ",")
n <- max(lengths(a))
a<-lapply(a, `length<-`, n)
a<- data.frame(a)
a<-data.frame(apply(a, 2, function(x) gsub("^$", NA, trimws(x))))
a<-a %>%
  select_if(not_any_na)
c <- 1:ncol(a)
names(a)<-c
a$Score <-  rev(rownames(a))
d <- a %>%
  gather(., key ="Score", value="Variable", as.character(c))
d<-na.omit(d)
d <- data.table(d, key = "Variable")
e<-d[, max(cumsum(Score)), by = key(d)]
e$V1 <- (as.numeric(e$V1)*100)/max(e$V1)
setnames(e, "V1", "Value")
library(plyr)
library(pracma)

Imp_Var_Pred <-e
Imp_Var_Pred <- Imp_Var_Pred[order(-as.numeric(Imp_Var_Pred$Value)),]

predictedOutput <- cbind(predictedOutput, m$ImportantVariables)

setnames(predictedOutput,"V2", "ImportantVariables")



write.csv(predictedOutput, file = "Prediction_Readmissions.csv")
write.csv(Imp_Var_Pred, file = "Variable_Importance.csv")
