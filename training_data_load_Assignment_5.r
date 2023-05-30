#load the expss library that will be used for variable labeling
library(expss)

#find your working directory for R
getwd()

#change your working directory to the location where your files are
#  (make sure you put the folder path for your new directory between the 
#   quotation marks)
setwd("C:/Users/sachi/OneDrive - University at Albany - SUNY/SUNY/Courses/Semester 04 - Fall 22/CINF 624/Assignment/Relevant Data/Training Data - 2008-2016 EMS Calls")

#load data
a<-readRDS("training_final.rds")

#label initial_call_type
val_lab(a$initial_call_type)=num_lab("
1 Abdominal Pain-Fever & Cough
2 Abdominal Pain-Fever & Cough
3 Abdominal Pain Fever-Travel
4 Abdominal Pain
5 ACC
6 Active Shooter
7 ADM
8 Altered Mental Status
9 Alt Mental Status-Fever&Cough
10 Amputation, Arm, Hand,Leg,Foot
11 Amputation, Fingers Or Toes
12 Anaphylactic Shock-Fever&Cough
13 Anaphylaxis
14 Card Or Resp Arrest-Fevercough
15 Cardiac Arrest
16 Asthma Attack - Fever&Cough
17 Asthma Patient Fever-Travel
18 Asthma Attack
19 BBP
20 Major Burns 18% Adlt 10% Child
21 Minor Burns <18% Adlt Or <10%
22 Cardiac Condition
23 CARDBR
24 Cardiac Condition-Fever&Cough
25 CDBRFC
26 Child Abuse
27 Choking
28 Choking Fever&Cough
29 Hypothermia
30 COVINF
31 Stroke
32 CVA (Stroke)
33 Stroke Critical - Fever&Cough
34 Stroke - Fever & Cough
35 Death Confirm By Medical Auth
36 Difficult Breather
37 Diff Breathing - Fever&Cough
38 Difficult Breathing Fever-Travel
39 Difficult Breather Rf
40 Death Confirm By Medical Auth
41 DRILL
42 Drowning
43 Hx Drug Or Alcohol Abuse
44 Hx Drug Or Alchl Abuse-Fev&Cou
45 Psychiatric Patient
46 EDPC
47 EDPW
48 Electrocution
49 Evac 
50 Fire75 Working Fire
51 Fire76 High Rise Commercial
52 Fire77 High Rise Residential
53 Gyn Bleeding-Pt Not Pregnant
54 Gyn-Severe Pain-Bleeding
55 Heat Exhaustion
56 Hypertension
57 Internal Bleeding
58 Internal Bleeding-Fever&Cough
59 Inhalation Of Smoke
60 Injury Lower Ext In Elderly
61 Major Injury
62 Minor Injury
63 Non-Critical Injury
64 Jumper Down
65 Jumper Up
66 One Alarm Fire
67 One Alarm Fire
68 Two Alarm Fire
69 Two Alarm Fire
70 Three Alarm Fire
71 Four Alarm Fire
72 MCI25
73 Five Alarm Fire Or Greater
74 Occupied High-Rise Building
75 Occupied High-Rise Building
76 Criminal Detection Facil Incid
77 Report Of Explosives
78 Report Of Explosives
79 Explosion
80 Rapid Transit-Rail Incident
81 Ground Transport Incident
82 Ground Transport Incident
83 Structural Collaspe [Specify]
84 Construction-Demolition Incid
85 Construction-Demolition Incid
86 Confined Space Incident
87 MCI37
88 MCI40
89 Aircraft Incident - Crash
90 Civil Distrubance
91 Hostage Situation - Barricaded
92 Hostage Situation - Barricaded
93 Power Failure - Blackout
94 Active Shooter
95 Active Shooter
96 All Other MCIs
97 All Other MCIs
98 MCI76
99 MCI77
100 Hazardous Materials Incident
101 Hazardous Materials Incident
102 MECHE
103 MECHV
104 Reaction To Med - Fever&Cough
105 Reaction To Medication
106 Medevac, T-C Authority Only
107 MOSILL
108 MOSINJ
109 Auto Accident, No Confirmd Inj
110 Auto Acc W-Injuries
111 MVAINM
112 NOVEH
113 Obstetric Complications
114 Female In Labor
115 Major Obstetrical Complaint
116 Miscarriage
117 Baby Out Or Imminent Birth
118 Unknown Condition
119 Police 10-13, Unconfirmed
120 Police 10-13, Confirmed
121 Sick Ped<5 Yrs-Fever & Cough
122 Sick Ped<5 Yrs-Rash & Fever
123 Pedestrian Struck
124 PEDSTS
125 RADIO
126 Rape
127 Resp Distress - Fever&Cough
128 Respiratory Distress Fever-Travel
129 Respiratory Distress
130 RESPRF
131 SAFE
132 Seizures - Fever & Cough
133 Seizures
134 Gun Shot Wound
135 Sick
136 Sick - Cough & Fever
137 Sick Patient Fever-Travel
138 Sick - Rash And Fever
139 Sick - Minor - Fever & Cough
140 Minor Illness
141 Sick Pediatric, <5 Year Old
142 Special Event
143 Stabbing
144 Status Epilepticus
145 Mult Or Prolong Seizur-Fev&Cou
146 Status Epilepticus Fever-Travel
147 STNDBM
148 Request For Stand-By
149 Stat Transfer Request
150 STUCK
151 T-ARST
152 T-DFBR
153 T-EDP
154 T-INJ
155 T-SICK
156 T-TEXT
157 T-TRMA
158 T-UNC
159 T-UNKN
160 Test Kdt-Modat
161 Multiple Trauma Patient
162 TRAUMS
163 Unconscious Patient
164 Unc Patient - Fever & Cough
165 Unconscious Fever-Travel Patient
166 Unconscious Patient-Rash&Fever
167 Caller Has No Pt Medical Info
168 Venom (Snake Bites)
")

#check that initial call type labels loaded correctly
data.frame(table(a$initial_call_type))

#label final_call_type
val_lab(a$final_call_type)=num_lab("
1 Abdominal Pain-Fever & Cough
2 Abdominal Pain-Fever & Cough
3 Abdominal Pain Fever-Travel
4 Abdominal Pain
5 Active Shooter
6 ALMNFC
7 Altered Mental Status
8 Alt Mental Status-Fever&Cough
9 Amputation, Arm, Hand,Leg,Foot
10 Amputation, Fingers Or Toes
11 Anaphylactic Shock-Fever&Cough
12 Anaphylaxis
13 Card Or Resp Arrest-Fevercough
14 Cardiac Arrest
15 ARSTFC
16 Asthma Attack - Fever&Cough
17 Asthma Attack
18 Major Burns 18% Adlt 10% Child
19 Minor Burns <18% Adlt Or <10%
20 Cardiac Condition
21 CARDBR
22 Cardiac Condition-Fever&Cough
23 CDBRFC
24 Child Abuse
25 Choking
26 Choking Fever&Cough
27 Hypothermia
28 COVINF
29 Stroke
30 CVA (Stroke)
31 Stroke Critical - Fever&Cough
32 Stroke - Fever & Cough
33 Difficult Breather
34 Diff Breathing - Fever&Cough
35 Difficult Breathing Fever-Travel
36 Difficult Breather Rf
37 Death Confirm By Medical Auth
38 Drowning
39 Hx Drug Or Alcohol Abuse
40 Hx Drug Or Alchl Abuse-Fev&Cou
41 Psychiatric Patient
42 EDPC
43 EDPW
44 Electrocution
45 Evac 
46 Fire75 Working Fire
47 Fire77 High Rise Residential
48 Gyn Bleeding-Pt Not Pregnant
49 Gyn-Severe Pain-Bleeding
50 Heat Exhaustion
51 Hypertension
52 Internal Bleeding
53 Internal Bleeding-Fever&Cough
54 Inhalation Of Smoke
55 Injury Lower Ext In Elderly
56 Major Injury
57 Minor Injury
58 Non-Critical Injury
59 Jumper Down
60 Jumper Up
61 One Alarm Fire
62 One Alarm Fire
63 Two Alarm Fire
64 Two Alarm Fire
65 Three Alarm Fire
66 Three Alarm Fire
67 Four Alarm Fire
68 Four Alarm Fire
69 MCI25
70 Five Alarm Fire Or Greater
71 Occupied High-Rise Building
72 Occupied High-Rise Building
73 MCI27
74 Medical Facility Evacuation
75 Criminal Detection Facil Incid
76 Criminal Detection Facil Incid
77 Report Of Explosives
78 Report Of Explosives
79 Explosion
80 Rapid Transit-Rail Incident
81 Rapid Transit-Rail Incident
82 Ground Transport Incident
83 Ground Transport Incident
84 Structural Collaspe [Specify]
85 Structural Collaspe [Specify]
86 Construction-Demolition Incid
87 Construction-Demolition Incid
88 MCI35
89 Confined Space Incident
90 MCI37
91 Marine - Harbor Incident
92 Marine - Harbor Incident
93 MCI40
94 Aircraft Incident - Crash
95 MCI42
96 Civil Distrubance
97 Hostage Situation - Barricaded
98 Hostage Situation - Barricaded
99 Power Failure - Blackout
100 Power Failure - Blackout
101 Active Shooter
102 Active Shooter
103 All Other MCIs
104 All Other MCIs
105 MCI76
106 MCI77
107 Hazardous Materials Incident
108 Hazardous Materials Incident
109 Reaction To Med - Fever&Cough
110 Reaction To Medication
111 Medevac, T-C Authority Only
112 Auto Accident, No Confirmd Inj
113 Auto Acc W-Injuries
114 MVAINM
115 Obstetric Complications
116 Female In Labor
117 Major Obstetrical Complaint
118 Miscarriage
119 Baby Out Or Imminent Birth
120 Unknown Condition
121 Police 10-13, Unconfirmed
122 Police 10-13, Confirmed
123 Sick Ped<5 Yrs-Fever & Cough
124 Sick Ped<5 Yrs-Rash & Fever
125 Pedestrian Struck
126 Rape
127 Resp Distress - Fever&Cough
128 Respiratory Distress Fever-Travel
129 Respiratory Distress
130 RESPRF
131 SAFE
132 Seizures - Fever & Cough
133 Seizures
134 Gun Shot Wound
135 Sick
136 Sick - Cough & Fever
137 Sick Patient Fever-Travel
138 Sick - Rash And Fever
139 SICMFC
140 Minor Illness
141 Sick Pediatric, <5 Year Old
142 Special Event
143 Stabbing
144 Status Epilepticus
145 Mult Or Prolong Seizur-Fev&Cou
146 Status Epilepticus Fever-Travel
147 STNDBM
148 Request For Stand-By
149 Stat Transfer Request
150 T-ABDP
151 T-ARST
152 T-CARD
153 T-CDBR
154 T-DFBR
155 T-EDP
156 T-INBL
157 T-INJ
158 T-OBST
159 T-OTHR
160 T-SICK
161 T-TEXT
162 T-TRMA
163 T-UNC
164 T-UNKN
165 Multiple Trauma Patient
166 Unconscious Patient
167 Unc Patient - Fever & Cough
168 Unconscious Fever-Travel Patient
169 Unconscious Patient-Rash&Fever
170 Caller Has No Pt Medical Info
171 Venom (Snake Bites)
")

#check that final call type labels loaded correctly
data.frame(table(a$final_call_type))

#create month variable
a$month <- format(as.Date(a$incident_dt), "%m")

#create day of week variable
a$dow <- weekdays(as.Date(a$incident_dt))

#label incident disposition code
val_lab(a$incident_disposition_code)=num_lab("
82	transporting patient
83	patient pronounced dead
87	cancelled
90	unfounded
91	condition corrected
92	treated not transported
93	refused medical aid
94	treated and transported
95	triaged at scene no transport
96	patient gone on arrival
")

#check that final call type labels loaded correctly
data.frame(table(a$incident_disposition_code))

#run table of incident disposition code by year
table(a$incident_disposition_code, a$incident_year)

#bar chart of incidents by year
counts_total <- table(a$incident_year) #creates a count of incidents by year and stores it in a new value frame
  barplot(counts_total, xlab="Year", ylab="# of Incidents", col=c("red"), main = "EMS Incidents by Year")

#line chart of incidents by year
  plot(counts_total, type="o", xlab="Year", ylab="# of Incidents", col=c("red"), main = "EMS Incidents by Year")
  
#multiple line chart of incidents by month and year
  counts_month <- as.data.frame.matrix(table(a$month,a$incident_year)) #creates a count of incidents by year and stores it in a new value frame
  plot(counts_month, type="o", xlab="Month", ylab="# of Incidents", col=c("red"), main = "EMS Incidents by Year")
  
  library(data.table)
  setDT(counts_month, keep.rownames = TRUE)[] #to create the first row as the row names, i.e month numbers
  colnames(counts_month)[1]<-"month" #rename the first column heading as Month
  setDT(counts_month, keep.rownames = TRUE)[]
  
  library(ggplot2) #powerful graphics generator for R
  library(reshape) #to transform a table of rows and columns into individual entries for each row-column combo - think 2008-March and 2009-Feb
  Molten <- melt(counts_month, id.vars = "month")
  colnames(Molten)[2]<-"year" #rename the first column heading as Month
  str(Molten) #checks to see whether we have a variable stored as a factor rather than numeric
  Molten$year<-as.numeric(as.character(Molten$year)) #change year from factor to numeric
  Molten$month<-as.numeric(as.character(Molten$month)) #change month from character to numeric
  ggplot(Molten, aes(x = month, y=value, group=year)) + geom_line(aes(color=year)) #generate multiple line plots by year
  
#multiple charts with incidents by month for a single year
countsmo<-ggplot(Molten, aes(month,value)) +       # we are creating a single year chart
    geom_line() +
    ggtitle("EMS Incident Calls by Month and Year") +
    xlab("Month") + ylab("# of Calls") +
    theme(plot.title = element_text(lineheight=.8, face="bold",
                                    size = 20)) +
    theme(text = element_text(size=18))
countsmo + facet_wrap(~ year, ncol=3) #the use of facet_wrap helps to repeat the single year chart across years

#stacked bar chart  of incident dispositions by year
  counts <- table(a$incident_disposition_code, a$incident_year) #creates a count of incidents by year and stores it in a new value frame
  #plot one - the bar chart with no legend
  opar = par(oma = c(0,0,0,14)) # creates large right margin for plot
  barplot(counts, xlab="Year", col=rainbow(10))
  par(opar) # Reset par
  #plot two - the legend on top of the bar chart
  opar = par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE) #creates a new margin set up for the legend
  legend(x = "right", legend = rownames(counts), fill = rainbow(10), bty = "n", y.intersp = 2) #creates a legend
  par(opar) # Reset par

  # bar chart calls by the year
  counts_year <- table(a$incident_year)
  barplot(counts_total, xlab="Year", ylab="# of Calls", col=c("red"), main = "EMS Calls by Year")

  # bar chart calls by the day of the week
  counts_days <- table(a$dow)
  barplot(counts_days, xlab="DOW", ylab="# of Calls", col=c("red"), main = "EMS Calls by days of the week")
  
  # bar chart calls by the month
  counts_calls_monthly <- table(a$month)
  barplot(counts_calls_monthly, xlab="Month", ylab="# of Calls", col=c("red"), main = "EMS Calls by Month")

data.frame(table(a$month))  

library(lubridate)
library(dplyr)
#create hourly variable
a$hourly <-  format(as.POSIXct(a$incident_dt), format = "%H")

# bar chart calls by the hourly
counts_calls_hourly <- table(a$hourly)
barplot(counts_calls_hourly, xlab="Hour", ylab="# of Calls", col=c("red"), main = "EMS Calls Hourly")

#create month year variable
a$yearmonnth <-  format(as.POSIXct(a$incident_dt), format = "%b %Y")

# Line chart calls by the yearmonth
counts_calls_yearmonthly <- table(a$yearmonnth)
barplot(counts_calls_yearmonthly, xlab="Month Year", ylab="# of Calls", col=c("red"), main = "EMS Calls by Month and Year ")
plot(counts_calls_yearmonthly, type="o", xlab="Month Year", ylab="# of Incidents", col=c("red"), main = "EMS Incidents by Month and Year")

#create month year variable
a$date <-  format(as.POSIXct(a$incident_dt), format = "%d/%m/%Y")

# Line chart calls by the date
counts_calls_date <- table(a$date)
plot(counts_calls_date, type="o", xlab="Date", ylab="# of Incidents", col=c("red"), main = "EMS Incidents by Date")

# bar chart calls by the borough
counts_calls_borough <- table(a$borough)
barplot(counts_calls_borough, xlab="Borough", ylab="# of Calls", col=c("red"), main = "EMS Calls by Borough")
data.frame(table(a$borough))

# table calls by the community district
counts_calls_community_dis <- table(a$communitydistrict)
data.frame(table(a$communitydistrict))

# table calls by the initial call type
table(a$initial_call_type) %>% as.data.frame() %>%  arrange(desc(Freq))

# table calls by the final call type
table(a$final_call_type) %>% as.data.frame() %>%  arrange(desc(Freq))

req <- substitute(require(x, character.only = TRUE))
libs<-c("sjPlot")
#sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})
sjPlot::tab_xtab(var.row = a$initial_call_type, var.col = a$final_call_type, title = "Table Title", show.row.prc = TRUE)

#bar chart of incidents by Response Time
counts_total <- table(a$incident_response_seconds_qy)
barplot(counts_total, xlab="Seconds", ylab="# of Incidents", col=c("red"), main = "EMS Incidents by Response Seconds")
#line chart of incidents by Response Time
plot(counts_total, type="o", xlab="Seconds", ylab="# of Incidents", col=c("red"), main = "EMS Incidents by Response Seconds")
data.frame(table(a$incident_response_seconds_qy))
table(a$incident_response_seconds_qy) %>% as.data.frame() %>%  arrange((Freq))
gc()
memory.size()
memory.limit()
memory.limit(size=56000)

#bar chart of dispatch by Response Time
counts_total <- table(a$dispatch_response_seconds_qy, a$incident_dt)
barplot(counts_total, xlab="Seconds", ylab="# of Incidents", col=c("red"), main = "EMS Dispatch by Response Seconds")
#line chart of incidents by Response Time
plot(counts_total, type="o", xlab="Seconds", ylab="# of Incidents", col=c("red"), main = "EMS Dispatch by Response Seconds")
data.frame(table(a$dispatch_response_seconds_qy,a$incident_dt))
table(a$dispatch_response_seconds_qy) %>% as.data.frame() %>%  arrange((Freq))

counts_calls_borough <- table(a$incident_response_seconds_qy, a$borough)
barplot(counts_calls_borough, xlab="Borough", ylab="Responded Seconds", col=c("red"), main = "EMS Calls Responded Seconds by Borough")
data.frame(table(a$incident_response_seconds_qy, a$borough))
table(a$incident_response_seconds_qy,a$borough) %>% as.data.frame() %>%  arrange(desc(Freq))

counts_calls_travel_tm <- table(a$incident_travel_tm_seconds_qy)
barplot(counts_calls_travel_tm, xlab="", ylab="Travel_TM", col=c("red"), main = "EMS Calls travel tm seconds")
data.frame(table(a$incident_travel_tm_seconds_qy))
table(a$incident_travel_tm_seconds_qy) %>% as.data.frame() %>%  arrange(desc(Freq))

counts_calls_inci_resp_dow <- table(a$incident_response_seconds_qy,a$dow)
barplot(counts_calls_inci_resp_dow, xlab="DOW", ylab="Incidents_Res_Seconds", col=c("red"), main = "EMS Calls Inci Resp Time DOW")
table(a$incident_response_seconds_qy,a$dow) %>% as.data.frame() %>%  arrange(desc(Freq))

counts_calls_inci_travel_tm_month <- table(a$incident_travel_tm_seconds_qy,a$month)
barplot(counts_calls_inci_travel_tm_month, xlab="Month", ylab="Incidents_Travel_tm", col=c("red"), main = "EMS Calls Inci Travel Month")
table(a$incident_travel_tm_seconds_qy,a$month) %>% as.data.frame() %>%  arrange(desc(Freq))

memory.limit(size=56000)
is.na(a)
gc()
memory.size()
memory.limit()
memory.limit(size=56000)
colSums(is.na(a))
which(colSums(is.na(a))>0)
names(which(colSums(is.na(a))>0))
df = subset(a, select = -c(held_indicator,policeprecinct,citycouncildistrict,communitydistrict,communityschooldistrict,congressionaldistrict,reopen_indicator,special_event_indicator,standby_indicator,transfer_indicator))
cor(df[sapply(df,is.numeric)])

library(MASS)
library(ISLR)
names(df)
lm.fit=lm(incident_response_seconds_qy~dispatch_response_seconds_qy,data=df)
#attach(df)
#lm.fit=lm(incident_response_seconds_qy~dispatch_response_seconds_qy)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
plot(incident_response_seconds_qy,dispatch_response_seconds_qy)

lm.fit1=lm(dispatch_response_seconds_qy~incident_response_seconds_qy,data=df)
#attach(df)
#lm.fit1=lm(dispatch_response_seconds_qy~incident_response_seconds_qy)
lm.fit1
summary(lm.fit1)
names(lm.fit1)
coef(lm.fit1)
confint(lm.fit1)
plot(dispatch_response_seconds_qy,incident_response_seconds_qy)

AIC(lm.fit1)
BIC(lm.fit1)

lm.fit3=lm(hourly~incident_response_seconds_qy ,data=df)
attach(df)
lm.fit3=lm(hourly~incident_response_seconds_qy)
lm.fit3
summary(lm.fit3)
names(lm.fit3)
coef(lm.fit3)
confint(lm.fit3)
AIC(lm.fit3)
BIC(lm.fit3)
plot(hourly,incident_response_seconds_qy)

lm.fit3=lm(hourly~incident_response_seconds_qy ,data=df)
#attach(df)
#lm.fit3=lm(hourly~incident_response_seconds_qy)
lm.fit3
summary(lm.fit3)
names(lm.fit3)
coef(lm.fit3)
confint(lm.fit3)
AIC(lm.fit3)
BIC(lm.fit3)
plot(hourly,incident_response_seconds_qy)

library(modelr)
data.frame(
  R2 = rsquare(lm.fit1, data = df),
  RMSE = rmse(lm.fit1, data = df),
  MAE = mae(lm.fit1, data = df)
)

data.frame(
  R2 = rsquare(lm.fit, data = df),
  RMSE = rmse(lm.fit, data = df),
  MAE = mae(lm.fit, data = df)
)



dim(df)

library(ISLR)
fix(df)
names(df)
dim(df)
#sum(is.na(df$incident_response_seconds_qy))
df=na.omit(df)
dim(df)
sum(is.na(df))
library(leaps)
#regfit.full=regsubsets(incident_response_seconds_qy~dispatch_response_seconds_qy+,df)
#validcols <- sapply(df, function(x)length(unique(x[!is.na(x)])) > 1)
#df <- df[,validcols]
#summary(regfit.full)
regfit.full=regsubsets(incident_response_seconds_qy ~ dispatch_response_seconds_qy + incident_travel_tm_seconds_qy,df)
summary(regfit.full)
regfit.full=regsubsets(incident_response_seconds_qy ~ dispatch_response_seconds_qy + incident_travel_tm_seconds_qy, data=df, nvmax=3)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(1,1))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,2)

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(df),rep=TRUE)
test=(!train)
regfit.best=regsubsets(incident_response_seconds_qy ~dispatch_response_seconds_qy+incident_travel_tm_seconds_qy,data=df[train,],nvmax=3)
test.mat=model.matrix(incident_response_seconds_qy ~dispatch_response_seconds_qy+incident_travel_tm_seconds_qy,data=df[test,])
val.errors=rep(NA,3)
for(i in 1:2){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((df$incident_response_seconds_qy[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,2)
AIC(lm.fit)
BIC(lm.fit)
rsq = reg.summary$rsq
rsq

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(incident_response_seconds_qy ~dispatch_response_seconds_qy+incident_travel_tm_seconds_qy,data=df,nvmax=3)
coef(regfit.best,2)
k=3
set.seed(1)
folds=sample(1:k,nrow(df),replace=TRUE)
cv.errors=matrix(NA,k,3, dimnames=list(NULL, paste(1:3)))
for(j in 1:k){
  best.fit=regsubsets(incident_response_seconds_qy ~dispatch_response_seconds_qy+incident_travel_tm_seconds_qy,data=df[folds!=j,],nvmax=2)
  for(i in 1:2){
    pred=predict(best.fit,df[folds==j,],id=i)
    cv.errors[j,i]=mean( (df$incident_response_seconds_qy[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(incident_response_seconds_qy ~dispatch_response_seconds_qy+incident_travel_tm_seconds_qy,data=df, nvmax=3)
coef(reg.best,2)
AIC(lm.fit3)
BIC(lm.fit3)
reg.summary_1 = summary(reg.best)
rsq = reg.summary$rsq
rsq


regfit.full=regsubsets(incident_travel_tm_seconds_qy ~ incident_response_seconds_qy + dispatch_response_seconds_qy + incident_year + initial_call_type,df)
summary(regfit.full)
regfit.full=regsubsets(incident_travel_tm_seconds_qy ~ incident_response_seconds_qy + dispatch_response_seconds_qy + incident_year + initial_call_type, data=df, nvmax=5)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(1,1))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,4)

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(df),rep=TRUE)
test=(!train)
regfit.best=regsubsets(incident_travel_tm_seconds_qy ~ incident_response_seconds_qy + dispatch_response_seconds_qy + incident_year + initial_call_type,data=df[train,],nvmax=5)
test.mat=model.matrix(incident_travel_tm_seconds_qy ~ incident_response_seconds_qy + dispatch_response_seconds_qy + incident_year + initial_call_type,data=df[test,])
val.errors=rep(NA,4)
for(i in 1:4){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((df$incident_response_seconds_qy[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,4)
AIC(lm.fit)
BIC(lm.fit)
rsq = reg.summary$rsq
rsq
summary(regfit.best)

lm.fit4=lm(incident_travel_tm_seconds_qy ~ incident_response_seconds_qy + dispatch_response_seconds_qy + incident_year + initial_call_type ,data=df)
#attach(df)
#lm.fit3=lm(hourly~incident_response_seconds_qy)
lm.fit4
summary(lm.fit4)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[4]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(incident_travel_tm_seconds_qy ~ incident_response_seconds_qy + dispatch_response_seconds_qy + incident_year + initial_call_type,data=df,nvmax=5)
coef(regfit.best,4)
k=5
set.seed(1)
folds=sample(1:k,nrow(df),replace=TRUE)
cv.errors=matrix(NA,k,5, dimnames=list(NULL, paste(1:5)))
lm.fit5=lm(incident_travel_tm_seconds_qy ~ incident_response_seconds_qy + dispatch_response_seconds_qy + incident_year + initial_call_type ,data=df)
#attach(df)
#lm.fit3=lm(hourly~incident_response_seconds_qy)
lm.fit5
summary(lm.fit5)
for(j in 1:k){
  best.fit=regsubsets(incident_travel_tm_seconds_qy ~ incident_response_seconds_qy + dispatch_response_seconds_qy + incident_year + initial_call_type,data=df[folds!=j,],nvmax=5)
  for(i in 1:3){
    pred=predict(best.fit,df[folds==j,],id=i)
    cv.errors[j,i]=mean( (df$incident_response_seconds_qy[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(incident_response_seconds_qy ~dispatch_response_seconds_qy+incident_travel_tm_seconds_qy,data=df, nvmax=3)
coef(reg.best,2)
AIC(lm.fit3)
BIC(lm.fit3)
reg.summary_1 = summary(reg.best)
rsq = reg.summary$rsq
rsq