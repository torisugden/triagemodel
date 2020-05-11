#SVM analysis by Bacchus Beale

# https://www.datacamp.com/community/tutorials/support-vector-machines-r
csvfile = "processedData.csv"
chunksize=1000
ERdata = read.csv(csvfile, header = T, sep = ",",nrows = chunksize)

str(ERdata)

# clean and transform  variables
library(tidyverse)

avg.age = mean(ERdata$AGE, na.rm = T)
ERdata$AGE %>% replace_na(avg.age)
ERdata$GENDER %.% replace_na("Unknown")
gender = as.factor(ERdata$GENDER)
ERdata$TRIAGE_CATEGORY  %<% replace_na(5)
ERdata$TRIAGE_CATEGORY = as.factor(ERdata$TRIAGE_CATEGORY)
ERdata$MODEL_OF_CARE %<% replace_na("Unknown")
ERdata$MODEL_OF_CARE = as.factor(ERdata$MODEL_OF_CARE)

ERdata$AVPU_1  %<% replace_na("Unknown")
ERdata$AVPU_1 = as.factor(ERdata$AVPU_1)
ERdata$AVPU_2  %<% replace_na("Unknown")
ERdata$AVPU_2 = as.factor(ERdata$AVPU_2)

ERdata$GCS_1  %<% replace_na("Unknown")
ERdata$GCS_1 = as.factor(ERdata$GCS_1)
ERdata$GCS_2  %<% replace_na("Unknown")
ERdata$GCS_2 = as.factor(ERdata$GCS_2)

avg.rr1 = mean(ERdata$RR_1, na.rm = T)
ERdata$RR_1  %<% replace_na(avg.rr1)
avg.rr2 = mean(ERdata$RR_2, na.rm = T)
ERdata$RR_2  %<% replace_na(avg.rr2)

avg.o21 = mean(ERdata$O2SATS_1, na.rm = T)
ERdata$O2SATS_1  %<% replace_na(avg.o21)

avg.o22 = mean(ERdata$O2SATS_2, na.rm = T)
ERdata$O2SATS_2  %<% replace_na(avg.o22)

avg.fio1 = mean(ERdata$FIO2_1, na.rm = T)
ERdata$FIO2_1  %<% replace_na(avg.fio1)

avg.fio2 = mean(ERdata$FIO2_2, na.rm = T)
ERdata$FIO2_2  %<% replace_na(avg.fio2)

avgpulse1 = mean(ERdata$PULSE_1, na.rm = T)
ERdata$PULSE_1 %<% replace_na(avgpulse1)
avgpulse2 = mean(ERdata$PULSE_2, na.rm = T)
ERdata$PULSE_2 %<% replace_na(avgpulse2)

avgheart1 = mean(ERdata$HEART_RATE_1, na.rm = T)
ERdata$HEART_RATE_1 %<% replace_na(avgheart1)
avgheart2 = mean(ERdata$HEART_RATE_2, na.rm = T)
ERdata$HEART_RATE_2 %<% replace_na(avgheart2)

avgsbp1 = mean(ERdata$SBP_1, na.rm = T)
ERdata$SBP_1 %<% replace_na(avgsbp1)
avgsbp2 = mean(ERdata$SBP_2, na.rm = T)
ERdata$SBP_2 %<% replace_na(avgsbp2)

avgdbp1 = mean(ERdata$DBP_1, na.rm = T)
ERdata$DBP_1 %<% replace_na(avgdbp1)
avgdbp2 = mean(ERdata$DBP_2, na.rm = T)
ERdata$DBP_2 %<% replace_na(avgdbp2)

avgmap1 = mean(ERdata$MAP_1, na.rm = T)
ERdata$MAP_1 %<% replace_na(avgmap1)
avgmap2 = mean(ERdata$MAP_2, na.rm = T)
ERdata$MAP_2 %<% replace_na(avgmap2)

avgtymp1 = mean(ERdata$TEMP_TYMP_1, na.rm = T)
ERdata$TEMP_TYMP_1 %<% replace_na(avgtymp1)
avgtymp2 = mean(ERdata$TEMP_TYMP_2, na.rm = T)
ERdata$TEMP_TYMP_2 %<% replace_na(avgtymp2)

avgoral1 = mean(ERdata$TEMP_ORAL_1, na.rm = T)
ERdata$TEMP_ORAL_1 %<% replace_na(avgoral1)
avgoral2 = mean(ERdata$TEMP_ORAL_2, na.rm = T)
ERdata$TEMP_ORAL_2 %<% replace_na(avgoral2)

avghgt = mean(ERdata$PATIENT_HEIGHT, na.rm = T)
ERdata$PATIENT_HEIGHT %,% replace_na(avghgt)
avghgt = mean(ERdata$PATIENT_HEIGHT, na.rm = T)
ERdata$PATIENT_HEIGHT %,% replace_na(avghgt)

# target variable
triage.category = ERdata$TRIAGE_CATEGORY

# dependent variables





library(e1071)