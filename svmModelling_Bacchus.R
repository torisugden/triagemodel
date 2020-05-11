#SVM analysis by Bacchus Beale

# https://www.datacamp.com/community/tutorials/support-vector-machines-r
csvfile <- "processedData.csv"
chunksize <- 1000
ERdata <- read.csv(csvfile, header = T, sep = ",",nrows = chunksize)

print("Before cleaning")
str(ERdata)

# clean and transform  variables
library(dplyr)

avg.age <- mean(ERdata$AGE, na.rm = T)
ERdata$AGE <- replace_na(ERdata$AGE, avg.age)

ERdata$GENDER <- replace_na(ERdata$GENDER,"Unknown")
gender = as.factor(ERdata$GENDER)

ERdata$TRIAGE_CATEGORY <- replace_na(ERdata$TRIAGE_CATEGORY,5)
ERdata$TRIAGE_CATEGORY <- as.factor(ERdata$TRIAGE_CATEGORY)

ERdata$MODEL_OF_CARE <- replace_na(ERdata$MODEL_OF_CARE,"Unknown")
ERdata$MODEL_OF_CARE <- as.factor(ERdata$MODEL_OF_CARE)

ERdata$AVPU_1 <- replace_na(ERdata$AVPU_1,"Unknown")
ERdata$AVPU_1 <- as.factor(ERdata$AVPU_1)
ERdata$AVPU_2 <- replace_na(ERdata$AVPU_2,"Unknown")
ERdata$AVPU_2 <- as.factor(ERdata$AVPU_2)

ERdata$GCS_1 <- replace_na(ERdata$GCS_1,"Unknown")
ERdata$GCS_1 <- as.factor(ERdata$GCS_1)
ERdata$GCS_2 <- replace_na(ERdata$GCS_2,"Unknown")
ERdata$GCS_2 <- as.factor(ERdata$GCS_2)

avg.rr1 <- mean(ERdata$RR_1, na.rm = T)
ERdata$RR_1 <- replace_na(ERdata$RR_1,avg.rr1)
avg.rr2 <- mean(ERdata$RR_2, na.rm = T)
ERdata$RR_2 <- replace_na(ERdata$RR_2,avg.rr2)

avg.o21 <- mean(ERdata$O2SATS_1, na.rm = T)
ERdata$O2SATS_1 <- replace_na(ERdata$O2SATS_1,avg.o21)

avg.o22 = mean(ERdata$O2SATS_2, na.rm = T)
ERdata$O2SATS_2 <- replace_na(ERdata$O2SATS_2,avg.o22)

avg.fio1 <- mean(ERdata$FIO2_1, na.rm = T)
ERdata$FIO2_1 <- replace_na(ERdata$FIO2_1,avg.fio1)

avg.fio2 <- mean(ERdata$FIO2_2, na.rm = T)
ERdata$FIO2_2 <- replace_na(ERdata$FIO2_2,avg.fio2)

avgpulse1 = mean(ERdata$PULSE_1, na.rm = T)
ERdata$PULSE_1 <- replace_na(ERdata$PULSE_1,avgpulse1)
avgpulse2 = mean(ERdata$PULSE_2, na.rm = T)
ERdata$PULSE_2 <- replace_na(ERdata$PULSE_2,avgpulse2)

avgheart1 <- mean(ERdata$HEART_RATE_1, na.rm = T)
ERdata$HEART_RATE_1 <- replace_na(ERdata$HEART_RATE_1,avgheart1)
avgheart2 <- mean(ERdata$HEART_RATE_2, na.rm = T)
ERdata$HEART_RATE_2 <- replace_na(ERdata$HEART_RATE_2,avgheart2)

avgsbp1 = mean(ERdata$SBP_1, na.rm = T)
replace_na(ERdata$SBP_1,avgsbp1)
avgsbp2 = mean(ERdata$SBP_2, na.rm = T)
replace_na(ERdata$SBP_2,avgsbp2)

avgdbp1 <- mean(ERdata$DBP_1, na.rm = T)
ERdata$DBP_1 <- replace_na(ERdata$DBP_1,avgdbp1)
avgdbp2 <- mean(ERdata$DBP_2, na.rm = T)
ERdata$DBP_2 <- replace_na(ERdata$DBP_2,avgdbp2)

avgmap1 <- mean(ERdata$MAP_1, na.rm = T)
ERdata$MAP_1 <- replace_na(ERdata$MAP_1, avgmap1)
avgmap2 <- mean(ERdata$MAP_2, na.rm = T)
ERdata$MAP_2<-replace_na(ERdata$MAP_2,avgmap2)

avgtymp1 <- mean(ERdata$TEMP_TYMP_1, na.rm = T)
replace_na(ERdata$TEMP_TYMP_1,avgtymp1)
avgtymp2 <- mean(ERdata$TEMP_TYMP_2, na.rm = T)
replace_na(ERdata$TEMP_TYMP_2,avgtymp2)

avgoral1 <- mean(ERdata$TEMP_ORAL_1, na.rm = T)
ERdata$TEMP_ORAL_1 <- replace_na(ERdata$TEMP_ORAL_1,avgoral1)
avgoral2 <- mean(ERdata$TEMP_ORAL_2, na.rm = T)
ERdata$TEMP_ORAL_2 <- replace_na(ERdata$TEMP_ORAL_2,avgoral2)

avghgt <- mean(ERdata$PATIENT_HEIGHT, na.rm = T)
ERdata$PATIENT_HEIGHT <- replace_na(ERdata$PATIENT_HEIGHT, avghgt)

avgweight <- mean(ERdata$PATIENT_WEIGHT, na.rm = T)
ERdata$PATIENT_WEIGHT <- replace_na(ERdata$PATIENT_WEIGHT,avgweight)

ERdata$PREGNANCY_STATUS <- replace_na(ERdata$PREGNANCY_STATUS,FALSE)
ERdata$SMOKING_STATUS <- replace_na(ERdata$SMOKING_STATUS,FALSE)

ERdata$RECENT_SURGERY_WITHIN_30_DAY <- replace_na(ERdata$RECENT_SURGERY_WITHIN_30_DAY,0)
ERdata$PREVIOUS_ENC_MEDS <- replace_na(ERdata$PREVIOUS_ENC_MEDS,FALSE)

print("After cleaning")
str(ERdata)

# target variable
triage.category = ERdata$TRIAGE_CATEGORY

# dependent variables


library(e1071)
# dat = data.frame(y = factor(y), x)
# fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)
# xgrid = expand.grid(X1 = px1, X2 = px2)
# ygrid = predict(fit, xgrid)
# plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
# points(x, col = y + 1, pch = 19)