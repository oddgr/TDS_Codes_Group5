#table 1
library(table1)

#Demographics
label(t$recruitment_age) <- "Age at recruitment"
units(t$recruitment_age) <- "years"

label(t$sex) <- "Sex"

label(t$education) <- "Education"

label(t$ethnic) <- "Ethnicity"

label(t$standing_height) <- "Height"
units(t$standing_height) <- "cm"

label(t$waist_circumference) <- "Waist circumference"
units(t$waist_circumference) <- "cm"

label(t$weight) <- "Weight"
units(t$weight) <- "kg"

label(t$hip_circumference) <- "Hip circumference"
units(t$hip_circumference) <- "cm"


#social
label(t$tdi) <- "Townsend Deprivation Index"

label(t$own_or_rent) <- "Own or rent accommodation lived in"

label(t$accommodation_type) <- "Type of accommodation lived in"

t$number_in_household <- factor(t$number_in_household, levels=c("1~2",">=3"),)
label(t$number_in_household) <- "Number in household"

t$ave_total_household_income <- factor(t$ave_total_household_income, levels = c("<18000", "18000~30999", "31000~51999", ">52000"))
label(t$ave_total_household_income) <- "Average total household income"

label(t$employment) <- "Employment"



#health risk factors

label(t$breastfed_as_baby) <- "Breastfed as a baby"

label(t$maternal_smoking) <- "Maternal smoking around birth"

t$alcohol_intake_frequency <- factor(t$alcohol_intake_frequency, levels = c("Never","Special occasions only","One to three times a month","Once or twice a week", "Three or four times a week","Daily or almost daily","Prefer not to answer"))
label(t$alcohol_drinker) <- "Alcohol drinker status"

label(t$alcohol_intake_frequency) <- "Alcohol intake frequency"

label(t$smoking_status) <- "Smoking status"

#bmi
t$BMI <- factor(t$BMI, levels = c("<25", "25-30", "30-40", ">40"))
units(t$BMI) <- "kg/m^2"


#lifestyle factor
label(t$moderate_activity_duration) <- "Duration of moderate activity"
units(t$moderate_activity_duration) <- "minutes/day"

label(t$num_moderate_activity) <- "Days of moderate activity"
units(t$num_moderate_activity) <- "days/Week"

label(t$vigorous_activity_duration) <- "Duration of vigorous activity"
units(t$vigorous_activity_duration) <- "minutes/day"

label(t$num_vigorous_activity) <- "Days of vigorous activity"
units(t$num_vigorous_activity) <- "days/week"

t$sleep_duration <- factor(t$sleep_duration, levels = c("<7", "7~8",">8"))
label(t$sleep_duration) <- "Sleep"
units(t$sleep_duration) <- "hours/day"

label(t$insomnia) <- "Insomnia"


#medical risk
label(t$family_bc_history ) <- "Family breast cancer history"
t$family_bc_history <- 
  factor(t$family_bc_history, levels=c(0,1),
         labels=c("No", 
                  "Yes"))

t$Diabetes <- factor(t$Diabetes, levels=c(0,1),
                     labels=c("No", 
                              "Yes"))
t$Hypertension <- factor(t$Hypertension, levels=c(0,1),
                            labels=c("No", 
                                     "Yes"))
t$Respiratory <-factor(t$Respiratory, levels=c(0,1),
                       labels=c("No", 
                                "Yes"))
t$Cardiovascular <- factor(t$Cardiovascular, levels=c(0,1),
                           labels=c("No", 
                                    "Yes"))

#enviromental

label(t$ap_Nitrogen_dioxide) <- "Nitrogen dioxide"
units(t$ap_Nitrogen_dioxide) <- "g/m^3"

label(t$ap_Nitrogen_oxides) <- "Nitrogen oxides"
units(t$ap_Nitrogen_oxides) <- "g/m^3"

label(t$ap_pm10) <- "PM10"
units(t$ap_pm10) <- "g/m^3"

label(t$ap_pm2.5) <- "PM2.5"
units(t$ap_pm2.5) <- "g/m^3"

label(t$ap_2.5.10um) <- "PM2.5-10um"
units(t$ap_2.5.10um) <- "g/m^3"

label(t$np_ave_daytime_sound_level) <-"Daytime of noise pollution"
units(t$np_ave_daytime_sound_level) <- "dB"

label(t$np_ave_evening_sound_level) <-"Evening of noise pollution"
units(t$np_ave_evening_sound_level) <- "dB"

label(t$np_ave_night.time_sound_level) <-"Night time of noise pollution"
units(t$np_ave_night.time_sound_level) <- "dB"

label(t$np_ave_16.hour_sound_level) <-"16-hour sound level of noise pollution"
units(t$np_ave_16.hour_sound_level) <- "dB"

label(t$np_ave_24.hour_sound_level) <-"24-hour sound level of noise pollution"
units(t$np_ave_24.hour_sound_level) <- "dB"


#biomark
label(t$bio_triglycerides) <- "Triglyceride"
units(t$bio_triglycerides) <- "mmol/L"

label(t$bio_hba1c) <- "Glycated haemoglobin"
units(t$bio_hba1c) <- "mmol/L"

label(t$bio_glucose) <- "Glucose"
units(t$bio_glucose) <- "mmol/L"

label(t$bio_HDL) <- "HDL cholesterol"
units(t$bio_HDL) <- "mmol/L"

label(t$bio_IGF1) <- "IGF-1"
units(t$bio_IGF1) <- "nmol/L"

label(t$bio_SHBG) <- "SHBG"
units(t$bio_SHBG) <- "nmol/L"

label(t$bio_oestrodial) <- "Oestrodial"
units(t$bio_oestrodial) <- "mmol/L"

#female-specific factors
label(t$breast_cancer_screening) <- "Ever had breast cancer screening"

label(t$menarche_age) <- "Age when periods started"
units(t$menarche_age) <- "years"

t$menopause_status <- factor(t$menopause_status,levels = c("No","Yes","hysterectomy") ,labels = c("No","Yes","Hysterectomy"))
label(t$menopause_status) <- "Had menopause"
#t$menopause_status <- factor(t$menopause_status,levels = c("No","Yes","hysterectomy",2) ,labels = c("Before menopause","Had menopause","Hysterectomy","P-Value"))


t$live_births_num <- factor(t$live_births_num, levels = c("0","1~3", ">3"))
label(t$live_births_num) <- "Number of live births"

label(t$stillbirth_status) <- "Ever had stillbirth"

label(t$oc_status) <- "Ever taken oral contraceptive pill"

label(t$HRT_status) <- "Ever used hormone-replacement therapy (HRT)"


#subtype
t$bc_C500 <- factor(t$bc_C500, levels=c(0,1),
                     labels=c("No", 
                              "Yes"))

t$bc_C501 <- factor(t$bc_C501, levels=c(0,1),
                   labels=c("No", 
                            "Yes"))
t$bc_C502 <- factor(t$bc_C502, levels=c(0,1),
                   labels=c("No", 
                            "Yes"))
t$bc_C503 <- factor(t$bc_C503, levels=c(0,1),
                   labels=c("No", 
                            "Yes"))
t$bc_C504 <- factor(t$bc_C504, levels=c(0,1),
                   labels=c("No", 
                            "Yes"))
t$bc_C505 <- factor(t$bc_C505, levels=c(0,1),
                   labels=c("No", 
                            "Yes"))
t$bc_C506 <- factor(t$bc_C506, levels=c(0,1),
                   labels=c("No", 
                            "Yes"))
t$bc_C508 <- factor(t$bc_C508, levels=c(0,1),
                   labels=c("No", 
                            "Yes"))
t$bc_C509 <- factor(t$bc_C509, levels=c(0,1),
                   labels=c("No", 
                            "Yes"))
label(t$bc_C500) <- "Nipple and areola"
label(t$bc_C501) <- "Central portion of breast"
label(t$bc_C502) <- "Upper-inner quadrant of breast"
label(t$bc_C503) <- "lower-inner quadrant of breast"
label(t$bc_C504) <- "Upper-outer quadrant of breast"
label(t$bc_C505) <- "Lower-outer quadrant of breast"
label(t$bc_C506) <- "Axillary tail of breast"
label(t$bc_C508) <- "Overlapping lesion of breast"
label(t$bc_C509) <- "Unspecified type of Breast "


t$outcome_status <- factor(t$outcome_status, levels = c(0,1,2),labels = c("Control","Case","P-Value"))




#p-value
rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- t[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)){ 
      p <- t.test(y ~ t$outcome_status)$p.value
    }
    if (is.integer(y)){ 
      p <- t.test(y ~ t$outcome_status)$p.value
    }
      else {
      p <- chisq.test(table(y, droplevels(t$outcome_status)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}



#table1(~ recruitment_age  +education + ethnic + standing_height + waist_circumference + weight + hip_circumference + tdi + own_or_rent + accommodation_type + number_in_household + ave_total_household_income + employment + breastfed_as_baby + maternal_smoking + alcohol_drinker + alcohol_intake_frequency  + smoking_status + BMI + moderate_activity_duration +num_moderate_activity + vigorous_activity_duration + num_vigorous_activity + sleep_duration + insomnia + cooked_vegetable_intake + raw_vegetable_intake + fresh_fruit_intake + dried_fruit_intake + oily_fish_intake + non.oily_fish_intake + processed_meat_intake + poultry_intake + beef_intake + lamb_intake + pork_intake + cheese_intake + bread_intake + cereal_intake + tea_intake + coffee_intake + water_intake + ap_Nitrogen_dioxide + ap_Nitrogen_oxides + ap_pm10 + ap_pm2.5 + ap_2.5.10um + np_ave_daytime_sound_level + np_ave_evening_sound_level+ np_ave_night.time_sound_level + np_ave_16.hour_sound_level + np_ave_24.hour_sound_level + bio_triglycerides + bio_hba1c + bio_glucose + bio_HDL + bio_oestrodial + bio_IGF1 + bio_SHBG + family_bc_history + menarche_age + menopause_status + live_births_num + stillbirth_status + oc_status + HRT_status + Diabetes+Cardiovascular+ Hypertension + Respiratory |breast_cancer_screening, data=t,  droplevels = F, render=rndr,render.strat=rndr.strat)
#table1(~ recruitment_age  +education + ethnic + standing_height + waist_circumference + weight + hip_circumference + tdi + own_or_rent + accommodation_type + number_in_household + ave_total_household_income + employment + breastfed_as_baby + maternal_smoking + alcohol_drinker + alcohol_intake_frequency  + smoking_status + BMI + moderate_activity_duration +num_moderate_activity + vigorous_activity_duration + num_vigorous_activity + sleep_duration + insomnia + cooked_vegetable_intake + raw_vegetable_intake + fresh_fruit_intake + dried_fruit_intake + oily_fish_intake + non.oily_fish_intake + processed_meat_intake + poultry_intake + beef_intake + lamb_intake + pork_intake + cheese_intake + bread_intake + cereal_intake + tea_intake + coffee_intake + water_intake + ap_Nitrogen_dioxide + ap_Nitrogen_oxides + ap_pm10 + ap_pm2.5 + ap_2.5.10um + np_ave_daytime_sound_level + np_ave_evening_sound_level+ np_ave_night.time_sound_level + np_ave_16.hour_sound_level + np_ave_24.hour_sound_level + bio_triglycerides + bio_hba1c + bio_glucose + bio_HDL + bio_oestrodial + bio_IGF1 + bio_SHBG + family_bc_history+breast_cancer_screening + menarche_age + menopause_status + live_births_num + oc_status + HRT_status + Diabetes+Cardiovascular+ Hypertension + Respiratory |stillbirth_status, data=t,  droplevels = F, render=rndr,render.strat=rndr.strat)
table1(~ recruitment_age  +education + ethnic + standing_height + waist_circumference + weight + hip_circumference + tdi + own_or_rent + accommodation_type + number_in_household + ave_total_household_income + employment + breastfed_as_baby + maternal_smoking + alcohol_drinker + alcohol_intake_frequency  + smoking_status + BMI +num_moderate_activity  + num_vigorous_activity + sleep_duration + insomnia + cooked_vegetable_intake + raw_vegetable_intake + fresh_fruit_intake + dried_fruit_intake + oily_fish_intake + non.oily_fish_intake + processed_meat_intake + poultry_intake + beef_intake + lamb_intake + pork_intake + cheese_intake + bread_intake + cereal_intake + tea_intake + coffee_intake + water_intake + ap_Nitrogen_dioxide + ap_Nitrogen_oxides + ap_pm10 + ap_pm2.5 + ap_2.5.10um + np_ave_daytime_sound_level + np_ave_evening_sound_level+ np_ave_night.time_sound_level + np_ave_16.hour_sound_level + np_ave_24.hour_sound_level + bio_triglycerides + bio_hba1c + bio_glucose + bio_HDL  + bio_IGF1 + bio_SHBG + family_bc_history+breast_cancer_screening + menarche_age + menopause_status + live_births_num + stillbirth_status + oc_status + HRT_status + Diabetes+Cardiovascular+ Hypertension + Respiratory |outcome_status, data=t,  droplevels = F, render=rndr,render.strat=rndr.strat)



