library(MatchIt)
library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyr)
library(plotly)
library(stringr)
library(stddiff)

path <- "X:\\Data Science Spring 2018\\R\\Propensity_Match\\data_20180302.csv"
data <- read.csv(path, sep = ",")
names(data)


data$FRANKLIN_COUNTY <- as.factor(data$FRANKLIN_COUNTY)
data$ENROLLED_IN_PERIOD <- as.factor(data$ENROLLED_IN_PERIOD)
data$PREVIOUSLY_ENROLLED <- as.factor(data$PREVIOUSLY_ENROLLED)
data$NCH_Previous <- as.factor(data$NCH_Previous)
data$NCH_Inpatient <- as.factor(data$NCH_Inpatient)
data$NCH_ED <- as.factor(data$NCH_ED)
data$PRIM_DX_HCUP_CHRONIC <- as.factor(data$PRIM_DX_HCUP_CHRONIC)
data$PRIM_DX_BH <- as.factor(data$PRIM_DX_BH)
data$ABD_FLAG <- as.factor(data$ABD_FLAG)
data$PREV_ASTHMA_ED <- as.factor(data$PREV_ASTHMA_ED)
data$PREV_DIABETES_ED <- as.factor(data$PREV_DIABETES_ED)
data$homehealth_flag <- as.factor(data$homehealth_flag)
data$NCH_ATTRIBUTED_ASSIGNED <- as.factor(data$NCH_ATTRIBUTED_ASSIGNED)
data$ABUSE_NEGLECT <- as.factor(data$ABUSE_NEGLECT)
data$NOT_WITH_PARENTS <- as.factor(data$NOT_WITH_PARENTS)
data$AVOIDABLE_ED_LAST_12_FLAG <- as.factor(data$AVOIDABLE_ED_LAST_12_FLAG)
data$OVER_10K_PAID <- as.factor(data$OVER_10K_PAID)
data$IP_LAST_12_FLAG <- as.factor(data$IP_LAST_12_FLAG)
data$ED_VISIT_FLAG_5 <- as.factor(data$ED_VISIT_FLAG_5)
data$USES_PCP<-as.factor(data$USES_PCP)

sapply(data,class)

data <- data %>%
  na.omit()


ps <- glm(ENROLLED_IN_PERIOD ~ FRANKLIN_COUNTY + PREVIOUS_ATTEMPTS
          + PREVIOUSLY_ENROLLED + NCH_Previous + ABD_FLAG + PREV_ASTHMA_ED
          + PREV_DIABETES_ED + abd_months + homehealth_flag + USES_PCP
          + OVER_10K_PAID + IP_LAST_12_FLAG + ED_VISIT_FLAG_5
          + NCH_Inpatient + NCH_ED + PRIM_DX_HCUP_CHRONIC
          + PRIM_DX_BH + NCH_ATTRIBUTED_ASSIGNED + ABUSE_NEGLECT
          + NOT_WITH_PARENTS + ZIP_PERCENT_URBAN + ZIP_PERCENT_WHITE
          + AVOIDABLE_ED_LAST_12_FLAG, family = binomial(), data = data)


options(scipen = 999)
summary(ps)

ps_df <- data.frame(pr_score = predict(ps,type = "response"),
                    ENROLLED_IN_PERIOD = ps$model$ENROLLED_IN_PERIOD, 
                    MEDICAIDID = data$MEDICAIDID)

#write.csv(ps_df,file = "propensity_scores.csv")

matched <- matchit(ENROLLED_IN_PERIOD ~ FRANKLIN_COUNTY + PREVIOUS_ATTEMPTS
                   + PREVIOUSLY_ENROLLED + NCH_Previous + ABD_FLAG + PREV_ASTHMA_ED
                   + PREV_DIABETES_ED + abd_months + homehealth_flag + USES_PCP
                   + OVER_10K_PAID + IP_LAST_12_FLAG + ED_VISIT_FLAG_5
                   + NCH_Inpatient + NCH_ED + PRIM_DX_HCUP_CHRONIC
                   + PRIM_DX_BH + NCH_ATTRIBUTED_ASSIGNED + ABUSE_NEGLECT
                   + NOT_WITH_PARENTS + ZIP_PERCENT_URBAN + ZIP_PERCENT_WHITE
                   + AVOIDABLE_ED_LAST_12_FLAG, method = "nearest", data = data)
matched_data <- match.data(matched)
summary(matched)
sapply(matched_data,class)


df1<-sqldf('
           
            select
           ENROLLED_IN_PERIOD,
           cast(sum(FRANKLIN_COUNTY) as float)/cast(count(FRANKLIN_COUNTY) as float) as P_FRANKLIN_COUNTY,
           cast(sum(PREVIOUSLY_ENROLLED) as float)/cast(count(PREVIOUSLY_ENROLLED) as float) as P_PREVIOUSLY_ENROLLED,
           cast(sum(PREVIOUS_ATTEMPTS)as float)*1000/cast(sum(CONT_ENROLLMENT_MONTHS) as float) AS PREV_ATTEMPTS_PER_1000,
           cast(sum(NCH_Previous) as float)/cast(count(NCH_Previous) as float) as P_NCH_Previous,
           cast(sum(ABD_FLAG) as float)/cast(count(ABD_FLAG) as float) as P_ABD_FLAG,
           cast(sum(PREV_ASTHMA_ED) as float)/cast(count(PREV_ASTHMA_ED) as float) as P_PREV_ASTHMA_ED,
           cast(sum(PREV_DIABETES_ED) as float)/cast(count(PREV_DIABETES_ED) as float) as P_PREV_DIABETES_ED,
           cast(sum(abd_months) as float)*1000/cast(sum(CONT_ENROLLMENT_MONTHS) as float) AS ABD_MONTHS_PER_1000,
           cast(sum(homehealth_flag) as float)/cast(count(homehealth_flag) as float) as P_homehealth_flag,
           cast(sum(USES_PCP) as float)/cast(count(USES_PCP) as float) as P_USES_PCP,
           cast(sum(OVER_10K_PAID) as float)/cast(count(OVER_10K_PAID) as float) as P_OVER_10K_PAID,
           cast(sum(IP_LAST_12_FLAG) as float)/cast(count(IP_LAST_12_FLAG) as float) as P_IP_LAST_12_FLAG,
           cast(sum(ED_VISIT_FLAG_5) as float)/cast(count(ED_VISIT_FLAG_5) as float) as P_ED_VISIT_FLAG_5,
           cast(sum(NCH_Inpatient) as float)/cast(count(NCH_Inpatient) as float) as P_NCH_Inpatient,
           cast(sum(NCH_ED) as float)/cast(count(NCH_ED) as float) as P_NCH_ED,
           cast(sum(PRIM_DX_HCUP_CHRONIC) as float)/cast(count(PRIM_DX_HCUP_CHRONIC) as float) as P_PRIM_DX_HCUP_CHRONIC,
           cast(sum(PRIM_DX_BH) as float)/cast(count(PRIM_DX_BH) as float) as P_PRIM_DX_BH,
           cast(sum(NCH_ATTRIBUTED_ASSIGNED) as float)/cast(count(NCH_ATTRIBUTED_ASSIGNED) as float) as P_NCH_ATTRIBUTED_ASSIGNED,
           cast(sum(ABUSE_NEGLECT) as float)/cast(count(ABUSE_NEGLECT) as float) as P_ABUSE_NEGLECT,
           cast(sum(NOT_WITH_PARENTS) as float)/cast(count(NOT_WITH_PARENTS) as float) as P_NOT_WITH_PARENTS,
           avg(ZIP_PERCENT_URBAN) as AVG_ZIP_PERCENT_URBAN,
           avg(ZIP_PERCENT_WHITE) as AVG_ZIP_PERCENT_WHITE,
           cast(sum(AVOIDABLE_ED_LAST_12_FLAG) as float)/cast(count(AVOIDABLE_ED_LAST_12_FLAG) as float) as P_AVOIDABLE_ED_LAST_12_FLAG
           
           
           from 
           data a
           group by
           a.ENROLLED_IN_PERIOD
           
           
           
           ')
df3<-sqldf('
           
           select
           ENROLLED_IN_PERIOD,
           cast(sum(FRANKLIN_COUNTY) as float)/cast(count(FRANKLIN_COUNTY) as float) as P_FRANKLIN_COUNTY,
           cast(sum(PREVIOUSLY_ENROLLED) as float)/cast(count(PREVIOUSLY_ENROLLED) as float) as P_PREVIOUSLY_ENROLLED,
           cast(sum(PREVIOUS_ATTEMPTS)as float)*1000/cast(sum(CONT_ENROLLMENT_MONTHS) as float) AS PREV_ATTEMPTS_PER_1000,
           cast(sum(NCH_Previous) as float)/cast(count(NCH_Previous) as float) as P_NCH_Previous,
           cast(sum(ABD_FLAG) as float)/cast(count(ABD_FLAG) as float) as P_ABD_FLAG,
           cast(sum(PREV_ASTHMA_ED) as float)/cast(count(PREV_ASTHMA_ED) as float) as P_PREV_ASTHMA_ED,
           cast(sum(PREV_DIABETES_ED) as float)/cast(count(PREV_DIABETES_ED) as float) as P_PREV_DIABETES_ED,
           cast(sum(abd_months) as float)*1000/cast(sum(CONT_ENROLLMENT_MONTHS) as float) AS ABD_MONTHS_PER_1000,
           cast(sum(homehealth_flag) as float)/cast(count(homehealth_flag) as float) as P_homehealth_flag,
           cast(sum(USES_PCP) as float)/cast(count(USES_PCP) as float) as P_USES_PCP,
           cast(sum(OVER_10K_PAID) as float)/cast(count(OVER_10K_PAID) as float) as P_OVER_10K_PAID,
           cast(sum(IP_LAST_12_FLAG) as float)/cast(count(IP_LAST_12_FLAG) as float) as P_IP_LAST_12_FLAG,
           cast(sum(ED_VISIT_FLAG_5) as float)/cast(count(ED_VISIT_FLAG_5) as float) as P_ED_VISIT_FLAG_5,
           cast(sum(NCH_Inpatient) as float)/cast(count(NCH_Inpatient) as float) as P_NCH_Inpatient,
           cast(sum(NCH_ED) as float)/cast(count(NCH_ED) as float) as P_NCH_ED,
           cast(sum(PRIM_DX_HCUP_CHRONIC) as float)/cast(count(PRIM_DX_HCUP_CHRONIC) as float) as P_PRIM_DX_HCUP_CHRONIC,
           cast(sum(PRIM_DX_BH) as float)/cast(count(PRIM_DX_BH) as float) as P_PRIM_DX_BH,
           cast(sum(NCH_ATTRIBUTED_ASSIGNED) as float)/cast(count(NCH_ATTRIBUTED_ASSIGNED) as float) as P_NCH_ATTRIBUTED_ASSIGNED,
           cast(sum(ABUSE_NEGLECT) as float)/cast(count(ABUSE_NEGLECT) as float) as P_ABUSE_NEGLECT,
           cast(sum(NOT_WITH_PARENTS) as float)/cast(count(NOT_WITH_PARENTS) as float) as P_NOT_WITH_PARENTS,
           avg(ZIP_PERCENT_URBAN) as AVG_ZIP_PERCENT_URBAN,
           avg(ZIP_PERCENT_WHITE) as AVG_ZIP_PERCENT_WHITE,
           cast(sum(AVOIDABLE_ED_LAST_12_FLAG) as float)/cast(count(AVOIDABLE_ED_LAST_12_FLAG) as float) as P_AVOIDABLE_ED_LAST_12_FLAG
           
           
           from 
           matched_data a
           group by
           a.ENROLLED_IN_PERIOD
           ')


match_compare <- sqldf("select
                       *,
                       case when ENROLLED_IN_PERIOD = '1' THEN 'ENROLLED' ELSE 'NOT ENROLLED' END AS ENROLLED_STATUS,
                       'Before Match' as Match_Status
                       from
                       df1
                       
                       union
                       
                       select
                       *,
                       case when ENROLLED_IN_PERIOD = '1' then 'ENROLLED' ELSE 'NOT ENROLLED' END AS ENROLLED_STATUS,
                       'After Match' as Match_Status
                       from
                       df3")
match_compare$ENROLLED_STATUS <-as.factor(match_compare$ENROLLED_STATUS)
match_compare$Match_Status <- as.factor(match_compare$Match_Status)
match_compare$Match_Status = factor(match_compare$Match_Status, levels = c("Before Match","After Match"))
match_compare1 <- as.data.frame(t(match_compare))
library(data.table)
setDT(match_compare1, keep.rownames = TRUE)
names <- c("METRIC_LABEL","BEFORE_NOT_ENROLLED","AFTER_NOT_ENROLLED","AFTER_ENROLLED","BEFORE_ENROLLED")
colnames(match_compare1) = names
match_compare1 <- match_compare1[-c(1,25,26)]

write.csv(match_compare1,file = "match_compare.csv")



ENROLLED_DF<-subset(df2,ENROLLED_IN_PERIOD==1)
NOT_ENROLLED_DF <- subset(df2,ENROLLED_IN_PERIOD==0)

standardized_differences <- list(
stddiff.binary(data=matched_data,gcol=4,vcol=grep("FRANKLIN_COUNTY",colnames(matched_data)))[5],
stddiff.numeric(data=matched_data,gcol=4,vcol=grep("PREVIOUS_ATTEMPTS",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("PREVIOUSLY_ENROLLED",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("NCH_Previous",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("ABD_FLAG",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("PREV_ASTHMA_ED",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("PREV_DIABETES_ED",colnames(matched_data)))[5],
stddiff.numeric(data=matched_data,gcol=4,vcol=grep("abd_months",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("homehealth_flag",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("USES_PCP",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("OVER_10K_PAID",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("IP_LAST_12_FLAG",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("ED_VISIT_FLAG_5",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("NCH_Inpatient",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("NCH_ED",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("PRIM_DX_HCUP_CHRONIC",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("PRIM_DX_BH",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("NCH_ATTRIBUTED_ASSIGNED",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("ABUSE_NEGLECT",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("NOT_WITH_PARENTS",colnames(matched_data)))[5],
stddiff.numeric(data=matched_data,gcol=4,vcol=grep("ZIP_PERCENT_URBAN",colnames(matched_data)))[5],
stddiff.numeric(data=matched_data,gcol=4,vcol=grep("ZIP_PERCENT_WHITE",colnames(matched_data)))[5],
stddiff.binary(data=matched_data,gcol=4,vcol=grep("AVOIDABLE_ED_LAST_12_FLAG",colnames(matched_data)))[5]
)

stddiff_names <- list ("FRANKLIN_COUNTY",
                       "PREVIOUS_ATTEMPTS",
                       "PREVIOUSLY_ENROLLED",
                       "NCH_Previous",
                       "ABD_FLAG",
                       "PREV_ASTHMA_ED",
                       "PREV_DIABETES_ED",
                       "abd_months",
                       "homehealth_flag",
                       "USES_PCP",
                       "OVER_10K_PAID",
                       "IP_LAST_12_FLAG",
                       "ED_VISIT_FLAG_5",
                       "NCH_Inpatient",
                       "NCH_ED",
                       "PRIM_DX_HCUP_CHRONIC",
                       "PRIM_DX_BH",
                       "NCH_ATTRIBUTED_ASSIGNED",
                       "ABUSE_NEGLECT",
                       "NOT_WITH_PARENTS",
                       "ZIP_PERCENT_URBAN",
                       "ZIP_PERCENT_WHITE",
                       "AVOIDABLE_ED_LAST_12_FLAG"
)

stddiff_df <- do.call(rbind, Map(data.frame,Covariate = stddiff_names,Standardized_Difference = standardized_differences))



ggplot(data = match_compare, aes(x=ENROLLED_STATUS, P_FRANKLIN_COUNTY, fill = ENROLLED_STATUS)) +
    geom_bar(stat = "identity") +
    facet_wrap(~Match_Status) +
    theme_bw() +
    labs(x="", y="Percent Previously an NCH Patient", legend = "Enrolled Status")
t <- stddiff.binary(data=matched_data,gcol=3,vcol=grep("FRANKLIN_COUNTY",colnames(matched_data)))
cat("Standardized Difference: ",t[5])




