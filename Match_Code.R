#install.packages("dplyr")
#install.packages("tidyr")
install.packages("bsselectR", repos="https://cloud.r-project.org")
#install.packages("knitr")
library(MatchIt)
library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyr)
library(plotly)
library(stringr)
#install.packages("bsselectR")
#devtools::install_github("walkerke/bsselectR")
#rsconnect::setAccountInfo(name='dwhfgr',
 #                         token='952E8695EE8F5F246C556B3BD6400D04',
  #                        secret='Tuyg2BpsGhJqeGFtfJxr4DAKzC8ZWd63YNtAgv0+')

path <- "X:\\Data Science Spring 2018\\R\\Propensity_Match\\data_20180302.csv"
data <- read.csv(path, sep = ",")
summary(data)

p <- ggplot(ps_df, aes(pr_score, colour = ENROLLED_IN_PERIOD)) +
  geom_density(aes(fill=ENROLLED_IN_PERIOD,COLOUR = ENROLLED_IN_PERIOD), alpha = 0.3) + 
  labs(x="Propensity to Enroll",y="Density") +
  theme_bw() +
  ggtitle("Treated vs Non-Treated Propensity Scores - Pre Match")
p <- ggplotly(p)



p1 <- ggplot(matched_data, aes(distance, colour = ENROLLED_IN_PERIOD)) +
  geom_density(aes(fill=ENROLLED_IN_PERIOD,COLOUR = ENROLLED_IN_PERIOD), alpha = 0.3) + 
  labs(x="Propensity to Enroll",y="Density") +
  theme_bw() +
  ggtitle("Treated vs Non-Treated Propensity Scores - Post Match")

p1<-ggplotly(p1)
htmlwidgets::saveWidget(as.widget(p),"Pre-Match.html")


library(stringr)
library(bsselectR)
plots <- paste0(list.files(path="plots",full.names = TRUE))
names(plots) <- str_replace_all(plots,c(".html" = "","plots/"=""))
bsselect(plots, type = "iframe",frame_height = 1500, select = "Pre-Match",live_search = TRUE,show_tick=TRUE)


htmlwidgets::saveWidget(as.widget(p),"Pre-Match.html")
#devtools::install_github("ropensci/plotly")


data$FRANKLIN_COUNTY <- as.factor(data$FRANKLIN_COUNTY)
data$ENROLLED_IN_PERIOD <- as.factor(data$ENROLLED_IN_PERIOD)
data$PREVIOUSLY_ENROLLED <- as.factor(data$PREVIOUSLY_ENROLLED)
data$NCH_Previous <- as.factor(data$NCH_Previous)
data$NCH_Inpatient <- as.factor(data$NCH_Inpatient)
data$NCH_ED <- as.factor(data$NCH_ED)
data$PayorID <- as.factor(data$PayorID)
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
data$REFUSED_UTC_PREV_6_MOS <- as.factor(data$REFUSED_UTC_PREV_6_MOS)
data$USES_PC<-as.factor(data$USES_PCP)

sapply(data,class)

data[c("MRN")][is.na(data[c("MRN")])] <- 0

missing <- data[!complete.cases(data),]


data <- data %>%
  na.omit()


ps <- glm(ENROLLED_IN_PERIOD ~ FRANKLIN_COUNTY + PREVIOUSLY_ENROLLED + NCH_ED + PayorID
          + NCH_Previous + OVER_10K_PAID + ED_VISIT_FLAG_5 + NCH_Inpatient
          + PRIM_DX_HCUP_CHRONIC + PRIM_DX_BH + ABD_FLAG + IP_LAST_12_FLAG
          + homehealth_flag + NCH_ATTRIBUTED_ASSIGNED + PREV_ASTHMA_ED
          + PREV_DIABETES_ED + ABUSE_NEGLECT + NOT_WITH_PARENTS + ZIP_PERCENT_URBAN
          + ZIP_PERCENT_WHITE + REFUSED_UTC_PREV_6_MOS + PREVIOUS_ATTEMPTS 
          + abd_months + USES_PCP
          + AVOIDABLE_ED_LAST_12_FLAG, family = binomial(), data = data)


options(scipen = 999)
summary(ps)

ps_df <- data.frame(pr_score = predict(ps,type = "response"),
                    ENROLLED_IN_PERIOD = ps$model$ENROLLED_IN_PERIOD, 
                    MEDICAIDID = data$MEDICAIDID)

#write.csv(ps_df,file = "propensity_scores.csv")

matched <- matchit(ENROLLED_IN_PERIOD ~ FRANKLIN_COUNTY 
                   + NCH_Previous + OVER_10K_PAID + ED_VISIT_FLAG_5 + NCH_Inpatient
                   + PRIM_DX_HCUP_CHRONIC + PRIM_DX_BH + ABD_FLAG + IP_LAST_12_FLAG
                   + homehealth_flag + NCH_ATTRIBUTED_ASSIGNED
                   + AVOIDABLE_ED_LAST_12_FLAG, method = "nearest", data = data)
matched_data <- match.data(matched)
summary(matched)

ggplot(matched_data, aes(distance, colour = ENROLLED_IN_PERIOD)) +
  geom_freqpoly(bins = 30) + labs(x="Propensity to Enroll",y="Count of Members")

ggplot(matched_data, aes(distance, colour = ENROLLED_IN_PERIOD)) +
  geom_density(aes(fill=ENROLLED_IN_PERIOD,COLOUR = ENROLLED_IN_PERIOD), alpha = 0.3) + 
  labs(x="Propensity to Enroll",y="Density") +
  theme_bw() +
  ggtitle("Treated vs Non-Treated Propensity Scores - Post Match")


ggplot(ps_df, aes(pr_score, colour = ENROLLED_IN_PERIOD)) +
  geom_density(aes(fill=ENROLLED_IN_PERIOD,COLOUR = ENROLLED_IN_PERIOD), alpha = 0.3) + 
  labs(x="Propensity to Enroll",y="Density") +
  theme_bw() +
  ggtitle("Treated vs Non-Treated Propensity Scores - Pre Match")

res <- wilcox.test(as.numeric(NCH_Previous) ~ ENROLLED_IN_PERIOD, data = matched_data, paired = TRUE)
cat("p Value = ",res$p.value)


meas_data <- read.csv("X:\\Data Science Spring 2018\\R\\Propensity_Match\\ED_MEAS.csv")

meas_data$AVOIDABLE_ED_MEAS = as.numeric(meas_data$AVOIDABLE_ED_MEAS)
meas_data$member_months_meas = as.numeric(meas_data$member_months_meas)
meas_summary <- sqldf(
  '
  select
  a.ENROLLED_IN_PERIOD,
  case when a.ENROLLED_IN_PERIOD = "1" THEN "Enrolled" ELSE "Not Enrolled" END AS ENROLLED_STATUS,
  sum(b.AVOIDABLE_ED_MEAS) as AVOIDABLE_ED_VISITS,
  sum(b.member_months_meas) as MEMBER_MONTHS,
  sum(b.AVOIDABLE_ED_MEAS)*1000/sum(b.member_months_meas) AS AVOIDABLE_ED_PER_1000
  
  from
  matched_data a
  
  left join
  meas_data b
  on a.MEDICAIDID = b.MEDICAIDID
  
  group by
  a.ENROLLED_IN_PERIOD
  '

)

meas_detail = sqldf(
  
'
select
a.MEDICAIDID,
a.ENROLLED_IN_PERIOD,
b.AVOIDABLE_ED_MEAS

from
matched_data a

left join
meas_data b
on a.MEDICAIDID = b.MEDICAIDID

'  
  
)
meas_detail$AVOIDABLE_ED_MEAS <- as.numeric(meas_detail$AVOIDABLE_ED_MEAS)
meas_detail$ENROLLED_IN_PERIOD <- as.numeric(meas_detail$ENROLLED_IN_PERIOD)
meas_detail[c("AVOIDABLE_ED_MEAS")][is.na(meas_detail[c("AVOIDABLE_ED_MEAS")])] <- 0

res <- wilcox.test(as.numeric(AVOIDABLE_ED_MEAS) ~ as.numeric(ENROLLED_IN_PERIOD), data = meas_detail, paired = TRUE)
cat("Comparison p Value = ",res$p.value)


df1<-sqldf('
  
           select
           ENROLLED_IN_PERIOD,
   cast(sum(FRANKLIN_COUNTY) as float)/cast(count(FRANKLIN_COUNTY) as float) as P_FRANKLIN_COUNTY,
cast(sum(PREVIOUSLY_ENROLLED) as float)/cast(count(PREVIOUSLY_ENROLLED) as float) as P_PREVIOUSLY_ENROLLED,
           cast(sum(NCH_Previous) as float)/cast(count(NCH_Previous) as float) as P_NCH_Previous,
           cast(sum(ABD_FLAG) as float)/cast(count(ABD_FLAG) as float) as P_ABD_FLAG,
           cast(sum(PREV_ASTHMA_ED) as float)/cast(count(PREV_ASTHMA_ED) as float) as P_PREV_ASTHMA_ED,
           cast(sum(PREV_DIABETES_ED) as float)/cast(count(PREV_DIABETES_ED) as float) as P_PREV_DIABETES_ED,
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
           cast(sum(AVOIDABLE_ED_LAST_12_FLAG) as float)/cast(count(AVOIDABLE_ED_LAST_12_FLAG) as float) as P_AVOIDABLE_ED_LAST_12_FLAG,
           cast(sum(REFUSED_UTC_PREV_6_MOS) as float)/cast(count(REFUSED_UTC_PREV_6_MOS) as float) as P_REFUSED_UTC_PREV_6_MOS
           
           
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
           cast(sum(NCH_Previous) as float)/cast(count(NCH_Previous) as float) as P_NCH_Previous,
           cast(sum(ABD_FLAG) as float)/cast(count(ABD_FLAG) as float) as P_ABD_FLAG,
           cast(sum(PREV_ASTHMA_ED) as float)/cast(count(PREV_ASTHMA_ED) as float) as P_PREV_ASTHMA_ED,
           cast(sum(PREV_DIABETES_ED) as float)/cast(count(PREV_DIABETES_ED) as float) as P_PREV_DIABETES_ED,
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
           cast(sum(AVOIDABLE_ED_LAST_12_FLAG) as float)/cast(count(AVOIDABLE_ED_LAST_12_FLAG) as float) as P_AVOIDABLE_ED_LAST_12_FLAG,
           cast(sum(REFUSED_UTC_PREV_6_MOS) as float)/cast(count(REFUSED_UTC_PREV_6_MOS) as float) as P_REFUSED_UTC_PREV_6_MOS
           

           from 
           matched_data a
           group by
           a.ENROLLED_IN_PERIOD
           ')

df2<-sqldf('
           
           select
           
           a.ENROLLED_IN_PERIOD,
           cast(sum(a.FRANKLIN_COUNTY) as float)/cast(count(a.FRANKLIN_COUNTY) as float) as percent_franklin,
           cast(sum(NCH_Previous) as float)/cast(count(NCH_Previous) as float) as NCH_PREV_COUNT,
           cast(sum(OVER_10K_PAID) as float)/cast(count(OVER_10K_PAID) as float) as PERCENT_OVER_10K,
           cast(sum(ED_VISIT_FLAG_5) as float)/cast(count(ED_VISIT_FLAG_5) as float) as PERCENT_ED_5,
           cast(sum(NCH_Inpatient) as float)/cast(count(NCH_Inpatient) as float) as percent_NCH_Inpatient,
           cast(sum(PRIM_DX_HCUP_CHRONIC) AS float)/cast(count(PRIM_DX_HCUP_CHRONIC) as float) as percent_HCUP_Chronic,
           cast(sum(PRIM_DX_BH) as float)/cast(count(PRIM_DX_BH) as float) as percent_BH,
           cast(sum(ABD_FLAG) as float)/cast(count(ABD_FLAG) as float) as percent_ABD,
           cast(sum(PREV_DIABETES_ED) as float)/cast(count(PREV_DIABETES_ED) as float) as percent_DIABETES,
           cast(sum(homehealth_flag) as float)/cast(count(homehealth_flag) as float) as percent_HOMEHEALTH,
           cast(sum(NCH_ATTRIBUTED_ASSIGNED) as float)/cast(count(NCH_ATTRIBUTED_ASSIGNED) as float) as percent_NCHATTR,
           cast(sum(IP_LAST_12_FLAG) as float)/cast(count(IP_LAST_12_FLAG) as float) as PERCENT_IP_LAST_12,
           cast(sum(AVOIDABLE_ED_LAST_12_FLAG) as float)/cast(count(AVOIDABLE_ED_LAST_12_FLAG) as float) as percent_AVOIDL12,
cast(sum(FRANKLIN_COUNTY) as float) as FRANKLIN_COUNTY_SUM,
cast(sum(PREVIOUSLY_ENROLLED) as float) as PREVIOUSLY_ENROLLED_SUM,
           cast(sum(NCH_Previous) as float) as NCH_Previous_SUM,
           cast(sum(ABD_FLAG) as float) as ABD_FLAG_SUM,
           cast(sum(PREV_ASTHMA_ED) as float) as PREV_ASTHMA_ED_SUM,
           cast(sum(PREV_DIABETES_ED) as float) as PREV_DIABETES_ED_SUM,
           cast(sum(homehealth_flag) as float) as homehealth_flag_SUM,
           cast(sum(USES_PCP) as float) as USES_PCP_SUM,
           cast(sum(OVER_10K_PAID) as float) as OVER_10K_PAID_SUM,
           cast(sum(IP_LAST_12_FLAG) as float) as IP_LAST_12_FLAG_SUM,
           cast(sum(ED_VISIT_FLAG_5) as float) as ED_VISIT_FLAG_5_SUM,
           cast(sum(NCH_Inpatient) as float) as NCH_Inpatient_SUM,
           cast(sum(NCH_ED) as float) as NCH_ED_SUM,
           cast(sum(PRIM_DX_HCUP_CHRONIC) as float) as PRIM_DX_HCUP_CHRONIC_SUM,
           cast(sum(PRIM_DX_BH) as float) as PRIM_DX_BH_SUM,
           cast(sum(NCH_ATTRIBUTED_ASSIGNED) as float) as NCH_ATTRIBUTED_ASSIGNED_SUM,
           cast(sum(ABUSE_NEGLECT) as float) as ABUSE_NEGLECT_SUM,
           cast(sum(NOT_WITH_PARENTS) as float) as NOT_WITH_PARENTS_SUM,
           cast(sum(AVOIDABLE_ED_LAST_12_FLAG) as float) as AVOIDABLE_ED_LAST_12_FLAG_SUM,
           cast(sum(REFUSED_UTC_PREV_6_MOS) as float) as REFUSED_UTC_PREV_6_MOS_SUM,
cast(COUNT(FRANKLIN_COUNTY) as float) as FRANKLIN_COUNTY_COUNT,
cast(COUNT(PREVIOUSLY_ENROLLED) as float) as PREVIOUSLY_ENROLLED_COUNT,
           cast(COUNT(NCH_Previous) as float) as NCH_Previous_COUNT,
           cast(COUNT(ABD_FLAG) as float) as ABD_FLAG_COUNT,
           cast(COUNT(PREV_ASTHMA_ED) as float) as PREV_ASTHMA_ED_COUNT,
           cast(COUNT(PREV_DIABETES_ED) as float) as PREV_DIABETES_ED_COUNT,
           cast(COUNT(homehealth_flag) as float) as homehealth_flag_COUNT,
           cast(COUNT(USES_PCP) as float) as USES_PCP_COUNT,
           cast(COUNT(OVER_10K_PAID) as float) as OVER_10K_PAID_COUNT,
           cast(COUNT(IP_LAST_12_FLAG) as float) as IP_LAST_12_FLAG_COUNT,
           cast(COUNT(ED_VISIT_FLAG_5) as float) as ED_VISIT_FLAG_5_COUNT,
           cast(COUNT(NCH_Inpatient) as float) as NCH_Inpatient_COUNT,
           cast(COUNT(NCH_ED) as float) as NCH_ED_COUNT,
           cast(COUNT(PRIM_DX_HCUP_CHRONIC) as float) as PRIM_DX_HCUP_CHRONIC_COUNT,
           cast(COUNT(PRIM_DX_BH) as float) as PRIM_DX_BH_COUNT,
           cast(COUNT(NCH_ATTRIBUTED_ASSIGNED) as float) as NCH_ATTRIBUTED_ASSIGNED_COUNT,
           cast(COUNT(ABUSE_NEGLECT) as float) as ABUSE_NEGLECT_COUNT,
           cast(COUNT(NOT_WITH_PARENTS) as float) as NOT_WITH_PARENTS_COUNT,
           cast(COUNT(AVOIDABLE_ED_LAST_12_FLAG) as float) as AVOIDABLE_ED_LAST_12_FLAG_COUNT,
           cast(COUNT(REFUSED_UTC_PREV_6_MOS) as float) as REFUSED_UTC_PREV_6_MOS_COUNT
           
           

           
           
           
           
           
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

ENROLLED_DF<-subset(df2,ENROLLED_IN_PERIOD==1)
NOT_ENROLLED_DF <- subset(df2,ENROLLED_IN_PERIOD==0)

ENROLLED_DF<-subset(df2,ENROLLED_IN_PERIOD==1)
NOT_ENROLLED_DF <- subset(df2,ENROLLED_IN_PERIOD==0)
sapply(ENROLLED_DF,class)
t<-binom.test(NOT_ENROLLED_DF$FRANKLIN_COUNTY_SUM,NOT_ENROLLED_DF$FRANKLIN_COUNTY_COUNT,ENROLLED_DF$percent_franklin)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$REFUSED_UTC_PREV_6_MOS_SUM,NOT_ENROLLED_DF$REFUSED_UTC_PREV_6_MOS_COUNT,ENROLLED_DF$percent_refused_utc)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$NCH_Previous_SUM,NOT_ENROLLED_DF$NCH_Previous_COUNT,ENROLLED_DF$NCH_PREV_COUNT)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$OVER_10K_PAID_SUM,NOT_ENROLLED_DF$OVER_10K_PAID_COUNT,ENROLLED_DF$PERCENT_OVER_10K)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$ED_VISIT_FLAG_5_SUM,NOT_ENROLLED_DF$ED_VISIT_FLAG_5_COUNT,ENROLLED_DF$PERCENT_ED_5)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$NCH_Inpatient_SUM,NOT_ENROLLED_DF$NCH_Inpatient_COUNT,ENROLLED_DF$percent_NCH_Inpatient)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$PRIM_DX_HCUP_CHRONIC_SUM,NOT_ENROLLED_DF$PRIM_DX_HCUP_CHRONIC_COUNT,ENROLLED_DF$percent_HCUP_Chronic)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$PRIM_DX_BH_SUM,NOT_ENROLLED_DF$PRIM_DX_BH_COUNT,ENROLLED_DF$percent_BH)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$ABD_FLAG_SUM,NOT_ENROLLED_DF$ABD_FLAG_COUNT,ENROLLED_DF$percent_ABD)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$homehealth_flag_SUM,NOT_ENROLLED_DF$homehealth_flag_COUNT,ENROLLED_DF$percent_HOMEHEALTH)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$NCH_ATTRIBUTED_ASSIGNED_SUM,NOT_ENROLLED_DF$NCH_ATTRIBUTED_ASSIGNED_COUNT,ENROLLED_DF$percent_NCHATTR)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$AVOIDABLE_ED_LAST_12_FLAG_SUM,NOT_ENROLLED_DF$AVOIDABLE_ED_LAST_12_FLAG_COUNT,ENROLLED_DF$percent_AVOIDL12)
t$p.value

t<-binom.test(NOT_ENROLLED_DF$IP_LAST_12_FLAG_SUM,NOT_ENROLLED_DF$IP_LAST_12_FLAG_COUNT,ENROLLED_DF$PERCENT_IP_LAST_12)
t$p.value

install.packages("rateratio.test")
library(rateratio.test)
no_ed_visits <- c(meas_summary$AVOIDABLE_ED_VISITS)
rate <- c(meas_summary$AVOIDABLE_ED_PER_1000)
pt<-rateratio.test(no_ed_visits,rate)
pt

library(stddiff)

set.seed(2016)
stddiff.binary(data=matched_data,gcol=5,vcol=grep("PREV_ASTHMA_ED",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("FRANKLIN_COUNTY",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("NCH_Previous",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("ABD_FLAG",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("homehealth_flag",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("OVER_10K_PAID",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("IP_LAST_12_FLAG",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("ED_VISIT_FLAG_5",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("NCH_Inpatient",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("PRIM_DX_HCUP_CHRONIC",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("PRIM_DX_BH",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("NCH_ATTRIBUTED_ASSIGNED",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("ABUSE_NEGLECT",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("NOT_WITH_PARENTS",colnames(matched_data)))
stddiff.binary(data=matched_data,gcol=5,vcol=grep("AVOIDABLE_ED_LAST_12_FLAG",colnames(matched_data)))
stddiff.numeric(data=matched_data,gcol=5,vcol=grep("ZIP_PERCENT_URBAN",colnames(matched_data)))
stddiff.numeric(data=matched_data,gcol=5,vcol=grep("ZIP_PERCENT_WHITE",colnames(matched_data)))
q<- stddiff.numeric(data=matched_data,gcol=5,vcol=grep("AVOIDABLE_ED_LAST_12_CNT",colnames(matched_data)))
q[7]

set.seed(2016)
treat<-round(abs(rnorm(100)+1)*10,0) %% 2
numeric<-round(abs(rnorm(100)+1)*10,0)
binary<-round(abs(rnorm(100)+1)*10,0) %% 2
category<-round(abs(rnorm(100)+1)*10,0) %% 3
testdata<-data.frame(treat,numeric,binary,category)
stddiff.binary(data=testdata,gcol=1,vcol=c(3,3))



names(matched_data)

ggplot(data = match_compare, aes(x=ENROLLED_STATUS, NCH_PREV_COUNT, fill = ENROLLED_STATUS)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Match_Status) +
  theme_bw() +
  labs(x="", y="Percent Previously an NCH Patient", legend = "Enrolled Status")

t<-binom.test(NOT_ENROLLED_DF$NCH_Previous_SUM,NOT_ENROLLED_DF$NCH_Previous_COUNT,ENROLLED_DF$NCH_PREV_COUNT)
cat("After-Match p Value = ",t$p.value)
