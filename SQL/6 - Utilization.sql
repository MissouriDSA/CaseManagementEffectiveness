if object_id ('tempdb..#temp') is not null drop table #temp

--calculate utilization, exlude IP visits with a primary diagnosis code of chemotherapy
SELECT 
pn.[MEDICAIDID]
,pn.[FRANKLIN_COUNTY]
,pn.[ZIP]
,pn.[ENROLLMENT_DATE]
,pn.[EVER_ENROLLED]
,pn.[ENROLLED_IN_PERIOD]
,pn.[MALE]
,pn.[MRN]
,pn.[PREVIOUS_ATTEMPTS]
,pn.[PREVIOUSLY_ENROLLED]
,pn.[first_episode_start]
,pn.[NCH_Previous]
,pn.age_at_anchor
,PN.ABD_FLAG
,PN.ASTHMA_FLAG
,PN.DIABETES_FLAG
,PN.CONT_ENROLLMENT_MONTHS
,PN.anchor_date
,te.PayorID
,pn.abd_months
,pn.homehealth_flag
,pn.COUNTY
,pn.uses_pcp
,pn.ABUSE_NEGLECT
,pn.NOT_WITH_PARENTS
,pn.ZIP_PERCENT_URBAN
,pn.ZIP_PERCENT_WHITE
,pn.AVOIDABLE_ED_LAST_12_CNT
,pn.AVOIDABLE_ED_LAST_12_FLAG
,pn.PREV_AVOIDABLE_ED_VISIT
,case 
	when sum(tc.paid) < 0 then 0
	else isnull(sum(tc.paid),0) end as totalpaid
,count(case when tc.majcat between 1 and 5 and tc.diag1 not in('V58.11','Z51.11')then tc.claimid else null end) as IPAdmits
,case 
	when sum(case when tc.Majcat between 1 and 5 and tc.diag1 not in('V58.11','Z51.11') then tc.paid else null end) < 0 then 0
	else isnull(sum(case when tc.Majcat between 1 and 5 and tc.diag1 not in('V58.11','Z51.11') then tc.paid else null end),0) end as IP_Paid
,count(case when tc.subcat = 15 then tc.claimid else null end) as EDVisits
,case
	when sum(case when tc.subcat = 15 then tc.paid else null end) < 0 then 0
	else isnull(sum(case when tc.subcat = 15 then tc.paid else null end),0) end as ED_Paid
,max(case when tc.majcat between 1 and 5 and tc.diag1 not in('V58.11','Z51.11') and Provname like 'NATIONWI%' THEN 1 else 0 end) as NCH_Inpatient
,max(case when tc.subcat = 15 and provname like 'NATIONWI%' THEN 1 ELSE 0 END) AS NCH_ED
,max(case when i.dx_cd is not null then 1 else 0 end) as PRIM_DX_HCUP_CHRONIC
,max(case when tc.diag1 between 'F01' AND 'F99.99' OR TC.DIAG1 BETWEEN '290' AND '319.99' THEN 1 ELSE 0 END) AS PRIM_DX_BH
,max(case when tc.subcat = 3 and tc.majcat = 1 then 1 else 0 end) as NEONATE
,max(case when npi.NPI is not null then 1 else 0 end) as NCH_ATTRIBUTED_ASSIGNED
,max(case when utc.MRN is not null then 1 else 0 end) as REFUSED_UTC_PREV_6_MOS
,max(case when dr.mrn is not null then 1 else 0 end) as PROVIDER_REFERRAL_PREV_12_MOS
into #temp
FROM 
[PFKCC].[dbo].[DS18_PREVIOUS_NCH] pn

LEFT JOIN
DWPFK.dbo.tblClaims tc
on pn.medicaidid = tc.medicaidid
and tc.SvcDt between dateadd(m,-12,pn.anchor_date) and pn.anchor_date

LEFT JOIN
PFKDB..tbldiagnosis_ICD i
on i.dx_cd = tc.diag1
and i.HCUP_Chronic_indicator = 1

left join
DWPFK..tblEligibility te
on te.MedicaidID = pn.MEDICAIDID
and te.MonthID = cast(LEFT(CONVERT(VARCHAR,pn.anchor_date,112),6) as int)
and te.EligMed = 1

left join
pfkdb..tblPayor_ProvID_x_NPI pa
on te.AttributedProvider = pa.Payor_ProvID

left join
PFKCC.dbo.DS18_DASH_REFERRALS dr
on pn.MRN = dr.mrn
and dr.entry_date between dateadd(m,-12,pn.anchor_date) and pn.anchor_date

left join
PFKDB..tblProviderByNPI npi
on pa.npi = npi.NPI
and npi.Practice like 'NCH%'

left join
pfkcc..ds18_prev_utc_refused utc
on utc.mrn = pn.mrn
and utc.end_date between dateadd(m,-6,pn.anchor_date) and pn.anchor_date

group by
pn.[MEDICAIDID]
,pn.[FRANKLIN_COUNTY]
,pn.[ZIP]
,pn.[ENROLLMENT_DATE]
,pn.[EVER_ENROLLED]
,pn.[ENROLLED_IN_PERIOD]
,pn.[MALE]
,pn.[MRN]
,pn.[PREVIOUS_ATTEMPTS]
,pn.[PREVIOUSLY_ENROLLED]
,pn.[first_episode_start]
,pn.[NCH_Previous]
,pn.age_at_anchor
,PN.ABD_FLAG
,PN.ASTHMA_FLAG
,PN.DIABETES_FLAG
,PN.CONT_ENROLLMENT_MONTHS
,PN.anchor_date
,te.PayorID
,pn.abd_months
,pn.homehealth_flag
,pn.COUNTY
,pn.uses_pcp
,pn.ABUSE_NEGLECT
,pn.NOT_WITH_PARENTS
,pn.ZIP_PERCENT_URBAN
,pn.ZIP_PERCENT_WHITE
,pn.AVOIDABLE_ED_LAST_12_CNT
,pn.AVOIDABLE_ED_LAST_12_FLAG
,pn.PREV_AVOIDABLE_ED_VISIT

IF OBJECT_ID ('PFKCC.DBO.DS18_UTILIZATION') IS NOT NULL DROP TABLE PFKCC.DBO.DS18_UTILIZATION
SELECT
pn.[MEDICAIDID]
,pn.[FRANKLIN_COUNTY]
,pn.[ZIP]
,pn.[ENROLLMENT_DATE]
,pn.[EVER_ENROLLED]
,pn.[ENROLLED_IN_PERIOD]
,pn.[MALE]
,pn.[MRN]
,pn.[PREVIOUS_ATTEMPTS]
,pn.[PREVIOUSLY_ENROLLED]
,pn.[first_episode_start]
,pn.[NCH_Previous]
,pn.age_at_anchor
,PN.ABD_FLAG
,PN.ASTHMA_FLAG AS PREV_ASTHMA_ED
,PN.DIABETES_FLAG AS PREV_DIABETES_ED
,PN.CONT_ENROLLMENT_MONTHS
,PN.anchor_date
,pn.PayorID
,pn.abd_months
,pn.homehealth_flag
,pn.COUNTY
,pn.USES_PCP
 ,CASE WHEN pn.totalpaid >= 10000 THEN 1 ELSE 0 END as OVER_10K_PAID
,CASE WHEN pn.IPAdmits > 0 then 1 else 0 END as IP_LAST_12_FLAG
,pn.IP_Paid as IP_Paid
,CASE WHEN pn.EDVisits >= 5 THEN 1 ELSE 0 END as ED_VISIT_FLAG_5
,pn.ED_Paid  as ED_Paid
,pn.NCH_Inpatient as NCH_Inpatient
,pn.NCH_ED  as NCH_ED
,pn.PRIM_DX_HCUP_CHRONIC as PRIM_DX_HCUP_CHRONIC
,pn.PRIM_DX_BH as PRIM_DX_BH
,pn.NEONATE as NEONATE
,pn.NCH_ATTRIBUTED_ASSIGNED
,pn.ABUSE_NEGLECT
,pn.NOT_WITH_PARENTS
,pn.ZIP_PERCENT_URBAN
,pn.ZIP_PERCENT_WHITE
,pn.AVOIDABLE_ED_LAST_12_CNT
,pn.AVOIDABLE_ED_LAST_12_FLAG
,pn.PREV_AVOIDABLE_ED_VISIT
 ,pn.REFUSED_UTC_PREV_6_MOS
 ,pn.PROVIDER_REFERRAL_PREV_12_MOS
INTO PFKCC.DBO.DS18_UTILIZATION
FROM
#temp pn