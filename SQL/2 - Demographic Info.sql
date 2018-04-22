if object_id ('PFKCC.dbo.DS18_DEMOGRAPHICS') is not null drop table PFKCC.dbo.DS18_DEMOGRAPHICS
--selecting care coordinated vs. not and adding important demographic characteristics

SELECT DISTINCT
te.MedicaidID,
max(e.ENROLLMENT_DATE) AS ENROLLMENT_DATE,
MAX(E.FIRST_EPISODE_ID) AS EPISODE_ID,
max(case when e.mrn is not null then 1 else 0 end) as enrolled,
CASE WHEN max(te.sex) = 'M' THEN 1 ELSE 0 END as MALE,
case when max(e.mrn) is not null then max(e.mrn) else max(mm.mrn) end as MRN,
min(tc.svcdt) as FIRST_SERVICE_DATE,
case when max(e.enrollment_date) is null then min(tc.svcdt) else max(e.enrollment_date) end as ANCHOR_DATE,
count(distinct case when te.abdcfc='ABD' then te.monthid else null end) as abd_months

INTO PFKCC.DBO.DS18_DEMOGRAPHICS
FROM
DWPFK.dbo.tbleligibility te

left join
pfkcc..DEDUPED_EPISODES e
on te.medicaidid = e.MR_MEDICAID_ID
and e.ENROLLMENT_DATE <= '12/31/2016'
and (e.EPISODE_END_DATE is null or e.EPISODE_END_DATE >= '1/1/2016')

left join
pfkcc..TBLMEMBER_MATCHED mm
on te.medicaidid = mm.MEDICAIDID

left join
dwpfk..tblclaims tc
on te.medicaidid = tc.medicaidid
and tc.incmonthid between 201601 and 201612

where
te.PayorID in (11,13,14)
and te.MonthID between 201601 and 201612
and te.EligMed = 1

group by
te.MedicaidID



