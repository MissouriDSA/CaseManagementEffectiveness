if object_id ('pfkcc.dbo.ds18_enrollment_outreach') is not null drop table pfkcc..DS18_ENROLLMENT_OUTREACH

--specifying care coordination enrollment information, any information associated with anchor date should be added in this query

SELECT
d.MedicaidID,
d.ENROLLMENT_DATE,
d.enrolled as ever_enrolled,
case when d.enrollment_date between '1/1/2016' and '12/31/2016' then 1 else 0 end as enrolled_in_period,
d.MALE,
d.MRN,
d.anchor_date,
datediff(hour,tm.dob,d.anchor_date)/8766.0 as AGE_AT_ANCHOR,
count(distinct ae.EPISODE_ID) as previous_attempts,
max(case when dd.mrn is not null then 1 else 0 end) as previously_enrolled,
min(ae.start_date) as first_episode_start,
case when d.enrolled = 1 and d.ENROLLMENT_DATE < '1/1/2016' then 1 else 0 end as exclude_flag,
count(distinct te.monthid) as CONT_ENROLLMENT_MONTHS,
max(case when te.ABDCFC = 'ABD' THEN 1 ELSE 0 END) AS ABD_FLAG,
Z.COUNTY,
Z.ZIP,
Z.PCP_METHOD,
d.abd_months

INTO PFKCC.dbo.DS18_ENROLLMENT_OUTREACH
FROM
PFKCC.dbo.DS18_DEMOGRAPHICS d

left join
pfkcc..ALL_EPISODES ae
on d.mrn = ae.MRN
AND (ae.EPISODE_ID < d.episode_id or d.episode_id is null)

left join
pfkcc.dbo.ds18_tbl_county_zip z
on d.medicaidid = z.medicaidid

left join
pfkcc..DEDUPED_EPISODES dd
on d.MRN = dd.MRN
and dd.ENROLLMENT_DATE < '1/1/2016'

left join
pfkcc..R9_NEW_CAMS r
on d.mrn = r.mrn
and r.enrollment_date < d.ENROLLMENT_DATE

left join
dwpfk..tbleligibility te
on te.medicaidid = d.medicaidid
and te.eligmed = 1
and te.monthid between cast(LEFT(CONVERT(VARCHAR,dateadd(m,-12,d.anchor_date),112),6) as int) and cast(LEFT(CONVERT(VARCHAR,dateadd(m,-1,d.anchor_date),112),6) as int)

left join
DWPFK..tblMember tm
on d.medicaidid = tm.medicaidid

group by
d.MedicaidID,
d.ENROLLMENT_DATE,
d.enrolled,
d.MALE,
d.MRN,
d.anchor_date,
tm.dob,
Z.COUNTY,
Z.ZIP,
Z.PCP_METHOD,
d.abd_months