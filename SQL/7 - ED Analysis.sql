if object_id('tempdb..#final') is not null drop table #final
select
u.MEDICAIDID,
u.anchor_date,
u.ENROLLED_IN_PERIOD,
u.ENROLLMENT_DATE,
min(te.monthid) as measurement_start,
max(te.MonthID) as measurement_end,
count(distinct te.monthid) as member_months_meas

into #final
from
pfkcc..DS18_UTILIZATION u

left join
DWPFK..tblEligibility te
on u.MEDICAIDID = te.MedicaidID
and te.EligMed = 1
and te.MonthID between cast(LEFT(CONVERT(VARCHAR,dateadd(m,1,anchor_date),112),6) as int)
															and cast(LEFT(CONVERT(VARCHAR,cast('8/31/2017' as date),112),6) as int)


where
u.PayorID is not null

group by
u.MEDICAIDID,
u.anchor_date,
u.ENROLLED_IN_PERIOD,
u.ENROLLMENT_DATE

if object_id('PFKCC.DBO.DS18_ED_MEASUREMENT') is not null drop table PFKCC.DBO.DS18_ED_MEASUREMENT
select
f.MEDICAIDID,
f.anchor_date,
f.ENROLLED_IN_PERIOD,
f.ENROLLMENT_DATE,
f.measurement_start,
f.measurement_end,
f.member_months_meas,
count(distinct concat(ed.DATE_OF_FIRST_SERVICE,ed.FACILITY_NAME_STANDARD,ed.MEMBERID)) as AVOIDABLE_ED_MEAS

INTO PFKCC.dbo.DS_18_ED_MEASUREMENT
from
#final f

left join
pfkcc..DS18_ED_COUNT ed
on f.MEDICAIDID = ed.MEDICAIDID
and ed.AVOIDABLE_FLAG = 1
and CAST(LEFT(CONVERT(VARCHAR,ed.DATE_OF_FIRST_SERVICE,112),6) AS INT) BETWEEN f.measurement_start and f.measurement_end

GROUP BY
f.MEDICAIDID,
f.anchor_date,
f.ENROLLED_IN_PERIOD,
f.ENROLLMENT_DATE,
f.measurement_start,
f.measurement_end,
f.member_months_meas