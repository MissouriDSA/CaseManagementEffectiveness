IF OBJECT_ID ('PFKCC.dbo.DS18_TBL_COUNTY_ZIP') IS NOT NULL DROP TABLE PFKCC.dbo.DS18_TBL_COUNTY_ZIP

SELECT
DD.MEDICAIDID,
TE.COUNTY,
TE.ZIP,
TE.PCP_METHOD
INTO 
PFKCC.DBO.DS18_TBL_COUNTY_ZIP

FROM
PFKCC.DBO.DS18_DEMOGRAPHICS DD


LEFT JOIN
DWPFK..TBLELIGIBILITY TE
ON DD.MEDICAIDID = TE.MEDICAIDID
AND TE.MONTHID = cast(LEFT(CONVERT(VARCHAR,dd.anchor_date,112),6) as int)