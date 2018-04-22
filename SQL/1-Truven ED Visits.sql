if object_id('PFKCC.dbo.DS18_ED_ALL_DIAG') IS NOT NULL DROP TABLE PFKCC.dbo.DS18_ED_ALL_DIAG
select
Member_ID_Unencrypted as MEMBERID,
Person_ID_Unencrypted AS MEDICAIDID,
Date_of_First_Service,
Facility_Name_Standard,
Diagnosis_Code_Principal,
Diagnosis_Code_02,
Diagnosis_Code_03,
Diagnosis_Code_04,
Diagnosis_Code_05,
Diagnosis_Code_06,
Diagnosis_Code_07,
Diagnosis_Code_08,
Diagnosis_Code_09,
Diagnosis_Code_10,
Diagnosis_Code_11,
Diagnosis_Code_12,
Diagnosis_Code_13,
Diagnosis_Code_14,
Diagnosis_Code_15,
Diagnosis_Code_16,
Diagnosis_Code_17,
Diagnosis_Code_18,
Diagnosis_Code_19,
Diagnosis_Code_20,
Diagnosis_Code_21,
Diagnosis_Code_22,
Diagnosis_Code_23,
Diagnosis_Code_24,
Diagnosis_Code_25,
ISNULL(E.AVOIDABLE_FLAG,0) AS AVOIDABLE_FLAG

INTO PFKCC.dbo.DS18_ED_ALL_DIAG
from
DWPFK_TRUVEN..Med m

LEFT JOIN
PFKCC..DS18_AVOIDABLE_ED_DX_LIST E
ON M.Diagnosis_Code_Principal = E.icd10cm
AND E.AVOIDABLE_FLAG = 1

where
Revenue_Code_UB in ('0450','0451','0452','0453','0454','0455','0457','0458','0459','0681','0682','0683','0684','0689','0981')


group by
Member_ID_Unencrypted,
Person_ID_Unencrypted,
Date_of_First_Service,
Facility_Name_Standard,
Diagnosis_Code_Principal,
Diagnosis_Code_02,
Diagnosis_Code_03,
Diagnosis_Code_04,
Diagnosis_Code_05,
Diagnosis_Code_06,
Diagnosis_Code_07,
Diagnosis_Code_08,
Diagnosis_Code_09,
Diagnosis_Code_10,
Diagnosis_Code_11,
Diagnosis_Code_12,
Diagnosis_Code_13,
Diagnosis_Code_14,
Diagnosis_Code_15,
Diagnosis_Code_16,
Diagnosis_Code_17,
Diagnosis_Code_18,
Diagnosis_Code_19,
Diagnosis_Code_20,
Diagnosis_Code_21,
Diagnosis_Code_22,
Diagnosis_Code_23,
Diagnosis_Code_24,
Diagnosis_Code_25,
E.AVOIDABLE_FLAG




order by 2,3

if object_id('PFKCC.dbo.DS18_ED_COUNT') IS NOT NULL DROP TABLE PFKCC.dbo.DS18_ED_COUNT
SELECT
MEMBERID,
MEDICAIDID,
DATE_OF_FIRST_SERVICE,
FACILITY_NAME_STANDARD,
COUNT(DISTINCT CONCAT(DATE_OF_FIRST_SERVICE,FACILITY_NAME_STANDARD)) AS VISIT_COUNT,
ISNULL(MAX(A.AVOIDABLE_FLAG),0) AS AVOIDABLE_FLAG,
CONCAT(MEMBERID,DATE_OF_FIRST_SERVICE,FACILITY_NAME_STANDARD) AS ENCOUNTER_ID

INTO PFKCC.dbo.DS18_ED_COUNT
FROM
PFKCC.dbo.DS18_ED_ALL_DIAG A

GROUP BY
MEMBERID,
MEDICAIDID,
DATE_OF_FIRST_SERVICE,
FACILITY_NAME_STANDARD

ORDER BY 4 DESC

