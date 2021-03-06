/*Sample SQL scripts for the HFMA Analytics Boot Camp*/
-- Extracting data from a table
SELECT TOP 1000 [ClaimNumber]
      ,[BeneNumber]
      ,[ClaimType]
      ,[ClaimFromDate]
      ,[ClaimThruDate]
      ,[ProviderSpecialty]
      ,[DRGcode]
      ,[HCPCScode]
      ,[NewPaidAmount]
FROM [dbo].[Claims]

SELECT * FROM [dbo].[Claims]
  -----------------------
  -- Join two tables
  -- Note that DRGcode has a leading zero; need to remove it
SELECT * FROM dbo.Claims
LEFT JOIN Reference.dbo.DRGs
ON [MS-DRG] = RIGHT(DRGcode,3)

-- What Reference.dbo.DRGs are in the data?
SELECT DISTINCT DRGcode
FROM dbo.Claims

-- Note the NULL fields
-- Filter down to inpatient claims only
SELECT * FROM dbo.Claims
LEFT JOIN Reference.dbo.DRGs
ON [MS-DRG] = RIGHT(DRGcode,3)
where len(DRGcode)>0 and ISNULL(DRGcode,'0000') <> '0000'

-- Filter down to medical DRGs
SELECT * FROM dbo.Claims
LEFT JOIN Reference.dbo.DRGs
ON [MS-DRG] = RIGHT(DRGcode,3)
WHERE ISNULL([MS-DRG],'0000') <> '0000'
	AND [TYPE] = 'MED'

-- Calculate LOS
SELECT *
	,DATEDIFF(d,ClaimFromDate,ClaimThruDate) AS LOS
FROM dbo.Claims
LEFT JOIN Reference.dbo.DRGs
ON [MS-DRG] = RIGHT(DRGcode,3)
WHERE ISNULL([MS-DRG],'0000') <> '0000'

-- INNER JOIN is a filter
SELECT *
FROM dbo.Claims
inner JOIN Reference.dbo.DRGs
ON [MS-DRG] = RIGHT(DRGcode,3)

-- Summarize data by highest-paid Reference.dbo.DRGs
SELECT [DRGcode], SUM(cast([NewPaidAmount] as float)) AS SumPaid
FROM dbo.Claims
WHERE ISNULL([DRGcode],'0000') <> '0000'
GROUP BY DRGcode
ORDER BY SUM(cast([NewPaidAmount] as float)) desc

-- Set a primary key on Reference.dbo.DRGs
ALTER TABLE dbo.DRGs ADD CONSTRAINT
	PK_DRGs PRIMARY KEY CLUSTERED 
	(
	[MS-DRG]
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

GO

-- Create the un-normalized data table
SELECT Claims.ClaimNumber, Claims.BeneNumber, Claims.ClaimType, Claims.ClaimFromDate, Claims.ClaimThruDate, 
    Claims.ProviderSpecialty, Claims.DRGcode, Claims.HCPCScode, Claims.NewPaidAmount, Members.MemberID, 
    Members.ZipCode, Members.DOB, Members.FirstName, Members.LastName, Reference.dbo.DRGs.[MS-DRG], Reference.dbo.DRGs.MDC, Reference.dbo.DRGs.TYPE, 
    Reference.dbo.DRGs.[MS-DRG Title], Reference.dbo.DRGs.DRGandTitle, Reference.dbo.DRGs.Weights, Reference.dbo.DRGs.[Geometric mean LOS], Reference.dbo.DRGs.[Arithmetic mean LOS], Reference.dbo.HCPCS.[cpt code], 
    Reference.dbo.HCPCS.[Major Category], Reference.dbo.HCPCS.[Minor category], Reference.dbo.HCPCS.[CPT description], Reference.dbo.MedicalSpecialties.SpecialtyCode, Reference.dbo.MedicalSpecialties.Specialty, 
    Reference.dbo.MedicalSpecialties.Professional, Reference.dbo.MedicalSpecialties.IsPhysician
FROM Claims 
LEFT JOIN Members ON Claims.BeneNumber = Members.MemberID 
LEFT JOIN Reference.dbo.DRGs ON Claims.DRGcode = Reference.dbo.DRGs.[MS-DRG] 
LEFT JOIN Reference.dbo.HCPCS ON Claims.HCPCScode = Reference.dbo.HCPCS.[cpt code] 
LEFT JOIN Reference.dbo.MedicalSpecialties ON Claims.ProviderSpecialty = Reference.dbo.MedicalSpecialties.SpecialtyCode
GO

-- Make the un-normalized data into a VIEW
CREATE VIEW vw_UnNormalData AS 
SELECT Claims.ClaimNumber, Claims.BeneNumber, Claims.ClaimType, Claims.ClaimFromDate, Claims.ClaimThruDate, 
    Claims.ProviderSpecialty, Claims.DRGcode, Claims.HCPCScode, Claims.NewPaidAmount, Members.MemberID, 
    Members.ZipCode, Members.DOB, Members.FirstName, Members.LastName, Reference.dbo.DRGs.[MS-DRG], Reference.dbo.DRGs.MDC, Reference.dbo.DRGs.TYPE, 
    Reference.dbo.DRGs.[MS-DRG Title], Reference.dbo.DRGs.DRGandTitle, Reference.dbo.DRGs.Weights, Reference.dbo.DRGs.[Geometric mean LOS], Reference.dbo.DRGs.[Arithmetic mean LOS], Reference.dbo.HCPCS.[cpt code], 
    Reference.dbo.HCPCS.[Major Category], Reference.dbo.HCPCS.[Minor category], Reference.dbo.HCPCS.[CPT description], Reference.dbo.MedicalSpecialties.SpecialtyCode, Reference.dbo.MedicalSpecialties.Specialty, 
    Reference.dbo.MedicalSpecialties.Professional, Reference.dbo.MedicalSpecialties.IsPhysician
FROM Claims 
LEFT JOIN Members ON Claims.BeneNumber = Members.MemberID 
LEFT JOIN Reference.dbo.DRGs ON Claims.DRGcode = Reference.dbo.DRGs.[MS-DRG] 
LEFT JOIN Reference.dbo.HCPCS ON Claims.HCPCScode = Reference.dbo.HCPCS.[cpt code] 
LEFT JOIN Reference.dbo.MedicalSpecialties ON Claims.ProviderSpecialty = Reference.dbo.MedicalSpecialties.SpecialtyCode
GO

-- Test
SELECT TOP 10 * FROM vw_UnNormalData

-- Clean up
DROP VIEW vw_UnNormalData
GO

-- Create a SQL program
CREATE PROC usp_CreateInpatientClaims AS
	-- Create the table
	SELECT *
	INTO InpatientClaims
	FROM dbo.Claims
	inner JOIN Reference.dbo.DRGs
	ON [MS-DRG] = RIGHT(DRGcode,3)

	UPDATE [dbo].[InpatientClaims]
	SET [DRGcode] = RIGHT(DRGcode,3)
GO
-- Test
EXEC dbo.usp_CreateInpatientClaims
SELECT * FROM InpatientClaims
-- Clean up
DROP TABLE [dbo].[InpatientClaims]
DROP PROC dbo.usp_CreateInpatientClaims

--STUFF TO TRY
-- Compute average LOS by DRG; then by MDC
-- Create a report by beneficiary by date
-- Which specialty has the highest paid amount?
