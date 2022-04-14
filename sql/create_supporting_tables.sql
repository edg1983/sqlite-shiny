CREATE TABLE "tblFamilyType" (
	"familyType"	TEXT UNIQUE NOT NULL,
	"description"	TEXT
);

CREATE TABLE "tblMainBranch" (
  "mainBranch"  TEXT UNIQUE NOT NULL,
  "description" TEXT
);

CREATE TABLE "tblScreeningMethod" (
  "screeningMethod"  TEXT UNIQUE NOT NULL,
  "description" TEXT
);

CREATE TABLE "tblCohortPClass" (
  "cohortPClass"  TEXT UNIQUE NOT NULL,
  "description" TEXT
);

CREATE TABLE "tblProbandRelationship" (
  "probandRelationship"  TEXT UNIQUE NOT NULL,
  "description" TEXT
);

CREATE TABLE "tblCaseClass" (
	"caseClass"	TEXT UNIQUE NOT NULL,
	"description"	TEXT	
);

CREATE TABLE "tblDiseaseGroup" (
	"diseaseGroup"	TEXT NOT NULL UNIQUE,
	"category"	TEXT NOT NULL,
	"description"	TEXT,
	FOREIGN KEY(category) REFERENCES tblCaseType(caseType)
);

CREATE TABLE "tblInheritance" (
	"inheritance"	TEXT UNIQUE NOT NULL,
	"description"	TEXT	
);

CREATE TABLE "tblCaseStatus" (
	"caseStatus"	TEXT UNIQUE NOT NULL,
	"description"	TEXT	
);

CREATE TABLE "tblAnalysisStage" (
	"analysisStage"	TEXT UNIQUE NOT NULL,
	"analysisPhase"	INTEGER,
	"description"	TEXT	
);

CREATE TABLE "tblSampleStage" (
	"sampleStage"	TEXT UNIQUE NOT NULL,
	"samplePhase"	INTEGER,
	"description"	TEXT	
);

CREATE TABLE "tblEthnicity" (
	"ethnicityCode"	TEXT UNIQUE NOT NULL,
	"description"	TEXT	
);

CREATE TABLE "tblVariantConsequence" (
  "consequence" TEXT NOT NULL,
  "description" TEXT
);

CREATE TABLE "tblWGS500class" (
  "variantClass" TEXT NOT NULL,
  "description" TEXT
)