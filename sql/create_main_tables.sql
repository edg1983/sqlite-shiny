DROP TABLE IF EXISTS tblPI;
CREATE TABLE "tblPI" (
	"PIId" INTEGER PRIMARY KEY,
	"PIName"	TEXT UNIQUE NOT NULL,
	"PIContact"	TEXT,
	"PICollaborator"	TEXT,
	"PICollaboratorContact"	TEXT,
	"PIEditBy"	TEXT,
	"PIEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TRIGGER tblPI_Timestamp_Trigger
AFTER UPDATE ON tblPI
BEGIN
   UPDATE tblPI SET PIEditDate = CURRENT_TIMESTAMP WHERE PIId = NEW.PIId;
END;

DROP TABLE IF EXISTS tblProject;
CREATE TABLE "tblProject" (
	"projectID"	INTEGER PRIMARY KEY,
	"projectName"	TEXT NOT NULL,
	"projectAcronym"	TEXT NOT NULL,
	"projectGrant"	TEXT,
	"projectPMID"	INTEGER,
	"projectEditBy"	TEXT NOT NULL,
	"projectEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TRIGGER tblProject_Timestamp_Trigger
AFTER UPDATE ON tblProject
BEGIN
   UPDATE tblProject SET projectEditDate = CURRENT_TIMESTAMP WHERE projectID = NEW.projectID;
END;

DROP TABLE IF EXISTS tblParticipant;
CREATE TABLE "tblParticipant" (
	"participantID" INTEGER PRIMARY KEY,
	"participantLocalID" TEXT UNIQUE NOT NULL,
	"participantRecruitProject" INTEGER NOT NULL,
	"participantRecruitPI" INTEGER NOT NULL,
	"participantEthnicity" TEXT,
	"participantIsProband" INTEGER NOT NULL,
	"participantFatherID" TEXT,
	"participantMotherID" TEXT,
	"participantSex" INTEGER NOT NULL,
	"participantDiseaseGroup" TEXT,
	"participantDisease" TEXT,
	"participantAffected" INTEGER NOT NULL,
	"participantLiving" INTEGER,
	"participantConsent" TEXT,
	"participantEditBy" TEXT,
	"participantEditDate" DATETIME DEFAULT CURRENT_TIMESTAMP,
	
	CHECK (participantSex IN (0, 1, 2)),
	CHECK (participantIsProband IN (0, 1)),
	CHECK (participantAffected IN (0, 1, 2)),
	CHECK (participantLiving IN (0, 1)),
  
  FOREIGN KEY (participantFatherID) REFERENCES tblParticipant (participantID),
  FOREIGN KEY (participantMotherID) REFERENCES tblParticipant (participantID),
  FOREIGN KEY (participantDiseaseGroup) REFERENCES tblDiseaseGroup (diseaseGroup),
	FOREIGN KEY (participantEthnicity) REFERENCES tblEthnicity (ethnicityCode),
	FOREIGN KEY (participantRecruitProject) REFERENCES tblProject (projectID),
	FOREIGN KEY (participantRecruitPI) REFERENCES tblPI (PIId)
);
CREATE INDEX tblParticipant_ID_idx ON tblParticipant (participantID);
CREATE INDEX tblParticipant_localID_idx ON tblParticipant (participantLocalID);

CREATE TRIGGER tblParticipant_Timestamp_Trigger
	AFTER UPDATE ON tblParticipant
	BEGIN
		UPDATE tblParticipant SET participantEditDate = CURRENT_TIMESTAMP WHERE participantID = NEW.participantID;
	END;

DROP TABLE IF EXISTS tblParticipant_altIDs;
CREATE TABLE "tblParticipant_altIDs" (
	"altIDParticipantID"	INTEGER NOT NULL,
	"altIDProjectID"	INTEGER NOT NULL,
	"altIDSource"	TEXT NOT NULL,
	"altIDAlternativeID"	TEXT NOT NULL,
	"altIDEditBy" TEXT,
	"altIDEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP,

	CHECK(altIDSource IN ('mainID','clinicianID','externalGroupID','other')),
	FOREIGN KEY (altIDAlternativeID) REFERENCES tblParticipant (participantID) ON DELETE CASCADE,
	FOREIGN KEY (altIDProjectID) REFERENCES tblProject (projectID)
);

CREATE TRIGGER tblParticipant_altIDs_Timestamp_Trigger
	AFTER UPDATE ON tblParticipant_altIDs
	BEGIN
		UPDATE tblParticipant_altIDs SET altIDEditDate = CURRENT_TIMESTAMP WHERE rowid = NEW.rowid;
	END;

DROP TABLE IF EXISTS tblKnownGenesScreening;
CREATE TABLE "tblKnownGenesScreening" (
	"screeningID"	INTEGER PRIMARY KEY,
	"screeningParticipantID"	INTEGER NOT NULL,
	"screeningMethod"	TEXT NOT NULL,
	"screeningGenes"	TEXT NOT NULL,
	"screeningEditBy" TEXT,
	"screeningEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP,

	FOREIGN KEY (screeningParticipantID) REFERENCES tblParticipant (participantID) ON DELETE CASCADE,
	FOREIGN KEY (screeningMethod) REFERENCES tblScreeningMethod (screeningMethod)
);

CREATE TRIGGER tblKnownGenesScreening_Timestamp_Trigger
	AFTER UPDATE ON tblKnownGenesScreening
	BEGIN
		UPDATE tblKnownGenesScreening SET screeningEditDate = CURRENT_TIMESTAMP WHERE screeningID = NEW.screeningID;
	END;

DROP TABLE IF EXISTS tblParticipant2cohort;
CREATE TABLE "tblParticipant2cohort" (
	"p2cParticipantID"	INTEGER NOT NULL,
	"p2cCohortID"	INTEGER NOT NULL,
	"p2cCohortParticipantClass" TEXT NOT NULL,
	"p2cEditBy" TEXT,
	"p2cEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP,

	FOREIGN KEY (p2cParticipantID) REFERENCES tblParticipant (participantID) ON DELETE CASCADE,
	FOREIGN KEY (p2cCohortID) REFERENCES tblCohort (cohortID) ON DELETE CASCADE,
	FOREIGN KEY (p2cCohortParticipantClass) REFERENCES tblCohortPClass (cohortPClass)
);

CREATE TRIGGER tblParticipant2cohort_Timestamp_Trigger
	AFTER UPDATE ON tblParticipant2cohort
	BEGIN
		UPDATE tblParticipant2cohort SET p2cEditDate = CURRENT_TIMESTAMP WHERE rowid = NEW.rowid;
	END;

DROP TABLE IF EXISTS tblParticipant2family;
CREATE TABLE "tblParticipant2family" (
	"p2fParticipantID"	INTEGER NOT NULL,
	"p2fFamilyID"	INTEGER NOT NULL,
	"p2fRelationToProband" TEXT NOT NULL,
	"p2fEditBy" TEXT,
	"p2fEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP,

	FOREIGN KEY (p2fParticipantID) REFERENCES tblParticipant (participantID) ON DELETE CASCADE,
	FOREIGN KEY (p2fFamilyID) REFERENCES tblFamily (familyID) ON DELETE CASCADE,
	FOREIGN KEY (p2fRelationToProband) REFERENCES tblProbandRelationship (probandRelationship)
);

CREATE TRIGGER tblParticipant2family_Timestamp_Trigger
	AFTER UPDATE ON tblParticipant2family
	BEGIN
		UPDATE tblParticipant2family SET p2fEditBy = CURRENT_TIMESTAMP WHERE rowid = NEW.rowid;
	END;

DROP TABLE IF EXISTS tblCohort;
CREATE TABLE "tblCohort" (
	"cohortID"	INTEGER PRIMARY KEY,
	"cohortProjectID"	INTEGER NOT NULL,
	"cohortLocalID" TEXT UNIQUE NOT NULL,
	"cohortPIId"  INTEGER,
	"cohortMainBranch"	TEXT,
	"cohortClass"	TEXT,
	"cohortDescription" TEXT,
	"cohortDiseaseGroup" TEXT,
	"cohortDisease"	TEXT,
	"cohortLeadAnalyst"	TEXT,
	"cohortAnalysisStage" TEXT,
	"cohortEditBy" TEXT,
	"cohortEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP,

	FOREIGN KEY (cohortMainBranch) REFERENCES tblMainBranch (mainBranch),
	FOREIGN KEY (cohortProjectID) REFERENCES tblProject (projectID),
	FOREIGN KEY (cohortDiseaseGroup) REFERENCES tblDiseaseGroup (diseaseGroup),
	FOREIGN KEY (cohortClass) REFERENCES tblCaseClass (caseClass),
	FOREIGN KEY (familyAnalysisStage) REFERENCES tblAnalysisStage (analysisStage)
);

CREATE TRIGGER tblCohort_Timestamp_Trigger
	AFTER UPDATE ON tblCohort
	BEGIN
		UPDATE tblCohort SET cohortEditBy = CURRENT_TIMESTAMP WHERE cohortID = NEW.cohortID;
	END;

DROP TABLE IF EXISTS tblFamily;
CREATE TABLE "tblFamily" (
	"familyID"	INTEGER PRIMARY KEY,
	"familyLocalID"	TEXT UNIQUE NOT NULL,
	"familyMainBranch"  TEXT NOT NULL,
	"familyType"	TEXT,
	"familyClass"	TEXT,
	"familyStatus"	TEXT,
	"familyDiseaseGroup"  TEXT,
	"familyDisease" TEXT,
	"familyInheritance"	TEXT,
	"familyConsanguinity"	INTEGER DEFAULT 0,
	"familyLeadAnalyst"	TEXT,
	"familyAnalysisStage"  TEXT,
	"familyEditBy"	TEXT,
	"familyEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP,

	CHECK(familyConsanguinity IN (0,1)),

	FOREIGN KEY (familyType) REFERENCES tblFamilyType (familyType),
	FOREIGN KEY (familyMainBranch) REFERENCES tblMainBranch (mainBranch),
	FOREIGN KEY (familyClass) REFERENCES tblCaseClass (caseClass),
	FOREIGN KEY (familyStatus) REFERENCES tblCaseStatus (caseStatus),
	FOREIGN KEY (familyDiseaseGroup) REFERENCES tblDiseaseGroup (diseaseGroup),
	FOREIGN KEY (familyInheritance) REFERENCES tblInheritance (inheritance),
	FOREIGN KEY (familyAnalysisStage) REFERENCES tblAnalysisStage (analysisStage)
);
CREATE INDEX tblFamily_ID_idx ON tblFamily (familyID);
CREATE INDEX tblFamily_localID_idx ON tblFamily (familyLocalID);

CREATE TRIGGER tblFamily_Timestamp_Trigger
	AFTER UPDATE ON tblFamily
	BEGIN
		UPDATE tblFamily SET familyEditDate = CURRENT_TIMESTAMP WHERE familyID = NEW.familyID;
	END;
	
DROP TABLE IF EXISTS tblResultRareDisease;
CREATE TABLE "tblResultRareDisease" (
	"resultID"	INTEGER PRIMARY KEY,
	"resultFamilyID"	INTEGER NOT NULL,
	"genome" TEXT NOT NULL,
	"chromosome" TEXT NOT NULL,
	"start_position" INTEGER NOT NULL,
	"end_position" INTEGER,
	"variant_type" TEXT NOT NULL,
	"gene_symbol" TEXT NOT NULL,
	"transcript_id" TEXT,
	"consequence" TEXT,
	"cDNA_variant" TEXT,
	"protein_variant" TEXT,
	"resultInheritance" TEXT,
	"additional_genes" TEXT,
	"WGS500_class" TEXT,
	"accepted_by_clinician" INTEGER,
	"resultRareEditBy" TEXT,
	"resultRareEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP,

  CHECK(accepted_by_clinician IN (0,1)),
  
  FOREIGN KEY (genome) REFERENCE tblGenomes(genome),
	FOREIGN KEY (resultFamilyID) REFERENCES tblFamily (familyID),
	FOREIGN KEY (variant_type) REFERENCES tblVariantTypes (variantType),
	FOREIGN KEY (consequence) REFERENCES tblVariantConsequence (consequence),
	FOREIGN KEY (resultInheritance) REFERENCES tblInheritance (inheritance),
	FOREIGN KEY (WGS500_class) REFERENCES tblWGS500class (variantClass)
);

CREATE TRIGGER tblResultRareDisease_Timestamp_Trigger
	AFTER UPDATE ON tblResultRareDisease
	BEGIN
		UPDATE tblResultRareDisease SET resultRareEditBy = CURRENT_TIMESTAMP WHERE rowid = NEW.rowid;
	END;
	
DROP TABLE IF EXISTS tblNotesResults;
CREATE TABLE "tblNotesResults" (
	"notesResultID"	INTEGER NOT NULL,
	"notesResultText" TEXT NOT NULL,
	"notesRareEditBy" TEXT,
	"notesRareEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP,

  FOREIGN KEY (notesResultID) REFERENCE tblResultRareDisease(resultID) ON DELETE CASCADE,
);

CREATE TRIGGER tblNotesResults_Timestamp_Trigger
	AFTER UPDATE ON tblNotesResults
	BEGIN
		UPDATE tblNotesResults SET notesRareEditBy = CURRENT_TIMESTAMP WHERE rowid = NEW.rowid;
	END;
	
DROP TABLE IF EXISTS tblNotesParticipants;
CREATE TABLE "tblNotesResults" (
	"notesParticipantID"	INTEGER NOT NULL,
	"notesParticipantText" TEXT NOT NULL,
	"notesParticipantEditBy" TEXT,
	"notesParticipantEditDate"	DATETIME DEFAULT CURRENT_TIMESTAMP,

  FOREIGN KEY (notesParticipantID) REFERENCE tblParticipant(participantID) ON DELETE CASCADE,
);

CREATE TRIGGER tblNotesParticipants_Timestamp_Trigger
	AFTER UPDATE ON tblNotesParticipants
	BEGIN
		UPDATE tblNotesParticipants SET notesParticipantEditBy = CURRENT_TIMESTAMP WHERE rowid = NEW.rowid;
	END;