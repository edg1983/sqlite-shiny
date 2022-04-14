DROP TABLE IF EXISTS tblCase;
CREATE TABLE tblCase (
  "caseID"  INTEGER PRIMARY KEY,
  "caseLocalID" TEXT UNIQUE NOT NULL,
  "caseConsanguineity" INTEGER,
  "caseWorkflowStatus"  TEXT,
  "caseType"  TEXT NOT NULL,
  "caseEditBy" TEXT NOT NULL,
  "caseEditDate" DATETIME DEFAULT CURRENT_TIMESTAMP,
  
  CHECK(caseConsanguineity IN (0,1)),
  FOREIGN KEY(caseWorkflowStatus) REFERENCES tblWorkflowStatus(status),
  FOREIGN KEY(caseType) REFERENCES tblCaseType(caseType)
);

DROP TABLE IF EXISTS tblParticipant;
CREATE TABLE tblParticipant (
  "participantID"  INTEGER PRIMARY KEY,
  "participantLocalID" TEXT UNIQUE NOT NULL,
  "participantCaseID" INTEGER NOT NULL,
  "participantProband" INTEGER NOT NULL,
  "participantAffected" INTEGER NOT NULL,
  "participantSex" INTEGER NOT NULL,
  "participantEthnicity" TEXT,
  "participantEditBy" TEXT NOT NULL,
  "participantEditDate" DATETIME DEFAULT CURRENT_TIMESTAMP,
  
  CHECK(participantProband IN (0,1)),
  CHECK(participantSex IN (0,1,2)),
  CHECK(participantAffected IN (0,1,2)),
  FOREIGN KEY(participantCaseID) REFERENCES tblCase(caseID)
);

DROP TABLE IF EXISTS tblSample;
CREATE TABLE tblSample (
  "sampleID"  INTEGER PRIMARY KEY,
  "sampleLocalID" TEXT NOT NULL,
  "sampleParticipantID" INTEGER NOT NULL,
  "sampleBioSource" TEXT NOT NULL,
  "sampleType" TEXT NOT NULL,
  "sampleAvailable" INTEGER NOT NULL,
  "sampleEditBy" TEXT NOT NULL,
  "sampleEditDate" DATETIME DEFAULT CURRENT_TIMESTAMP,
  
  CHECK(sampleAvailable IN (0,1)),
  FOREIGN KEY(sampleParticipantID) REFERENCES tblParticipant(participantID),
  FOREIGN KEY(sampleBioSource) REFERENCES tblBioSource(bioSource),
  FOREIGN KEY(sampleType) REFERENCES tblSampleType(sampleType)
);

DROP TABLE IF EXISTS tblWorkflowStatus;
CREATE TABLE tblWorkflowStatus (
  "status"  TEXT NOT NULL,
  "description" TEXT
);

DROP TABLE IF EXISTS tblCaseType;
CREATE TABLE tblCaseType (
  "caseType"  TEXT NOT NULL,
  "description" TEXT
);

DROP TABLE IF EXISTS tblSampleType;
CREATE TABLE tblSampleType (
  "sampleType"  TEXT NOT NULL,
  "description" TEXT
);

DROP TABLE IF EXISTS tblBioSource;
CREATE TABLE tblBioSource (
  "bioSource"  TEXT NOT NULL,
  "description" TEXT
);

DROP VIEW IF EXISTS caseSamples;
CREATE VIEW caseSamples AS
SELECT 
  caseLocalID, caseWorkflowStatus,
  participantLocalID, participantProband, participantAffected,
  sampleLocalID, sampleBioSource, sampleType
FROM tblCase
LEFT JOIN tblParticipant ON participantCaseID == caseID
LEFT JOIN tblSample ON sampleParticipantID == participantID;

DROP VIEW IF EXISTS samplesStatus;
CREATE VIEW samplesStatus AS
SELECT
  sampleLocalID, sampleBioSource, sampleType,
  participantLocalID,
  caseLocalID, caseWorkflowStatus
FROM tblSample
LEFT JOIN tblParticipant ON participantID == sampleParticipantID
LEFT JOIN tblCase ON caseID == participantCaseID;
  
FROM tblCase
LEFT JOIN tblParticipant ON participantCaseID == caseID
LEFT JOIN tblSample ON sampleParticipantID == participantID;

INSERT INTO tblBioSource(bioSource, description)
  VALUES
  ("blood", "whole blood drawn"),
  ("saliva", "saliva sample"),
  ("plasma", "plasma sample"),
  ("cultured cells", "extracted from cell culture");

INSERT INTO tblSampleType(sampleType, description)
  VALUES
  ("DNA", "whole genome DNA"),
  ("RNA", "RNA"),
  ("protein", "proteins");

INSERT INTO tblWorkflowStatus(status, description)
  VALUES
  ("open", "under investigation"),
  ("closed", "not further work"),
  ("interpretation", "interpretation of variants");
  
INSERT INTO tblCaseType(caseType, description)
  VALUES
  ("cancer", "cancer related"),
  ("rare disease", "rare disease"),
  ("complex phenotype", "complex risk alleles"),
  ("other", "other type");