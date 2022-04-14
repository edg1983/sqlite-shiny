INSERT INTO tblPI (PIName, PIContact, PIEditBy)
VALUES
  ("PI1","pi1@email.com", "EG"),
  ("PI2","pi2@email.com", "EG"),
  ("PI3","pi3@email.com", "EG");
  
INSERT INTO tblWorkflowStatus (workflowStatus, workflowPhase)
VALUES
  ("sample collected", 1),
  ("DNA extracted", 2),
  ("library prep", 2),
  ("sequencing", 3),
  ("analysis", 3),
  ("interpretation", 4);
  
INSERT INTO tblProject (projectName, projectAcronym, projectEditBy)
VALUES
  ("OxClinWGS", "HICF2", "EG"),
  ("SCZ trios", "MIND", "EG");
  
INSERT INTO tblInheritance (inheritance, description)
VALUES
  ("autosomal recessive", "AR"),
  ("autosomal dominant", "AD"),
  ("X-linked recessive", "XR"),
  ("X-linked dominant", "XD");
  
INSERT INTO tblMainBranch (mainBranch, description)
VALUES
  ("Rare disease", "rare disease study"),
  ("Cancer", "cancer related"),
  ("Complex phenotype", "complex pheno");
  
INSERT INTO tblEthnicity (ethnicityCode, description)
VALUES
  ("AFR", "African"),
  ("NFE", "non-finnish european"),
  ("AMR", "Admixed american"),
  ("SAS", "south asian"),
  ("EAS", "east asian"),
  ("FIN", "finnish");
  
INSERT INTO tblDiseaseGroup (diseaseGroup, category)
VALUES
  ("neurology", "Rare disease"),
  ("developmental", "Rare disease"),
  ("muscoloskeletal", "Rare disease"),
  ("gastrointestinal", "Cancer"),
  ("brain", "cancer");
  
INSERT INTO tblCaseStatus (caseStatus, description)
VALUES
  ("open", "under investigation"),
  ("closed", "closed with a solution"),
  ("abandoned", "withdrawn or not solved");
  
INSERT INTO tblCaseClass (caseClass, description)
VALUES
  ("research", "reaserch case"),
  ("clinical", "clinical sample"),
  ("validation", "technical validation sample");
  
INSERT INTO tblCohortPClass (cohortPClass, description)
VALUES
  ("case", "case in case-control"),
  ("control", "control in case-control"),
  ("quantitative", "quantitative trait");
  
INSERT INTO tblProbandRelationship (probandRelationship, description)
VALUES
  ("proband", "this is the proband"),
  ("mother", "mother"),
  ("father", "father"),
  ("uncle", "uncle"),
  ("grandfather", "grandfather"),
  ("grandmother", "grandmother");

INSERT INTO tblFamilyType (familyType, description)
VALUES
  ("singleton", "single individual"),
  ("due", "proband and one parent"),
  ("trio", "canonical trio proband and parents"),
  ("quad", "parents and 2 siblings"),
  ("large family", "larger pedigree");
  
INSERT INTO tblScreeningMethod (screeningMethod, description)
VALUES
  ("panel sequencing", "all gene sequenced"),
  ("sanger single mutation", "known mut sequenced"),
  ("WES", "whole exome"),
  ("MLPA", "CNV detected with MLPA"),
  ("aCGH", "array CHG");
  
