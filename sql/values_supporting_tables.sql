INSERT INTO tblFamilyType VALUES
("singleton", "single individual"),
("duo", "one parent and a child"),
("trio", "parents and one children"),
("quad", "parents and 2 siblings"),
("large pedigree", "more than 4 individuals"),
("duo and distant relative", "parent-child and a distant relative"),
("trio and distant relative", "parents-child and a distant relative"),
("proband and distant relative", "proband and a distant relative"),
("other small", "non canonical pedigree with 3 or fewer individuals"),
("other large", "non canonical pedigree with more than 3 individuals");

INSERT INTO tblMainBranch VALUES
("Rare Disease", "Rare disease"),
("Cancer", "Cancer related research"),
("Complex phenotype", "Any complex phenotype");

INSERT INTO tblScreeningMethod VALUES
("Gene panel", "Sequencing of a gene panel"),
("Karyotype", "Screening of the karyotype"),
("WES", "Exome sequencing"),
("Single gene", "Single candidate gene sequenced");

INSERT INTO tblCohortPClass VALUES
("case", "affected individual in a case-control study"),
("control", "healthy control in a case-control study"),
("quantitative phenotype", "individual with a measureable qantitative phenotype");

INSERT INTO tblProbandRelationship VALUES
("father", "biological father"),
("mother", "biological mother"),
("full sibling", "full sibling"),
("half sibling", "sibling with only one parent in common"),
("aunt/uncle", "aunt or uncle"),
("grandparent", "grandfather or grandmother"),
("cousin", "cousin"),
("child", "son or daughter of the proband"),
("other", "any other relationship");

INSERT INTO tblCaseClass VALUES
("Research", "Research related"),
("Clinical", "Clinical related"),
("Validation", "Used in technical validation");

INSERT INTO tblDiseaseGroup VALUES
("Cardiology", "Rare disease", "Cardiology"),
("Connective tissue disorder", "Rare disease", "Connective tissue disorder"),
("Craniofacial", "Rare disease", "Craniofacial"),
("Developmental", "Rare disease", "Developmental"),
("Gastroenterology", "Rare disease", "Gastroenterology"),
("Haematology", "Rare disease", "Haematology"),
("Immunology", "Rare disease", "Immunology"),
("Metabolism", "Rare disease", "Metabolism"),
("Mitochondrial Disease", "Rare disease", "Mitochondrial Disease"),
("Myopathy", "Rare disease", "Myopathy"),
("Nephrology", "Rare disease", "Nephrology"),
("Neurology", "Rare disease", "Neurology"),
("Oncology", "Rare disease", "Oncology"),
("Osteology", "Rare disease", "Osteology"),
("Other", "Rare disease", "Other rare disease"),
("Vascular Disorder", "Rare disease", "Vascular Disorder"),
("Musculoskeletal", "Rare disease", "Musculoskeletal"),
("Neonatal Endocrinology", "Rare disease", "Neonatal Endocrinology"),
("Endochrinology", "Rare disease", "Endochrinology"),
("Other", "Cancer", "Other cancer related");

INSERT INTO tblInheritance VALUES
("HOM", "Strictly recessive where you expect a homo var (consanguinity)"),
("AR",	"Autosomal recessive"),
("DN", "Denovo variant"),
("AD",	"Autosomal dominant"),
("XR",	"X-linked recessive"),
("XD", "X-linked dominant"),
("CH", "Compound heterozygote"),
("CH,DN", "Compound heterozygote with one DN variant");

INSERT INTO tblCaseStatus VALUES
("open", "case is under investigation"),
("closed - unsolved", "no definitice result, no further analysis planned"),
("closed - abandoned", "case is abandoned for any reason (withdraw consent, technical problems, etc)"),
("solved", "definitive solution found, nor further investigation needed"),
("reported", "candidate mutation as been reported to clinician, waiting confirmation");

INSERT INTO tblAnalysisStage VALUES 
("Primary analysis", 1, "Alignement and variant calling"),
("Annotation", 1, "annotation of variants"),
("Interpretation", 2, "candidate variants interpretation"),
("Reporting", 2, "candidate variant(s) reported"),
("Experimental follow-up", 3, "Additional functional / genetic studies on going for a candidate variant");

INSERT INTO tblSampleStage VALUES
("awaiting sample", 1, "waiting to collect sample from individual"),
("sample collected", 1, "the biological sample has been collected"),
("cell culture", 1, "expanding cells in culture"),
("DNA/RNA extraction", 2, "DNA/RNA extracted from sample"),
("extraction failed", 2, "DNA/RNA extraction failed"),
("library preparation", 3, "NGS library preparation in progress"),
("library failed", 3, "failed library preparation"),
("sequencing", 3, "sequencing in progress"),
("data available", 4, "sequencing data available for analysis");


INSERT INTO tblEthnicity VALUES
("AMR", "Admixed american"),
("AFR", "African"),
("NFE", "Non-finnish european"),
("FIN", "Finnish european"),
("EAS", "East-asian"),
("SAS", "South-asian"),
("ASN", "Generic asian"),
("ASJ", "Ashkenazi Jewish"),
("OTH", "Other");

INSERT INTO tblWGS500class VALUES
("A", "Novel gene"),
("B", "Novel gene for this phenotype (but known for other diseases)"),
("C", "Known gene"),
("D", "Strong candidate"),
("E", "Interesting candidate"),
("F", "Putative candidate");