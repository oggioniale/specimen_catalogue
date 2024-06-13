# Specimen catalogue
This repository includes a comprehensive solution to manage specimen information. The solution encompasses the following processes:

1. Data Collection: Collect information about specimens using an Excel file.
2. Data Transformation: Convert the collected information into XML and Turtle (TTL) formats using R functions.
3. Data Presentation: Translate the transformed data into HTML landing pages via XSLT.

*Implementation Standards*

The implementation adheres to several standards and recommendations to ensure compliance and interoperability.

- ESIP: the solution follows the [Physical Sample Curation recommendations](https://wiki.esipfed.org/Physical_Sample_Curation);
- SOSA Ontology: the main version of the SOSA ontology (19-10-2017) - [W3C SOSA Ontology](https://www.w3.org/TR/vocab-ssn/);
- The new draft of the SOSA ontology (09-02-2024) -  [W3C SOSA-SSN Draft and its GitHub repository](https://github.com/w3c/sdw-sosa-ssn?tab=readme-ov-file);
- SESAR (System for Earth Sample Registration) XSD schema solutions [1];
- IGSN (International Generic Sample Number) CSIRO (Commonwealth Scientific and Industrial Research Organisation) schema compliance [2].

By adhering to the above standards and schemas, the solution ensures compliance with DataCite requirements, facilitating standardized and interoperable specimen data management.

Production Flow
The production flow of this implementation is illustrated in the figure below:

![image](https://github.com/oggioniale/specimen_catalogue/assets/1393893/11f6019e-7a48-424b-a6c9-6b557b501331)

# The workflow of this application consists of the following steps:

1. Fill the Spreadsheet: complete the `specimen_template.xlsx` spreadsheet with the relevant specimen information;

2. Generate XML and TTL: use the `s`ecimen_catalogue()` function to generate IGSN CSIRO XML and TTL (Turtle) files based on the SOSA Ontology for each record present in the spreadsheet.
