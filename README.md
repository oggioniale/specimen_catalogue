# Specimen catalogue
This repository includes a solution to collect info about specimen (by excel file), transform info into XML and to turtle (TTL) using R functions, and translate them to HTML landing pages trough XSLT.

The implementation has been done following [Physical Sample Curation recommendations of ESIP](https://wiki.esipfed.org/Physical_Sample_Curation), SOSA ontology main version https://www.w3.org/TR/vocab-ssn/ (19-10-2017) and the new draft https://w3c.github.io/sdw-sosa-ssn/ssn/ (09-02-2024 - https://github.com/w3c/sdw-sosa-ssn?tab=readme-ov-file), but also XSD schema solutions from SESAR (System for Earth Sample Registration) [1] and from IGSN CSIRO (Commonwealth Scientific and Industrial Research Organisation) [2].
Thins compliance allows to have a compliant also with DataCite.

[1] System for Earth Sample Registration (SESAR). 2020. SESAR XML Schema for samples (Version 4.0). Zenodo. http://doi.org/10.5281/zenodo.3875531

[2] IGSN CSIRO version 3.0 - https://igsn.csiro.au/schemas/3.0/ 08-2019

The production flow is illustrated in the figure:

![Screenshot 2024-05-16 at 14 25 00](https://github.com/oggioniale/specimen_catalogue/assets/1393893/ef91cc5c-f761-470a-b89e-310b396fc15d)


# The workflow of this app is:
1. fill the `specimen_template.xlsx` spreadsheet
2. use `specimen_catalogue()` function for obtain IGSN CSIRO XML and ttl from SOSA Ontology for each record(s) present in the speadsheet.
