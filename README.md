[![](https://img.shields.io/badge/doi-10.5281/zenodo.14517494-blue.svg)](https://doi.org/10.5281/zenodo.14517494)

# Specimen Catalogue
This repository includes a comprehensive solution to manage specimen information. The solution encompasses the following processes:

1. Data Collection: Collect information about specimens using an Excel file.
2. Data Transformation: Convert the collected information into XML and Turtle (TTL) formats using R functions.
3. Data Presentation: Translate the transformed data into HTML landing pages via XSLT.

*Implementation Standards*

The implementation adheres to several standards and recommendations to ensure compliance and interoperability.

- ESIP: the solution follows the [Physical Sample Curation recommendations](https://wiki.esipfed.org/Physical_Sample_Curation);
- SOSA Ontology: the main version of the SOSA ontology (19-10-2017) - [W3C SOSA Ontology](https://www.w3.org/TR/vocab-ssn/);
- The extension of the SOSA ontology (05-09-2024) - [W3C SOSA-SSN Draft](https://w3c.github.io/sdw-sosa-ssn/ssn/) and its [GitHub repository](https://github.com/w3c/sdw-sosa-ssn?tab=readme-ov-file);
- SESAR (System for Earth Sample Registration) XSD schema solutions [1];
- IGSN (International Generic Sample Number) CSIRO (Commonwealth Scientific and Industrial Research Organisation) schema compliance [2];
- [TDWG MIDS (Minimum Information about a Digital Specimen) specification](https://github.com/tdwg/mids/blob/working-draft/current-draft%20/MIDS-definition-v0.17-13Jul2023.md) at [level 1](https://github.com/tdwg/mids/blob/working-draft/current-draft/MIDS-definition-v0.17-13Jul2023.md#42-information-elements-expected-at-mids-level-1).

By adhering to the above standards and schemas, the solution ensures compliance with DataCite requirements, facilitating standardized and interoperable specimen data management.

Production Flow
The production flow of this implementation is illustrated in the figure below:

![image](https://github.com/oggioniale/specimen_catalogue/assets/1393893/3363c96e-ac55-46bd-9c82-cf15348e4b0c)


# The workflow of this application consists of the following steps:

1. Fill the Spreadsheet: complete the `specimen_template.xlsx` spreadsheet with the relevant specimen information;

2. Generate XML and TTL: use the `specimen_catalogue()` function to generate IGSN CSIRO XML and TTL (Turtle) files based on the SOSA Ontology for each record present in the spreadsheet.

3. The workflow includes an integrated RDF validation stage that automatically checks the quality and consistency of each generated Turtle (TTL) file.
The validation is executed by the internal function specimen_validate_ttl(), which implements a set of SHACL-like rules designed to verify the structural completeness and semantic correctness of the produced RDF graphs.

# 📊 Interactive Validation & Inspection App (Shiny)

The package also includes an interactive Shiny-based dashboard that allows users to visually inspect the generated specimen metadata and their validation status.

The application can be launched directly from R or RStudio using:

```
library(Specimen)
specimen_runApp(launch.browser = rstudioapi::viewer)
```

The dashboard offers an intuitive interface to explore the output of the specimen catalogue:
<img width="1431" height="727" alt="image" src="https://github.com/user-attachments/assets/e0991ef9-9794-4f7a-bb6b-5bad68f52edd" />
