# plumber.R
library(plumber)
library(SpecimenCat)
library(readxl)

# ============================================================================
# API METADATA (Swagger)
# ============================================================================

#* @apiTitle eLTER-IT Specimen Catalogue API
#* @apiDescription
#* The Specimen Catalogue API allows users to upload a structured Excel file
#* containing metadata about physical samples and automatically generates:
#* 
#* - XML resources following IGSN-style structures  
#* - RDF/Turtle (TTL) representations compliant with SOSA/SSN, PROV, DCAT  
#* - A validation report for each generated sample  
#*
#* This API is part of the eLTER-Italy data workflow and powers
#* specimen catalogue generation used by research sites.
#*
#* **Usage:**  
#* Submit an Excel file (`excel:file`) together with optional creator metadata.  
#*
#* **Supported sheets inside the Excel file:**  
#* - `SpecimenInfo`  
#* - `CuratorsInfo`  
#* - `RelationInfo`  
#* - `SamplersInfo`
#*
#* @apiContact list(
#*   name = "eLTER-IT Support",
#*   url = "https://www.lteritalia.it",
#*   email = "alessandro.oggioni@@cnr.it"
#* )
#*
#* @apiVersion 1.0.1
#* @apiLicense list(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")


# ============================================================================
# HEALTH CHECK
# ============================================================================

#* Health check endpoint
#* @get /health
#* @serializer json
function() {
  list(
    status = "ok",
    service = "Specimen Catalogue API",
    time = as.character(Sys.time())
  )
}

# ============================================================================
# RUN WORKFLOW
# ============================================================================

#* Run the full Specimen Catalogue workflow
#*
#* Upload an Excel file and generate XML/TTL specimen resources.
#*
#* @param excel:file Excel file (.xlsx) containing specimen metadata.
#* @param creator_name:string Creator first name (optional).
#* @param creator_surname:string Creator last name (optional).
#* @param creator_orcid:string ORCID identifier (optional).
#*
#* @post /run
#* @serializer json list(na="string")
#*
#* @response 200 Successful processing. Returns output folder & metadata.
#* @response 400 Bad request. Input file missing or invalid.
#* @response 500 Internal error while generating the catalogue.
#*
#* @example curl
#* curl -X POST \
#*   "http://127.0.0.1:8000/run?creator_name=John&creator_surname=Doe&creator_orcid=0000-0000-0000-0000" \
#*   -H "Content-Type: multipart/form-data" \
#*   -F "excel=@specimen_template.xlsx;type=application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
#*
function(excel,
         creator_name = NULL,
         creator_surname = NULL,
         creator_orcid = NULL) {
  
  # ---- 1) Validate input ----
  if (is.null(excel)) {
    res <- list(status = "error", message = "Missing argument 'excel:file'")
    attr(res, "plumber.status") <- 400
    return(res)
  }
  
  # ---- 2) Save uploaded .xlsx ----
  tmp <- tempfile("specimen", fileext = paste0("_", basename(names(excel))))
  on.exit(unlink(tmp), add = TRUE)
  writeBin(excel[[1]], tmp)
  
  # ---- 3) Run workflow ----
  result <- tryCatch(
    specimen_catalogue(
      excel_path      = tmp,
      creator_name    = creator_name,
      creator_surname = creator_surname,
      creator_orcid   = creator_orcid
    ),
    error = function(e) e
  )
  
  if (inherits(result, "error")) {
    res <- list(status = "error", message = result$message)
    attr(res, "plumber.status") <- 500
    return(res)
  }
  
  # ---- 4) Normalize path (absolute) ----
  full_path <- normalizePath(result$output_dir)
  
  # ---- 5) Return metadata ----
  list(
    status = "success",
    output_dir = result$output_dir,
    full_path  = full_path,
    uuids      = result$specimen_uuids,
    validation = result$validation_df
  )
}