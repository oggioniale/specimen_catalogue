#' Create a catalogue of samples (XML + TTL + validation)
#'
#' @description `r lifecycle::badge("experimental")`
#' This function reads a specimen spreadsheet template
#' (e.g. `'specimen_template.xlsx'`) and generates, for each row
#' in the `SpecimenInfo` sheet:
#'
#' * an XML representation of the sample, following the IGSN / ODM2 model;
#' * a Turtle (TTL) representation using SOSA/SSN, PROV-O, DCAT and related
#'   vocabularies;
#' * a SHACL-like validation report on each TTL file
#'   (via [specimen_validate_ttl()]).
#'
#' The XML files are created by [specimen_XML()], while the TTL files and
#' the validation are handled by [specimen_ttl()].
#'
#' @param excel_path A character string with the path to the spreadsheet
#'   template (typically `'specimen_template.xlsx'`).
#' @param creator_name Character. First name of the sample(s) creator to be
#'   used in the TTL provenance metadata (DCMI / PROV).
#' @param creator_surname Character. Last name of the sample(s) creator.
#' @param creator_orcid Character. ORCID of the sample(s) creator
#'   (e.g. `"0000-0002-1825-0097"`). It will be normalised to a full ORCID URL
#'   (`https://orcid.org/...`) if needed.
#'
#' @return A list with:
#' * `output_dir` – the root directory created for this run
#' * `specimen_uuids` – vector of UUIDs assigned to specimens
#' * `validation` – list of tibbles, one per TTL file, as returned by
#'   [specimen_ttl()] / [specimen_validate_ttl()].
#'
#' The function also writes XML and TTL files to disk under `output_dir`.
#'
#' @author Alessandro Oggioni, PhD (2023–2025) \email{oggioni.a@@cnr.it}
#' @importFrom readxl read_excel
#' @importFrom uuid UUIDgenerate
#' @export
#'
#' @examples
#' \dontrun{
#' specimen_catalogue(
#'   excel_path = "./specimen_template.xlsx",
#'   creator_name = "Alessandro",
#'   creator_surname = "Oggioni",
#'   creator_orcid = "0000-0002-7997-219X"
#' )
#' }
#' 
### function specimen_catalogue
specimen_catalogue <- function(
    excel_path = NULL,
    creator_name = NULL,
    creator_surname = NULL,
    creator_orcid = NULL
  ) {
  # read excel file sheets
  excel_file <- readxl::read_excel(excel_path, sheet = "SpecimenInfo")
  excel_curators <- readxl::read_excel(excel_path, sheet = "CuratorsInfo")
  excel_relation <- readxl::read_excel(excel_path, sheet = "RelationInfo")
  excel_sampler <- readxl::read_excel(excel_path, sheet = "SamplersInfo")
  # lines concerning examples, units of measurement and data types are removed
  # excel_file <- excel_file[-c(1:3), ]
  # excel_curators <- excel_curators[-c(1:5), ]
  # excel_relation <- excel_relation[-c(1:3), ]
  # excel_sampler <- excel_sampler[-c(1:3), ]
  # if excel file contains only example sensor
  excel_file <- excel_file[-1, ]
  excel_curators <- excel_curators[-1, ]
  excel_relation <- excel_relation[-1, ]
  excel_sampler <- excel_sampler[-1, ]
  # assign sample IDs, so that references can be made
  # TODO change when the DOI can generate trough DataCite
  n_specimen <- nrow(excel_file)
  specimen_uuids <- sapply(1:n_specimen, uuid::UUIDgenerate)
  # folders creation
  # root folder
  root_dir <- paste0(
    format(Sys.time(), format = "%Y%m%d_%H%M%S"),
    "_specimens"
  )
  if (!dir.exists(root_dir)) {
    dir.create(root_dir)
  }
  specimen_XML(excel_file = excel_file, excel_curators = excel_curators,
               excel_relation = excel_relation, excel_sampler = excel_sampler,
               specimen_uuids = specimen_uuids, root_dir = root_dir)
  # --- Generate TTL files + validation
  validation_report <- specimen_ttl(
    excel_file     = excel_file,
    excel_curators = excel_curators,
    excel_relation = excel_relation,
    excel_sampler  = excel_sampler,
    specimen_uuids = specimen_uuids,
    root_dir       = root_dir,
    creator_name   = creator_name,
    creator_surname = creator_surname,
    creator_orcid  = creator_orcid
  )
  # === POST-PROCESSING REPORT =============================================
  
  # Flatten results
  val_df <- dplyr::bind_rows(validation_report)
  
  # Total triples
  tot_triples <- sum(val_df$total_triples, na.rm = TRUE)
  
  # Count TRUE/FALSE per predicate
  logical_cols <- names(val_df)[vapply(val_df, is.logical, logical(1))]
  predicate_summary <- colSums(val_df[logical_cols, drop = FALSE])
  
  # ---- PRINT SUMMARY ----
  message("\n", crayon::green$bold("Specimen Catalogue created successfully!\n"))
  message(crayon::cyan$bold("Output folder: "), root_dir)
  message(crayon::cyan$bold("Total samples: "), length(specimen_uuids))
  message(crayon::cyan$bold("Total triples across all TTL files: "), tot_triples, "\n")
  
  message(crayon::yellow$bold("Validation summary per predicate (TRUE count):"))
  for (p in names(predicate_summary)) {
    message(" - ", p, ": ", predicate_summary[[p]])
  }
  
  # ---- FILE-BY-FILE REPORT WITH COLORS ----
  message(crayon::yellow$bold("\nTTL(s) can be improved:"))
  
  for (i in seq_len(nrow(val_df))) {
    
    file    <- val_df$ttl_file[[i]]
    missing <- val_df$missing_predicates[[i]]
    triples <- val_df$total_triples[[i]]
    
    # --- CASE 1: TRUE ERROR ---
    if (is.na(triples) || missing == "Validation error") {
      
      message(
        crayon::red$bold(
          paste0(" ❌ ", file, " — ERROR: cannot validate (", missing, ")")
        )
      )
      
      # --- CASE 2: OK ---
    } else if (missing == "None") {
      
      message(
        crayon::green$bold(
          paste0(" ✅ ", file, " — OK (complete)")
        )
      )
      
      # --- CASE 3: WARNING ---
    } else {
      
      message(
        crayon::yellow$bold(
          paste0(" ⚠️ ", file, " — Suggested improvements: ", missing)
        )
      )
    }
  }
  
  # ---- RETURN OBJECT ----
  invisible(list(
    output_dir      = root_dir,
    specimen_uuids  = specimen_uuids,
    validation_df   = val_df,
    validation_list = validation_report
  ))
}

#' Parse Excel / text date-time to ISO 8601 with Z
#'
#' @param x A single value: Excel numeric date, POSIXt, Date, or character.
#' @return A character string in ISO 8601 UTC: "YYYY-MM-DDTHH:MM:SSZ",
#'   or NA_character_ if it cannot be parsed.
#' @keywords internal
#' @importFrom lubridate ymd dmy
parse_datetime_iso_z <- function(x) {
  if (length(x) == 0 || is.na(x) || (is.character(x) && nchar(trimws(x)) == 0)) {
    return(NA_character_)
  }
  
  dt <- NA
  
  # 1) Excel numeric date / datetime (serial)
  if (is.numeric(x)) {
    # Excel origin: 1899-12-30; convert days -> seconds
    dt <- as.POSIXct((x - 25569) * 86400, origin = "1970-01-01", tz = "UTC")
  } else {
    xs <- as.character(x)
    xs <- trimws(xs)
    
    # if already ISO 8601 con Z, proviamo solo a parse
    if (grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", xs)) {
      dt <- as.POSIXct(xs, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    } else {
      # Normalizziamo un po': / -> -
      xs_norm <- gsub("/", "-", xs)
      
      # separiamo data e ora se c'è "T" o uno spazio
      if (grepl("T", xs_norm)) {
        parts <- strsplit(xs_norm, "T", fixed = TRUE)[[1]]
      } else if (grepl(" ", xs_norm)) {
        parts <- strsplit(xs_norm, " +")[[1]]
      } else {
        parts <- c(xs_norm)
      }
      
      date_s <- parts[1]
      time_s <- ifelse(length(parts) > 1, parts[2], "00:00:00")
      
      # normalizza time_s a HH:MM:SS
      if (!grepl(":", time_s)) {
        time_s <- "00:00:00"
      } else if (grepl("^\\d{1,2}:\\d{2}$", time_s)) {
        time_s <- paste0(time_s, ":00")
      }
      
      # prova YMD prima
      dt_date <- suppressWarnings(lubridate::ymd(date_s, tz = "UTC"))
      # se fallisce, prova DMY (per cose tipo 12-02-2008)
      if (is.na(dt_date)) {
        dt_date <- suppressWarnings(lubridate::dmy(date_s, tz = "UTC"))
      }
      
      if (!is.na(dt_date)) {
        dt <- as.POSIXct(
          paste(format(dt_date, "%Y-%m-%d"), time_s),
          tz = "UTC"
        )
      }
    }
  }
  
  if (is.na(dt)) {
    return(NA_character_)
  }
  
  # output canonico ISO 8601 in UTC
  strftime(dt, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Create the XML of sample
#' @description
#' This function generates XML files for each sample.
#'
#' @param excel_file A `tibble` that contains the columns present in the
#' 'SpecimenInfo' sheet of the spreadsheet template.
#' @param excel_curators A `tibble` that contains the columns present in the
#' 'CuratorsInfo' sheet of the spreadsheet template.
#' @param excel_relation A `tibble` that contains the columns present in the
#' 'RelationInfo' sheet of the spreadsheet template.
#' @param excel_sampler A `tibble` that contains the columns present in the
#' 'SamplersInfo' sheet of the spreadsheet template.
#' @param specimen_uuids A `character` that contains a number of strings
#' UUIDs equal to the number of samples present in the SpecimenInfo' sheet
#' of the spreadsheet template.
#' @param root_dir A `string` with the path to the folder where the ttl and
#' XML files will be written.
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom dplyr filter
#' @importFrom xml2 read_xml xml_add_child xml_add_sibling write_xml
#' @importFrom stringr str_to_lower
#' @importFrom ReLTER get_location_info
#' @importFrom sf st_as_text
#' @keywords internal
#'
### function specimen_XML
specimen_XML <- function(excel_file = NULL, excel_curators = NULL,
                         excel_relation = NULL, excel_sampler = NULL,
                         specimen_uuids = NULL, root_dir = NULL) {
  # XML files creation
  for (i in 1:length(specimen_uuids)) {
    sp_id <- excel_file$specimen_id[[i]]
    excel_curator <- excel_curators |>
      dplyr::filter(specimen_id == sp_id)
    excel_rel <- excel_relation |>
      dplyr::filter(specimen_id == sp_id)
    # name of XML specimen file
    file_name <- paste0("specimen_", specimen_uuids[[i]])
    # read XML
    specimen_XML_base <- xml2::read_xml("base_resource.xml")
    # XML
    # cs:resource
    a <- specimen_XML_base |>
      xml2::xml_add_child(
        "cs:resource",
        "registeredObjectType" = "http://pid.geoscience.gov.au/def/voc/igsn-codelists/PhysicalSample"
      ) |>
      xml2::xml_add_child(
        "cs:resourceIdentifier",
        paste0("specimen_", specimen_uuids[[i]]) # the XML schema needs that the format of this element seems like <string>-<string> e.g. specimen_xxxxxx or doi-xxxxxx
      ) |>
      xml2::xml_add_sibling(
        "cs:landingPage",
        paste0(
          "http://rdfdata.lteritalia.it/samples/",
          file_name,
          ".xml"
        )
      ) |>
      xml2::xml_add_sibling(
        "cs:isPublic",
        stringr::str_to_lower(
          excel_file$is_public[[i]]
        )
      ) |>
      xml2::xml_add_sibling(
        "cs:resourceTitle",
        excel_file$resource_title[[i]]
      ) |>
      xml2::xml_add_sibling(
        "cs:resourceTypes"
      )
    xml2::xml_add_child(
      a,
      "cs:resourceType",
      paste0(
        "http://vocabulary.odm2.org/specimentype/",
        excel_file$resource_type[[i]]
      )
    )
    b <- xml2::xml_add_sibling(
      a,
      "cs:materialTypes"
    )
    xml2::xml_add_child(
      b,
      "cs:materialType",
      paste0(
        "http://vocabulary.odm2.org/medium/",
        excel_file$material_type[[i]]
      )
    )
    if (!is.na(excel_file$purpose[[i]])) {
      c <- xml2::xml_add_sibling(
        b,
        "cs:purpose",
        excel_file$purpose[[i]]
      )
    } else {
      c <- b
    }
    d <- xml2::xml_add_sibling(
      c,
      "cs:sampledFeatures"
    )
    xml2::xml_add_child(
      d,
      "cs:sampledFeature",
      "sampledFeatureURI" = excel_file$site_id[[i]]
    )
    e <- xml2::xml_add_sibling(
      d,
      "cs:location"
    )
    xml2::xml_add_child(
      e,
      "cs:locality",
      "localityURI" = excel_file$location_id[[i]]
    )
    location_info <- ReLTER::get_location_info(
      locationid = excel_file$location_id[[i]]
    )
    geo_type <- sf::st_as_text(
      location_info$boundaries
    )
    xml2::xml_add_child(
      e,
      "cs:geometry",
      "srid" = "https://epsg.io/8311",
      "verticalDatum" = "https://epsg.io/4326",
      geo_type
    )
    f <- xml2::xml_add_sibling(
      e,
      "cs:date"
    )
    # robust ISO 8601 datetime in UTC
    created_iso <- parse_datetime_iso_z(excel_file$date_time[[i]])
    xml2::xml_add_child(
      f,
      "cs:timeInstant",
      created_iso
    )
    if (!is.na(excel_file$method_doi[[i]]) & !is.na(excel_file$sampler[[i]])) {
      g <- xml2::xml_add_sibling(
        f,
        "cs:method",
        "methodURI" = excel_file$method_doi[[i]],
        excel_file$sampler[[i]]
      )
    } else if (!is.na(excel_file$method_doi[[i]]) & is.na(excel_file$sampler[[i]])) {
      g <- xml2::xml_add_sibling(
        f,
        "cs:method",
        excel_file$sampler[[i]]
      )
    } else if (is.na(excel_file$method_doi[[i]]) & is.na(excel_file$sampler[[i]])) {
      g <- f
    }
    if (!is.na(excel_file$campaign[[i]])) {
      g <- g |>
        xml2::xml_add_sibling(
          "cs:campaign",
          excel_file$campaign[[i]]
        )
    } else {
      g <- g
    }
    # curators
    h <- xml2::xml_add_sibling(
      g,
      "cs:curationDetails"
    )
    n_curators <- nrow(excel_curator)
    for (l in 1:n_curators) {
      n <- xml2::xml_add_child(
        h,
        "cs:curation"
      )
      xml2::xml_add_child(
        n,
        "cs:curator",
        excel_curator$contact_person[l]
      )
      xml2::xml_add_child(
        n,
        "cs:curatingInstitution",
        "institutionURI" = excel_curator$institution_ror[l]
      )
    }
    # contributors
    o <- xml2::xml_add_sibling(
      h,
      "cs:contributors"
    )
    n_contributors <- nrow(excel_curator)
    for (m in 1:n_contributors) {
      p <- xml2::xml_add_child(
        o,
        "cs:contributor",
        "contributorType" = paste0(
          "http://inspire.ec.europa.eu/metadata-codelist/ResponsiblePartyRole/",
          gsub(
            " ",
            "",
            excel_curator$contact_person_type[m]
          )
        )
      )
      xml2::xml_add_child(
        p,
        "cs:contributorName",
        excel_curator$contact_person[m]
      )
      xml2::xml_add_child(
        p,
        "cs:contributorIdentifier",
        "contributorIdentifierType" = "http://pid.geoscience.gov.au/def/voc/igsn-codelists/ORCID",
        excel_curator$contact_person_orcid[m]
      )
    }
    n_relation <- nrow(excel_rel)
    if (nrow(excel_rel) != 0) {
      q <- xml2::xml_add_sibling(
        o,
        "cs:relatedResources"
      )
      for (r in 1:n_relation) {
        xml2::xml_add_child(
          q,
          "cs:relatedResource",
          "relatedResourceIdentifierType" = paste0(
            "http://pid.geoscience.gov.au/def/voc/igsn-codelists/",
            excel_rel$related_resource_identifier_type[r]
          ),
          "relationType" = paste0(
            "http://pid.geoscience.gov.au/def/voc/igsn-codelists/",
            excel_rel$relation_type[r]
          ),
          excel_rel$related_resources[r]
        )
      }
    } else {
      q <- o
    }
    if (!is.na(excel_file$description[[i]])) {
      s <- xml2::xml_add_sibling(
        q,
        "cs:comments",
        excel_file$description[[i]]
      )
    } else {
      s <- q
    }
    xml2::xml_add_sibling(
      s,
      "cs:logDate",
      "eventType" = "registered",
      as.character(Sys.Date())
    )
    # write file
    xml2::write_xml(
      specimen_XML_base,
      paste0(
        root_dir,
        "/",
        file_name,
        ".xml"
      )
    )
  }
}


#' Create the ttl of sample
#' @description
#' This function generates Turtle (TTL) files for each sample listed
#' in the input `excel_file`, using SOSA/SSN, PROV-O, DCAT and related
#' vocabularies. For each generated TTL, a SHACL-like validation is
#' performed via [specimen_validate_ttl()], and a per-file validation
#' tibble is returned.
#' @param excel_file A `tibble` that contains the columns present in the
#' 'SpecimenInfo' sheet of the spreadsheet template.
#' @param excel_curators A `tibble` that contains the columns present in the
#' 'CuratorsInfo' sheet of the spreadsheet template.
#' @param excel_relation A `tibble` that contains the columns present in the
#' 'RelationInfo' sheet of the spreadsheet template.
#' @param excel_sampler A `tibble` that contains the columns present in the
#' 'SamplersInfo' sheet of the spreadsheet template.
#' @param specimen_uuids A `character` that contains a number of strings
#' UUIDs equal to the number of samples present in the SpecimenInfo' sheet
#' of the spreadsheet template.
#' @param root_dir A `string` with the path to the folder where the ttl and
#' XML files will be written.
#' @return A list of tibbles, each one being the validation result for a
#'   single TTL file, as returned by [specimen_validate_ttl()].
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom httr2 request req_url_query req_method req_auth_basic
#' @importFrom httr2 req_headers req_retry req_perform req_body_file
#' @importFrom httr2 resp_check_status resp_body_json
#' @importFrom dplyr filter mutate select
#' @importFrom purrr pluck
#' @importFrom tibble as_tibble
#' @importFrom uuid UUIDgenerate
#' @importFrom ReLTER get_location_info
#' @importFrom sf st_as_text st_geometry_type
#' @keywords internal
#'
### function specimen_ttl
specimen_ttl <- function(excel_file = NULL, excel_curators = NULL,
                         excel_relation = NULL, excel_sampler = NULL,
                         specimen_uuids = NULL, root_dir = NULL,
                         creator_name = creator_name,
                         creator_surname = creator_surname,
                         creator_orcid = creator_orcid) {
  # --- Default metadata for the creator and contact point ---
  if (is.null(creator_name))     creator_name    <- "Alessandro"
  if (is.null(creator_surname))  creator_surname <- "Oggioni"
  if (is.null(creator_orcid))    creator_orcid   <- "https://orcid.org/0000-0002-7997-219X"
  # ORCID normalization
  if (!startsWith(creator_orcid, "https://orcid.org/"))
    creator_orcid <- paste0("https://orcid.org/", creator_orcid)
  # For saving the outputs of the validation process
  validation_report <- vector("list", length(specimen_uuids))
  
  if (!requireNamespace("crayon", quietly = TRUE)) {
    stop("Package 'crayon' is required. Please install it with install.packages('crayon').")
  }
  if (!requireNamespace("progress", quietly = TRUE)) {
    stop("Package 'progress' is required. Please install it with install.packages('progress').")
  }
  
  message(crayon::cyan$bold("\nStarting specimen TTL generation...\n"))
  
  # --- TTL FILE CREATION LOOP ---
  for (i in 1:length(specimen_uuids)) {
    sp_id <- excel_file$specimen_id[[i]]
    excel_curator <- excel_curators |> dplyr::filter(specimen_id == sp_id)
    excel_rel     <- excel_relation  |> dplyr::filter(specimen_id == sp_id)
    
    # Filename
    file_name <- paste0("specimen_", specimen_uuids[[i]])
    ttl_path  <- file.path(root_dir, paste0(file_name, ".ttl"))
    
    # Open connection
    specimen_file_ttl <- file(ttl_path)
    # names space
    specimen <- "@prefix sosa: <http://www.w3.org/ns/sosa/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix geo: <http://www.opengis.net/ont/geosparql#> .
@prefix sf: <http://www.opengis.net/ont/sf#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix dcat: <http://www.w3.org/ns/dcat#> .
@prefix dct: <http://purl.org/dc/terms/> .
@prefix prov: <http://www.w3.org/ns/prov#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix sosa-rel: <http://www.w3.org/ns/sosa/sampling/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> ."
    uuid <- specimen_uuids[[i]]
    site_id <- excel_file$site_id[[i]]
    location_id <- excel_file$location_id[[i]]
    activity_id <- excel_file$campaign[[i]]
    #  date and time in ISO 8601 with Z
    created_iso <- parse_datetime_iso_z(excel_file$date_time[[i]])
    # samplers
    sampler_name <- excel_file$sampler[[i]]
    if (check_sampler_exist(sampler_name = sampler_name)) {
      sampler_query <- paste0("PREFIX sosa: <http://www.w3.org/ns/sosa/>
     PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     SELECT ?c ?label 
     WHERE {
       ?c rdf:type sosa:Sampler .
       ?c rdfs:label ?label .
      FILTER( REGEX( ?label, '",
                              sampler_name,
                              "', 'i' ))
    }
    ORDER BY ASC(?l)")
      sampler_qr <- httr2::request("http://fuseki1.get-it.it/specimen") |>
        httr2::req_url_query(query = sampler_query) |>
        httr2::req_method("POST") |>
        httr2::req_headers(Accept = "application/sparql-results+json") |>
        httr2::req_retry(max_tries = 3, max_seconds = 120) |>
        httr2::req_perform()
      httr2::resp_check_status(sampler_qr)
      sampler_list <- httr2::resp_body_json(sampler_qr, simplifyVector = TRUE) |>
        purrr::pluck("results") |>
        tibble::as_tibble() |>
        dplyr::mutate(
          sampler_uri = bindings$c$value,
          specimen_label = bindings$label$value,
          .keep = "used"
        ) |>
        dplyr::select(
          sampler_uri, specimen_label
        )
      sampler_uri <- paste0("<", sampler_list$sampler_uri, ">")
      madeBy <- paste0("  sosa:MadeBySampler ", sampler_uri, " ;")
      # sosa:Sampler
      sosa_sampler <- ""
      if (!is.na(activity_id)) {
        act_sampler_ass <- paste0(
          "<", activity_id, ">\n",
          "  prov:wasAssociatedWith ", sampler_uri, " .\n",
          sampler_uri, "\n",
          "  prov:wasAssociatedWith <", activity_id, "> .\n"
        )
      } else {
        act_sampler_ass <- ""
      }
    } else {
      if (!is.na(sampler_name)) {
        sampler_uuid <- sapply(
          length(
            excel_file$sampler[[i]]
          ), 
          uuid::UUIDgenerate
        )
        sampler_uri <- paste0("<http://rdfdata.lteritalia.it/sampler/", sampler_uuid, ">")
        # sosa:Sampler
        sosa_sampler <- c(
          paste0(sampler_uri, " rdf:type sosa:Sampler , prov:Agent , prov:Entity ;"),
          paste0("  rdfs:label '", sampler_name, "'@en .")
        )
        if (!is.na(activity_id)) {
          act_sampler_ass <- paste0(
            "  <", activity_id, ">\n",
            "    prov:wasAssociatedWith ", sampler_uri, " ;\n",
            sampler_uri, "\n",
            "    prov:wasAssociatedWith <", activity_id, "> ;\n"
          )
          sosa_sampler <- c(
            sosa_sampler,
            "",
            act_sampler_ass
          )
        } 
        madeBy <- paste0("  sosa:MadeBySampler ", sampler_uri, " ;")
      } else {
        sampler_uri <- ""
        sosa_sampler <- ""
        madeBy <- ""
      }
    }
    location_info <- ReLTER::get_location_info(
      locationid = location_id
    )
    geo_wkt <- sf::st_as_text(
      location_info$boundaries
    )
    geo_type <- sf::st_geometry_type(
      location_info$boundaries,
      by_geometry = TRUE) |>
        as.character()
    # curators
    curators <- ""
    institution_ror <- excel_curator$institution_ror |>
      unique()
    for (j in 1:length(institution_ror)) {
      curators <- c(
        curators,
        "  dcat:contactPoint [",
        "    rdf:type       prov:Agent, prov:Organization, foaf:Organization ;",
        paste0("    rdfs:seeAlso   <", institution_ror[[j]], "> ;"),
        "  ] ;"
      )
    }
    # contributors
    contributors <- ""
    for (y in 1:nrow(excel_curator)) {
      contributors <- c(
        contributors,
        "  dcat:contactPoint [",
        "    rdf:type       prov:Person, foaf:Person ;",
        paste0("    rdfs:seeAlso <", excel_curator$contact_person_orcid[[y]], "> ;"),
        paste0("    foaf:fullName '", excel_curator$contact_person[[y]], "' ;"),
        "  ] ;",
        "  prov:qualifiedAttribution [",
        "    a prov:Attribution ;",
        paste0("    prov:agent <", excel_curator$contact_person_orcid[[y]], "> ;"),
        paste0(
          "    prov:hadRole <http://inspire.ec.europa.eu/metadata-codelist/ResponsiblePartyRole/",
          gsub(
            " ",
            "",
            excel_curator$contact_person_type[[y]]
          ),
          "> ;"
        ),
        "  ] ;"
      )
    }
    # description + purpose
    if (!is.na(excel_file$description[[i]]) & !is.na(excel_file$purpose[[i]])) {
      description <- paste0("  rdfs:comment 'Description: ", excel_file$description[[i]], " - Purpose: ", excel_file$purpose[[i]], "'@en ;")
    } else if (is.na(excel_file$description[[i]]) & !is.na(excel_file$purpose[[i]])) {
      description <- paste0("  rdfs:comment 'Purpose: ", excel_file$purpose[[i]], "'@en ;")
    } else if (!is.na(excel_file$description[[i]]) & is.na(excel_file$purpose[[i]])) {
      description <- paste0("  rdfs:comment 'Description: ", excel_file$description[[i]], "'@en ;")
    } else {
      description <- ""
    }
    # related samples
    if (nrow(excel_rel) != 0) {
      for (w in 1:nrow(excel_rel)) {
        related <- c(
          "  sosa-rel:hasSampleRelationship [",
          "     rdf:type sosa-rel:SampleRelationship ;",
          paste0("     sosa-rel:natureOfRelationship",
          " <http://pid.geoscience.gov.au/def/voc/igsn-codelists/", excel_rel$relation_type[[w]], "> ;"),
          paste0("     sosa-rel:relatedSample <", excel_rel$related_resources[[w]], "> ;"),
          "  ] ;",
          paste0("  prov:wasDerivedFrom <", excel_rel$related_resources[[w]], "> ;"),
          "  prov:qualifiedDerivation [",
          "    a prov:Derivation ;",
          paste0("    prov:entity <", excel_rel$related_resources[[w]], "> ;"),
          "  ] ;"
        )
      }
    } else {
      related <- ""
    }
    # procedure
    if (!is.na(excel_file$method_doi[[i]])) {
      procedure <- paste0("  sosa:Procedure <", excel_file$method_doi[[i]], "> ;")
    } else {
      procedure <- ""
    }
    # meterial
    material <- paste0("  sosa:MaterialSample <http://vocabulary.odm2.org/medium/", excel_file$material_type[[i]], "> .")
    # sosa:Sample
    if (!is.na(activity_id)) {
      activityId <- paste0(
        "  sosa:madeSampling <", activity_id, "> ;\n",
        "  prov:wasGeneratedBy <", activity_id, "> ;"
      )
    } else {
      activityId <- ""
    }
    # Sample
    sosa_sample <- c(
      paste0("<http://rdfdata.lteritalia.it/samples/specimen_", uuid, "> rdf:type sosa:Sample , sosa:sampleMaterial , prov:Entity ;"),
      paste0("  dcat:landingPage <http://rdfdata.lteritalia.it/samples/specimen_", uuid, ".xml> ;"),
      description,
      paste0("  rdfs:label '", excel_file$resource_title[[i]], "'@en ;"),
      paste0(
        '  dct:created "', created_iso, '"^^xsd:dateTime ;\n',
        '  prov:generatedAtTime "', created_iso, '"^^xsd:dateTime ;'
      ),
      paste0('  dct:creator <', creator_orcid, '> ;'),
      paste0("  sosa:isSampleOf <", site_id, "> ;"),
      activityId,
      madeBy,
      "  geo:hasGeometry [ ",
      paste0("    rdf:type sf:", stringr::str_to_title(geo_type), " ;"),
      paste0("    geo:asWKT '<urn:ogc:def:crs:EPSG::4283> ", geo_wkt, "'^^geo:wktLiteral ;"),
      "  ] ;",
      curators,
      contributors,
      related,
      procedure,
      material
    )
    # sosa:hasSample
    if (!is.na(activity_id)) {
      sosa_hasSampling <- c(
        paste0("<", site_id, "> sosa:hasSample <http://rdfdata.lteritalia.it/samples/specimen_", uuid, "> .")
      )
      # reuse same ISO datetime for resultTime
      result_iso <- created_iso
      sosa_sampling <- c(
        paste0("<", activity_id, "> rdf:type sosa:Sampling ;"),
        paste0("  sosa:hasResult <http://rdfdata.lteritalia.it/samples/specimen_", uuid, "> ;"),
        madeBy,
        paste0("  sosa:resultTime '", result_iso, "'^^xsd:dateTime ;"),
        paste0("  sosa:hasFeatureOfInterest <", site_id, "> .")
      )
    } else {
      sosa_sampling <- ""
    }
    
    specimen <- c(
      specimen,
      "",
      sosa_sample,
      "",
      sosa_hasSampling,
      "",
      sosa_sampling,
      "",
      sosa_sampler,
      "",
      act_sampler_ass,
      ""
    )
    # --- Write TTL file (SAFE AND SIMPLE) ---
    suppressWarnings(writeLines(specimen, con = ttl_path, useBytes = TRUE))
    suppressWarnings(close(specimen_file_ttl))
    
    message(crayon::green(paste0("✔ TTL created: ", ttl_path)))
    
    # --- RDF Validation with SHACL-like checks ---
    val_res <- tryCatch(
      specimen_validate_ttl(ttl_path),
      error = function(e) {
        message(crayon::red$bold(
          paste0("✖ RDF validation failed for ", file_name, ": ", e$message)
        ))
        tibble::tibble(
          ttl_file = paste0(file_name, ".ttl"),
          total_triples = NA_integer_,
          predicates_found = NA_integer_,
          missing_predicates = "Validation error",
          has_attribution = FALSE,
          has_contactPoint = FALSE,
          has_geometry = FALSE,
          has_sampler = FALSE,
          has_created = FALSE,
          has_creator = FALSE,
          has_valid_dates = FALSE,
          has_valid_geometry = FALSE,
          has_valid_sampler_shape = FALSE,
          has_valid_sampling_shape = FALSE,
          has_valid_sampleRelationship_shape = FALSE,
          has_valid_derivation_shape = FALSE,
          is_valid = FALSE
        )
      }
    )
    # Store result
    validation_report[[i]] <- val_res
  }
  # Return full validation report (one tibble per specimen)
  invisible(validation_report)
}

#' Check of the sampler's existence in the catalog.
#' @description `r lifecycle::badge("experimental")`
#' Checks if a sampler already exists in the catalog or not.
#' @param sampler_name description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom httr2 request req_url_query req_method
#' @importFrom httr2 req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @examples
#' \dontrun{
#' ## Not run:
#' check_sampler_exist(sampler_name = "Niskin bottle")
#' 
#' }
#' ## End (Not run)
#' @keywords internal
#'
### function check_sampler_exist
check_sampler_exist <- function(sampler_name = NULL) {
  library(magrittr)
  sampler_query <- paste0("PREFIX sosa: <http://www.w3.org/ns/sosa/>
     PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     SELECT ?c ?label 
     WHERE {
       ?c rdf:type sosa:Sampler .
       ?c rdfs:label ?label .
      FILTER( REGEX( ?label, '",
                           sampler_name,
                           "', 'i' ))
    }
    ORDER BY ASC(?l)")
  sampler_qr <- httr2::request("http://fuseki1.get-it.it/specimen") |>
    httr2::req_url_query(query = sampler_query) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Accept = "application/sparql-results+json") |>
    httr2::req_retry(max_tries = 3, max_seconds = 120) |>
    httr2::req_perform()
  httr2::resp_check_status(sampler_qr)
  sampler_list <- httr2::resp_body_json(sampler_qr, simplifyVector = TRUE) |>
    purrr::pluck("results") |>
    tibble::as_tibble()
  if (nrow(sampler_list) == 0) {
    sampler_exist <- FALSE
  } else {
    sampler_exist <- TRUE
  }
  sampler_exist
}

#' Launch the eLTER-IT Catalogue (Samples view)
#' @description
#' This function launches the unified eLTER-IT Shiny catalogue
#' and automatically opens the **Samples** tab.
#' It provides a user-friendly entry point to the Samples
#' Catalogue within the integrated multi-view application.
#' @details
#' The eLTER-IT catalogue Shiny application includes:
#' \itemize{
#'   \item the **Samples Catalogue**, and
#'   \item the **Sensor Type Catalogue**.
#' }
#' Calling \code{specimen_runApp()} opens the application with
#' the \strong{"Samples"} tab preselected.
#' Internally, this function calls:
#' \code{Specimen::elter_catalogues_app(default_tab = "samples")}.
#' @author
#' Alessandro Oggioni, PhD (2023–2025)  
#' \email{oggioni.a@@cnr.it}
#' @return
#' No return value.  
#' The function launches the Shiny application.
#' @examples
#' if (interactive()) {
#'   specimen_runApp()
#' }
#' @export
### specimen_runApp
specimen_runApp <- function(...) {
  elter_catalogues_app(default_tab = "samples")
}

#' Validate a Specimen Turtle (TTL) File
#' @description
#' Performs a robust validation of a Turtle (TTL) file representing
#' specimen metadata.  
#' Validation includes:
#' - Turtle syntax check  
#' - Presence of required predicates  
#' - Existence of at least one `prov:qualifiedAttribution` block  
#' - At least one `dcat:contactPoint`  
#' - At least one geometry block with `geo:asWKT`  
#' - At least one sampler (`sosa:MadeBySampler`)  
#'
#' The function returns a structured tibble summarizing
#' the validation results without stopping the main workflow.
#' @param ttl_path Character path to the Turtle file to validate.
#' @author
#' Alessandro Oggioni, PhD (2023) \email{oggioni.a@@cnr.it}
#' @return A tibble containing:
#' \describe{
#'   \item{ttl_file}{Filename of the validated TTL}
#'   \item{total_triples}{Number of RDF triples parsed}
#'   \item{predicates_found}{Count of unique predicates found}
#'   \item{missing_predicates}{List of required predicates not found}
#'   \item{has_attribution}{TRUE if at least one prov:qualifiedAttribution is found}
#'   \item{has_contactPoint}{TRUE if at least one dcat:contactPoint is found}
#'   \item{has_geometry}{TRUE if a geo:asWKT geometry is present}
#'   \item{has_sampler}{TRUE if at least one sosa:MadeBySampler is present}
#' }
#' @details
#' This validator is designed for your specimen TTL model and follows the 
#' structure generated by `specimen_ttl()`.  
#' It does **not** enforce external ontology constraints,  
#' but checks internal consistency and key structural elements.
#' @examples
#' \dontrun{
#'   specimen_validate_ttl("specimen_12345.ttl")
#' }
#' @importFrom rdflib rdf_parse rdf_query
#' @importFrom tibble tibble
#' @keywords internal
#'
### function sample_validate_ttl
specimen_validate_ttl <- function(ttl_path) {
  if (!requireNamespace("rdflib", quietly = TRUE)) {
    stop("Package 'rdflib' is required. Please install it with install.packages('rdflib').")
  }
  
  # --- 0. File existence check ---
  if (!file.exists(ttl_path)) {
    stop("File not found: ", ttl_path)
  }
  
  # --- 1. Parse RDF safely (silence librdf warnings) ---
  g <- tryCatch(
    {
      suppressWarnings(
        rdflib::rdf_parse(ttl_path, format = "turtle")
      )
    },
    error = function(e) {
      # Hard syntax error: return minimal report
      return(tibble::tibble(
        ttl_file          = basename(ttl_path),
        total_triples     = 0L,
        predicates_found  = 0L,
        missing_predicates = "Syntax error",
        has_attribution   = FALSE,
        has_contactPoint  = FALSE,
        has_geometry      = FALSE,
        has_WKT_EPSG      = FALSE,
        has_sampler       = FALSE,
        has_sampling      = FALSE,
        has_sampleRelationship = FALSE,
        has_qualifiedDerivation = FALSE,
        has_created       = FALSE,
        has_creator       = FALSE,
        has_iso_datetime  = FALSE
      ))
    }
  )
  
  # --- 2. Extract all triples (avoid SELECT * to reduce librdf warnings) ---
  triples <- tryCatch(
    suppressWarnings(
      rdflib::rdf_query(g, "SELECT ?s ?p ?o WHERE { ?s ?p ?o . }")
    ),
    error = function(e) {
      # If query fails, return minimal report
      return(tibble::tibble(
        ttl_file          = basename(ttl_path),
        total_triples     = 0L,
        predicates_found  = 0L,
        missing_predicates = "Query error",
        has_attribution   = FALSE,
        has_contactPoint  = FALSE,
        has_geometry      = FALSE,
        has_WKT_EPSG      = FALSE,
        has_sampler       = FALSE,
        has_sampling      = FALSE,
        has_sampleRelationship = FALSE,
        has_qualifiedDerivation = FALSE,
        has_created       = FALSE,
        has_creator       = FALSE,
        has_iso_datetime  = FALSE
      ))
    }
  )
  
  if (is.null(triples) || nrow(triples) == 0) {
    return(tibble::tibble(
      ttl_file          = basename(ttl_path),
      total_triples     = 0L,
      predicates_found  = 0L,
      missing_predicates = "No triples",
      has_attribution   = FALSE,
      has_contactPoint  = FALSE,
      has_geometry      = FALSE,
      has_WKT_EPSG      = FALSE,
      has_sampler       = FALSE,
      has_sampling      = FALSE,
      has_sampleRelationship = FALSE,
      has_qualifiedDerivation = FALSE,
      has_created       = FALSE,
      has_creator       = FALSE,
      has_iso_datetime  = FALSE
    ))
  }
  
  preds <- unique(triples$p)
  
  # --- 3. Useful URIs (shortcuts) ---
  uri <- list(
    rdf_type      = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    sosa_Sample   = "http://www.w3.org/ns/sosa/Sample",
    sosa_Sampler  = "http://www.w3.org/ns/sosa/Sampler",
    sosa_MadeBySampler = "http://www.w3.org/ns/sosa/MadeBySampler",
    sosa_Sampling = "http://www.w3.org/ns/sosa/Sampling",
    
    geo_hasGeometry = "http://www.opengis.net/ont/geosparql#hasGeometry",
    geo_asWKT       = "http://www.opengis.net/ont/geosparql#asWKT",
    
    dcat_contactPoint = "http://www.w3.org/ns/dcat#contactPoint",
    
    dct_created   = "http://purl.org/dc/terms/created",
    dct_creator   = "http://purl.org/dc/terms/creator",
    
    prov_qualifiedAttribution = "http://www.w3.org/ns/prov#qualifiedAttribution",
    prov_Attribution          = "http://www.w3.org/ns/prov#Attribution",
    prov_agent                = "http://www.w3.org/ns/prov#agent",
    prov_hadRole              = "http://www.w3.org/ns/prov#hadRole",
    prov_wasDerivedFrom       = "http://www.w3.org/ns/prov#wasDerivedFrom",
    prov_qualifiedDerivation  = "http://www.w3.org/ns/prov#qualifiedDerivation",
    prov_Derivation           = "http://www.w3.org/ns/prov#Derivation",
    prov_entity               = "http://www.w3.org/ns/prov#entity",
    prov_generatedAtTime      = "http://www.w3.org/ns/prov#generatedAtTime",
    
    sosa_rel_hasSampleRel     = "http://www.w3.org/ns/sosa/sampling/hasSampleRelationship",
    sosa_rel_SampleRel        = "http://www.w3.org/ns/sosa/sampling/SampleRelationship",
    sosa_rel_natureOfRel      = "http://www.w3.org/ns/sosa/sampling/natureOfRelationship",
    sosa_rel_relatedSample    = "http://www.w3.org/ns/sosa/sampling/relatedSample",
    
    sosa_resultTime           = "http://www.w3.org/ns/sosa/resultTime"
  )
  
  # Helper: quick predicate presence
  has_pred <- function(u) any(preds == u)
  
  # --- 4. Basic predicate presence checks ---
  has_contactPoint  <- has_pred(uri$dcat_contactPoint)
  has_geometry      <- has_pred(uri$geo_hasGeometry)
  has_created       <- has_pred(uri$dct_created)
  has_creator       <- has_pred(uri$dct_creator)
  has_qualifiedAttr <- has_pred(uri$prov_qualifiedAttribution)
  has_sampler_pred  <- has_pred(uri$sosa_MadeBySampler)
  has_sampling      <- any(triples$p == uri$rdf_type & triples$o == uri$sosa_Sampling)
  has_qualDeriv     <- has_pred(uri$prov_qualifiedDerivation)
  has_sampleRel_pred <- has_pred(uri$sosa_rel_hasSampleRel)
  
  # --- 5. Attribution shape: prov:qualifiedAttribution with agent + role ---
  attr_nodes <- unique(triples$o[triples$p == uri$prov_qualifiedAttribution])
  has_attribution <- FALSE
  if (length(attr_nodes) > 0) {
    # nodes that are typed prov:Attribution
    attr_typed <- attr_nodes[attr_nodes %in% triples$s[
      triples$p == uri$rdf_type & triples$o == uri$prov_Attribution
    ]]
    if (length(attr_typed) > 0) {
      # for these nodes check agent + hadRole
      has_agent <- attr_typed %in% triples$s[triples$p == uri$prov_agent]
      has_role  <- attr_typed %in% triples$s[triples$p == uri$prov_hadRole]
      has_attribution <- all(has_agent & has_role)
    }
  }
  
  # --- 6. Geometry + WKT + EPSG ---
  geom_nodes <- unique(triples$o[triples$p == uri$geo_hasGeometry])
  wkt_vals <- character(0)
  if (length(geom_nodes) > 0) {
    wkt_vals <- triples$o[triples$s %in% geom_nodes & triples$p == uri$geo_asWKT]
  }
  
  has_WKT_EPSG <- FALSE
  if (length(wkt_vals) > 0) {
    # very simple pattern: <urn:ogc:def:crs:EPSG::XXXX> POINT (...)
    pattern <- "^<urn:ogc:def:crs:EPSG::[0-9]+> POINT \\([-+0-9\\.]+ [-+0-9\\.]+\\)$"
    # strip possible quotes if rdf_query returns them
    clean_wkt <- gsub('^"|"$', "", wkt_vals)
    has_WKT_EPSG <- all(grepl(pattern, clean_wkt))
  }
  
  # --- 7. Sampler SHACL-like: MadeBySampler + Sampler class ---
  sampler_links <- unique(triples$o[triples$p == uri$sosa_MadeBySampler])
  sampler_typed <- unique(triples$s[
    triples$p == uri$rdf_type & triples$o == uri$sosa_Sampler
  ])
  has_sampler <- FALSE
  if (length(sampler_links) > 0) {
    # all used samplers should be typed Sampler
    has_sampler <- all(sampler_links %in% sampler_typed)
  }
  
  # --- 8. SampleRelationship shape ---
  has_sampleRelationship <- FALSE
  if (has_sampleRel_pred) {
    rel_nodes <- unique(triples$o[triples$p == uri$sosa_rel_hasSampleRel])
    if (length(rel_nodes) > 0) {
      # typed as SampleRelationship
      typed_rel <- rel_nodes[rel_nodes %in% triples$s[
        triples$p == uri$rdf_type & triples$o == uri$sosa_rel_SampleRel
      ]]
      if (length(typed_rel) > 0) {
        has_nature <- typed_rel %in% triples$s[triples$p == uri$sosa_rel_natureOfRel]
        has_related <- typed_rel %in% triples$s[triples$p == uri$sosa_rel_relatedSample]
        has_sampleRelationship <- all(has_nature & has_related)
      }
    }
  }
  
  # --- 9. QualifiedDerivation shape (very minimal) ---
  qual_deriv_nodes <- unique(triples$o[triples$p == uri$prov_qualifiedDerivation])
  has_qualifiedDerivation <- FALSE
  if (length(qual_deriv_nodes) > 0) {
    typed_deriv <- qual_deriv_nodes[qual_deriv_nodes %in% triples$s[
      triples$p == uri$rdf_type & triples$o == uri$prov_Derivation
    ]]
    if (length(typed_deriv) > 0) {
      has_entity <- typed_deriv %in% triples$s[triples$p == uri$prov_entity]
      has_qualifiedDerivation <- all(has_entity)
    }
  }
  
  # --- 10. ISO 8601 datetime checks for created / generatedAtTime / resultTime ---
  datetime_preds <- c(uri$dct_created, uri$prov_generatedAtTime, uri$sosa_resultTime)
  dt_vals <- triples$o[triples$p %in% datetime_preds]
  
  has_iso_datetime <- TRUE
  if (length(dt_vals) > 0) {
    # strip possible quotes
    clean_dt <- gsub('^"|"$', "", dt_vals)
    # strict ISO: YYYY-MM-DDThh:mm:ss
    iso_pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
    has_iso_datetime <- all(grepl(iso_pattern, clean_dt))
  }
  
  # --- 11. Build summary tibble ---
  out <- tibble::tibble(
    ttl_file          = basename(ttl_path),
    total_triples     = nrow(triples),
    predicates_found  = length(preds),
    missing_predicates = "None",
    has_attribution      = has_attribution,
    has_contactPoint     = has_contactPoint,
    has_geometry         = has_geometry,
    has_WKT_EPSG         = has_WKT_EPSG,
    has_sampler          = has_sampler,
    has_sampling         = has_sampling,
    has_sampleRelationship = has_sampleRelationship,
    has_qualifiedDerivation = has_qualifiedDerivation,
    has_created          = has_created,
    has_creator          = has_creator,
    has_iso_datetime     = has_iso_datetime
  )
  
  # --- 12. Human-readable "missing" summary ---
  missing <- character(0)
  if (!has_attribution)         missing <- c(missing, "prov:qualifiedAttribution / Attribution shape")
  if (!has_contactPoint)        missing <- c(missing, "dcat:contactPoint")
  if (!has_geometry)            missing <- c(missing, "geo:hasGeometry")
  if (!has_WKT_EPSG)            missing <- c(missing, "geo:asWKT with EPSG::XXXX POINT(...)")
  if (!has_sampler)             missing <- c(missing, "sosa:MadeBySampler with typed sosa:Sampler")
  if (!has_sampling)            missing <- c(missing, "sosa:Sampling instances")
  if (!has_sampleRelationship)  missing <- c(missing, "sosa-rel:SampleRelationship shape")
  if (!has_qualifiedDerivation) missing <- c(missing, "prov:qualifiedDerivation / Derivation shape")
  if (!has_created)             missing <- c(missing, "dct:created")
  if (!has_creator)             missing <- c(missing, "dct:creator")
  if (!has_iso_datetime)        missing <- c(missing, "ISO 8601 datetime (YYYY-MM-DDThh:mm:ss)")
  
  if (length(missing) > 0) {
    out$missing_predicates <- paste(missing, collapse = "; ")
  }
  
  out
}

