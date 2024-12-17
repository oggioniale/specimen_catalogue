#' Create a catalogue of samples
#' @description `r lifecycle::badge("experimental")`
#' This function generates a series of files in TTL and XML formats based on a
#' spreadsheet template ('specimen_template.xlsx'). These files reproduce the
#' contents of each row according to the Semantic Sensor Network Ontology (SSN)
#' model and the relevant standards. For more details on compatibility, refer
#' to: https://github.com/oggioniale/specimen_catalogue?tab=readme-ov-file.
#'
#' @param excel_path A `string` of the location where the spreadsheet template
#' is located
#' @return A folder in the working directory containing two files, TTL and XML,
#' for each sample listed in the 'SpecimenInfo' worksheet of the spreadsheet
#' template.
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom readxl read_excel
#' @importFrom uuid UUIDgenerate
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' specimen_catalogue(excel_path = "./specimen_template.xlsx")
#'
#' }
#' ## End (Not run)
#'
### function specimen_catalogue
specimen_catalogue <- function(excel_path = NULL) {
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
  excel_file <- excel_file[c(-1), ]
  excel_curators <- excel_curators[c(-1), ]
  excel_relation <- excel_relation[c(-1), ]
  excel_sampler <- excel_sampler[c(-1), ]
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
  specimen_ttl(excel_file = excel_file, excel_curators = excel_curators,
               excel_relation = excel_relation, excel_sampler = excel_sampler,
               specimen_uuids = specimen_uuids, root_dir = root_dir)
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
    sp_id <- excel_file$specimen_id[i]
    excel_curator <- excel_curators |>
      dplyr::filter(specimen_id == sp_id)
    excel_rel <- excel_relation |>
      dplyr::filter(specimen_id == sp_id)
    # name of XML specimen file
    file_name <- paste0("specimen_", specimen_uuids[i])
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
        paste0("uuid-", specimen_uuids[i]) # the XML schema needs that the format of this element seems like <string>-<string> e.g. uuuid-xxxxxx or doi-xxxxxx
      ) |>
      xml2::xml_add_sibling(
        "cs:landingPage",
        paste0(
          "http://www.lteritalia.it/samples/",
          file_name,
          ".xml"
        )
      ) |>
      xml2::xml_add_sibling(
        "cs:isPublic",
        stringr::str_to_lower(
          excel_file$is_public[i]
        )
      ) |>
      xml2::xml_add_sibling(
        "cs:resourceTitle",
        excel_file$resource_title[i]
      ) |>
      xml2::xml_add_sibling(
        "cs:resourceTypes"
      )
    xml2::xml_add_child(
      a,
      "cs:resourceType",
      paste0(
        "http://vocabulary.odm2.org/specimentype/",
        excel_file$resource_type[i]
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
        excel_file$material_type[i]
      )
    )
    if (!is.na(excel_file$purpose[i])) {
      c <- xml2::xml_add_sibling(
        b,
        "cs:purpose",
        excel_file$purpose[i]
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
      "sampledFeatureURI" = excel_file$site_id[i]
    )
    e <- xml2::xml_add_sibling(
      d,
      "cs:location"
    )
    xml2::xml_add_child(
      e,
      "cs:locality",
      "localityURI" = excel_file$location_id[i]
    )
    location_info <- ReLTER::get_location_info(
      location_id = excel_file$location_id[i]
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
    xml2::xml_add_child(
      f,
      "cs:timeInstant",
      as.character(as.Date(
        as.numeric(excel_file$date[i]),
        origin = "1899-12-30"
      ))
    )
    if (!is.na(excel_file$method_doi[i]) & !is.na(excel_file$sampler[i])) {
      g <- xml2::xml_add_sibling(
        f,
        "cs:method",
        "methodURI" = excel_file$method_doi[i],
        excel_file$sampler[i]
      )
    } else if (!is.na(excel_file$method_doi[i]) & is.na(excel_file$sampler[i])) {
      g <- xml2::xml_add_sibling(
        f,
        "cs:method",
        excel_file$sampler[i]
      )
    } else if (is.na(excel_file$method_doi[i]) & is.na(excel_file$sampler[i])) {
      g <- f
    }
    if (!is.na(excel_file$campaign[i])) {
      g <- g |>
        xml2::xml_add_sibling(
          "cs:campaign",
          excel_file$campaign[i]
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
    if (!is.na(excel_file$description[i])) {
      s <- xml2::xml_add_sibling(
        q,
        "cs:comments",
        excel_file$description[i]
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
#' This function generates ttl files for each sample.
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
                         specimen_uuids = NULL, root_dir = NULL) {
  # ttl files creation
  for (i in 1:length(specimen_uuids)) {
    sp_id <- excel_file$specimen_id[i]
    excel_curator <- excel_curators |>
      dplyr::filter(specimen_id == sp_id)
    excel_rel <- excel_relation |>
      dplyr::filter(specimen_id == sp_id)
    # name of XML specimen file
    file_name <- paste0("specimen_", specimen_uuids[i])
    specimen_file_ttl <- file(
      paste0(root_dir, "/", file_name, ".ttl")
    )
    # names space
    specimen <- "@prefix sosa: <http://www.w3.org/ns/sosa/>
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix geo: <http://www.opengis.net/ont/geosparql#>
@prefix sf: <http://www.opengis.net/ont/sf#>
@prefix foaf: <http://xmlns.com/foaf/0.1/>
@prefix dcat: <http://www.w3.org/ns/dcat#>
@prefix prov: <http://www.w3.org/ns/prov#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix sosa-rel: <http://www.w3.org/ns/sosa/sampling/>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>"
    uuid <- specimen_uuids[i]
    site_id <- excel_file$site_id[i]
    location_id <- excel_file$location_id[i]
    activity_id <- excel_file$campaign[i]
    # samplers
    sampler_name <- excel_file$sampler[i]
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
      sampler_uri <- sampler_list$sampler_uri
      madeBy <- paste0("  sosa:MadeBySampler <", sampler_uri, "> ;")
      # sosa:Sampler
      sosa_sampler <- ""
    } else {
      if (!is.na(sampler_name)) {
        sampler_uuid <- sapply(
          length(
            excel_file$sampler[i]
          ), 
          uuid::UUIDgenerate
        )
        sampler_uri <- paste0("<http://rdfdata.lteritalia.it/sampler/", sampler_uuid, ">")
        # sosa:Sampler
        sosa_sampler <- c(
          paste0(sampler_uri, " rdf:type sosa:Sampler, prov:Agent , prov:Entity ;"),
          paste0("  rdfs:label '", sampler_name, "'@en .")
        )
        madeBy <- paste0("  sosa:MadeBySampler <", sampler_uri, "> ;")
      } else {
        sampler_uri <- ""
        sosa_sampler <- ""
        madeBy <- ""
      }
    }
    location_info <- ReLTER::get_location_info(
      location_id = location_id
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
        paste0("    rdfs:seeAlso   <", institution_ror[j], ">"),
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
        paste0("    prov:hadRole   <http://inspire.ec.europa.eu/metadata-codelist/ResponsiblePartyRole/",
        gsub(
            " ",
            "",
            excel_curator$contact_person_type[y]
          ),
        "> ;"),
        paste0("    rdfs:seeAlso   <", excel_curator$contact_person_orcid[y], "> ;"),
        paste0("    foaf:fullName  '", excel_curator$contact_person[y], "'"),
        "  ] ;"
      )
    }
    # description + purpose
    if (!is.na(excel_file$description[i]) & !is.na(excel_file$purpose[i])) {
      description <- paste0("  rdfs:comment 'Description: ", excel_file$description[i], " - Purpose: ", excel_file$purpose[i], "'@en ;")
    } else if (is.na(excel_file$description[i]) & !is.na(excel_file$purpose[i])) {
      description <- paste0("  rdfs:comment 'Purpose: ", excel_file$purpose[i], "'@en ;")
    } else if (!is.na(excel_file$description[i]) & is.na(excel_file$purpose[i])) {
      description <- paste0("  rdfs:comment 'Description: ", excel_file$description[i], "'@en ;")
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
          " <http://pid.geoscience.gov.au/def/voc/igsn-codelists/", excel_rel$relation_type[w], "> ;"),
          paste0("     sosa-rel:relatedSample <", excel_rel$related_resources[w], "> ;"),
          "  ] ;"
        )
      }
    } else {
      related <- ""
    }
    # procedure
    if (!is.na(excel_file$method_doi[i])) {
      procedure <- paste0("  sosa:Procedure <", excel_file$method_doi[i], "> ;")
    } else {
      procedure <- ""
    }
    # meterial
    material <- paste0("  sosa:MaterialSample <http://vocabulary.odm2.org/medium/", excel_file$material_type[i], "> .")
    # sosa:Sample
    if (!is.na(activity_id)) {
      activityId <- paste0("  sosa:madeSampling <", activity_id, "> ;")
    } else {
      activityId <- ""
    }
    
    sosa_sample <- c(
      paste0("<http://rdfdata.lteritalia.it/samples/uuid-", uuid, "> rdf:type sosa:Sample, prov:Entity ;"),
      paste0("  dcat:landingPage <http://www.lteritalia.it/samples/uuid-", uuid, ".xml> ;"),
      description,
      paste0("  rdfs:label '", excel_file$resource_title[i], "'@en ;"),
      paste0("  sosa:isSampleOf <", site_id, "> ;"),
      activityId,
      madeBy,
      "  geo:hasGeometry [ ",
      paste0("    rdf:type sf:", stringr::str_to_title(geo_type), " ;"),
      paste0("    geo:asWKT '<urn:ogc:def:crs:EPSG::4283> ", geo_wkt, "'^^geo:wktLiteral"),
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
        paste0("<", site_id, "> sosa:hasSample <http://rdfdata.lteritalia.it/samples/uuid-", uuid, "> .")
      )
      sosa_sampling <- c(
        paste0("<", activity_id, "> rdf:type sosa:Sampling ;"),
        paste0("  sosa:hasResult <http://rdfdata.lteritalia.it/samples/uuid-", uuid, "> ;"),
        madeBy,
        paste0("  sosa:resultTime '",
               as.Date(
                 as.numeric(excel_file$date[i]),
                 origin = "1899-12-30"
               ),
               "'^^xsd:date ;"
        ),
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
      ""
    )
    # write file specimen_file_ttl
    write(specimen, file = specimen_file_ttl)
    # close file
    close(specimen_file_ttl)
  }
  # send the ttl files to SPARQL endpoint
  # ttl_files <- list.files(path = root_dir, pattern = "\\.ttl$")
  # for (j in 1:length(ttl_files)) {
  #   new_sensorType_qr <- httr2::request("http://fuseki1.get-it.it/specimen") |>
  #     httr2::req_auth_basic(username = Sys.getenv("USERNAME_FUSEKI1"), password = Sys.getenv("PWD_FUSEKI1")) |>
  #     httr2::req_method("POST") |>
  #     httr2::req_body_file(
  #       path = paste0(root_dir, "/", ttl_files[j]),
  #       type = "text/turtle"
  #     ) |>
  #     httr2::req_retry(max_tries = 3, max_seconds = 120)
  #   httr2::req_perform(new_sensorType_qr, verbosity = 3)
  # }
}

#' Check of the sampler's existence in the catalog.
#' @description `r lifecycle::badge("experimental")`
#' Checks if a sampler already exists in the catalog or not.
#' 
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
#' @export
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

#' Run shiny app for get and visualize sample catalogue
#' @description `r lifecycle::badge("experimental")`
#' This function run the Shiny App
#' @author Alessandro Oggioni, phD \email{alessandro.oggioni@@cnr.it}
#' @import shiny
#' @import shinydashboard
#' @importFrom httr2 request req_url_query req_method req_headers
#' @importFrom httr2 req_retry req_perform resp_check_status
#' @importFrom httr2 resp_body_json resp_body_string
#' @importFrom purrr pluck discard
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select group_by summarize ungroup bind_rows
#' @importFrom jqr jq
#' @importFrom jsonlite stream_in
#' @importFrom dtplyr lazy_dt
#' @importFrom xml2 read_xml xml_find_all
#' @importFrom stringr str_c
#' @importFrom sf st_as_sf
#' @importFrom DT renderDT datatable
#' @importFrom htmltools tags em
#' @importFrom leaflet renderLeaflet leaflet addProviderTiles addMarkers
#' @importFrom leaflet providerTileOptions
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' specimen_runApp(launch.browser = rstudioapi::viewer)
#' 
#' }
#' ## End (Not run)
#' 
### specimen_runApp
specimen_runApp <- function(...) {
  require(shiny)
  require(crosstalk)
  require(shinydashboard)
  shinyApp(
    ui = dashboardPage(
      # skin = "blue",
      # collapse_sidebar = TRUE,
      header = dashboardHeader(
        title = tagList(
          tags$head(tags$style(HTML('.navbar {
                              background-color: #334155 !important;
                              }
                              .column {
                                float: left;
                                width: 50%;
                              }

                              /* Clear floats after the columns */
                              .row:after {
                                content: "";
                                display: table;
                                clear: both;
                              }
                              '))),
          tags$span(class = "logo-lg", "LTER-Italy samples"), 
          tags$img(src = "//www.lteritalia.it/wordpress/wp-content/uploads/2023/09/solo_foglia.png")), 
        # fixed = FALSE,
        # enable_rightsidebar = TRUE,
        # rightSidebarIcon = "gears",
        tags$li(
          class ="dropdown", 
          tags$a(
            href = "http://www.lteritalia.it",
            tags$img(
              src = "//www.lteritalia.it/wordpress/wp-content/uploads/LTER-IT-03_72DPI.png",
              height = "35%",
              width = "35%",
              align = "right"
            ),
            style = "margin:0;padding-top:2px;padding-bottom:2px;padding-left:10px;padding-right:10px;",
            target = "_blank"
          )
        )#,
        # tags$li(class = "dropdown",
        #         actionButton("help", "Give me an overview", style="margin-right: 10px; margin-top: 8px; color: #fff; background-color: #0069D9; border-color: #0069D9")
        # )
      ),
      sidebar = dashboardSidebar(
        collapsed = TRUE,
        sidebarMenu(
          menuItem("Samples", tabName = "fixed", icon = icon("map-marked-alt", lib = "font-awesome"))
        )
      ),
      body = dashboardBody(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
        ),
        tabItems(
          tabItem(
            tabName = "fixed",
            fluidRow(
              box(
                width = 12,
                title = "Catalogue of LTER-Italy samples", 
                closable = FALSE, 
                status = "info", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                column(12, leaflet::leafletOutput("specimen_map")),
                column(12, DT::dataTableOutput("specimenTbl"))
              )
            )
          )
        ),
        tags$footer(HTML('<div class="row"><div class="column">
               <p><span style="color: #94c5e5"><strong>Contacts:</strong></span><br><strong>Secretariat: </strong>Via Roberto Cozzi, 53 20156 Milan (Italy)<br><strong>Phone: </strong>+02 66173307<br><strong>E-mail: </strong><a href="mailto:lteritaly@gmail.com" target="_blank">lteritaly@gmail.com</a></p>
            </div><div class="column"><span style="color: #94c5e5"><strong>Useful links</strong></span><br><a href="http://sparql.lteritalia.it/" target="_blank">SPARQL Endpoint</a></div></div>'), align = 'left')
      )
    ),
    server = function(input, output, session) {
      # data table
      specimen_dataset <- "http://fuseki1.get-it.it/specimen/query"
      specimen_query <- "PREFIX sosa: <http://www.w3.org/ns/sosa/>
     PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     PREFIX geosparql: <http://www.opengis.net/ont/geosparql#>
     PREFIX dcat: <http://www.w3.org/ns/dcat#>
     PREFIX foaf: <http://xmlns.com/foaf/0.1/>
     SELECT ?c ?label ?geom_wkt ?landing_page ?ror ?material ?sampler
     WHERE {
       ?c rdf:type sosa:Sample .
       OPTIONAL { ?c rdfs:label ?label . }
       OPTIONAL {
         ?c geosparql:hasGeometry ?geom .
           OPTIONAL { ?geom geosparql:asWKT ?geom_wkt . }
       }
       OPTIONAL { ?c dcat:landingPage ?landing_page . }
       OPTIONAL {
         ?c dcat:contactPoint ?contact .
           OPTIONAL { ?contact rdf:type foaf:Organization .
             ?contact rdfs:seeAlso ?ror
           }
       }
       OPTIONAL { ?c sosa:MaterialSample ?material . }
       OPTIONAL { ?c sosa:isResultOfMadeBySampler ?sampler . }
       OPTIONAL { ?c sosa:SamplingProcedure ?procedure . }
     }
     ORDER BY ASC(?l)"
      list_specimen_req <- httr2::request(specimen_dataset) |>
        httr2::req_url_query(query = specimen_query) |>
        httr2::req_method("POST") |>
        httr2::req_headers(Accept = "application/sparql-results+json") |>
        httr2::req_retry(max_tries = 3, max_seconds = 120) |>
        httr2::req_perform()
      httr2::resp_check_status(list_specimen_req)
      list_specimen <-
        httr2::resp_body_json(list_specimen_req, simplifyVector = TRUE) |>
        purrr::pluck("results") |>
        tibble::as_tibble() |>
        dplyr::mutate(
          specimen_uri = bindings$c$value,
          specimen_label = bindings$label$value,
          specimen_geom = bindings$geom_wkt$value,
          specimen_landingPage = bindings$landing_page$value,
          specimen_ror = bindings$ror$value,
          specimen_material = bindings$material$value,
          specimen_sampler = bindings$sampler$value,
          .keep = "used"
        ) |>
        dplyr::select(
          specimen_uri,
          specimen_label,
          specimen_geom,
          specimen_landingPage,
          specimen_ror,
          specimen_material,
          specimen_sampler
        )
      # get labels
      rors <- unique(list_specimen$specimen_ror)
      materials <- unique(list_specimen$specimen_material)
      samplers <- unique(list_specimen$specimen_sampler)
      # ror label
      q <- '{
        label: .names[] | select(.types[]=="label" and .lang=="en") | .value
      }'
      if (any(is.na(rors))) {
        rors <- rors |>
          purrr::discard(is.na)
        rors_tbl <- lapply(1:length(rors), function(n) {
          ror_id <- sub(
            pattern = "https://ror.org/",
            replacement = "",
            x = rors[n]
          )
          export <- httr2::request(base_url = paste0("https://api.ror.org/v2/organizations/", ror_id)) |>
            httr2::req_method("GET") |>
            httr2::req_headers(Accept = "application/json") |>
            httr2::req_retry(max_tries = 3, max_seconds = 120) |>
            httr2::req_perform()
          httr2::resp_check_status(export)
          jj <- httr2::resp_body_string(export)
          ror_label <- jj |>
            jqr::jq(as.character(q)) |>
            textConnection(encoding = "UTF-8") |>
            jsonlite::stream_in(simplifyDataFrame = TRUE, verbose = FALSE) |>
            dtplyr::lazy_dt() |>
            dplyr::as_tibble() |>
            dplyr::mutate(
              specimen_ror = rors[n],
              ror_label = label,
              .before = "ror_label",
              .keep = "none"
            )
        }) |> dplyr::bind_rows()
        rors_tbl <- rbind(rors_tbl, NA)
      } else {
        rors_tbl <- lapply(1:length(rors), function(n) {
          ror_id <- sub(
            pattern = "https://ror.org/",
            replacement = "",
            x = rors[n]
          )
          export <- httr2::request(base_url = paste0("https://api.ror.org/v2/organizations/", ror_id)) |>
            httr2::req_method("GET") |>
            httr2::req_headers(Accept = "application/json") |>
            httr2::req_retry(max_tries = 3, max_seconds = 120) |>
            httr2::req_perform()
          httr2::resp_check_status(export)
          jj <- httr2::resp_body_string(export)
          ror_label <- jj |>
            jqr::jq(as.character(q)) |>
            textConnection(encoding = "UTF-8") |>
            jsonlite::stream_in(simplifyDataFrame = TRUE, verbose = FALSE) |>
            dtplyr::lazy_dt() |>
            dplyr::as_tibble() |>
            dplyr::mutate(
              specimen_ror = rors[n],
              ror_label = label,
              .before = "ror_label",
              .keep = "none"
            )
        }) |> dplyr::bind_rows()
      }
      # material label
      if (any(is.na(materials))) {
        materials <- materials |>
          purrr::discard(is.na)
        materials_tbl <- lapply(1:length(materials), function(m){
          material_id <- sub(
            pattern = "http://vocabulary.odm2.org/medium/",
            replacement = "",
            x = materials[m]
          )
          xml_skos <- xml2::read_xml(paste0("http://vocabulary.odm2.org/api/v1/medium/", material_id, "/?format=skos"))
          material_label <- xml2::xml_find_all(
            xml_skos,
            ".//skos:prefLabel/text()"
          ) |>
            as.character() |>
            dplyr::as_tibble() |>
            dplyr::mutate(
              specimen_material = materials[m],
              material_label = value,
              .before = "material_label",
              .keep = "none"
            )
        }) |> dplyr::bind_rows()
        materials_tbl <- rbind(materials_tbl, NA)
      } else {
        materials_tbl <- lapply(1:length(materials), function(m){
          material_id <- sub(
            pattern = "http://vocabulary.odm2.org/medium/",
            replacement = "",
            x = materials[m]
          )
          xml_skos <- xml2::read_xml(paste0("http://vocabulary.odm2.org/api/v1/medium/", material_id, "/?format=skos"))
          material_label <- xml2::xml_find_all(
            xml_skos,
            ".//skos:prefLabel/text()"
          ) |>
            as.character() |>
            dplyr::as_tibble() |>
            dplyr::mutate(
              specimen_material = materials[m],
              material_label = value,
              .before = "material_label",
              .keep = "none"
            )
        }) |> dplyr::bind_rows()
      }
      # sampler label
      if (any(is.na(samplers))) {
        samplers <- samplers |>
          purrr::discard(is.na)
        samplers_tbl <- lapply(1:length(samplers), function(x){
          sampler_uri <- samplers[x]
          sampler_query <- paste0(
            "PREFIX sosa: <http://www.w3.org/ns/sosa/>
     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
     SELECT ?sampler ?label
     WHERE {
       BIND(<", sampler_uri, "> as ?sampler)
       ?sampler rdf:type sosa:Sampler .
       ?sampler rdfs:label ?label .
     }
     ORDER BY ASC(?l)"
          )
          list_sampler_req <- httr2::request(specimen_dataset) |>
            httr2::req_url_query(query = sampler_query) |>
            httr2::req_method("POST") |>
            httr2::req_headers(Accept = "application/sparql-results+json") |>
            httr2::req_user_agent("ReLTER dev") |>
            httr2::req_retry(max_tries = 3, max_seconds = 120) |>
            httr2::req_perform()
          httr2::resp_check_status(list_sampler_req)
          list_sampler <-
            httr2::resp_body_json(list_sampler_req, simplifyVector = TRUE) |>
            purrr::pluck("results") |>
            tibble::as_tibble()|>
            dplyr::mutate(
              specimen_sampler = bindings$sampler$value,
              sampler_label = bindings$label$value,
              .before = "sampler_label",
              .keep = "none"
            )
        }) |> dplyr::bind_rows()
        samplers_tbl <- rbind(samplers_tbl, NA)
      } else {
        samplers_tbl <- lapply(1:length(samplers), function(x){
          sampler_uri <- samplers[x]
          sampler_query <- paste0(
            "PREFIX sosa: <http://www.w3.org/ns/sosa/>
     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
     SELECT ?sampler ?label
     WHERE {
       BIND(<", sampler_uri, "> as ?sampler)
       ?sampler rdf:type sosa:Sampler .
       ?sampler rdfs:label ?label .
     }
     ORDER BY ASC(?l)"
          )
          list_sampler_req <- httr2::request(specimen_dataset) |>
            httr2::req_url_query(query = sampler_query) |>
            httr2::req_method("POST") |>
            httr2::req_headers(Accept = "application/sparql-results+json") |>
            httr2::req_user_agent("ReLTER dev") |>
            httr2::req_retry(max_tries = 3, max_seconds = 120) |>
            httr2::req_perform()
          httr2::resp_check_status(list_sampler_req)
          list_sampler <-
            httr2::resp_body_json(list_sampler_req, simplifyVector = TRUE) |>
            purrr::pluck("results") |>
            tibble::as_tibble()|>
            dplyr::mutate(
              specimen_sampler = bindings$sampler$value,
              sampler_label = bindings$label$value,
              .before = "sampler_label",
              .keep = "none"
            )
        }) |> dplyr::bind_rows()
      }
      list_specimen <- merge(x = list_specimen, y = rors_tbl, by = "specimen_ror", all.y = TRUE)
      list_specimen <- merge(x = list_specimen, y = materials_tbl, by = "specimen_material", all.y = TRUE)
      list_specimen <- merge(x = list_specimen, y = samplers_tbl, by = "specimen_sampler", all.y = TRUE)
      # data shared
      shared_specimenData <- list_specimen |>
        dplyr::filter(!is.na(specimen_ror)) |>
        dplyr::mutate(
          `Specimen name` = paste0(
            "<a href='",
            specimen_landingPage,
            "' target = '_blank'>",
            specimen_label,
            "</a>"
          ),
          Institution = paste0(
            "<a href='",
            specimen_ror,
            "' target = '_blank'>",
            ror_label,
            "</a>"
          ),
          `Specimen material` = paste0(
            "<a href='",
            specimen_material,
            "' target = '_blank'>",
            material_label,
            "</a>"
          ),
          Sampler = paste0(
            "<a href='",
            specimen_sampler,
            "' target = '_blank'>",
            sampler_label,
            "</a>"
          ),
          geom = stringr::str_replace_all(
            specimen_geom,
            pattern = "<urn:ogc:def:crs:EPSG::4283> ",
            replacement = ""
          ),
          .keep = "used"
        ) |>
        unique() |>
        dplyr::select(`Specimen name`, Institution, `Specimen material`, Sampler, geom) |>
        dplyr::group_by(`Specimen name`, geom, `Specimen material`, Sampler) |>
        dplyr::summarize(Institution = stringr::str_c(Institution, collapse = "<br> ")) |>
        dplyr::ungroup() |>
        sf::st_as_sf(wkt = "geom")
      shared_data <- SharedData$new(shared_specimenData)
      
      # data table output
      output$specimenTbl <- DT::renderDT({
        DT::datatable(
          shared_data,
          escape = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            'Table - ', htmltools::em(paste0(
              'Samples collected by LTER-Italy network'
            ))
          ),
          filter = 'top'
        )
      }, server = FALSE)
      
      # map output
      output$specimen_map <- leaflet::renderLeaflet({
        leaflet::leaflet(shared_data) |>
          leaflet::addProviderTiles(
            "CartoDB.Positron",
            options = leaflet::providerTileOptions(opacity = 0.99)) |>
          leaflet::addMarkers(
            popup = paste0(
              "<b>Specimen name: </b>",
              shared_data$`Specimen name`,
              "<br>Material: ",
              shared_data$`Specimen material`,
              "<br>Sampler: ",
              shared_data$Sampler,
              "<br>Institution: ",
              shared_data$Institution
            )
          )
      })
    }
  )
}
