#' @title
#' @description
#' A short description...
#' 
#' @param excel_path description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom package function
#' @export
#' @example
#' specimen_catalogue(excel_path = "./specimen_template.xlsx")
#' 
### function specimen_catalogue
specimen_catalogue <- function(excel_path = NULL) {
  # read excel file sheets ----
  excel_file <- readxl::read_excel(excel_path, sheet = "SpecimentsInfo")
  excel_curators <- readxl::read_excel(excel_path, sheet = "CuratorsInfo")
  excel_relation <- readxl::read_excel(excel_path, sheet = "RelationInfo")
  # lines concerning examples, units of measurement and data types are removed ----
  # excel_file <- excel_file[-c(1:3),]
  # excel_curators <- excel_curators[-c(1:3),]
  # excel_relation <- excel_relation[-c(1:3),]
  # if excel file contains only example sensor ----
  excel_file <- excel_file[c(-1),]
  excel_curators <- excel_curators[c(-1),]
  excel_relation <- excel_relation[c(-1),]
  # assign sample IDs, so that references can be made ----
  # TODO change when the DOI can generate trough DataCite ----
  n_speciments <- nrow(excel_file)
  speciments_uuids <- sapply(1:n_speciments, uuid::UUIDgenerate)
  # folders creation ----
  # root folder
  root_dir <- paste0(
    format(Sys.time(), format = "%Y%m%d_%H%M%S"),
    "_specimens"
  )
  if (!dir.exists(root_dir)) {
    dir.create(root_dir)
  }
  specimen_XML(excel_file = excel_file, excel_curators = excel_curators,
               excel_relation = excel_relation,
               speciments_uuids = speciments_uuids, root_dir = root_dir)
  specimen_ttl(excel_file = excel_file, excel_curators = excel_curators,
               excel_relation = excel_relation,
               speciments_uuids = speciments_uuids, root_dir = root_dir)
}

#' @title
#' @param excel_file
#' @param excel_curators
#' @param excel_relation
#' @param speciments_uuids
#' @param root_dir
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom httr2 request req_url_query req_method
#' @importFrom httr2 req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @keywords internal
#'
### function specimen_XML
specimen_XML <- function(excel_file = NULL, excel_curators = NULL,
                         excel_relation = NULL, speciments_uuids = NULL,
                         root_dir = NULL) {
  # XML files creation ----
  for (i in 1:length(speciments_uuids)) {
    sp_id <- excel_file$specimen_id[i]
    excel_curator <- excel_curators |>
      dplyr::filter(specimen_id == sp_id)
    excel_rel <- excel_relation |>
      dplyr::filter(specimen_id == sp_id)
    # name of XML specimen file ----
    file_name <- paste0("specimen_", speciments_uuids[i])
    # read XML ----
    specimen_XML_base <- xml2::read_xml("base_resource.xml")
    # XML ----
    # cs:resource
    a <- specimen_XML_base |>
      xml2::xml_add_child(
        "cs:resource",
        "registeredObjectType" = "http://pid.geoscience.gov.au/def/voc/igsn-codelists/PhysicalSample"
      ) |>
      xml2::xml_add_child(
        "cs:resourceIdentifier",
        paste0("uuid-", speciments_uuids[i]) # this element needs format like <string>-<string> e.g. uuuid-xxxxxx or doi-xxxxxx
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
    g <- xml2::xml_add_sibling(
      f,
      "cs:method",
      "methodURI" = excel_file$method_doi[i],
      excel_file$sampler[i]
    ) |>
      xml2::xml_add_sibling(
        "cs:campaign",
        excel_file$campaign[i]
      )
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
    
    # write file ----
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

#' @title
#' @param excel_file
#' @param excel_curators
#' @param excel_relation
#' @param speciments_uuids
#' @param root_dir
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom httr2 request req_url_query req_method
#' @importFrom httr2 req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @keywords internal
#'
### function specimen_ttl
specimen_ttl <- function(excel_file = NULL, excel_curators = NULL,
                         excel_relation = NULL, speciments_uuids = NULL,
                         root_dir = NULL) {
  # ttl files creation ----
  for (i in 1:length(speciments_uuids)) {
    sp_id <- excel_file$specimen_id[i]
    excel_curator <- excel_curators |>
      dplyr::filter(specimen_id == sp_id)
    excel_rel <- excel_relation |>
      dplyr::filter(specimen_id == sp_id)
    # name of XML specimen file ----
    file_name <- paste0("specimen_", speciments_uuids[i])
    specimen_file_ttl <- file(
      paste0(root_dir, "/", file_name, ".ttl")
    )
    # names space
    specimen <- "@prefix sosa: <http://www.w3.org/ns/sosa/>
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix geosparql: <http://www.opengis.net/ont/geosparql#>
@prefix sf: <http://www.opengis.net/ont/sf#>
@prefix foaf: <http://xmlns.com/foaf/0.1/>
@prefix dcat: <http://www.w3.org/ns/dcat#>
@prefix prov: <http://www.w3.org/ns/prov#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix sosa-rel: <http://www.w3.org/ns/sosa/sampling/>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>"
    uuid <- speciments_uuids[i]
    site_id <- excel_file$site_id[i]
    location_id <- excel_file$location_id[i]
    activity_id <- excel_file$campaign[i]
    sampler_name <- excel_file$sampler[i]
    sampler_uuid <- sapply(
      length(
        excel_file$sampler[i]
      ), 
      uuid::UUIDgenerate
    )
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
    # description
    if (!is.na(excel_file$description[i])) {
      description <- paste0("  rdfs:comment '", excel_file$description[i], "'@en ;")
    } else {
      ""
    }
    # related samples
    if (nrow(excel_rel) != 0) {
      samples <- ""
      for (w in 1:nrow(excel_rel)) {
        samples <- c(
          samples,
          c(
            "  sosa-rel:hasSampleRelationship [",
            "     rdf:type sosa-rel:SampleRelationship ;",
            paste0("     sosa-rel:natureOfRelationship",
            " <http://pid.geoscience.gov.au/def/voc/igsn-codelists/", excel_rel$relation_type[w], "> ;"),
            paste0("     sosa-rel:relatedSample <", excel_rel$related_resources[w], "> ;"),
            "  ] ;"
          )
        )
      }
      related_samples <- c(
        samples,
        paste0("  sosa:SamplingProcedure <", excel_file$method_doi[i], "> .")
      )
    } else {
      related_samples <- c(
        paste0("  sosa:SamplingProcedure <", excel_file$method_doi[i], "> .")
      )
    }
    # sosa:Sample
    sosa_sample <- c(
      paste0("<http://rdfdata.lteritalia.it/samples/uuid-", uuid, "> rdf:type sosa:Sample, prov:Entity ;"),
      paste0("  dcat:landingPage <http://www.lteritalia.it/samples/uuid-", uuid, ".xml> ;"),
      description,
      paste0("  rdfs:label '", excel_file$resource_title[i], "'@en ;"),
      paste0("  sosa:isSampleOf <", site_id, "> ;"),
      paste0("  sosa:madeSampling <", activity_id, "> ;"),
      paste0("  sosa:isResultOfMadeBySampler <http://rdfdata.lteritalia.it/sampler/", sampler_uuid, "> ;"),
      "  geosparql:hasGeometry [ ",
      paste0("    rdf:type sf:", stringr::str_to_title(geo_type), " ;"),
      paste0("    geosparql:asWKT '<urn:ogc:def:crs:EPSG::4283> ", geo_wkt, "'^^geosparql:wktLiteral"),
      "  ] ;",
      curators,
      contributors,
      paste0("  sosa:MaterialSample <http://vocabulary.odm2.org/medium/", excel_file$material_type[i], "> ;"),
      related_samples
    )
    # sosa:hasSample
    sosa_hasSampling <- c(
      paste0("<", site_id, "> sosa:hasSample <http://rdfdata.lteritalia.it/samples/uuid-", uuid, "> .")
    )
    sosa_sampling <- c(
      paste0("<", activity_id, "> rdf:type sosa:Sampling ;"),
      paste0("  sosa:hasResult <http://rdfdata.lteritalia.it/samples/uuid-", uuid, "> ;"),
      paste0("  sosa:madeBySampler <http://rdfdata.lteritalia.it/sampler/", sampler_uuid, "> ;"),
      paste0("  sosa:resultTime '",
        as.Date(
          as.numeric(excel_file$date[i]),
          origin = "1899-12-30"
        ),
        "'^^xsd:date ;"
      ),
      paste0("  sosa:hasFeatureOfInterest <", site_id, "> .")
    )
    # sosa:Sampler
    sosa_sampler <- c(
      paste0("<http://rdfdata.lteritalia.it/sampler/", sampler_uuid , "> rdf:type sosa:Sampler, prov:Agent , prov:Entity ;"),
      paste0("  rdfs:label '", sampler_name, "'@en .")
    )
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
  ttl_files <- list.files(path = root_dir, pattern = "\\.ttl$")
  for (j in 1:length(ttl_files)) {
    new_sensorType_qr <- httr2::request("http://fuseki1.get-it.it/specimen") |>
      httr2::req_auth_basic(username = username_fuseki1, password = pwd_fuseki1) |>
      httr2::req_method("POST") |>
      httr2::req_body_file(
        path = paste0(root_dir, "/", ttl_files[j]),
        type = "text/turtle"
      ) |>
      httr2::req_retry(max_tries = 3, max_seconds = 120)
    httr2::req_perform(new_sensorType_qr, verbosity = 3)
  }
}
