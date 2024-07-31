library(shiny)
library(crosstalk)
library(shinydashboard)

ui <- dashboardPage(
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
)
  


# ui <- fluidPage(
#   fluidRow(
#     column(12, leaflet::leafletOutput("specimen_map")),
#     column(12, DT::dataTableOutput("specimenTbl"))
#   )
# )

server <- function(input, output, session) {
  # data table ----
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
  # data shared ----
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

shinyApp(ui, server)