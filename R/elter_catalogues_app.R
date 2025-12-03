#' Launch the Unified eLTER-IT Shiny Catalogues Application
#' @description `r lifecycle::badge("stable")`
#' This function launches the unified Shiny application that includes:
#' \itemize{
#'   \item the **Samples Catalogue**, and
#'   \item the **Sensor Type Catalogue**.
#' }
#' It serves as the main entry point for the complete eLTER-IT
#' catalogues environment.
#' The function returns a fully integrated Shiny dashboard application
#' with a shared header, footer, styling, and navigation logic.
#' Both catalogues are rendered within separate tabs, and users can
#' seamlessly navigate between them.
#' @param default_tab Character string indicating which tab should be
#'   activated when the application starts.
#'   Allowed values:
#'   \itemize{
#'     \item \code{"samples"} – open Samples Catalogue
#'     \item \code{"sensors"} – open Sensor Type Catalogue
#'   }
#'   Default is \code{"samples"}.
#' @details
#' The wrapper functions \code{specimen_runApp()} and
#' \code{sensor_runApp()} simply call this function with the
#' appropriate \code{default_tab} value.
#' Internally, this function assembles:
#' \itemize{
#'   \item a shared dashboard header (branding, favicon, styling),
#'   \item a sidebar menu with both catalogues,
#'   \item a footer consistent with the eLTER/LTER-Italy style,
#'   \item the server logic for Samples and Sensor Type sections.
#' }
#' @return
#' No return value.  
#' The function launches the unified Shiny application.
#' @examples
#' if (interactive()) {
#'   # Launch the full catalogue with the Samples tab open
#'   elter_catalogues_app(default_tab = "samples")
#'
#'   # Launch the full catalogue with the Sensors tab open
#'   elter_catalogues_app(default_tab = "sensors")
#' }
#' @author
#' Alessandro Oggioni, PhD (2023–2025)  
#' \email{alesssandro.oggioni@cnr.it}
#' @importFrom magrittr %>%
#' @export
#'
### function elter_catalogues_app
elter_catalogues_app <- function(default_tab = c("samples", "sensors")) {
  default_tab <- match.arg(default_tab)
  
  shiny::shinyApp(
    ui = shinydashboard::dashboardPage(
      shinyjs::useShinyjs(),
      header = shinydashboard::dashboardHeader(
        title = shiny::tagList(
          shiny::tags$head(
            # Title TAB
            shiny::tags$title("eLTER-IT catalogues"),
            # Favicon TAB
            shiny::tags$link(
              rel  = "icon",
              type = "image/png",
              href = "https://www.lteritalia.it/wp-content/uploads/2023/09/solo_foglia.png"
            ),
            # Style
            shiny::tags$style(shiny::HTML("
              .navbar { background-color: #334155 !important; }
              .column { float: left; width: 50%; }
              .row:after { content: ''; display: table; clear: both; }
            "))
          ),
          shiny::tags$span(class = "logo-lg", "eLTER-IT catalogues")
        ),
        # logo eLTER-IT to the right
        shiny::tags$li(
          class = "dropdown",
          shiny::tags$a(
            href  = "http://www.lteritalia.it",
            shiny::tags$img(
              src   = "https://www.lteritalia.it/wp-content/uploads/LTER-IT-033-scaled-480x143.png",
              height = "35%", width = "35%", align = "right"
            ),
            style  = "margin:0;padding-top:2px;padding-bottom:2px;padding-left:10px;padding-right:10px;",
            target = "_blank"
          )
        )
      ),
      
      sidebar = shinydashboard::dashboardSidebar(
        collapsed = TRUE,
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem(
            "Samples",
            tabName = "samples",
            icon    = shiny::icon("map-marked-alt", lib = "font-awesome")
          ),
          shinydashboard::menuItem(
            "Samples metadata",
            tabName = "sample_api_run",
            icon    = shiny::icon("map-marked-alt", lib = "font-awesome")
          ),
          shinydashboard::menuItem(
            "Sensors type",
            tabName = "sensors",
            icon    = shiny::icon("thermometer-half", lib = "font-awesome")
          )
        )
      ),
      
      body = shinydashboard::dashboardBody(
        shiny::tags$head(
          shiny::tags$link(
            rel  = "stylesheet",
            type = "text/css"
          )
        ),
        # API popup message
        shinyjs::useShinyjs(),
        uiOutput("api_warning"),
        # tab items
        shinydashboard::tabItems(
          # ------------ TAB SAMPLES
          shinydashboard::tabItem(
            tabName = "samples",
            shiny::fluidRow(
              shinydashboard::box(
                width        = 12,
                title        = "Catalogue of eLTER-IT samples",
                closable     = FALSE,
                status       = "info",
                solidHeader  = FALSE,
                collapsible  = TRUE,
                enable_sidebar = TRUE,
                shiny::column(12, leaflet::leafletOutput("specimen_map")),
                shiny::column(12, DT::dataTableOutput("specimenTbl"))
              )
            )
          ),
          # ------------ TAB SAMPLES upload
          shinydashboard::tabItem(
            tabName = "sample_api_run",
            shiny::fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Specimen Metadata generator",
                closable = FALSE,
                status = "info",
                solidHeader = FALSE,
                collapsible = TRUE,
                enable_sidebar = TRUE,
                fileInput("excel_file", "Upload Excel (.xlsx)"),
                textInput("creator_name", "Creator name"),
                textInput("creator_surname", "Creator surname"),
                textInput("creator_orcid", "Creator ORCID"),
                actionButton("run_api", "Upload"),
                verbatimTextOutput("api_response")
              )
            )
          ),
          
          # ------------ TAB SENSORS
          shinydashboard::tabItem(
            tabName = "sensors",
            shiny::fluidRow(
              shinydashboard::box(
                width        = 12,
                title        = "Catalogue of eLTER-IT sensors type",
                closable     = FALSE,
                status       = "info",
                solidHeader  = FALSE,
                collapsible  = TRUE,
                enable_sidebar = TRUE,
                shiny::column(12, DT::DTOutput("sensorTbl"))
              )
            )
          )
        ),
        
        shiny::tags$footer(
          shiny::HTML('
          <div class="row">
            <div class="column">
              <p><span style="color: #94c5e5"><strong>Contacts:</strong></span><br>
                 <strong>Secretariat: </strong>Via Roberto Cozzi, 53 20156 Milan (Italy)<br>
                 <strong>Phone: </strong>+02 66173307<br>
                 <strong>E-mail: </strong><a href="mailto:lteritaly@gmail.com" target="_blank">lteritaly@gmail.com</a>
              </p>
            </div>
            <div class="column">
              <span style="color: #94c5e5"><strong>Useful links</strong></span>
              <ul style="margin-top: 5px;">
                <li><a href="http://sparql.lteritalia.it/" target="_blank">SPARQL Endpoint</a></li>
                <li><a href="http://127.0.0.1:8000/__docs__/" target="_blank">eLTER-IT Specimen Catalogue API</a></li>
                <li><a href="https://www.lteritalia.it" target="_blank">eLTERIT</a></li>
                <li><a href="https://elter-ri.eu" target="_blank">eLTER RI</a></li>
              </ul>
            </div>
          </div>'),
          align = 'left'
        )
      )
    ),
    
    server = function(input, output, session) {
      api_results <- reactiveVal(NULL)
      # INPUT VALIDATION (shinyvalidate)
      iv <- shinyvalidate::InputValidator$new()
      # Excel file required
      iv$add_rule("excel_file", shinyvalidate::sv_required("Please upload an Excel file (.xlsx)"))
      iv$add_rule("excel_file", function(value) {
        if (!is.null(value) && !grepl("\\.xlsx$", value$name)) {
          return("Only .xlsx files are allowed")
        }
      })
      # Creator name
      iv$add_rule("creator_name", shinyvalidate::sv_required("Creator name is required"))
      # Creator surname
      iv$add_rule("creator_surname", shinyvalidate::sv_required("Creator surname is required"))
      # ORCID
      iv$add_rule("creator_orcid", shinyvalidate::sv_required("ORCID is required"))
      iv$add_rule(
        "creator_orcid",
        shinyvalidate::sv_regex(
          "^(https://orcid\\.org/)?\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$",
          "ORCID must follow the format 0000-0000-0000-0000 or end with X"
        )
      )
      # Enable validation
      iv$enable()
      # Disable Run button when input invalid
      observe({
        valid <- iv$is_valid()
        shinyjs::toggleState("run_api", condition = valid)
      })
      # Other server parts
      session$userData$api_available <- FALSE
      pr_file <- system.file("plumber/plumber.R", package = "SpecimenCat")
      api_process <- NULL
      observe({
        if (session$userData$api_available) return()
        if (!file.exists("/.dockerenv")) {
          try({
            message(">> Starting Specimen API locally...")
            api_process <<- callr::r_bg(function(pr_path) {
              pr <- plumber::plumb(pr_path)
              pr$run(host="127.0.0.1", port=8000)
            }, args = list(pr_file))
          })
        }
        # Multiple attempts to allow plumber to start
        attempt_health_check <- function(tries = 3, wait = 2) {
          for (i in seq_len(tries)) {
            ok <- tryCatch({
              httr2::request("http://127.0.0.1:8000/health") |>
                httr2::req_timeout(2) |>
                httr2::req_perform()
              TRUE
            }, error = function(e) FALSE)
            if (ok) return(TRUE)
            message(">> Health check failed (attempt ", i, "), retrying...")
            Sys.sleep(wait)
          }
          return(FALSE)
        }
        later::later(function() {
          ok <- attempt_health_check()
          session$userData$api_available <- ok
          message(">> API available: ", ok)
        }, delay = 1)
      })
      # warning banner
      output$api_warning <- renderUI({
        invalidateLater(2000)
        if (!isTRUE(session$userData$api_available)) {
          return(tags$div(
            style = "background:#fee2e2; padding:12px; color:#991b1b; margin-bottom:15px;",
            tags$b("⚠️ The Specimen API server is not running."),
            br(),
            "Local mode: automatic startup failed. Run API manually or use Docker."
          ))
        }
        return(NULL)
      })
      # download zip
      output$download_zip <- downloadHandler(
        filename = function() {
          paste0("specimens_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
        },
        content = function(file) {
          # get output_dir from reactive value
          out_dir <- api_results()$output_dir
          validate(need(dir.exists(out_dir), "Output directory not found"))
          # temp ZIP path
          tmp_zip <- tempfile(fileext = ".zip")
          # --- ZIP ONLY THE CONTENTS OF THE FOLDER (NO NESTING!) ---
          old_wd <- setwd(out_dir)
          utils::zip(zipfile = tmp_zip, files = list.files(".", full.names = FALSE))
          setwd(old_wd)
          # copy final ZIP to Shiny download location
          file.copy(tmp_zip, file)
        }
      )
      # exclude run button
      observe({
        shinyjs::toggleState("run_catalogue", session$userData$api_available)
      })
      # Run API
      observeEvent(input$run_api, {
        cat("\n=== EVENT: run_api clicked ===\n")
        # validation
        if (!iv$is_valid()) {
          cat("Validation failed — modal not triggered\n")
          iv$enable()
          return(NULL)
        }
        cat("Validation OK\n")
        req(input$excel_file)
        # prepare request
        api_url <- "http://127.0.0.1:8000/run"
        params <- list(
          creator_name    = trimws(input$creator_name),
          creator_surname = trimws(input$creator_surname),
          creator_orcid   = trimws(input$creator_orcid)
        )
        body_list <- list(
          excel = httr::upload_file(
            input$excel_file$datapath,
            type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
          )
        )
        # debug curl
        cat("\n=== CURL SENT ===\n")
        curl_cmd <- glue::glue(
          "curl -X POST '{api_url}?creator_name={params$creator_name}&creator_surname={params$creator_surname}&creator_orcid={params$creator_orcid}' \\
-H 'accept: application/json' \\
-H 'Content-Type: multipart/form-data' \\
-F 'excel=@{input$excel_file$datapath};type=application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'"
        )
        cat(curl_cmd, "\n================\n")
        # send request
        cat("Sending POST request...\n")
        res <- httr::POST(
          url = api_url,
          query = params,
          body = body_list,
          encode = "multipart"
        )
        cat("POST completed. Status:", res$status_code, "\n")
        # parse response
        content_res <- httr::content(res)
        cat("=== API response (raw) ===\n")
        print(content_res)
        status_value <- tryCatch(as.character(content_res$status[[1]]), error = function(e) NA)
        output_dir_value <- tryCatch(as.character(content_res$output_dir[[1]]), error = function(e) NA)
        full_path_value  <- tryCatch(as.character(content_res$full_path[[1]]),  error = function(e) NA)
        cat("\nParsed values:\n")
        cat(" status_value     =", status_value, "\n")
        cat(" output_dir_value =", output_dir_value, "\n")
        cat(" full_path_value  =", full_path_value, "\n")
        # Save results
        api_results(list(
          status     = status_value,
          output_dir = output_dir_value,
          full_path  = full_path_value
        ))
        # Success modal
        if (res$status_code == 200 && identical(status_value, "success")) {
          showModal(
            modalDialog(
              title = "File for describe the specimen are created",
              size = "l",
              tags$div(
                style="color:#065f46; background:#ecfdf5; padding:10px; border-left:4px solid #059669;",
                tags$strong("The specimen catalogue has been successfully generated!")
              ),
              br(),
              downloadButton("download_zip", "Download ZIP"),
              br(), br(),
              tags$div(
                style="background:#eef6ff; padding:12px; border-left:4px solid #3b82f6;",
                tags$strong("Where should I upload these files?"),
                tags$p("The generated XML and TTL files should be uploaded to your institutional RDF repository or the eLTER-IT catalogue:"),
                tags$ul(
                  tags$li("eLTER-IT RDF triple store (Fuseki) - https://sparql.lteritalia.it")
                )
              ),
              easyClose = TRUE,
              footer = modalButton("Close")
            )
          )
          # reset input fields (only if success)
          try(iv$reset(), silent = TRUE)
          try(updateTextInput(session, "creator_name", value = ""), silent = TRUE)
          try(updateTextInput(session, "creator_surname", value = ""), silent = TRUE)
          try(updateTextInput(session, "creator_orcid", value = ""), silent = TRUE)
          try(shinyjs::reset("excel_file"), silent = TRUE)
          output$api_response <- renderPrint({invisible(NULL)})
          # error modal
        } else {
          showModal(
            modalDialog(
              title = "Error Running API",
              size = "m",
              tags$div(
                style="background:#fef2f2; padding:10px; color:#991b1b; border-left:4px solid #dc2626;",
                tags$strong("The API returned an error."),
                tags$p(content_res$message)
              ),
              easyClose = TRUE,
              footer = modalButton("Close")
            )
          )
        }
      })
      
      shinydashboard::updateTabItems(session, "tabs", default_tab)
      ## ------------------------------------------------------------------
      ## SERVER SAMPLES (Specimen)
      ## ------------------------------------------------------------------
      specimen_dataset <- "http://fuseki1.get-it.it/specimen/query"
      specimen_query <- "PREFIX sosa: <http://www.w3.org/ns/sosa/>
        PREFIX prov: <http://www.w3.org/ns/prov#>
        PREFIX dct:  <http://purl.org/dc/terms/>
        PREFIX dcat: <http://www.w3.org/ns/dcat#>
        PREFIX geosparql: <http://www.opengis.net/ont/geosparql#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT
          ?c ?label ?geom_wkt ?landing_page
          ?created ?site
          ?ror ?material ?sampler
        WHERE {
          ?c rdf:type sosa:Sample .
          OPTIONAL { ?c rdfs:label ?label . }
          OPTIONAL {
            ?c geosparql:hasGeometry ?geom .
            OPTIONAL { ?geom geosparql:asWKT ?geom_wkt . }
          }
          OPTIONAL { ?c dcat:landingPage ?landing_page . }
          OPTIONAL { ?c dct:created ?created . }
          OPTIONAL { ?c sosa:isSampleOf ?site . }
          OPTIONAL {
            ?c dcat:contactPoint ?orgContact .
            ?orgContact rdf:type foaf:Organization ;
                rdfs:seeAlso ?ror .
          }
          OPTIONAL { ?c sosa:MaterialSample ?material . }
          OPTIONAL { ?c sosa:MadeBySampler ?sampler . }
        }
        ORDER BY ?label"
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
          specimen_uri   = bindings$c$value,
          specimen_label = bindings$label$value,
          specimen_geom_raw = bindings$geom_wkt$value,
          specimen_geom = specimen_geom_raw |>
            stringr::str_remove("^<urn:ogc:def:crs:EPSG::4283>\\s*") |>
            trimws(),
          lon = stringr::str_match(specimen_geom, "POINT\\s*\\(([^ ]+)")[,2],
          lat = stringr::str_match(specimen_geom, "POINT\\s*\\([^ ]+\\s+([^\\)]+)")[,2],
          specimen_landingPage = bindings$landing_page$value,
          specimen_created     = bindings$created$value,
          specimen_site        = bindings$site$value,
          specimen_ror         = bindings$ror$value,
          specimen_material    = bindings$material$value,
          specimen_sampler     = bindings$sampler$value,
          .keep = "used"
        )
      # get labels
      rors <- unique(list_specimen$specimen_ror)
      materials <- unique(list_specimen$specimen_material)
      samplers <- unique(list_specimen$specimen_sampler)
      # ---- ROR TABLE
      rors_clean <- rors |> purrr::discard(is.na)
      if (length(rors_clean) > 0) {
        rors_tbl <- lapply(seq_along(rors_clean), function(n) {
          ror_id <- sub("https://ror.org/", "", rors_clean[n])
          export <- httr2::request(paste0("https://api.ror.org/v2/organizations/", ror_id)) |>
            httr2::req_method("GET") |>
            httr2::req_headers(Accept = "application/json") |>
            httr2::req_retry(max_tries = 3, max_seconds = 120) |>
            httr2::req_perform()
          jj <- httr2::resp_body_string(export)
          lab <- jqr::jq(jj, '{label: .names[] | select(.types[]=="label" and .lang=="en") | .value}') |>
            textConnection(encoding = "UTF-8") |>
            jsonlite::stream_in(simplifyDataFrame = TRUE, verbose = FALSE)
          
          tibble::tibble(
            specimen_ror = rors_clean[n],
            ror_label    = lab$label
          )
        }) |> dplyr::bind_rows()
      } else {
        rors_tbl <- tibble::tibble(
          specimen_ror = character(),
          ror_label    = character()
        )
      }
      # ---- MATERIAL TABLE
      materials_clean <- materials |> purrr::discard(is.na)
      if (length(materials_clean) > 0) {
        materials_tbl <- lapply(seq_along(materials_clean), function(m) {
          material_id <- sub("http://vocabulary.odm2.org/medium/", "", materials_clean[m])
          xml_skos <- xml2::read_xml(
            paste0("http://vocabulary.odm2.org/api/v1/medium/", material_id, "/?format=skos")
          )
          mat_label <- xml2::xml_find_all(xml_skos, ".//skos:prefLabel/text()") |>
            as.character()
          tibble::tibble(
            specimen_material = materials_clean[m],
            material_label    = mat_label
          )
        }) |> dplyr::bind_rows()
      } else {
        materials_tbl <- tibble::tibble(
          specimen_material = character(),
          material_label    = character()
        )
      }
      # ---- SAMPLER TABLE
      samplers_clean <- samplers |> purrr::discard(is.na)
      if (length(samplers_clean) > 0) {
        samplers_tbl <- lapply(seq_along(samplers_clean), function(x) {
          sampler_uri <- samplers_clean[x]
          sampler_query <- paste0("PREFIX sosa: <http://www.w3.org/ns/sosa/>
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
            httr2::req_retry(max_tries = 3, max_seconds = 120) |>
            httr2::req_perform()
          sampler_res <- httr2::resp_body_json(list_sampler_req, simplifyVector = TRUE)
          tibble::tibble(
            specimen_sampler = sampler_uri,
            sampler_label    = sampler_res$results$bindings$label$value
          )
        }) |> dplyr::bind_rows()
      } else {
        samplers_tbl <- tibble::tibble(
          specimen_sampler = character(),
          sampler_label    = character()
        )
      }
      # lists
      list_specimen <- merge(
        x = list_specimen,
        y = rors_tbl,
        by = "specimen_ror",
        all.x = TRUE
      )
      list_specimen <- merge(
        x = list_specimen,
        y = materials_tbl,
        by = "specimen_material",
        all.x = TRUE
      )
      list_specimen <- merge(
        x = list_specimen,
        y = samplers_tbl,
        by = "specimen_sampler",
        all.x = TRUE
      )
      collapse_no_na <- function(x) {
        x_clean <- unique(x[!is.na(x)])
        if (length(x_clean) == 0) NA else paste(x_clean, collapse = "||")
      }
      list_specimen <- list_specimen |>
        dplyr::group_by(
          specimen_uri,
          specimen_label,
          specimen_geom,
          lon, lat,
          specimen_landingPage,
          specimen_site
        ) |>
        dplyr::summarise(
          specimen_ror        = paste(unique(specimen_ror), collapse = "||"),
          ror_label           = paste(unique(ror_label), collapse = "||"),
          specimen_material   = paste(unique(specimen_material), collapse = "||"),
          specimen_sampler    = paste(unique(specimen_sampler), collapse = "||"),
          sampler_label       = paste(unique(sampler_label), collapse = "||"),
          specimen_created    = paste(unique(specimen_created), collapse = "||"),
          .groups = "drop"
        )
      list_specimen <- list_specimen |>
        dplyr::mutate(
          Institution = stringr::str_split(specimen_ror, "\\|\\|") |>
            purrr::map2(
              stringr::str_split(ror_label, "\\|\\|"),
              ~ paste0(
                purrr::map2_chr(.x, .y, function(ror, lab) {
                  paste0(
                    "<a href='", ror, "' target = '_blank'>",
                    "<img src='https://raw.githubusercontent.com/ror-community/ror-logos/main/ror-icon-rgb.svg' ",
                    "alt='ROR' style='height:20px; vertical-align:middle; margin-right:6px;'>",
                    lab,
                    "</a>"
                  )
                }),
                collapse = "<br>"
              )
            ) |> unlist()
        )
      # data shared
      shared_specimenData <- list_specimen |>
        dplyr::mutate(
          `Specimen name` = paste0(
            "<a href='", specimen_landingPage, "' target='_blank'>",
            specimen_label, "</a>"
          ),
          Institution = purrr::pmap_chr(
            list(
              stringr::str_split(specimen_ror,  "\\|\\|"),
              stringr::str_split(ror_label,     "\\|\\|")
            ),
            function(uris, labels) {
              if (all(is.na(uris)) || all(is.na(labels))) return("-")
              if (length(labels) != length(uris)) {
                labels <- rep(labels, length.out = length(uris))
              }
              paste0(
                purrr::map2_chr(uris, labels, ~{
                  paste0(
                    "<a href='", .x, "' target='_blank'>",
                    "<img src='https://raw.githubusercontent.com/ror-community/ror-logos/main/ror-icon-rgb.svg' ",
                    "alt='ROR' style='height:18px;vertical-align:middle;margin-right:5px;'>",
                    .y,
                    "</a>"
                  )
                }),
                collapse = "<br>"
              )
            }
          ),
          `Specimen material` = purrr::pmap_chr(
            list(stringr::str_split(specimen_material, "\\|\\|")),
            function(materials) {
              paste0(
                purrr::map_chr(materials, function(uri) {
                  id <- sub("http://vocabulary.odm2.org/medium/", "", uri)
                  json_url <- paste0(
                    "http://vocabulary.odm2.org/api/v1/medium/",
                    id,
                    "/?format=json"
                  )
                  jj <- tryCatch(
                    httr2::request(json_url) |>
                      httr2::req_perform() |>
                      httr2::resp_body_string(),
                    error = function(e) NA
                  )
                  label <- tryCatch(
                    jsonlite::fromJSON(jj)$name,
                    error = function(e) NA
                  )
                  if (is.na(label)) label <- basename(uri)
                  paste0("<a href='", uri, "' target='_blank'>", label, "</a>")
                }),
                collapse = "<br>"
              )
            }
          ),
          `Sampled at site` = purrr::pmap_chr(
            list(stringr::str_split(specimen_site, "\\|\\|")),
            function(site) {
              paste0(
                purrr::map_chr(site, function(site_uri) {
                  site_id <- sub("https://deims.org/", "", site_uri)
                  json_url <- paste0(
                    "https://deims.org/api/sites/",
                    site_id
                  )
                  jj <- tryCatch(
                    httr2::request(json_url) |>
                      httr2::req_perform() |>
                      httr2::resp_body_string(),
                    error = function(e) NA
                  )
                  site_label <- tryCatch(
                    jsonlite::fromJSON(jj)$title,
                    error = function(e) NA
                  )
                  if (is.na(site_label)) site_label <- basename(uri)
                  paste0("<a href='", site_uri, "' target='_blank'>", site_label, "</a>")
                }),
                collapse = "<br>"
              )
            }
          ),
          Created = specimen_created,
          Sampler = purrr::pmap_chr(
            list(
              stringr::str_split(specimen_sampler, "\\|\\|"),
              stringr::str_split(sampler_label, "\\|\\|")
            ),
            function(uris, labs) {
              if (length(labs) != length(uris)) {
                labs <- rep(labs, length.out = length(uris))
              }
              paste0(
                purrr::map2_chr(uris, labs, ~ {
                  paste0("<a href='", .x, "' target='_blank'>", .y, "</a>")
                }),
                collapse = "<br>"
              )
            }
          ),
          Location = ifelse(
            !is.na(lon) & !is.na(lat),
            paste0(
              "<a href='https://www.openstreetmap.org/?mlat=", lat,
              "&mlon=", lon, "#map=14/", lat, "/", lon,
              "' target='_blank'>POINT (", lon, " ", lat, ")</a>"
            ),
            "-"
          ),
          lon = suppressWarnings(as.numeric(lon)),
          lat = suppressWarnings(as.numeric(lat)),
          geom = specimen_geom,
          .keep = "used"
        ) |>
        dplyr::select(
          `Specimen name`,
          Created,
          `Sampled at site`,
          Institution,
          `Specimen material`,
          Sampler,
          Location,
          geom
        ) |>
        sf::st_as_sf(wkt = "geom")
      # data shared
      shared_data <- crosstalk::SharedData$new(
        shared_specimenData,
        # key = ~specimen_uri,
        group = "specimens"
      )
      output$specimenTbl <- DT::renderDT({
        table_data <- shared_specimenData |>
          sf::st_drop_geometry() |>
          dplyr::select(
            `Specimen name`,
            Created,
            `Sampled at site`,
            Institution,
            `Specimen material`,
            Sampler,
            Location
          )
        DT::datatable(
          crosstalk::SharedData$new(table_data, group = "specimens"),
          escape = FALSE,
          filter = 'top',
          options = list(pageLength = 10),
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom;text-align:center;',
            'Table - ', htmltools::em('Samples collected by eLTER-IT network')
          )
        )
      }, server = FALSE)
      # map output
      output$specimen_map <- leaflet::renderLeaflet({
        leaflet::leaflet(shared_data) |>
          leaflet::addProviderTiles(
            "CartoDB.Positron",
            options = leaflet::providerTileOptions(opacity = 0.99)) |>
          leaflet::addMarkers(
            popup = ~paste0(
              "<b>Specimen name: </b>", `Specimen name`, "<br>",
              "<b>Created: </b>", Created, "<br>",
              "<b>Sampled at site: </b>", `Sampled at site`, "<br>",
              "<b>Institution: </b>",    Institution, "<br>",
              "<b>Material: </b>",       `Specimen material`, "<br>",
              "<b>Sampler: </b>",        Sampler, "<br>",
              "<b>Location: </b>",        Location
            )
          )
      })
      
      ## ------------------------------------------------------------------
      ## SERVER SENSORS (Systems Type)
      ## ------------------------------------------------------------------
      # data table
      sensorsType_dataset <- "http://fuseki1.get-it.it/systemsType/query"
      systemsType_query <- paste0(
        "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
         PREFIX dcmitype: <http://purl.org/dc/dcmitype/>
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
         PREFIX sosa: <http://www.w3.org/ns/sosa/>
         PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         PREFIX prov: <http://www.w3.org/ns/prov#>
         PREFIX ssn: <http://www.w3.org/ns/ssn/>
         PREFIX dcat: <http://www.w3.org/ns/dcat#>
         PREFIX foaf: <http://xmlns.com/foaf/0.1/>
         SELECT ?c ?label ?doc ?image ?descr ?hosts ?comp_label ?man_name ?man_uri
         WHERE {
           ?c rdf:type sosa:System .
           OPTIONAL { ?c rdfs:label ?label . }
           OPTIONAL { ?c prov:wasDerivedFrom ?doc .}
           OPTIONAL { ?c dcat:contactPoint ?contact .}
           OPTIONAL { ?c dcmitype:Image ?image . }
           OPTIONAL { ?c rdfs:comment ?descr . }
           OPTIONAL { ?c sosa:hasSubSystem ?hosts . 
               OPTIONAL { ?hosts rdfs:label ?comp_label .}
           }
           OPTIONAL {
             ?contact foaf:name ?man_name .
             ?contact rdfs:seeAlso ?man_uri .
           }
         }
         ORDER BY ASC(?l)"
      )
      list_systemsType_req <- httr2::request(sensorsType_dataset) %>%
        httr2::req_url_query(query = systemsType_query) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(Accept = "application/sparql-results+json") %>%
        httr2::req_user_agent("ReLTER dev") %>%
        httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
        httr2::req_perform()
      httr2::resp_check_status(list_systemsType_req)
      list_systemType <- httr2::resp_body_json(list_systemsType_req, simplifyVector = TRUE) %>%
        purrr::pluck("results") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          system_uri = bindings$c$value,
          system_label = bindings$label$value,
          system_description = bindings$descr$value,
          system_doc = bindings$doc$value,
          system_image = bindings$image$value,
          component_uri = bindings$hosts$value,
          component_label = bindings$comp_label$value,
          manufacturer_name = bindings$man_name$value,
          manufacturer_uri = bindings$man_uri$value,
          .keep = "used"
        ) %>%
        dplyr::select(system_uri, system_label, system_description, system_doc, system_image, component_uri, component_label, manufacturer_name, manufacturer_uri)
      # data table output
      output$sensorTbl <- DT::renderDataTable({
        dfSensorsType <- list_systemType %>%
          dplyr::filter(!is.na(manufacturer_name) & manufacturer_name != "") %>%
          dplyr::mutate(
            rdfURL_html = paste0(
              "<a href='",
              system_uri,
              "' target = '_blank'>", system_label, "</a>"
            ),
            doc = paste0(
              "<a href='",
              system_doc,
              "' target = '_blank'>",
              as.character(icon("file", lib = "glyphicon")),
              " click to read the documentation</a>"
            ),
            thumb_html = paste0(
              "<a href='",
              system_image,
              "' target = '_blank'>",
              "<img src='",
              system_image,
              "' height='52'/></a>"
            ),
            compURL_html = paste0(
              "<a href='",
              component_uri,
              "' target = '_blank'>", component_label, "</a>"
            ),
            man_html = paste0(
              "<a href='",
              manufacturer_uri,
              "' target = '_blank'>", manufacturer_name, "</a>"
            ),
            .keep = "used"
          ) %>%
          unique() %>%
          dplyr::select(system_uri, rdfURL_html, doc, thumb_html, compURL_html, man_html) %>%
          dplyr::group_by(system_uri, rdfURL_html, doc, thumb_html, man_html) %>%
          dplyr::summarize(compURL_html = stringr::str_c(compURL_html, collapse = "<br> "),
                           .groups = "drop") %>%
          dplyr::mutate(
            your_sensor = paste0(
              "<a href='",
              system_uri,
              "' target='_blank' data-toggle='tooltip' title='Use this UUID to reference this system when creating your sensor instance'>",
              system_uri,
              "</a>"
            )
          ) %>%
          dplyr::select(
            `System name` = rdfURL_html,
            Documentation = doc,
            Image = thumb_html,
            `System components` = compURL_html,
            Manufacturer = man_html,
            UUID = your_sensor
          )
        DT::datatable(
          dfSensorsType,
          escape = FALSE,
          caption = htmltools::tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            'Table - ', htmltools::em(paste0(
              'List of sensor type models with metadata and links.'
            ))
          ),
          filter = 'top'
        )
      })
    }
  )
}
