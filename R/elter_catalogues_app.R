#' Launch the Unified eLTER-IT Shiny Catalogues Application
#' @description `r lifecycle::badge("stable")`
#' This function launches the unified Shiny application that includes:
#' \itemize{
#'   \item the **Samples Catalogue**, and
#'   \item the **Sensor Type Catalogue**.
#' }
#' @param default_tab Character string indicating which tab should be
#'   activated when the application starts.
#'   Allowed values: \code{"samples"}, \code{"sensors"}.
#' @return No return value. The function launches the Shiny application.
#' @importFrom shiny shinyApp reactiveVal observeEvent observe reactive req
#' @importFrom shiny renderUI renderText renderTable isolate validate need withProgress
#' @importFrom shiny incProgress showNotification updateTextInput updateSelectInput
#' @importFrom shiny fluidRow column tags tagList icon actionButton textInput passwordInput
#' @importFrom shiny selectInput numericInput dateInput radioButtons checkboxInput
#' @importFrom shiny conditionalPanel fileInput verbatimTextOutput textOutput tableOutput
#' @importFrom shiny uiOutput downloadButton
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shinydashboard dashboardBody dashboardSidebar sidebarMenu menuItem tabItems tabItem
#' @importFrom shinydashboard box valueBox valueBoxOutput renderValueBox dashboardSidebar updateTabItems
#' @importFrom shinyjs useShinyjs hidden show hide toggleState
#' @importFrom shinyFiles shinyDirChoose shinyDirButton parseDirPath getVolumes
#' @importFrom shinyTime timeInput
#' @importFrom shinyvalidate InputValidator sv_required sv_regex
#' @importFrom DT datatable renderDataTable renderDT DTOutput dataTableOutput
#' @importFrom leaflet leaflet renderLeaflet leafletOutput addProviderTiles
#' @importFrom leaflet providerTileOptions addMarkers
#' @importFrom crosstalk SharedData
#' @importFrom httr2 request req_method req_headers req_body_form req_perform
#' @importFrom httr2 req_timeout req_retry req_url_query req_auth_basic req_body_raw
#' @importFrom httr2 resp_body_string resp_body_json resp_check_status resp_status
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr mutate select filter group_by summarize n_distinct
#' @importFrom dplyr distinct bind_rows coalesce
#' @importFrom tibble as_tibble
#' @importFrom purrr pluck map_chr map2_chr pmap_chr discard
#' @importFrom stringr str_remove str_match str_sub str_split str_detect
#' @importFrom stringr str_c
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom htmltools tags
#' @importFrom stats setNames
#' @importFrom lifecycle badge
#' @examples
#' if (interactive()) {
#'   elter_catalogues_app(default_tab = "samples")
#' }
#' @author
#' Alessandro Oggioni, PhD (2023–2025) \email{alesssandro.oggioni@cnr.it}
#' @export
elter_catalogues_app <- function(default_tab = c("samples", "sensors")) {
  default_tab <- match.arg(default_tab)
  
  `%||%` <- function(a, b) {
    if (is.null(a) || length(a) == 0) return(b)
    a1 <- a[[1]]  # primo elemento
    if (is.na(a1)) return(b)
    if (is.character(a1) && !nzchar(a1)) return(b)
    a1
  }
  
  fetch_sensor_types_from_fuseki <- function(endpoint) {
    if (!requireNamespace("httr2", quietly = TRUE)) stop("Install httr2.")
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite.")
    if (!nzchar(endpoint)) stop("Missing SPARQL endpoint.")
    
    query <- '
PREFIX sosa: <http://www.w3.org/ns/sosa/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?uri ?label WHERE {
  ?uri a sosa:System .
  OPTIONAL { ?uri rdfs:label ?label . FILTER(lang(?label) = "en") }
}
'
  
  resp <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Accept" = "application/sparql-results+json",
      "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8"
    ) |>
    httr2::req_body_form(query = query) |>
    httr2::req_perform()
  
  txt <- httr2::resp_body_string(resp)
  js  <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  
  b <- js$results$bindings
  if (is.null(b) || NROW(b) == 0) {
    return(data.frame(uri = character(), label = character(), stringsAsFactors = FALSE))
  }
  
  out <- data.frame(
    uri   = b$uri$value,
    label = if (!is.null(b$label$value)) b$label$value else NA_character_,
    stringsAsFactors = FALSE
  )
  
  # label fallback
  missing_lab <- is.na(out$label) | out$label == ""
  out$label[missing_lab] <- basename(out$uri[missing_lab])
  
  out <- out[!duplicated(out$uri), ]
  out <- out[order(tolower(out$label)), ]
  out
  }

  fetch_sparql_count <- function(endpoint, query, varname) {
    if (!requireNamespace("httr2", quietly = TRUE)) stop("Install httr2")
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite")
    
    resp <- httr2::request(endpoint) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Accept" = "application/sparql-results+json",
        "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8"
      ) |>
      httr2::req_body_form(query = query) |>
      httr2::req_timeout(20) |>
      httr2::req_perform()
    
    js <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
    
    val <- js$results$bindings[[varname]]$value
    as.integer(val %||% 0)
  }

  make_login_box <- function(pwd_id, btn_id) {
    shinydashboard::box(
      width = 12,
      title = "Protected area",
      status = "warning",
      solidHeader = TRUE,
      collapsible = FALSE,
      shiny::tags$div(
        class = "alert alert-info",
        shiny::tags$b("Info: "),
        "This tool is restricted. Please login to continue."
      ),
      shiny::passwordInput(pwd_id, "Password"),
      shiny::actionButton(btn_id, "Unlock")
    )
  }

shiny::shinyApp(
  ui = shinydashboard::dashboardPage(
    shinyjs::useShinyjs(),
    
    header = shinydashboard::dashboardHeader(
      title = shiny::tagList(
        shiny::tags$head(
          shiny::tags$title("eLTER-IT catalogues"),
          shiny::tags$link(
            rel  = "icon",
            type = "image/png",
            href = "https://www.lteritalia.it/wp-content/uploads/2023/09/solo_foglia.png"
          ),
          shiny::tags$style(shiny::HTML("
              .navbar { background-color: #334155 !important; }
              .column { float: left; width: 50%; }
              .row:after { content: ''; display: table; clear: both; }
              .sidebar { padding-bottom: 70px; }
              .elter-counter {
                display: inline-flex;
                align-items: center;
                gap: 6px;
                padding: 6px 10px;
                border-radius: 999px;
                background: rgba(255,255,255,0.12);
                color: #fff;
                margin-right: 8px;
                font-size: 13px;
                opacity: .85;
                font-weight:600;
                border-radius:999px;
                padding:2px 10px;
                cursor: pointer;
              }
              .elter-counter-title {
                color:white;
                font-size:14px;
                font-weight:600;
                white-space:nowrap;
              }
              .elter-counter-link {
                text-decoration: none;
                color: inherit;
              }
              .elter-counter-link:hover {
                text-decoration: none;
                color: inherit;
              }
            "))
        ),
        shiny::tags$span(class = "logo-lg", "eLTER-IT catalogues")
      ),
      shiny::tags$li(
        class = "dropdown",
        shiny::uiOutput("header_counters"),
        style = "margin:0; padding-top:10px; padding-right:10px;"
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
      collapsed = FALSE,
      shinydashboard::sidebarMenu(
        id = "tabs",
        shiny::tags$li(class = "header", "PUBLIC"),
        shinydashboard::menuItem(
          "Samples catalogue",
          tabName = "samples",
          icon = shiny::icon("map-marked-alt", lib = "font-awesome")
        ),
        shinydashboard::menuItem(
          "Sensors type catalogue",
          tabName = "sensors",
          icon = shiny::icon("thermometer-half", lib = "font-awesome")
        ),
        shinydashboard::menuItem(
          "Sensors instance catalogue",
          tabName = "sensors_instance",
          icon = shiny::icon("thermometer", lib = "font-awesome")
        ),
        # RESTRICTED
        shiny::tags$li(class = "header", "RESTRICTED"),
        shinydashboard::menuItem(
          shiny::tagList(
            "Sensors - Create new instance",
            shiny::tags$br(),
            shiny::tags$small("(restricted)")
          ),
          tabName = "sensor_instance_ttl",
          icon = shiny::icon("lock", lib = "font-awesome")
        ),
        shinydashboard::menuItem(
          shiny::tagList(
            "Sensors - Add DOI to instance",
            shiny::tags$br(),
            shiny::tags$small("(restricted)")
          ),
          tabName = "sensor_instance_doi",
          icon = shiny::icon("lock", lib = "font-awesome")
        ),
        shinydashboard::menuItem(
          shiny::tagList(
            "Sensors - Add new manufacturer",
            shiny::tags$br(),
            shiny::tags$small("(restricted)")
          ),
          tabName = "manufacturer",
          icon = shiny::icon("lock", lib = "font-awesome")
        ),
        shinydashboard::menuItem(
          shiny::tagList(
            "Sensors - Acquisition",
            shiny::tags$br(),
            shiny::tags$small("(restricted)"),
          ),
          tabName = "sensor_acquisition",
          icon = shiny::icon("lock", lib = "font-awesome")
        ),
        shinydashboard::menuItem(
          shiny::tagList(
            "Sensors - Deploy",
            shiny::tags$br(),
            shiny::tags$small("(restricted)"),
          ),
          tabName = "sensor_deploy",
          icon = shiny::icon("lock", lib = "font-awesome")
        ),
        shinydashboard::menuItem(
          shiny::tagList(
            "Sensors - Calibration",
            shiny::tags$br(),
            shiny::tags$small("(restricted)"),
          ),
          tabName = "sensor_calibration",
          icon = shiny::icon("lock", lib = "font-awesome")
        )
      ),
      
      # ✅ SINGLE logout button (only here)
      shiny::tags$div(
        id = "sidebar_logout",
        style = "display:none; padding: 12px; position: absolute; bottom: 0; width: 100%;",
        shiny::actionButton(
          "logout_global",
          "Logout",
          icon = shiny::icon("sign-out-alt"),
          class = "btn btn-danger btn-block"
        )
      )
    ),
    
    body = shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      
      shinydashboard::tabItems(
        # ------------ TAB SAMPLES
        shinydashboard::tabItem(
          tabName = "samples",
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("vb_samples", width = 3),
            shinydashboard::valueBoxOutput("vb_samplers", width = 3)
          ),
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
        
        # ------------ TAB SENSORS
        shinydashboard::tabItem(
          tabName = "sensors",
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("vb_sensor_types", width = 3),
            shinydashboard::valueBoxOutput("vb_manufacturers", width = 3)
          ),
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
        ),
        
        # ------------ TAB SENSORS INSTANCE
        shinydashboard::tabItem(
          tabName = "sensors_instance",
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("vb_sensor_instances", width = 3),
            shinydashboard::valueBoxOutput("vb_instance_owners",  width = 3)
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width       = 12,
              title       = "Catalogue of eLTER-IT sensor instances",
              closable    = FALSE,
              status      = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              shiny::column(12, DT::DTOutput("instanceTbl"))
            )
          )
        ),
        
        # ------------ TAB SENSOR manufacturer (restricted)
        shinydashboard::tabItem(
          tabName = "manufacturer",
          
          # login box (shown only when NOT authed)
          shiny::uiOutput("login_ui_man"),
          
          # protected content (shown only when authed)
          shinyjs::hidden(
            shiny::tags$div(
              id = "man_app",
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shinydashboard::box(
                    width = 12,
                    title = "Add new manufacturer",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    shiny::textInput("manName", "Manufacturer name", placeholder = "PincoPallo Acoustic Devices"),
                    shiny::textInput("manTel", "Phone number", placeholder = "+39270000"),
                    shiny::textInput("manAdd", "Address", placeholder = "Castello Square"),
                    shiny::textInput("manNumber", "Address number", placeholder = "1"),
                    shiny::textInput("manCity", "City", placeholder = "Milano"),
                    shiny::textInput("manAdm", "Administrative area", placeholder = "Lombardia"),
                    shiny::textInput("manPostCode", "Postal code", placeholder = "20100"),
                    shiny::textInput("manCountry", "Country", placeholder = "Italy"),
                    shiny::textInput("manEMail", "E-mail address", placeholder = "info@picopalloacoustic.devices"),
                    shiny::textInput("manWebSite", "Web site", placeholder = "https://pincopalloacoustic.devices/"),
                    
                    shiny::tags$hr(),
                    shiny::actionButton("man_send", "Add new manufacturer", class = "btn-success")
                  )
                ),
                shiny::column(
                  width = 8,
                  shinydashboard::box(
                    width = 12,
                    title = "SPARQL preview",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    shiny::verbatimTextOutput("man_sparql")
                  ),
                  shinydashboard::box(
                    width = 12,
                    title = "Result",
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    shiny::verbatimTextOutput("man_result")
                  )
                )
              )
            )
          )
        ),
        
        # ------------ TAB SENSOR INSTANCE TTL (restricted)
        shinydashboard::tabItem(
          tabName = "sensor_instance_ttl",
          shiny::uiOutput("login_ui_ttl"),
          shinyjs::hidden(
            shiny::tags$div(
              id = "ttl_app",
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shinydashboard::box(
                    width = 12,
                    title = "Create Sensor Instance TTL file",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    shiny::selectInput("ttl_sensor_type_uri", "Select sensor type", choices = c("Loading..." = "")),
                    shiny::actionButton("ttl_refresh_types", "Refresh sensors type", class = "btn-xs"),
                    
                    shiny::tags$hr(),
                    shiny::tags$p(
                      class = "text-muted",
                      "Sensor Instance owner details."
                    ),
                    shiny::textInput("ttl_owner_name", "Owner givenName", placeholder = "Alessandro"),
                    shiny::textInput("ttl_owner_surname", "Owner familyName", placeholder = "Oggioni"),
                    shiny::textInput("ttl_owner_orcid", "Owner ORCID", placeholder = "0000-0002-7997-219X"),
                    shiny::textInput("ttl_serial_number", "Serial number", placeholder = "4CE0460D0G"),
                    shiny::textInput("ttl_part_number", "Part number", placeholder = "37SI.11200"),
                    # shiny::checkboxInput("ttl_do_validate", "Validate TTL after creation", placeholder = TRUE),
                    shiny::textInput("ttl_output_dir", "Output directory", value = getwd()),
                    shinyFiles::shinyDirButton("ttl_browse_dir", "Browse...", "Select a folder", icon = shiny::icon("folder-open")),
                    shiny::actionButton("ttl_generate", "Generate TTL", class = "btn-primary")
                  )
                ),
                shiny::column(
                  width = 8,
                  shinydashboard::box(width = 12, title = "Status", status = "info", solidHeader = TRUE,
                                      shiny::verbatimTextOutput("ttl_status")),
                  shinydashboard::box(width = 12, title = "Generated file", status = "info", solidHeader = TRUE,
                                      shiny::tableOutput("ttl_meta_tbl")),
                  # shinydashboard::box(width = 12, title = "Validation checks", status = "info", solidHeader = TRUE,
                  #                     shiny::tableOutput("ttl_checks_tbl")),
                  shinydashboard::box(width = 12, title = "TTL preview", status = "info", solidHeader = TRUE,
                                      shiny::tags$pre(style = "white-space: pre-wrap;", shiny::textOutput("ttl_preview")))
                )
              )
            )
          )
        ),
        
        # ------------ TAB SENSOR INSTANCE DOI (restricted)
        shinydashboard::tabItem(
          tabName = "sensor_instance_doi",
          shiny::uiOutput("login_ui_doi"),
          shinyjs::hidden(
            shiny::tags$div(
              id = "doi_app",
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shinydashboard::box(
                    width = 12,
                    title = "Create DataCite/Fabrica DOI record from Sensor instance TTL file",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    shiny::fileInput(
                      "doi_ttl",
                      shiny::tagList(shiny::icon("file-code"), "Sensor Instance file (.ttl) — generated in the previous step"),
                      accept = ".ttl"
                    ),
                    shiny::tags$small(
                      class = "text-muted",
                      style = "margin-top:-8px; display:block;",
                      "Example: sensor_instance_20260114_....ttl"
                    ),
                    
                    shinyjs::hidden(shiny::selectInput("doi_env", "Environment", choices = c("test", "prod"), selected = "prod")),
                    shinyjs::hidden(shiny::textInput("doi_prefix", "DOI Prefix", value = "10.82159")),
                    shinyjs::hidden(shiny::textInput("doi_publisher", "Publisher", value = "LTER-Italy")),
                    shinyjs::hidden(shiny::textInput("doi_publisherId", "Publisher Identifier (ROR URI)", value = "https://ror.org/05ma8mw15")),
                    shinyjs::hidden(shiny::selectInput("doi_state", "State", choices = c("draft", "register", "publish"), selected = "draft")),
                    shinyjs::hidden(shiny::selectInput("doi_method", "HTTP Method", choices = c("POST", "PUT"), selected = "POST")),
                    shinyjs::hidden(shiny::textInput("doi_url_override", "Landing page URL override (optional)", value = "")),
                    shinyjs::hidden(shiny::textInput("doi_existing_doi", "Existing DOI to update (required for PUT)", value = "")),
                    shiny::tags$hr(),
                    shiny::actionButton("doi_run", "Upload", class = "btn-primary")#,
                    # shiny::checkboxInput("doi_show_response_raw", "Show full response JSON", value = TRUE)
                  )
                ),
                shiny::column(
                  width = 8,
                  shinydashboard::box(width = 12, title = "Status", status = "info", solidHeader = TRUE,
                                      shiny::verbatimTextOutput("doi_status")),
                  shinydashboard::box(width = 12, title = "Extracted subject URI / identifiers", status = "info", solidHeader = TRUE,
                                      shiny::tableOutput("doi_summary_tbl")),
                  shinydashboard::box(width = 12, title = "Payload (JSON:API)", status = "info", solidHeader = TRUE,
                                      shiny::verbatimTextOutput("doi_payload_json")),
                  shinydashboard::box(width = 12, title = "DataCite response", status = "info", solidHeader = TRUE,
                                      shiny::verbatimTextOutput("doi_resp_out"))
                )
              )
            )
          )
        ),
        # ------------ TAB SENSOR acquisition (restricted)
        shinydashboard::tabItem(
          tabName = "sensor_acquisition",
          shiny::uiOutput("login_ui_acq"),
          shinyjs::hidden(
            shiny::tags$div(
              id = "acq_app",
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shinydashboard::box(
                    width = 12, title = "Create Acquisition TTL file",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    
                    # --- Sensor instance URI
                    shiny::tags$p(class = "text-muted", "Sensor instance URI."),
                    shiny::selectInput("acq_sensor_uri", "Select sensor instance", choices = c("Loading..." = "")),
                    shiny::actionButton("acq_refresh_instances", "Refresh instances", class = "btn-xs"),
                    # shiny::tags$small(class = "text-muted", "Or type manually:"),
                    # shiny::textInput("acq_sensor_uri_manual", NULL, placeholder = "https://rdfdata.lteritalia.it/sensors/instance/UUID-sensor-01"),
                    
                    shiny::tags$hr(),
                    # --- Activity label
                    shiny::textInput("acq_label", "Activity label (en)",
                                     placeholder = "Purchase of sensor UUID-sensor-01"),
                    
                    # --- Date
                    shiny::tags$hr(),
                    shiny::tags$p(class = "text-muted", "Acquisition date."),
                    
                    shiny::radioButtons(
                      "acq_date_precision",
                      "Date precision",
                      choices  = c("Year only" = "year", "Full date (YYYY-MM-DD)" = "full"),
                      selected = "year",
                      inline   = TRUE
                    ),
                    
                    # mostrato solo se "year"
                    shiny::conditionalPanel(
                      condition = "input.acq_date_precision == 'year'",
                      shiny::numericInput(
                        "acq_year", "Year of acquisition",
                        value = as.integer(format(Sys.Date(), "%Y")),
                        min = 1900, max = 2100, step = 1
                      )
                    ),
                    
                    # mostrato solo se "full"
                    shiny::conditionalPanel(
                      condition = "input.acq_date_precision == 'full'",
                      shiny::dateInput(
                        "acq_date_full", "Date of acquisition",
                        value = Sys.Date()
                      )
                    ),
                    
                    shiny::tags$hr(),
                    
                    # Numero d'ordine (opzionale)
                    shiny::textInput(
                      "acq_po_number",
                      "Purchase order / contract number (optional)",
                      placeholder = "PO-2026-0042"
                    ),
                    
                    shiny::tags$hr(),
                    # --- Agent
                    shiny::tags$p(class = "text-muted", "Agent (project/funder/buyer)."),
                    shiny::selectInput("acq_agent_uri", "Select known agent", choices = c("Loading..." = "")),
                    shiny::actionButton("acq_refresh_agents", "Refresh agents", class = "btn-xs"),
                    shiny::tags$small(class = "text-muted", "Or type manually:"),
                    shiny::textInput("acq_agent_uri_manual", NULL,
                                     placeholder = "https://www.itineris.cnr.it/"),
                    
                    shiny::tags$hr(),
                    shiny::textInput("acq_output_dir", "Output directory", value = getwd()),
                    shinyFiles::shinyDirButton("acq_browse_dir", "Browse...", "Select a folder",
                                               icon = shiny::icon("folder-open")),
                    shiny::actionButton("acq_generate", "Generate TTL", class = "btn-primary")
                  )
                ),
                shiny::column(
                  width = 8,
                  shinydashboard::box(width = 12, title = "Status", status = "info",
                                      solidHeader = TRUE,
                                      shiny::verbatimTextOutput("acq_status")),
                  shinydashboard::box(width = 12, title = "Generated file", status = "info",
                                      solidHeader = TRUE,
                                      shiny::tableOutput("acq_meta_tbl")),
                  shinydashboard::box(width = 12, title = "TTL preview", status = "info",
                                      solidHeader = TRUE,
                                      shiny::tags$pre(style = "white-space: pre-wrap;",
                                                      shiny::textOutput("acq_preview")))
                )
              )
            )
          )
        ),
        
        # ------------ TAB SENSOR deploy (restricted)
        shinydashboard::tabItem(
          tabName = "sensor_deploy",
          shiny::uiOutput("login_ui_dep"),
          shinyjs::hidden(
            shiny::tags$div(
              id = "dep_app",
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shinydashboard::box(
                    width = 12, title = "Create Deployment TTL file",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    
                    shiny::tags$p(class = "text-muted", "Sensor instance URI."),
                    shiny::selectInput("dep_sensor_uri", "Select sensor instance", choices = c("Loading..." = "")),
                    shiny::actionButton("dep_refresh_instances", "Refresh instances", class = "btn-xs"),
                    # shiny::tags$small(class = "text-muted", "Or type manually:"),
                    # shiny::textInput("dep_sensor_uri_manual", NULL,
                    #                  placeholder = "https://rdfdata.lteritalia.it/sensors/instance/UUID-sensor-01"),
                    
                    shiny::tags$hr(),
                    shiny::textInput("dep_label", "Deployment label (en)",
                                     placeholder = "Deployment of sensor UUID-sensor-01 on platform E1"),
                    shiny::textInput("dep_comment", "Comment (en)",
                                     placeholder = "Sensor installed at 2 m above ground level."),
                    
                    # --- DateTime
                    shiny::dateInput("dep_date", "Deployment date", value = Sys.Date()),
                    shinyTime::timeInput(
                      "dep_time",
                      "Deployment time (UTC)",
                      value = strptime("00:00:00", "%T"),   # default mezzanotte
                      seconds = TRUE                         # mostra anche i secondi
                    ),
                    
                    shiny::tags$hr(),
                    # --- Platform
                    shiny::tags$p(class = "text-muted", "Deployment platform URI."),
                    shiny::selectInput("dep_platform_uri", "Select known platform", choices = c("Loading..." = "")),
                    shiny::actionButton("dep_refresh_platforms", "Refresh platforms", class = "btn-xs"),
                    shiny::tags$small(class = "text-muted", "Or type manually:"),
                    shiny::textInput("dep_platform_uri_manual", NULL,
                                     placeholder = "https://rdfdata.lteritalia.it/sensors/platform/E1/level/AMSL"),
                    
                    shiny::tags$hr(),
                    shiny::textInput("dep_output_dir", "Output directory", value = getwd()),
                    shinyFiles::shinyDirButton("dep_browse_dir", "Browse...", "Select a folder",
                                               icon = shiny::icon("folder-open")),
                    shiny::actionButton("dep_generate", "Generate TTL", class = "btn-primary")
                  )
                ),
                shiny::column(
                  width = 8,
                  shinydashboard::box(width = 12, title = "Status", status = "info",
                                      solidHeader = TRUE,
                                      shiny::verbatimTextOutput("dep_status")),
                  shinydashboard::box(width = 12, title = "Generated file", status = "info",
                                      solidHeader = TRUE,
                                      shiny::tableOutput("dep_meta_tbl")),
                  shinydashboard::box(width = 12, title = "TTL preview", status = "info",
                                      solidHeader = TRUE,
                                      shiny::tags$pre(style = "white-space: pre-wrap;",
                                                      shiny::textOutput("dep_preview")))
                )
              )
            )
          )
        ),
        
        # ------------ TAB SENSOR calibration (restricted)
        shinydashboard::tabItem(
          tabName = "sensor_calibration",
          shiny::uiOutput("login_ui_cal"),
          shinyjs::hidden(
            shiny::tags$div(
              id = "cal_app",
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shinydashboard::box(
                    width = 12, title = "Create Calibration TTL file",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    
                    shiny::tags$p(class = "text-muted", "Sensor instance URI."),
                    shiny::selectInput("cal_sensor_uri", "Select sensor instance", choices = c("Loading..." = "")),
                    shiny::actionButton("cal_refresh_instances", "Refresh instances", class = "btn-xs"),
                    # shiny::tags$small(class = "text-muted", "Or type manually:"),
                    # shiny::textInput("cal_sensor_uri_manual", NULL,
                    #                  placeholder = "https://rdfdata.lteritalia.it/sensors/instance/UUID-sensor-01"),
                    
                    shiny::tags$hr(),
                    shiny::textInput("cal_label", "Calibration label (en)",
                                     placeholder = "Factory calibration of sensor UUID-sensor-01"),
                    shiny::textInput("cal_comment", "Comment (en)",
                                     placeholder = "Calibration performed by manufacturer at production time. No date available."),
                    
                    shiny::tags$hr(),
                    # --- Calibration agent (usually manufacturer)
                    shiny::tags$p(class = "text-muted", "Calibration agent (e.g. manufacturer)."),
                    shiny::selectInput("cal_agent_uri", "Select known agent", choices = c("Loading..." = "")),
                    shiny::actionButton("cal_refresh_agents", "Refresh agents", class = "btn-xs"),
                    shiny::tags$small(class = "text-muted", "Or type manually:"),
                    shiny::textInput("cal_agent_uri_manual", NULL,
                                     placeholder = "https://rdfdata.get-it.it/sensors/manufacturers/StevensWater"),
                    
                    shiny::selectInput(
                      "cal_type",
                      "Calibration type",
                      choices = c(
                        "Factory (by manufacturer at production)" = "factory",
                        "Field (on-site)"                          = "field",
                        "Laboratory"                               = "laboratory",
                        "Cross-validation (vs another sensor)"     = "cross-validation"
                      )
                    ),
                    
                    shiny::textInput(
                      "cal_certificate",
                      "Calibration certificate URI (optional)",
                      placeholder = "https://docs.example.org/calibration-cert.pdf"
                    ),
                    
                    shiny::tags$hr(),
                    # --- Optional date
                    shiny::checkboxInput("cal_has_date", "Calibration date known?", value = FALSE),
                    shiny::conditionalPanel(
                      condition = "input.cal_has_date == true",
                      shiny::dateInput("cal_date", "Calibration date", value = Sys.Date())
                    ),
                    
                    shiny::tags$hr(),
                    shiny::textInput("cal_output_dir", "Output directory", value = getwd()),
                    shinyFiles::shinyDirButton("cal_browse_dir", "Browse...", "Select a folder",
                                               icon = shiny::icon("folder-open")),
                    shiny::actionButton("cal_generate", "Generate TTL", class = "btn-primary")
                  )
                ),
                shiny::column(
                  width = 8,
                  shinydashboard::box(width = 12, title = "Status", status = "info",
                                      solidHeader = TRUE,
                                      shiny::verbatimTextOutput("cal_status")),
                  shinydashboard::box(width = 12, title = "Generated file", status = "info",
                                      solidHeader = TRUE,
                                      shiny::tableOutput("cal_meta_tbl")),
                  shinydashboard::box(width = 12, title = "TTL preview", status = "info",
                                      solidHeader = TRUE,
                                      shiny::tags$pre(style = "white-space: pre-wrap;",
                                                      shiny::textOutput("cal_preview")))
                )
              )
            )
          )
        )
      ),
      
      shiny::tags$footer(
        shiny::HTML('
          <div class="row"
               style="
                 padding-top:12px;
                 padding-left:12px;
                 padding-bottom:8px;
                 display:flex;
                 align-items:center;
                 gap:40px;
               ">
      
            <!-- Contacts column -->
            <div class="column">
              <p style="margin-bottom:0;">
                <span style="color:#94c5e5;">
                  <strong>Contacts:</strong>
                </span><br>
      
                <strong>Secretariat:</strong>
                Via Roberto Cozzi, 53 20156 Milan (Italy)<br>
      
                <strong>
                  <i class="fa fa-phone"></i> Phone:
                </strong>
                +02 66173307<br>
      
                <strong>
                  <i class="fa fa-envelope"></i> E-mail:
                </strong>
                <a href="mailto:lteritaly@gmail.com" target="_blank">
                  lteritaly@gmail.com
                </a>
              </p>
            </div>
      
            <!-- Links column -->
            <div class="column">
              <span style="color:#94c5e5;">
                <strong>Useful links:</strong>
              </span>
      
              <ul style="margin-top:5px; margin-bottom:0;">
                <li>
                  <i class="fa fa-database"></i>
                  <a href="http://sparql.lteritalia.it/" target="_blank">
                    LTER-IT SPARQL Endpoint
                  </a>
                </li>
      
                <li>
                  <i class="fa fa-globe"></i>
                  <a href="https://www.lteritalia.it" target="_blank">
                    LTER-IT
                  </a>
                </li>
      
                <li>
                  <i class="fa fa-globe-europe"></i>
                  <a href="https://elter-ri.eu" target="_blank">
                    eLTER RI
                  </a>
                </li>
              </ul>
            </div>
      
          </div>
        '),
        align = "left"
      )
    )
  ),
  
  server = function(input, output, session) {
    # =========================================================
    # AUTH (single state for all restricted tabs)
    # =========================================================
    restricted_authed <- shiny::reactiveVal(FALSE)
    n_samples_rv <- shiny::reactiveVal(NA_integer_)
    n_types_rv <- shiny::reactiveVal(NA_integer_)
    n_samplers <- shiny::reactiveVal(NA_integer_)
    n_samplers_rv <- shiny::reactiveVal(NA_integer_)
    n_manufacturers <- shiny::reactiveVal(NA_integer_)
    n_manufacturers_rv <- shiny::reactiveVal(NA_integer_)
    n_locations <- shiny::reactiveVal(NA_integer_)
    n_locations_rv <- shiny::reactiveVal(NA_integer_)
    n_sites <- shiny::reactiveVal(NA_integer_)
    n_sites_rv <- shiny::reactiveVal(NA_integer_)
    n_instances_rv <- shiny::reactiveVal(NA_integer_)
    
    observeEvent(input$go_tab, {
      shinydashboard::updateTabItems(
        session,
        inputId = "tabs",
        selected = input$go_tab
      )
    })
    
    check_password <- function(pwd) {
      expected_pwd <- Sys.getenv("SAMPLE_META_PASSWORD", unset = "")
      if (!nzchar(expected_pwd)) {
        shiny::showNotification("Server password not configured (SAMPLE_META_PASSWORD).", type = "error")
        return(FALSE)
      }
      isTRUE(identical(pwd, expected_pwd))
    }
    
    # show/hide logout button in sidebar
    shiny::observe({
      if (isTRUE(restricted_authed())) shinyjs::show("sidebar_logout") else shinyjs::hide("sidebar_logout")
    })
    
    # logout
    shiny::observeEvent(input$logout_global, {
      restricted_authed(FALSE)
      shiny::showNotification("Logged out.", type = "message")
      
      # clean password fields if present
      shiny::updateTextInput(session, "sample_pwd", value = "")
      shiny::updateTextInput(session, "ttl_pwd", value = "")
      shiny::updateTextInput(session, "doi_pwd", value = "")
      shiny::updateTextInput(session, "man_pwd", value = "")
    })
    
    # =========================================================
    # RESTRICTED TAB: Samples metadata generator
    # =========================================================
    output$sample_api_run_ui <- shiny::renderUI({
      if (!isTRUE(restricted_authed())) {
        make_login_box(pwd_id = "sample_pwd", btn_id = "sample_unlock")
      } else {
        shinydashboard::box(
          width = 12,
          title = "Specimen Metadata generator",
          closable = FALSE,
          status = "info",
          solidHeader = FALSE,
          collapsible = TRUE,
          shinydashboard::box(
            width = 12,
            title = shiny::tagList(shiny::icon("info-circle"), "Info"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            shiny::tags$p("This section is restricted because it can generate/upload metadata artefacts."),
            shiny::tags$p("Use Logout when you finish.")
          ),
          enable_sidebar = TRUE,
          shiny::fileInput(
            "excel_file",
            shiny::tagList(shiny::icon("file-excel"), "Upload sensor template Excel file (.xlsx)"),
            accept = ".xlsx"
          ),
          shiny::textInput("creator_name", "Creator name"),
          shiny::textInput("creator_surname", "Creator surname"),
          shiny::textInput("creator_orcid", "Creator ORCID"),
          shiny::actionButton("run_api", "Upload"),
          shiny::verbatimTextOutput("api_response")
        )
      }
    })
    
    shiny::observeEvent(input$sample_unlock, {
      if (check_password(input$sample_pwd)) {
        restricted_authed(TRUE)
        shiny::showNotification("Unlocked.", type = "message")
      } else {
        shiny::showNotification("Wrong password.", type = "error")
      }
    })
    
    # ========================================================
    # RESTRICTED TAB: Manufacturer
    # ========================================================
    shiny::observeEvent(TRUE, {
      
      
      # ---- Manufacturers ----
      manufacturers_query <- '
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT (COUNT(DISTINCT ?m) AS ?n_manufacturers)
        WHERE { ?m a foaf:Organization . }
      '
      
      n_manufacturers(
        fetch_sparql_count(
          endpoint = "https://fuseki.lteritalia.it/manufacturers/query",
          query = manufacturers_query,
          varname = "n_manufacturers"
        )
      )
      n_manufacturers_rv(n_manufacturers())
      
      # ---- Samplers ----
      samplers_query <- '
        PREFIX sosa: <http://www.w3.org/ns/sosa/>
        SELECT (COUNT(DISTINCT ?s) AS ?n_samplers)
        WHERE { GRAPH <http://rdfdata.lteritalia.it/graph/samplers> {
        ?s a sosa:Sampler . }}
      '
      
      n_samplers(
        fetch_sparql_count(
          endpoint = "https://fuseki.lteritalia.it/specimen/query",
          query    = samplers_query,
          varname  = "n_samplers"
        )
      )
      n_samplers_rv(n_samplers())
      
      # ---- Sites ----
      sites_query <- "
        PREFIX ef: <http://www.w3.org/2015/03/inspire/ef#>
        SELECT (COUNT(DISTINCT ?s) AS ?n_sites)
        WHERE {
          ?s a ef:EnvironmentalMonitoringFacility ;
             ef:specialisedEMFType 'site'@en ;
             ef:belongsTo <https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3> .
        }
      "
      
      n_sites(
        fetch_sparql_count(
          endpoint = "http://fuseki1.get-it.it/elter/query",
          query    = sites_query,
          varname  = "n_sites"
        )
      )
      n_sites_rv(n_sites())
      
      # ---- Locations ----
      locations_query <- '
        PREFIX ef:  <http://www.w3.org/2015/03/inspire/ef#>
        SELECT (COUNT(DISTINCT ?locations) AS ?n_locations)
        WHERE {
          ?site a ef:EnvironmentalMonitoringFacility ;
                ef:belongsTo <https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3> ;
                ef:contains ?locations .
        }
      '
  
      n_locations(
        fetch_sparql_count(
          endpoint = "http://fuseki1.get-it.it/elter/query",
          query    = locations_query,
          varname  = "n_locations"
        )
      )
      n_locations_rv(n_locations())
  
      # ---- Sensor instance ----
      # notifications
      shiny::showNotification(
        paste0("Loaded ", n_samplers(), " samplers."),
        type = if (n_samplers() == 0) "warning" else "message",
        duration = 4
      )
      
      shiny::showNotification(
        paste0("Loaded ", n_manufacturers(), " manufacturers."),
        type = if (n_manufacturers() == 0) "warning" else "message",
        duration = 4
      )
      
      shiny::showNotification(
        paste0("Loaded ", dplyr::coalesce(n_locations(), 0L), " locations."),
        type = if (isTRUE(dplyr::coalesce(n_locations(), 0L) == 0L)) "warning" else "message",
        duration = 4
      )
      
      shiny::showNotification(
        paste0("Loaded ", dplyr::coalesce(n_sites(), 0L), " sites."),
        type = if (isTRUE(dplyr::coalesce(n_sites(), 0L) == 0L)) "warning" else "message",
        duration = 4
      )
    }, once = TRUE)
    
    shiny::observeEvent(input$man_unlock, {
      expected_pwd <- Sys.getenv("SAMPLE_META_PASSWORD", unset = "")
      if (!nzchar(expected_pwd)) {
        shiny::showNotification("Server password not configured (SAMPLE_META_PASSWORD).", type = "error")
        return()
      }
      if (identical(input$man_pwd, expected_pwd)) {
        restricted_authed(TRUE)
        shiny::showNotification("Unlocked.", type = "message")
      } else {
        shiny::showNotification("Wrong password.", type = "error")
      }
    })
    
    # ---- Directory chooser (TTL) ----
    volumes <- shinyFiles::getVolumes()()   # <-- IMPORTANT: double ()
    
    shinyFiles::shinyDirChoose(
      input,
      id = "ttl_browse_dir",
      roots = volumes,
      session = session
    )
    
    shiny::observeEvent(input$ttl_browse_dir, {
      req(input$ttl_browse_dir)
      
      dir <- shinyFiles::parseDirPath(volumes, input$ttl_browse_dir)
      
      # parseDirPath può tornare character(0) o vector: normalizziamo
      if (length(dir) >= 1) {
        dir1 <- as.character(dir[1])
        if (nzchar(dir1)) {
          shiny::updateTextInput(session, "ttl_output_dir", value = dir1)
        }
      }
    })
    
    # QUERIES
    endpointUpdate <- "https://fuseki.lteritalia.it/manufacturers/update"
    
    # helper: slug per URI
    make_slug <- function(x) {
      x <- trimws(x)
      x <- gsub("\\s+", "", x)
      x <- gsub("[^A-Za-z0-9_\\-]", "", x)
      if (!nzchar(x)) "manufacturer" else x
    }
    
    # helper: escape minimal per stringhe in apici singoli SPARQL
    sparql_escape_squote <- function(x) {
      x <- ifelse(is.null(x) || is.na(x), "", x)
      gsub("'", "\\\\'", x)
    }
    
    # Costruisci SPARQL (reactive)
    man_build_sparql <- shiny::reactive({
      man_id <- make_slug(input$manName)
      
      paste0(
        "PREFIX : <http://rdfdata.get-it.it/sensors/manufacturers/>\n",
        "PREFIX foaf:  <http://xmlns.com/foaf/0.1/>\n",
        "PREFIX addr:  <http://wymiwyg.org/ontologies/foaf/postaddress#>\n",
        "PREFIX vcard: <http://www.w3.org/2006/vcard/ns#>\n\n",
        "INSERT DATA {\n",
        "  <http://rdfdata.get-it.it/sensors/manufacturers/", man_id, "> a foaf:Organization ;\n",
        "    vcard:email <mailto:", sparql_escape_squote(input$manEMail), "> ;\n",
        "    foaf:homepage <", sparql_escape_squote(input$manWebSite), "> ;\n",
        "    foaf:name '", sparql_escape_squote(input$manName), "' ;\n",
        "    foaf:phone <tel:", sparql_escape_squote(input$manTel), "> ;\n",
        "    addr:address [ addr:deliveryPoint [ addr:location [\n",
        "      addr:building '' ;\n",
        "      addr:country '", sparql_escape_squote(input$manCountry), "' ;\n",
        "      addr:postcode '", sparql_escape_squote(input$manPostCode), "' ;\n",
        "      addr:region '", sparql_escape_squote(input$manAdm), "' ;\n",
        "      addr:streetNr '", sparql_escape_squote(input$manNumber), "' ;\n",
        "      addr:thoroughfareName '", sparql_escape_squote(input$manAdd), "' ;\n",
        "      addr:town '", sparql_escape_squote(input$manCity), "'\n",
        "    ] ] ] .\n",
        "}\n"
      )
    })
    
    # Preview in UI
    output$man_sparql <- shiny::renderText({
      man_build_sparql()
    })
    
    # (Opzionale) output per risultato
    man_result_txt <- shiny::reactiveVal("No action yet.")
    output$man_result <- shiny::renderText(man_result_txt())
    
    # Enable/disable button
    shiny::observe({
      ok <- nzchar(input$manName) &&
        nzchar(input$manTel) &&
        nzchar(input$manAdd) &&
        nzchar(input$manNumber) &&
        nzchar(input$manCity) &&
        nzchar(input$manAdm) &&
        nzchar(input$manPostCode) &&
        nzchar(input$manCountry) &&
        nzchar(input$manEMail) &&
        nzchar(input$manWebSite)
      
      shinyjs::toggleState("man_send", condition = ok && isTRUE(restricted_authed()))
    })
    
    # Click: Add new manufacturer
    shiny::observeEvent(input$man_send, {
      
      if (!isTRUE(restricted_authed())) {
        shiny::showNotification("Access denied. Please login first.", type = "error")
        return()
      }
      
      if (!requireNamespace("httr2", quietly = TRUE)) {
        man_result_txt("ERROR: install.packages('httr2')")
        shiny::showNotification("Missing package: httr2", type = "error")
        return()
      }
      
      user <- Sys.getenv("USERNAME_FUSEKI1", unset = "")
      pass <- Sys.getenv("PWD_FUSEKI1", unset = "")
      if (!nzchar(user) || !nzchar(pass)) {
        msg <- "ERROR: missing credentials. Set env vars USERNAME_FUSEKI and PWD_FUSEKI on the server (https://fuseki.lteritalia.it)."
        man_result_txt(msg)
        shiny::showNotification(msg, type = "error", duration = 8)
        return()
      }
      
      sparql <- man_build_sparql()
      
      shiny::withProgress(message = "Submitting SPARQL UPDATE to Fuseki...", value = 0.2, {
        tryCatch({
          resp <- httr2::request(endpointUpdate) |>
            httr2::req_method("POST") |>
            httr2::req_auth_basic(user, pass) |>
            httr2::req_headers("Content-Type" = "application/sparql-update; charset=UTF-8") |>
            httr2::req_body_raw(sparql) |>
            httr2::req_timeout(20) |>
            httr2::req_perform()
          
          code <- httr2::resp_status(resp)
          man_result_txt(paste0("OK. Fuseki response HTTP ", code))
          shiny::incProgress(0.8)
          
          shiny::showNotification("Manufacturer inserted into Fuseki.", type = "message")
          
        }, error = function(e) {
          man_result_txt(paste0("ERROR: ", e$message))
          shiny::showNotification(paste("Error:", e$message), type = "error", duration = 8)
        })
      })
    })
    
    # =========================================================
    # RESTRICTED all tabs
    # =========================================================
    
    restricted_tabs <- list(
      ttl = "ttl_app",
      doi = "doi_app",
      man = "man_app",
      acq = "acq_app",   
      dep = "dep_app",
      cal = "cal_app"
    )
    
    for (sfx in names(restricted_tabs)) {
      local({
        s   <- sfx
        div <- restricted_tabs[[s]]
        
        # Login box
        output[[paste0("login_ui_", s)]] <- shiny::renderUI({
          if (isTRUE(restricted_authed())) return(NULL)
          make_login_box(
            pwd_id = paste0(s, "_pwd"),
            btn_id = paste0(s, "_unlock")
          )
        })
        
        if (!is.null(div)) {
          shiny::observe({
            if (isTRUE(restricted_authed())) shinyjs::show(div) else shinyjs::hide(div)
          })
        }
        
        shiny::observeEvent(input[[paste0(s, "_unlock")]], {
          if (check_password(input[[paste0(s, "_pwd")]])) {
            restricted_authed(TRUE)
            shiny::showNotification("Unlocked.", type = "message")
          } else {
            shiny::showNotification("Wrong password.", type = "error")
          }
        }, ignoreNULL = TRUE)
      })
    }
    
    shiny::observe({
      if (isTRUE(restricted_authed())) shinyjs::show("doi_app") else shinyjs::hide("doi_app")
    })
    
    shiny::observeEvent(input$doi_unlock, {
      if (check_password(input$doi_pwd)) {
        restricted_authed(TRUE)
        shiny::showNotification("Unlocked.", type = "message")
      } else {
        shiny::showNotification("Wrong password.", type = "error")
      }
    })
    
    # Optional: warn if user opens restricted tabs not authed
    shiny::observeEvent(input$tabs, {
      if (input$tabs %in% c(
        "manufacturer",
        "sensor_instance_ttl",
        "sensor_instance_doi",
        "sensor_acquisition",
        "sensor_deploy",
        "sensor_calibration"
      ) &&
          !isTRUE(restricted_authed())) {
        shiny::showNotification("This tab is restricted. Please login.", type = "warning")
      }
    }, ignoreInit = TRUE)
    
    # =========================================================
    # HELPERS: fetch instances / agents / platforms da SPARQL
    # (con graceful fallback se endpoint non ancora esistente)
    # =========================================================
    
    # Endpoint placeholders — aggiorna quando i repo saranno pronti
    ENDPOINT_INSTANCES <- "https://fuseki.lteritalia.it/sensor_instances/sparql"
    ENDPOINT_AGENTS    <- "https://fuseki.lteritalia.it/agents/query"
    ENDPOINT_PLATFORMS <- "https://fuseki.lteritalia.it/platforms/query"
    
    fetch_instances <- function() {
      tryCatch({
        query <- '
          PREFIX sosa: <http://www.w3.org/ns/sosa/>
          PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          SELECT ?uri ?label WHERE {
            ?uri a sosa:Sensor .
            OPTIONAL { ?uri rdfs:label ?label . FILTER(lang(?label) = "en") }
          }
        '
        resp <- httr2::request(ENDPOINT_INSTANCES) |>
          httr2::req_method("POST") |>
          httr2::req_headers("Accept" = "application/sparql-results+json",
                             "Content-Type" = "application/x-www-form-urlencoded") |>
          httr2::req_body_form(query = query) |>
          httr2::req_timeout(10) |>
          httr2::req_perform()
        b <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)$results$bindings
        if (is.null(b) || NROW(b) == 0) return(c("No instances found" = ""))
        uris   <- b$uri$value
        labels <- ifelse(is.null(b$label$value) | is.na(b$label$value), basename(uris), b$label$value)
        stats::setNames(uris, labels)
      }, error = function(e) {
        # Endpoint non ancora attivo: torna lista vuota con messaggio
        c("(endpoint not yet available — use manual URI below)" = "")
      })
    }
    
    fetch_agents <- function() {
      tryCatch({
        query <- '
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?uri ?label WHERE {
  ?uri a foaf:Organization .
  OPTIONAL { ?uri foaf:name ?label . }
}'
        resp <- httr2::request(ENDPOINT_AGENTS) |>
          httr2::req_method("POST") |>
          httr2::req_headers("Accept" = "application/sparql-results+json",
                             "Content-Type" = "application/x-www-form-urlencoded") |>
          httr2::req_body_form(query = query) |>
          httr2::req_timeout(10) |>
          httr2::req_perform()
        b <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)$results$bindings
        if (is.null(b) || NROW(b) == 0) return(c("No agents found" = ""))
        uris   <- b$uri$value
        labels <- ifelse(is.null(b$label$value) | is.na(b$label$value), basename(uris), b$label$value)
        stats::setNames(uris, labels)
      }, error = function(e) {
        c("(endpoint not yet available — use manual URI below)" = "")
      })
    }
    
    fetch_platforms <- function() {
      tryCatch({
        query <- '
PREFIX sosa: <http://www.w3.org/ns/sosa/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?uri ?label WHERE {
  ?uri a sosa:Platform .
  OPTIONAL { ?uri rdfs:label ?label . FILTER(lang(?label) = "en") }
}'
        resp <- httr2::request(ENDPOINT_PLATFORMS) |>
          httr2::req_method("POST") |>
          httr2::req_headers("Accept" = "application/sparql-results+json",
                             "Content-Type" = "application/x-www-form-urlencoded") |>
          httr2::req_body_form(query = query) |>
          httr2::req_timeout(10) |>
          httr2::req_perform()
        b <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)$results$bindings
        if (is.null(b) || NROW(b) == 0) return(c("No platforms found" = ""))
        uris   <- b$uri$value
        labels <- ifelse(is.null(b$label$value) | is.na(b$label$value), basename(uris), b$label$value)
        stats::setNames(uris, labels)
      }, error = function(e) {
        c("(endpoint not yet available — use manual URI below)" = "")
      })
    }
    
    # Helper: URI effettivo = dropdown se non vuoto, altrimenti manuale
    resolve_uri <- function(dropdown_val, manual_val) {
      v <- trimws(dropdown_val)
      m <- trimws(manual_val)
      if (nzchar(v) && !startsWith(v, "(")) v else m
    }
    
    # =========================================================
    # ACQUISITION — directory chooser
    # =========================================================
    shinyFiles::shinyDirChoose(input, "acq_browse_dir", roots = volumes, session = session)
    shiny::observeEvent(input$acq_browse_dir, {
      dir <- shinyFiles::parseDirPath(volumes, input$acq_browse_dir)
      if (length(dir) >= 1 && nzchar(dir[[1]])) shiny::updateTextInput(session, "acq_output_dir", value = dir[[1]])
    })
    
    # Carica liste al primo accesso e su refresh
    shiny::observeEvent(TRUE, {
      shiny::updateSelectInput(session, "acq_sensor_uri", choices = fetch_instances())
      shiny::updateSelectInput(session, "acq_agent_uri",  choices = fetch_agents())
    }, once = TRUE)
    shiny::observeEvent(input$acq_refresh_instances, { shiny::updateSelectInput(session, "acq_sensor_uri", choices = fetch_instances()) })
    shiny::observeEvent(input$acq_refresh_agents,    { shiny::updateSelectInput(session, "acq_agent_uri",  choices = fetch_agents()) })
    
    acq_generated_path <- shiny::reactiveVal(NULL)
    
    # =========================================================
    # VALIDATION: Acquisition form
    # =========================================================
    acq_iv <- shinyvalidate::InputValidator$new()
    
    # --- Campi sempre obbligatori
    acq_iv$add_rule("acq_label", shinyvalidate::sv_required("Activity label is required."))
    
    acq_iv$add_rule("acq_label", function(value) {
      if (!nzchar(trimws(value))) return()  # sv_required già gestisce il vuoto
      if (!grepl("^[A-Za-z0-9\\s\\(\\)\\-\\.,']+$", trimws(value), perl = TRUE))
        return("Label contains invalid characters.")
    })
    
    acq_iv$add_rule("acq_agent_uri_manual", function(value) {
      dropdown <- isolate(input$acq_agent_uri)
      # obbligatorio solo se il dropdown non ha un URI valido
      if (nzchar(trimws(dropdown)) && !startsWith(dropdown, "(")) return()
      if (!nzchar(trimws(value)))
        return("Provide agent URI (select from list or type manually).")
      if (!grepl("^https?://", trimws(value)))
        return("Agent URI must start with http:// or https://")
    })
    
    acq_iv$add_rule("acq_sensor_uri_manual", function(value) {
      dropdown <- isolate(input$acq_sensor_uri)
      if (nzchar(trimws(dropdown)) && !startsWith(dropdown, "(")) return()
      if (!nzchar(trimws(value)))
        return("Provide sensor instance URI (select from list or type manually).")
      if (!grepl("^https?://", trimws(value)))
        return("Sensor URI must start with http:// or https://")
    })
    
    acq_iv$add_rule("acq_po_number", shinyvalidate::sv_required("Purchase order / contract number is required."))
    acq_iv$add_rule("acq_po_number", function(value) {
      if (!nzchar(trimws(value))) return()  # sv_required già gestisce il vuoto
      if (!grepl("^[A-Za-z0-9\\-_/\\.]+$", trimws(value), perl = TRUE))
        return("PO number: only letters, numbers, - _ / . allowed.")
    })
    
    acq_iv$add_rule("acq_output_dir", shinyvalidate::sv_required("Output directory is required."))
    acq_iv$add_rule("acq_output_dir", function(value) {
      if (nzchar(trimws(value)) && !dir.exists(trimws(value)))
        return("Output directory does not exist.")
    })
    
    # --- Regole condizionali sulla data
    acq_iv$add_rule("acq_year", function(value) {
      if (isolate(input$acq_date_precision) != "year") return()
      if (is.na(value) || !nzchar(as.character(value)))
        return("Year is required.")
      if (!grepl("^\\d{4}$", as.character(value)))
        return("Year must be a 4-digit number (e.g. 2026).")
      yr <- as.integer(value)
      if (yr < 1900 || yr > 2100)
        return("Year must be between 1900 and 2100.")
    })
    
    acq_iv$add_rule("acq_date_full", function(value) {
      if (isolate(input$acq_date_precision) != "full") return()
      if (is.na(value))
        return("Full date is required.")
      if (as.Date(value) > Sys.Date())
        return("Acquisition date cannot be in the future.")
    })
    
    # --- Attiva validazione
    acq_iv$enable()
    
    # --- Disabilita il pulsante finché il form non è valido + autenticato
    shiny::observe({
      shinyjs::toggleState(
        "acq_generate",
        condition = isTRUE(acq_iv$is_valid()) && isTRUE(restricted_authed())
      )
    })
    
    # --- Guard aggiuntivo al click (difesa in profondità)
    shiny::observeEvent(input$acq_generate, {
      if (!isTRUE(acq_iv$is_valid())) {
        acq_iv$enable()
        shiny::showNotification("Please fix the highlighted fields.", type = "error", duration = 5)
        return()
      }
      # ... qui sotto continua la logica di generazione TTL già esistente
    }, ignoreInit = TRUE, priority = 10)  # priority alta: gira prima dell'observer che scrive il file
    
    shiny::observeEvent(input$acq_generate, {
      shiny::validate(shiny::need(isTRUE(restricted_authed()), "Access denied."))
      
      sensor_uri <- resolve_uri(input$acq_sensor_uri, input$acq_sensor_uri_manual)
      agent_uri  <- resolve_uri(input$acq_agent_uri,  input$acq_agent_uri_manual)
      
      shiny::validate(
        shiny::need(nzchar(sensor_uri),      "Provide sensor instance URI."),
        shiny::need(nzchar(agent_uri),       "Provide agent URI."),
        shiny::need(nzchar(input$acq_label), "Provide activity label.")
      )
      
      instance_uuid <- basename(sensor_uri)
      
      # --- Data con precisione variabile ----------------------------
      if (input$acq_date_precision == "year") {
        shiny::validate(shiny::need(!is.na(input$acq_year), "Provide acquisition year."))
        date_value   <- as.character(input$acq_year)
        date_type    <- "xsd:gYear"
      } else {
        shiny::validate(shiny::need(!is.na(input$acq_date_full), "Provide acquisition date."))
        date_value   <- format(input$acq_date_full, "%Y-%m-%d")
        date_type    <- "xsd:date"
      }
      
      date_triple <- paste0(
        '    prov:startedAtTime "', date_value, '"^^', date_type, ' ;\n'
      )
      
      # --- Numero d'ordine (opzionale) ------------------------------
      po_triple <- if (nzchar(trimws(input$acq_po_number)))
        paste0('    dct:identifier "', trimws(input$acq_po_number), '" ;\n')
      else
        ""
      
      # --- TTL ------------------------------------------------------
      ttl_content <- paste0(
        '@prefix acquisition: <https://rdfdata.lteritalia.it/sensors/acquisition/> .\n',
        '@prefix sensor:      <https://rdfdata.lteritalia.it/sensors/instance/> .\n',
        '@prefix prov:        <http://www.w3.org/ns/prov#> .\n',
        '@prefix rdfs:        <http://www.w3.org/2000/01/rdf-schema#> .\n',
        '@prefix role:        <https://rdfdata.lteritalia.it/sensors/role/> .\n',
        '@prefix dct:         <http://purl.org/dc/terms/> .\n',
        '@prefix xsd:         <http://www.w3.org/2001/XMLSchema#> .\n\n',
        
        'acquisition:', instance_uuid, '\n',
        '    a prov:Activity ;\n',
        '    rdfs:label "', input$acq_label, '"@en ;\n',
        date_triple,
        po_triple,
        '    prov:wasAssociatedWith <', agent_uri, '> ;\n',
        '    prov:qualifiedAssociation [\n',
        '        a prov:Association ;\n',
        '        prov:agent   <', agent_uri, '> ;\n',
        '        prov:hadRole role:PurchaseFunder\n',
        '    ] .\n\n',
        '# Link back to sensor instance\n',
        'sensor:', instance_uuid, '\n',
        '    prov:wasGeneratedBy acquisition:', instance_uuid, ' .\n'
      )
      
      out_file <- file.path(
        input$acq_output_dir,
        paste0("acquisition_", instance_uuid, "_", format(Sys.Date(), "%Y%m%d"), ".ttl")
      )
      
      shiny::withProgress(message = "Writing TTL...", value = 0.5, {
        writeLines(ttl_content, out_file)
        shiny::incProgress(0.5)
      })
      
      acq_generated_path(out_file)
      shiny::showNotification("Acquisition TTL generated.", type = "message", duration = 4)
    })
    
    output$acq_status <- shiny::renderText({
      p <- acq_generated_path()
      if (is.null(p)) "No file generated yet." else paste0("\u2705 TTL generated: ", p)
    })
    output$acq_meta_tbl <- shiny::renderTable({
      p <- acq_generated_path(); if (is.null(p)) return(NULL)
      data.frame(field = c("path", "sensor_instance_uri", "agent_uri"),
                 value = c(p,
                           resolve_uri(input$acq_sensor_uri, input$acq_sensor_uri_manual),
                           resolve_uri(input$acq_agent_uri,  input$acq_agent_uri_manual)),
                 stringsAsFactors = FALSE)
    })
    output$acq_preview <- shiny::renderText({
      p <- acq_generated_path(); if (is.null(p)) return("")
      paste(readLines(p, warn = FALSE), collapse = "\n")
    })
    
    # =========================================================
    # DEPLOY — directory chooser
    # =========================================================
    shinyFiles::shinyDirChoose(input, "dep_browse_dir", roots = volumes, session = session)
    shiny::observeEvent(input$dep_browse_dir, {
      dir <- shinyFiles::parseDirPath(volumes, input$dep_browse_dir)
      if (length(dir) >= 1 && nzchar(dir[[1]])) shiny::updateTextInput(session, "dep_output_dir", value = dir[[1]])
    })
    
    shiny::observeEvent(TRUE, {
      shiny::updateSelectInput(session, "dep_sensor_uri",   choices = fetch_instances())
      shiny::updateSelectInput(session, "dep_platform_uri", choices = fetch_platforms())
    }, once = TRUE)
    shiny::observeEvent(input$dep_refresh_instances, { shiny::updateSelectInput(session, "dep_sensor_uri",   choices = fetch_instances()) })
    shiny::observeEvent(input$dep_refresh_platforms, { shiny::updateSelectInput(session, "dep_platform_uri", choices = fetch_platforms()) })
    
    dep_generated_path <- shiny::reactiveVal(NULL)
    
    # =========================================================
    # DEPLOY — VALIDATION
    # =========================================================
    dep_iv <- shinyvalidate::InputValidator$new()
    
    # --- Sensor instance URI (obbligatorio)
    dep_iv$add_rule("dep_sensor_uri_manual", function(value) {
      dropdown <- isolate(input$dep_sensor_uri)
      if (nzchar(trimws(dropdown)) && !startsWith(dropdown, "(")) return()
      if (!nzchar(trimws(value)))
        return("Provide sensor instance URI (select from list or type manually).")
      if (!grepl("^https?://", trimws(value), perl = TRUE))
        return("Sensor URI must start with http:// or https://")
    })
    
    # --- Deployment label (obbligatorio)
    dep_iv$add_rule("dep_label", shinyvalidate::sv_required("Deployment label is required."))
    dep_iv$add_rule("dep_label", function(value) {
      if (!nzchar(trimws(value))) return()
      if (!grepl("^[A-Za-z0-9\\s\\(\\)\\-\\.,']+$", trimws(value), perl = TRUE))
        return("Label contains invalid characters.")
    })
    
    # --- Comment (obbligatorio)
    dep_iv$add_rule("dep_comment", shinyvalidate::sv_required("Comment is required."))
    dep_iv$add_rule("dep_comment", function(value) {
      if (!nzchar(trimws(value))) return()
      if (!grepl("^[A-Za-z0-9\\s\\(\\)\\-\\.,']+$", trimws(value), perl = TRUE))
        return("Comment contains invalid characters.")
    })
    
    # --- Deployment date (obbligatorio)
    dep_iv$add_rule("dep_date", function(value) {
      if (is.na(value))
        return("Deployment date is required.")
      if (as.Date(value) > Sys.Date())
        return("Deployment date cannot be in the future.")
    })
    
    # --- Deployment time (obbligatorio — shinyTime non può essere NA ma verifichiamo)
    dep_iv$add_rule("dep_time", function(value) {
      if (is.null(value) || is.na(value))
        return("Deployment time is required.")
    })
    
    # --- Output directory (obbligatorio)
    dep_iv$add_rule("dep_output_dir", shinyvalidate::sv_required("Output directory is required."))
    dep_iv$add_rule("dep_output_dir", function(value) {
      if (nzchar(trimws(value)) && !dir.exists(trimws(value)))
        return("Output directory does not exist.")
    })
    
    dep_iv$enable()
    
    # --- Disabilita il pulsante finché form non valido + autenticato
    shiny::observe({
      shinyjs::toggleState(
        "dep_generate",
        condition = isTRUE(dep_iv$is_valid()) && isTRUE(restricted_authed())
      )
    })
    
    shiny::observeEvent(input$dep_generate, {
      if (!isTRUE(dep_iv$is_valid())) {
        dep_iv$enable()
        shiny::showNotification(
          "Please fix the highlighted fields.",
          type = "error", duration = 5
        )
        return()
      }
      
      shiny::validate(shiny::need(isTRUE(restricted_authed()), "Access denied."))
      
      sensor_uri    <- resolve_uri(input$dep_sensor_uri, input$dep_sensor_uri_manual)
      platform_uri  <- resolve_uri(input$dep_platform_uri, input$dep_platform_uri_manual)
      instance_uuid <- basename(sensor_uri)
      
      dep_time_str  <- format(input$dep_time, "%H:%M:%S")
      dep_date_time <- paste0(format(input$dep_date, "%Y-%m-%d"), "T", dep_time_str, "Z")
      dep_id        <- paste0(instance_uuid, "-", format(input$dep_date, "%Y%m%d"))
      
      # Triple opzionale piattaforma
      platform_triple <- if (nzchar(trimws(platform_uri)))
        paste0('    ssn:deployedOnPlatform <', platform_uri, '> ;\n')
      else ""
      
      ttl_content <- paste0(
        '@prefix deploy: <https://rdfdata.lteritalia.it/sensors/deployment/> .\n',
        '@prefix sensor: <https://rdfdata.lteritalia.it/sensors/instance/> .\n',
        '@prefix ssn:    <http://www.w3.org/ns/ssn/> .\n',
        '@prefix rdfs:   <http://www.w3.org/2000/01/rdf-schema#> .\n',
        '@prefix xsd:    <http://www.w3.org/2001/XMLSchema#> .\n\n',
        'deploy:', dep_id, '\n',
        '    a ssn:Deployment ;\n',
        '    rdfs:label "', input$dep_label, '"@en ;\n',
        '    rdfs:comment "', input$dep_comment, '"@en ;\n',
        '    ssn:deployedSystem sensor:', instance_uuid, ' ;\n',
        platform_triple,
        '    ssn:deploymentTime "', dep_date_time, '"^^xsd:dateTime .\n'
      )
      
      out_file <- file.path(
        input$dep_output_dir,
        paste0("deployment_", dep_id, ".ttl")
      )
      
      shiny::withProgress(message = "Writing TTL...", value = 0.5, {
        writeLines(ttl_content, out_file)
        shiny::incProgress(0.5)
      })
      
      dep_generated_path(out_file)
      shiny::showNotification("Deployment TTL generated.", type = "message", duration = 4)
    })
    
    output$dep_status <- shiny::renderText({
      p <- dep_generated_path()
      if (is.null(p)) "No file generated yet." else paste0("\u2705 TTL generated: ", p)
    })
    output$dep_meta_tbl <- shiny::renderTable({
      p <- dep_generated_path(); if (is.null(p)) return(NULL)
      data.frame(field = c("path", "sensor_instance_uri", "platform_uri", "deployment_datetime"),
                 value = c(p,
                           resolve_uri(input$dep_sensor_uri, input$dep_sensor_uri_manual),
                           resolve_uri(input$dep_platform_uri, input$dep_platform_uri_manual),
                           paste0(format(input$dep_date, "%Y-%m-%d"), "T", input$dep_time, "Z")),
                 stringsAsFactors = FALSE)
    })
    output$dep_preview <- shiny::renderText({
      p <- dep_generated_path(); if (is.null(p)) return("")
      paste(readLines(p, warn = FALSE), collapse = "\n")
    })
    
    # =========================================================
    # CALIBRATION — directory chooser
    # =========================================================
    shinyFiles::shinyDirChoose(input, "cal_browse_dir", roots = volumes, session = session)
    shiny::observeEvent(input$cal_browse_dir, {
      dir <- shinyFiles::parseDirPath(volumes, input$cal_browse_dir)
      if (length(dir) >= 1 && nzchar(dir[[1]])) shiny::updateTextInput(session, "cal_output_dir", value = dir[[1]])
    })
    
    shiny::observeEvent(TRUE, {
      shiny::updateSelectInput(session, "cal_sensor_uri", choices = fetch_instances())
      shiny::updateSelectInput(session, "cal_agent_uri",  choices = fetch_agents())
    }, once = TRUE)
    shiny::observeEvent(input$cal_refresh_instances, { shiny::updateSelectInput(session, "cal_sensor_uri", choices = fetch_instances()) })
    shiny::observeEvent(input$cal_refresh_agents,    { shiny::updateSelectInput(session, "cal_agent_uri",  choices = fetch_agents()) })
    
    cal_generated_path <- shiny::reactiveVal(NULL)
    
    # =========================================================
    # CALIBRATION — VALIDATION
    # =========================================================
    cal_iv <- shinyvalidate::InputValidator$new()
    
    # --- Sensor instance URI (obbligatorio)
    cal_iv$add_rule("cal_sensor_uri_manual", function(value) {
      dropdown <- isolate(input$cal_sensor_uri)
      if (nzchar(trimws(dropdown)) && !startsWith(dropdown, "(")) return()
      if (!nzchar(trimws(value)))
        return("Provide sensor instance URI (select from list or type manually).")
      if (!grepl("^https?://", trimws(value), perl = TRUE))
        return("Sensor URI must start with http:// or https://")
    })
    
    # --- Label (obbligatorio)
    cal_iv$add_rule("cal_label", shinyvalidate::sv_required("Calibration label is required."))
    cal_iv$add_rule("cal_label", function(value) {
      if (!nzchar(trimws(value))) return()
      if (!grepl("^[A-Za-z0-9\\s\\(\\)\\-\\.,']+$", trimws(value), perl = TRUE))
        return("Label contains invalid characters.")
    })
    
    # --- Comment (obbligatorio)
    cal_iv$add_rule("cal_comment", shinyvalidate::sv_required("Comment is required."))
    cal_iv$add_rule("cal_comment", function(value) {
      if (!nzchar(trimws(value))) return()
      if (!grepl("^[A-Za-z0-9\\s\\(\\)\\-\\.,']+$", trimws(value), perl = TRUE))
        return("Comment contains invalid characters.")
    })
    
    # --- Agent URI (obbligatorio)
    cal_iv$add_rule("cal_agent_uri_manual", function(value) {
      dropdown <- isolate(input$cal_agent_uri)
      if (nzchar(trimws(dropdown)) && !startsWith(dropdown, "(")) return()
      if (!nzchar(trimws(value)))
        return("Provide agent URI (select from list or type manually).")
      if (!grepl("^https?://", trimws(value), perl = TRUE))
        return("Agent URI must start with http:// or https://")
    })
    
    # --- Data calibrazione (solo se checkbox attivo)
    cal_iv$add_rule("cal_date", function(value) {
      if (!isTRUE(isolate(input$cal_has_date))) return()
      if (is.na(value))
        return("Calibration date is required when date is known.")
      if (as.Date(value) > Sys.Date())
        return("Calibration date cannot be in the future.")
    })
    
    # --- Output directory (obbligatorio)
    cal_iv$add_rule("cal_output_dir", shinyvalidate::sv_required("Output directory is required."))
    cal_iv$add_rule("cal_output_dir", function(value) {
      if (nzchar(trimws(value)) && !dir.exists(trimws(value)))
        return("Output directory does not exist.")
    })
    
    cal_iv$enable()
    
    # --- Disabilita il pulsante finché form non valido + autenticato
    shiny::observe({
      shinyjs::toggleState(
        "cal_generate",
        condition = isTRUE(cal_iv$is_valid()) && isTRUE(restricted_authed())
      )
    })
    
    shiny::observeEvent(input$cal_generate, {
      if (!isTRUE(cal_iv$is_valid())) {
        cal_iv$enable()
        shiny::showNotification(
          "Please fix the highlighted fields.",
          type = "error", duration = 5
        )
        return()
      }
      
      shiny::validate(shiny::need(isTRUE(restricted_authed()), "Access denied."))
      
      sensor_uri <- resolve_uri(input$cal_sensor_uri, input$cal_sensor_uri_manual)
      agent_uri  <- resolve_uri(input$cal_agent_uri,  input$cal_agent_uri_manual)
      
      shiny::validate(
        shiny::need(nzchar(sensor_uri),  "Provide sensor instance URI."),
        shiny::need(nzchar(agent_uri),   "Provide calibration agent URI."),
        shiny::need(nzchar(input$cal_label), "Provide calibration label.")
      )
      
      instance_uuid <- basename(sensor_uri)
      cal_suffix <- if (isTRUE(input$cal_has_date))
        format(input$cal_date, "%Y%m%d")
      else
        "factory"
      
      cal_id <- paste0(instance_uuid, "-", cal_suffix)
      
      date_triple <- if (isTRUE(input$cal_has_date))
        paste0('    prov:atTime "', format(input$cal_date, "%Y-%m-%d"), '"^^xsd:date ;\n')
      else ""
      
      type_triple <- paste0(
        '    dct:type <https://rdfdata.lteritalia.it/sensors/calibrationType/',
        input$cal_type, '> ;\n'
      )
      
      cert_triple <- if (nzchar(trimws(input$cal_certificate)))
        paste0('    dct:references <', trimws(input$cal_certificate), '> ;\n')
      else ""
      
      ttl_content <- paste0(
        '@prefix calibration: <https://rdfdata.lteritalia.it/sensors/calibration/> .\n',
        '@prefix sensor:      <https://rdfdata.lteritalia.it/sensors/instance/> .\n',
        '@prefix prov:        <http://www.w3.org/ns/prov#> .\n',
        '@prefix rdfs:        <http://www.w3.org/2000/01/rdf-schema#> .\n',
        '@prefix role:        <https://rdfdata.lteritalia.it/sensors/role/> .\n',
        '@prefix dct:         <http://purl.org/dc/terms/> .\n',      # <-- aggiunto
        '@prefix xsd:         <http://www.w3.org/2001/XMLSchema#> .\n\n',
        
        'calibration:', cal_id, '\n',
        '    a prov:Activity ;\n',
        '    rdfs:label "', input$cal_label, '"@en ;\n',
        '    rdfs:comment "', input$cal_comment, '"@en ;\n',
        '    dct:type <https://rdfdata.lteritalia.it/sensors/calibrationType/', input$cal_type, '> ;\n',
        cert_triple,                                                   # <-- opzionale
        date_triple,                                                   # <-- già esistente
        '    prov:qualifiedAssociation [\n',
        '        a prov:Association ;\n',
        '        prov:agent <', agent_uri, '> ;\n',
        '        prov:hadRole role:CalibrationProvider\n',
        '    ] .\n\n',
        '# Link back to sensor instance\n',
        'sensor:', instance_uuid, '\n',
        '    prov:wasInfluencedBy calibration:', cal_id, ' .\n'
      )
      
      out_file <- file.path(
        input$cal_output_dir,
        paste0("calibration_", cal_id, ".ttl")
      )
      
      shiny::withProgress(message = "Writing TTL...", value = 0.5, {
        writeLines(ttl_content, out_file)
        shiny::incProgress(0.5)
      })
      
      cal_generated_path(out_file)
      shiny::showNotification("Calibration TTL generated.", type = "message", duration = 4)
    })
    
    output$cal_status <- shiny::renderText({
      p <- cal_generated_path()
      if (is.null(p)) "No file generated yet." else paste0("\u2705 TTL generated: ", p)
    })
    output$cal_meta_tbl <- shiny::renderTable({
      p <- cal_generated_path(); if (is.null(p)) return(NULL)
      data.frame(field = c("path", "sensor_instance_uri", "agent_uri",
                           "calibration_date"),
                 value = c(p,
                           resolve_uri(input$cal_sensor_uri, input$cal_sensor_uri_manual),
                           resolve_uri(input$cal_agent_uri,  input$cal_agent_uri_manual),
                           if (isTRUE(input$cal_has_date)) as.character(input$cal_date) else "factory (no date)"),
                 stringsAsFactors = FALSE)
    })
    output$cal_preview <- shiny::renderText({
      p <- cal_generated_path(); if (is.null(p)) return("")
      paste(readLines(p, warn = FALSE), collapse = "\n")
    })
    
    # =========================================================
    # SENSOR INSTANCE TTL logic
    # =========================================================
    ttl_generated_path   <- shiny::reactiveVal(NULL)
    ttl_generated_checks <- shiny::reactiveVal(NULL)
    
    ttl_load_types <- function() {
      tryCatch(
        {
          df <- fetch_sensor_types_from_fuseki("https://fuseki.lteritalia.it/systemsType/query")
          if (NROW(df) == 0) {
            shiny::updateSelectInput(session, "ttl_sensor_type_uri", choices = c("No types found" = ""))
            shiny::showNotification("No sensors type found from the SPARQL query.", type = "warning", duration = 6)
          } else {
            choices <- stats::setNames(df$uri, df$label)
            shiny::updateSelectInput(session, "ttl_sensor_type_uri", choices = choices, selected = df$uri[1])
            shiny::showNotification(paste0("Loaded ", NROW(df), " sensors type."), type = "message", duration = 4)
          }
        },
        error = function(e) {
          shiny::updateSelectInput(session, "ttl_sensor_type_uri", choices = c("Error loading types" = ""))
          shiny::showNotification(paste("Error querying Fuseki:", e$message), type = "error", duration = 10)
        }
      )
    }
    
    shiny::observeEvent(TRUE, { ttl_load_types() }, once = TRUE)
    shiny::observeEvent(input$ttl_refresh_types, { ttl_load_types() })
    
    shiny::observeEvent(input$ttl_generate, {
      shiny::validate(
        shiny::need(isTRUE(restricted_authed()), "Access denied."),
        shiny::need(nzchar(input$ttl_sensor_type_uri), "Select a sensor type."),
        shiny::need(nzchar(input$ttl_output_dir), "Provide output_dir.")
      )
      
      owner_orcid <- input$ttl_owner_orcid
      if (!startsWith(owner_orcid, "https://orcid.org/")) {
        owner_orcid <- paste0("https://orcid.org/", owner_orcid)
      }
      
      shiny::withProgress(message = "Generating TTL...", value = 0.2, {
        res <- SensorCat:::sensors_instance_ttl(
          sensor_type_uri = input$ttl_sensor_type_uri,
          owner_name = input$ttl_owner_name,
          owner_surname = input$ttl_owner_surname,
          owner_orcid = owner_orcid,
          serial_number = if (nzchar(input$ttl_serial_number)) input$ttl_serial_number else NULL,
          part_number = if (nzchar(input$ttl_part_number)) input$ttl_part_number else NULL,
          # validate = isTRUE(input$ttl_do_validate),
          output_dir = input$ttl_output_dir
        )
        shiny::incProgress(0.8)
        
        out_path <- if (is.list(res) && !is.null(res$path)) res$path else as.character(res)
        ttl_generated_path(out_path)
        
        if (is.list(res) && !is.null(res$checks)) ttl_generated_checks(res$checks) else ttl_generated_checks(NULL)
        shiny::showNotification("TTL generated successfully.", type = "message", duration = 4)
      })
    })
    
    output$ttl_status <- shiny::renderText({
      p <- ttl_generated_path()
      if (is.null(p)) return("No file generated yet.")
      paste0("✅ TTL generated: ", p)
    })
    
    output$ttl_meta_tbl <- shiny::renderTable({
      p <- ttl_generated_path()
      if (is.null(p)) return(NULL)
      
      ttl_lines <- readLines(p, warn = FALSE)
      subj <- ttl_lines[grep("^<https?://", ttl_lines)][1]
      uuid_line <- ttl_lines[grep("dct:identifier", ttl_lines)][1]
      uuid_val <- sub('.*dct:identifier\\s+"([^"]+)".*', "\\1", uuid_line)
      
      data.frame(
        field = c("path", "instance_uuid", "instance_subject", "sensor_type_uri"),
        value = c(p, uuid_val, subj, input$ttl_sensor_type_uri),
        stringsAsFactors = FALSE
      )
    })
    
    output$ttl_checks_tbl <- shiny::renderTable({
      ch <- ttl_generated_checks()
      if (is.null(ch)) return(NULL)
      data.frame(
        predicate = names(ch),
        ok = as.logical(ch),
        stringsAsFactors = FALSE
      )
    })
    
    output$ttl_preview <- shiny::renderText({
      p <- ttl_generated_path()
      if (is.null(p)) return("")
      paste(readLines(p, warn = FALSE), collapse = "\n")
    })
    
    # =========================================================
    # SENSOR INSTANCE DOI logic
    # =========================================================
    doi_res <- shiny::eventReactive(input$doi_run, {
      shiny::validate(shiny::need(isTRUE(restricted_authed()), "Access denied."))
      shiny::req(input$doi_ttl)
      
      ttl_path <- input$doi_ttl$datapath
      url_override <- if (nzchar(input$doi_url_override)) input$doi_url_override else NULL
      doi <- if (input$doi_method == "PUT" && nzchar(input$doi_existing_doi)) input$doi_existing_doi else NULL
      
      shiny::validate(
        shiny::need(nzchar(input$doi_prefix), "Provide DOI prefix."),
        shiny::need(nzchar(input$doi_publisher), "Provide publisher."),
        shiny::need(nzchar(input$doi_publisherId), "Provide publisherIdentifier (ROR URI)."),
        shiny::need(input$doi_method != "PUT" || nzchar(input$doi_existing_doi), "PUT requires an existing DOI.")
      )
      
      shiny::withProgress(message = "Calling DataCite...", value = 0.2, {
        out <- SensorCat::sensors_to_datacite(
          ttl_file = ttl_path,
          env = input$doi_env,
          prefix = input$doi_prefix,
          publisher = input$doi_publisher,
          publisherIdentifier = input$doi_publisherId,
          state = input$doi_state,
          url_override = url_override,
          method = input$doi_method,
          doi = doi
        )
        shiny::incProgress(0.8)
        out
      })
    })
    
    output$doi_status <- shiny::renderText({
      r <- doi_res()
      shiny::req(r)
      paste0(
        "OK\nMinted/returned DOI: ",
        ifelse(is.na(r$minted_doi) || !nzchar(r$minted_doi), "<none>", r$minted_doi)
      )
    })
    
    output$doi_summary_tbl <- shiny::renderTable({
      r <- doi_res()
      shiny::req(r)
      data.frame(
        field = c("subject_uri", "instance_uuid", "serial_number", "created"),
        value = c(r$subject_uri, r$instance_uuid, r$serial_number %||% "", r$created %||% ""),
        stringsAsFactors = FALSE
      )
    })
    
    output$doi_payload_json <- shiny::renderText({
      r <- doi_res()
      shiny::req(r)
      jsonlite::toJSON(r$payload, auto_unbox = TRUE, pretty = TRUE, null = "null")
    })
    
    output$doi_resp_out <- shiny::renderText({
      r <- doi_res()
      shiny::req(r)
      if (isTRUE(input$doi_show_response_raw)) {
        jsonlite::toJSON(r$response, auto_unbox = TRUE, pretty = TRUE, null = "null")
      } else {
        attrs <- r$response$data$attributes
        paste0(
          "doi: ", attrs$doi %||% "<none>", "\n",
          "state: ", attrs$state %||% "<none>", "\n",
          "url: ", attrs$url %||% "<none>"
        )
      }
    })
    
    # =========================================================
    # VALIDATION: Manufacturer form (red highlight + messages)
    # =========================================================
    man_iv <- shinyvalidate::InputValidator$new()
    
    # required fields
    man_iv$add_rule("manName",     shinyvalidate::sv_required("Manufacturer name is required"))
    man_iv$add_rule("manTel",      shinyvalidate::sv_required("Phone number is required"))
    man_iv$add_rule("manAdd",      shinyvalidate::sv_required("Address is required"))
    man_iv$add_rule("manNumber",   shinyvalidate::sv_required("Address number is required"))
    man_iv$add_rule("manCity",     shinyvalidate::sv_required("City is required"))
    man_iv$add_rule("manAdm",      shinyvalidate::sv_required("Administrative area is required"))
    man_iv$add_rule("manPostCode", shinyvalidate::sv_required("Postal code is required"))
    man_iv$add_rule("manCountry",  shinyvalidate::sv_required("Country is required"))
    man_iv$add_rule("manEMail",    shinyvalidate::sv_required("E-mail address is required"))
    man_iv$add_rule("manWebSite",  shinyvalidate::sv_required("Web site is required"))
    
    # e-mail format
    man_iv$add_rule(
      "manEMail",
      shinyvalidate::sv_regex(
        "^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$",
        "Please provide a valid e-mail address"
      )
    )
    
    # website format (accepts http/https; you can relax if you want)
    man_iv$add_rule(
      "manWebSite",
      shinyvalidate::sv_regex(
        "^https?://.+",
        "Web site must start with http:// or https://"
      )
    )
    
    # phone: permissive (digits, spaces, +, (), -)
    man_iv$add_rule(
      "manTel",
      shinyvalidate::sv_regex(
        "^\\+[0-9]{8,15}$",
        "Phone number must be in international format, e.g. +3902270000. Without space or other characters."
      )
    )
    
    # address number: simple rule (digits + optional letters, e.g. 12, 12A)
    man_iv$add_rule(
      "manNumber",
      shinyvalidate::sv_regex(
        "^[0-9]+[A-Za-z]?$",
        "Address number must be like 12 or 12A"
      )
    )
    
    # postal code: keep it generic (3–10 alphanum, space or dash allowed)
    man_iv$add_rule(
      "manPostCode",
      shinyvalidate::sv_regex(
        "^[A-Za-z0-9][A-Za-z0-9\\s-]{2,9}$",
        "Postal code looks invalid"
      )
    )
    
    # activate validation (this enables red borders/messages)
    man_iv$enable()
    
    # disable "Add new manufacturer" until the form is valid AND user is authed
    shiny::observe({
      ok <- isTRUE(man_iv$is_valid())
      # se vuoi anche imporre l'autenticazione:
      ok <- ok && isTRUE(restricted_authed())
      shinyjs::toggleState("man_send", condition = ok)
    })
    
    # OPTIONAL: on click, stop immediately if invalid and show errors
    shiny::observeEvent(input$man_send, {
      if (!isTRUE(man_iv$is_valid())) {
        # forza la visualizzazione dei messaggi (di solito già appaiono)
        man_iv$enable()
        shiny::showNotification("Please fix the highlighted fields.", type = "error", duration = 5)
        return()
      }
      
      # qui sotto ci metti la tua logica di INSERT su Fuseki
      # (es: build_sparql() + httr2 POST con auth)
    })
    
    # =========================================================
    # VALIDATION: TTL form
    # =========================================================
    ttl_iv <- shinyvalidate::InputValidator$new()
    
    ttl_iv$add_rule("ttl_sensor_type_uri",
                    shinyvalidate::sv_required("Select a sensor type."))
    
    ttl_iv$add_rule("ttl_owner_name",
                    shinyvalidate::sv_required("Owner givenName is required."))
    ttl_iv$add_rule("ttl_owner_surname",
                    shinyvalidate::sv_required("Owner familyName is required."))
    
    # ORCID (accept 0000-0000-0000-0000 or finale X; with or witout https://orcid.org/)
    ttl_iv$add_rule("ttl_owner_orcid",
                    shinyvalidate::sv_required("Owner ORCID is required."))
    ttl_iv$add_rule(
      "ttl_owner_orcid",
      shinyvalidate::sv_regex(
        "^(https://orcid\\.org/)?\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$",
        "ORCID must be like 0000-0002-7997-219X (optionally with https://orcid.org/)."
      )
    )
    
    ttl_iv$add_rule("ttl_serial_number", function(value) {
      if (!nzchar(value)) return()
      if (!grepl("^[A-Za-z0-9._:/\\-\\s]+$", value)) {
        return("Serial number contains invalid characters (allowed: letters, numbers, space, . _ - : /).")
      }
    })
    
    ttl_iv$add_rule("ttl_part_number", function(value) {
      if (!nzchar(value)) return()
      if (!grepl("^[A-Za-z0-9._:/\\-\\s]+$", value)) {
        return("Part number contains invalid characters (allowed: letters, numbers, space, . _ - : /).")
      }
    })
    
    ttl_iv$enable()
    
    shiny::observe({
      shinyjs::toggleState("ttl_generate", condition = ttl_iv$is_valid())
    })
    
    
    # =========================================================
    # SERVER SAMPLES (Specimen)
    # =========================================================
    specimen_dataset <- "https://fuseki.lteritalia.it/specimen/query"
    specimen_query <- "
      PREFIX sosa: <http://www.w3.org/ns/sosa/>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX dct:  <http://purl.org/dc/terms/>
      PREFIX dcat: <http://www.w3.org/ns/dcat#>
      PREFIX geosparql: <http://www.opengis.net/ont/geosparql#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?c ?label ?geom_wkt ?landing_page ?created ?site ?ror ?material ?sampler
      WHERE {
        GRAPH <http://rdfdata.lteritalia.it/graph/samples> {
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
          OPTIONAL { ?c sosa:madeBySampler ?sampler . }
        }
      }
      ORDER BY ?label
    "
  
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
        stringr::str_remove("^<urn:ogc:def:crs:EPSG::4326>\\s*") |>
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
  # get labels from extarnal repos
  rors <- unique(list_specimen$specimen_ror)
  materials <- unique(list_specimen$specimen_material)
  samplers <- unique(list_specimen$specimen_sampler)
  sites <- unique(list_specimen$specimen_site)
  # ---- ROR TABLE
  .ror_cache <- new.env(parent = emptyenv())
  get_ror_label <- function(ror_uri) {
    # ---- input guard (scalare) ----
    if (length(ror_uri) != 1 || is.na(ror_uri) || !nzchar(ror_uri)) {
      return(NA_character_)
    }
    
    # ---- cache ----
    if (exists(ror_uri, envir = .ror_cache, inherits = FALSE)) {
      return(get(ror_uri, envir = .ror_cache, inherits = FALSE))
    }
    
    ror_id <- sub("^https://ror\\.org/", "", ror_uri)
    url <- paste0("https://api.ror.org/v2/organizations/", ror_id)
    
    jj <- tryCatch(
      httr2::request(url) |>
        httr2::req_headers(Accept = "application/json") |>
        httr2::req_timeout(10) |>
        httr2::req_retry(max_tries = 2) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = FALSE),   # <-- IMPORTANT: keep as list
      error = function(e) NULL
    )
    
    lab <- basename(ror_uri)
    
    # ---- parse name in a scalar-safe way ----
    if (!is.null(jj)) {
      lab <- as.character(jj$"names"[[1]]$value)
    }
    assign(ror_uri, lab, envir = .ror_cache)
    lab
  }
  rors_clean <- unique(na.omit(rors))
  
  if (length(rors_clean) > 0) {
    rors_tbl <- tibble::tibble(specimen_ror = rors_clean) |>
      dplyr::mutate(
        ror_label = purrr::map_chr(specimen_ror, get_ror_label)
      )
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
      
      json_url <- paste0("http://vocabulary.odm2.org/api/v1/medium/", material_id, "/?format=json")
      jj <- httr2::request(json_url) |> httr2::req_perform() |> httr2::resp_body_string()
      mat_label <- jsonlite::fromJSON(jj)$name
      
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
              GRAPH <http://rdfdata.lteritalia.it/graph/samplers> {
                BIND(<", sampler_uri, "> as ?sampler)
                ?sampler rdf:type sosa:Sampler .
                ?sampler rdfs:label ?label .
              }
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
        sampler_label = sampler_res$results$bindings$label$value
      )
    }) |> dplyr::bind_rows()
  } else {
    samplers_tbl <- tibble::tibble(
      specimen_sampler = character(),
      sampler_label    = character()
    )
  }
  # ---- SITE TABLE
  sites_clean <- sites |> purrr::discard(is.na)
  sites_tbl <- if (length(sites_clean) == 0) {
    tibble::tibble(
      specimen_site = character(),
      site_label = character()
    )
  } else {
    lapply(seq_along(sites_clean), function(n) {
      site_url <- sites_clean[n]
      # Detect if it is a location
      if (stringr::str_detect(site_url, "/locations/")) {
        site_id <- sub("https://deims.org/locations/", "", site_url)
        json_url <- paste0("https://deims.org/api/locations/", site_id)
        jj <- httr2::request(json_url) |> httr2::req_perform() |> httr2::resp_body_string()
        site_label <- jsonlite::fromJSON(jj)$properties$title
      } else {
        site_id <- sub("https://deims.org/", "", site_url)
        json_url <- paste0("https://deims.org/api/sites/", site_id)
        jj <- httr2::request(json_url) |> httr2::req_perform() |> httr2::resp_body_string()
        site_label <- jsonlite::fromJSON(jj)$title
      }
      tibble::tibble(
        specimen_site = sites_clean[n],
        site_label = site_label
      )
    }) |> dplyr::bind_rows()
  }
  # merging   
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
  list_specimen <- merge(
    x = list_specimen,
    y = sites_tbl,
    by = "specimen_site",
    all.x = TRUE
  )
  collapse_no_na <- function(x) {
    x_clean <- unique(x[!is.na(x)])
    if (length(x_clean) == 0) NA else paste(x_clean, collapse = "||")
  }
  
  output$vb_samples <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = NROW(list_specimen),
      subtitle = "Samples in the catalogue",
      icon = shiny::icon("vial"),
      color = "aqua"
    )
  })
  output$vb_samplers <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value    = n_samplers(),
      subtitle = "Samplers in the catalogue",
      icon     = shiny::icon("microscope"),
      color    = "purple"
    )
  })
  # ---- NOTIFICATION: number of samples (once per session) ----
  n_samples <- NROW(list_specimen)
  n_samples_rv(n_samples)
  shiny::observeEvent(TRUE, {
    shiny::showNotification(
      paste0("Loaded ", n_samples, " samples."),
      type = if (n_samples == 0) "warning" else "message",
      duration = 4
    )
  }, once = TRUE)
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
        list(
          stringr::str_split(specimen_material, "\\|\\|"),
          stringr::str_split(material_label, "\\|\\|")
        ),
        function(uris_mat, labels_mat) {
          if (all(is.na(uris_mat)) || all(is.na(labels_mat))) return("-")
          if (length(labels_mat) != length(uris_mat)) {
            labels_mat <- rep(labels_mat, length.out = length(uris_mat))
          }
          paste0(
            purrr::map2_chr(uris_mat, labels_mat, ~{
              paste0(
                "<a href='", .x, "' target='_blank'>",
                .y,
                "</a>"
              )
            }),
            collapse = "<br>"
          )
        }
      ),
      `Sampled at site or location` = purrr::pmap_chr(
        list(
          stringr::str_split(specimen_site, "\\|\\|"),
          stringr::str_split(site_label, "\\|\\|")
        ),
        function(uris_site, labels_site) {
          if (all(is.na(uris_site)) || all(is.na(labels_site))) return("-")
          if (length(labels_site) != length(uris_site)) {
            labels_site <- rep(labels_site, length.out = length(uris_site))
          }
          paste0(
            purrr::map2_chr(uris_site, labels_site, ~{
              paste0(
                "<a href='", .x, "' target='_blank'>",
                .y,
                "</a>"
              )
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
      `Sampled at site or location`,
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
  # factors
  shared_specimenData$Sampler <- shared_specimenData$Sampler |>
    factor()
  # output
  output$specimenTbl <- DT::renderDT({
    table_data <- shared_specimenData |>
      sf::st_drop_geometry() |>
      dplyr::select(
        `Specimen name`,
        Created,
        `Sampled at site or location`,
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
          "<b>Sampled at site or location: </b>", `Sampled at site or location`, "<br>",
          "<b>Institution: </b>",    Institution, "<br>",
          "<b>Material: </b>",       `Specimen material`, "<br>",
          "<b>Sampler: </b>",        Sampler, "<br>",
          "<b>Location: </b>",        Location
        )#,
        # clusterOptions = leaflet::markerClusterOptions()
      )
  })
  
  ## ------------------------------------------------------------------
  ## SERVER SENSORS (Systems Type)
  ## ------------------------------------------------------------------
  # data table
  sensorsType_dataset <- "https://fuseki.lteritalia.it/systemsType/query"
  systemsType_query <- "
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
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
    ORDER BY ASC(?l)
  "
  list_systemsType_req <- httr2::request(sensorsType_dataset) |>
    httr2::req_url_query(query = systemsType_query) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Accept = "application/sparql-results+json") |>
    httr2::req_user_agent("ReLTER dev") |>
    httr2::req_retry(max_tries = 3, max_seconds = 120) |>
    httr2::req_perform()
  httr2::resp_check_status(list_systemsType_req)
  list_systemType <- httr2::resp_body_json(list_systemsType_req, simplifyVector = TRUE) |>
    purrr::pluck("results") |>
    tibble::as_tibble() |>
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
    ) |>
    dplyr::select(system_uri, system_label, system_description, system_doc, system_image, component_uri, component_label, manufacturer_name, manufacturer_uri)
  n_types_rv(dplyr::n_distinct(list_systemType$system_uri))
  # ---- VALUE BOX: number of sensors type ----
  output$vb_sensor_types <- shinydashboard::renderValueBox({
    n_types <- dplyr::n_distinct(list_systemType$system_uri)
    shinydashboard::valueBox(
      value = n_types,
      subtitle = "sensors type in the catalogue",
      icon = shiny::icon("thermometer-half"),
      color = "green"
    )
  })
  output$vb_manufacturers <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value    = n_manufacturers(),
      subtitle = "Manufacturers in the catalogue",
      icon     = shiny::icon("industry"),
      color    = "teal"
    )
  })
  
  # Counters of entities
  output$header_counters <- shiny::renderUI({
    ns <- n_samples_rv()
    nsa <- n_samplers()
    nt <- n_types_rv()
    nm <- n_manufacturers()
    nl <- n_locations()
    nsi <- n_sites()
    # nsin <- n_sensors_instance()
    
    ns_txt <- if (is.na(ns))  "…" else format(ns,  big.mark = ",", decimal.mark = ".")
    nsa_txt <- if (is.na(nsa)) "…" else format(nsa, big.mark = ",", decimal.mark = ".")
    nt_txt <- if (is.na(nt))  "…" else format(nt,  big.mark = ",", decimal.mark = ".")
    nm_txt <- if (is.na(nm))  "…" else format(nm,  big.mark = ",", decimal.mark = ".")
    nl_txt <- if (is.na(nl))  "…" else format(nl,  big.mark = ",", decimal.mark = ".")
    nsi_txt <- if (is.na(nsi))  "…" else format(nsi,  big.mark = ",", decimal.mark = ".")
    # nsin_txt <- if (is.na(nsin))  "…" else format(nsin,  big.mark = ",", decimal.mark = ".")
    
    shiny::tagList(
      tags$span(
        class = "elter-counter-title",
        "Summary metrics by resource type:"
      ),
      
      # --- goes to tab "samples"
      shiny::tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('go_tab', 'samples', {priority: 'event'}); return false;",
        class = "elter-counter-link",
        shiny::tags$span(class="elter-counter",
                         shiny::icon("vial"),
                         shiny::tags$span(class="num", ns_txt),
                         shiny::tags$span("samples"))
      ),
      shiny::tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('go_tab', 'samples', {priority: 'event'}); return false;",
        class = "elter-counter-link",
        shiny::tags$span(class="elter-counter",
                         shiny::icon("microscope"),
                         shiny::tags$span(class="num", nsa_txt),
                         shiny::tags$span("samplers"))
      ),
      
      # --- goes to tab "sensors"
      shiny::tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('go_tab', 'sensors', {priority: 'event'}); return false;",
        class = "elter-counter-link",
        shiny::tags$span(class="elter-counter",
                         shiny::icon("thermometer-half"),
                         shiny::tags$span(class="num", nt_txt),
                         shiny::tags$span("sensors type"))
      ),
      shiny::tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('go_tab', 'sensors', {priority: 'event'}); return false;",
        class = "elter-counter-link",
        shiny::tags$span(class="elter-counter",
                         shiny::icon("industry"),
                         shiny::tags$span(class="num", nm_txt),
                         shiny::tags$span("manufacturers"))
      ),
      shiny::tags$a(
        href    = "#",
        onclick = "Shiny.setInputValue('go_tab', 'sensors_instance', {priority: 'event'}); return false;",
        class   = "elter-counter-link",
        shiny::tags$span(
          class = "elter-counter",
          shiny::icon("thermometer"),
          shiny::tags$span(class = "num", if (is.na(n_instances_rv())) "…" else format(n_instances_rv(), big.mark = ",")),
          shiny::tags$span("sensor instances")
        )
      ),
      
      # --- external link (new tab)
      shiny::tags$a(
        href = "https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3",
        target = "_blank",
        class = "elter-counter-link",
        shiny::tags$span(class="elter-counter",
                         shiny::icon("location-dot"),
                         shiny::tags$span(class="num", nl_txt),
                         shiny::tags$span("locations"))
      ),
      shiny::tags$a(
        href = "https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3",
        target = "_blank",
        class = "elter-counter-link",
        shiny::tags$span(class="elter-counter",
                         shiny::icon("tower-observation"),
                         shiny::tags$span(class="num", nsi_txt),
                         shiny::tags$span("sites"))
      )
    )
  })
  
  # ---- NOTIFICATION: number of sensors type (once per session) ----
  n_types <- dplyr::n_distinct(list_systemType$system_uri)
  
  # data table output
  output$sensorTbl <- DT::renderDataTable({
    dfSensorsType <- list_systemType |>
      dplyr::filter(!is.na(manufacturer_name) & manufacturer_name != "") |>
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
      ) |>
      unique() |>
      dplyr::select(system_uri, rdfURL_html, doc, thumb_html, compURL_html, man_html) |>
      dplyr::group_by(system_uri, rdfURL_html, doc, thumb_html, man_html) |>
      dplyr::summarize(compURL_html = stringr::str_c(compURL_html, collapse = "<br> "),
                       .groups = "drop") |>
      dplyr::mutate(
        your_sensor = paste0(
          "<a href='",
          system_uri,
          "' target='_blank' data-toggle='tooltip' title='Use this UUID to reference this system type as a unique identifiers'>",
          system_uri,
          "</a>"
        )
      ) |>
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
  ## ------------------------------------------------------------------
  ## SERVER SENSOR INSTANCES
  ## ------------------------------------------------------------------
  instance_query <- '
    PREFIX sosa:   <http://www.w3.org/ns/sosa/>
    PREFIX ssn:    <http://www.w3.org/ns/ssn/>
    PREFIX dct:    <http://purl.org/dc/terms/>
    PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX dcat:   <http://www.w3.org/ns/dcat#>
    PREFIX schema: <https://schema.org/>
    PREFIX foaf:   <http://xmlns.com/foaf/0.1/>
    PREFIX prov:   <http://www.w3.org/ns/prov#>
    
    SELECT ?uri ?label ?identifier ?serial ?created ?owner_name ?owner_surname ?orcid ?landing_page
    WHERE {
      ?uri a sosa:Sensor .
      OPTIONAL { ?uri rdfs:label      ?label . }
      OPTIONAL { ?uri dct:identifier  ?identifier . }
      OPTIONAL { ?uri schema:serialNumber ?serial . }
      OPTIONAL { ?uri dct:created     ?created . }
      OPTIONAL { ?uri dcat:landingPage ?landing_page . }
      OPTIONAL {
        ?uri dcat:contactPoint ?cp .
        ?cp foaf:givenName   ?owner_name .
        ?cp foaf:familyName  ?owner_surname .
        ?cp foaf:account     ?orcid .
      }
    }
    ORDER BY ?label
    '
  list_instances <- tryCatch({
    resp <- httr2::request(ENDPOINT_INSTANCES) |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Accept"       = "application/sparql-results+json",
        "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8"
      ) |>
      httr2::req_body_form(query = instance_query) |>
      httr2::req_retry(max_tries = 3, max_seconds = 60) |>
      httr2::req_perform()
    
    httr2::resp_body_json(resp, simplifyVector = TRUE) |>
      purrr::pluck("results") |>
      tibble::as_tibble() |>
      dplyr::mutate(
        instance_uri     = bindings$uri$value,
        instance_label   = bindings$label$value,
        instance_id      = bindings$identifier$value,
        serial_number    = bindings$serial$value,
        created          = bindings$created$value,
        owner_name       = bindings$owner_name$value,
        owner_surname    = bindings$owner_surname$value,
        orcid            = bindings$orcid$value,
        landing_page     = bindings$landing_page$value,
        .keep = "used"
      )
  }, error = function(e) {
    shiny::showNotification(
      paste("Error loading sensor instances:", e$message),
      type = "error", duration = 8
    )
    tibble::tibble(
      instance_uri   = character(), instance_label = character(),
      instance_id    = character(), serial_number  = character(),
      created        = character(), owner_name     = character(),
      owner_surname  = character(), orcid          = character(),
      landing_page   = character()
    )
  })
  
  n_instances_rv(dplyr::n_distinct(list_instances$instance_uri))
  
  # Value boxes
  output$vb_sensor_instances <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value    = dplyr::n_distinct(list_instances$instance_uri),
      subtitle = "Sensor instances in the catalogue",
      icon     = shiny::icon("thermometer"),
      color    = "olive"
    )
  })
  
  output$vb_instance_owners <- shinydashboard::renderValueBox({
    n_owners <- dplyr::n_distinct(
      list_instances$orcid[!is.na(list_instances$orcid)]
    )
    shinydashboard::valueBox(
      value    = n_owners,
      subtitle = "Distinct owners",
      icon     = shiny::icon("users"),
      color    = "maroon"
    )
  })
  
  # Notifica
  shiny::observeEvent(TRUE, {
    n <- dplyr::n_distinct(list_instances$instance_uri)
    shiny::showNotification(
      paste0("Loaded ", n, " sensor instances."),
      type     = if (n == 0) "warning" else "message",
      duration = 4
    )
  }, once = TRUE)
  
  # Tabella
  output$instanceTbl <- DT::renderDataTable({
    df <- list_instances |>
      dplyr::mutate(
        `Instance` = paste0(
          "<a href='", instance_uri, "' target='_blank'>", instance_label, "</a>"
        ),
        `UUID`          = instance_id,
        `Serial number` = serial_number,
        `Created`       = stringr::str_sub(created, 1, 10),  # solo data YYYY-MM-DD
        `Owner` = paste0(
          "<a href='", orcid, "' target='_blank'>",
          "<img src='https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png' ",
          "alt='ORCID' style='height:16px; vertical-align:middle; margin-right:4px;'>",
          owner_name, " ", owner_surname,
          "</a>"
        )
      ) |>
      dplyr::select(`Instance`, `UUID`, `Serial number`, `Created`, `Owner`) |>
      dplyr::distinct()
    
    DT::datatable(
      df,
      escape  = FALSE,
      filter  = "top",
      options = list(pageLength = 10),
      caption = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: center;",
        "Table - ", htmltools::em("Sensor instances registered in eLTER-IT")
      )
    )
  })
  }
)}
