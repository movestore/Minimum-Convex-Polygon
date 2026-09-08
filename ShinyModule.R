library(move2)
library(adehabitatHR)
library(shiny)
library(zip)
library(shinyBS)
library(sf)
library(pals)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(callr)
library(shinybusy)
library(dplyr)
library(jsonlite)
library(shinycssloaders)

`%||%` <- function(x, y) if (is.null(x)) y else x

##### Interface ######
shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    # Small corner spinner whenever the server is busy
    shinybusy::add_busy_spinner(spin = "fading-circle", color = "#0275D8",
                                position = "bottom-left", timeout = 800,
                                onstart = FALSE),
    titlePanel("Minimum Convex Polygon (MCP)"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("perc"), "Percentage of points included in MCP", min = 1, max = 100, value = 95, width = "100%"),
        #checkboxGroupInput(ns("animal_selector"), "Select Track:", choices = NULL),
        uiOutput(ns("animals_ui")),
        tags$div(style = "display:none;", textInput(ns("animals_json"), label = NULL, value = "")),
        downloadButton(ns("save_html"),"Download as HTML", class = "btn-sm"),
        downloadButton(ns("save_png"), "Save Map as PNG", class = "btn-sm"),
        # downloadButton(ns("download_geojson"), "Download MCP as GeoJSON", class = "btn-sm"),
        downloadButton(ns("download_kmz"), "Download as KMZ", class = "btn-sm"),
        bsTooltip(id=ns("download_kmz"), title="Format for GoogleEarth", placement = "bottom", trigger = "hover", options = list(container = "body")),
        downloadButton(ns("download_gpkg"), "Download as GPKG", class = "btn-sm"),
        bsTooltip(id=ns("download_gpkg"), title="Shapefile for QGIS/ArcGIS", placement = "bottom", trigger = "hover", options = list(container = "body")),
        downloadButton(ns("download_mcp_table"), "Download MCP Areas Table", class = "btn-sm"),
        width = 3),
      mainPanel(
        div(
          style = "position:relative;",
          withSpinner(leafletOutput(ns("leafmap"), height = "85vh")),
          conditionalPanel(
            condition = "output.map_ready !== true", ns = ns,
            style = "position:absolute; top:0; left:0; right:0; bottom:0; background:#fff; z-index:500;",
            div(style = "position:absolute; top:50%; left:50%; transform:translate(-50%,-50%); text-align:center;",
                shinybusy::spin_kit(spin = "fading-circle", color = "#0275D8",
                                    style = "width:60px; height:60px; margin:0 auto;"),
                tags$p("Calculating MCPs\u2026", style = "margin-top:16px;")))
        ),
        width = 9
      )
    )
  )
}



#####server######

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
  
  # exclude all individuals with less than 5 locations
  data_filtered <- reactive({
    req(data)
    data %>%
      group_by(mt_track_id()) %>%
      filter(n() >= 5) %>%
      ungroup()
  })
  
  all_ids_vec <- reactive({
    req(data_filtered())
    sort(unique(as.character(mt_track_id(data_filtered()))))
  })
  output$animals_ui <- renderUI({
    animal_choices <- all_ids_vec()
    restored_sel <- isolate(input$animal_selector)
    sel <- if (!is.null(restored_sel)) restored_sel else animal_choices
    
    checkboxGroupInput( ns("animal_selector"),"Select Track:",choices = animal_choices,selected = sel )
  })
  
  applied_animals <- reactiveVal(NULL)
  init_applied <- reactiveVal(FALSE)
  
  observeEvent(input$animal_selector, {
    applied_animals(as.character(input$animal_selector %||% character(0)))
    init_applied(TRUE)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  observeEvent(input$animal_selector, {
    vals <- input$animal_selector %||% character(0)
    updateTextInput(session,"animals_json", value = jsonlite::toJSON(vals, auto_unbox = FALSE))
  }, ignoreInit = TRUE)
  
  selected_data <- reactive({
    req(init_applied())
    
    sel <- applied_animals()
    df  <- data_filtered()
    
    if (is.null(sel) || length(sel) == 0) return(df[0, ])
    
    filter_track_data(df, .track_id = sel)
  })
  
  # Compute the MCP 
  mcp_cal <- reactive({
    req(input$perc)
    data_sel <- selected_data()
    shiny::validate(shiny::need(nrow(data_sel) > 0, "Select at least one track."))
    
    crs_proj <- mt_aeqd_crs(data_sel, center = "center", units = "m")
    sf_data_proj <- st_transform(data_sel, crs_proj) 
    sf_data_proj$id <- mt_track_id(sf_data_proj)
    sp_data_proj <- as_Spatial(sf_data_proj[,'id'])
    sp_data_proj <- sp_data_proj[,(names(sp_data_proj) %in% "id")] 
    sp_data_proj$id <- make.names(as.character(sp_data_proj$id),allow_=F)
    
    data_mcp <- adehabitatHR::mcp(sp_data_proj, input$perc, "m", "km2")
    
    sf_mcp <- st_as_sf(data_mcp) %>% 
      rename(track_id = id) %>%
      st_transform(4326)
    sf_mcp$track_id <- as.character(sf_mcp$track_id)
    
    data_sel <-  mutate_track_data(data_sel, track_id= make.names(data.frame(mt_track_data(data_sel)[,mt_track_id_column(data_sel)])[,1],allow_=F)) ## adding column 'track_id' to data
    
    return(list(data_mcp = sf_mcp, track_lines = mt_track_lines(data_sel)))
    
  })
  
  
  
  ##leaflet map####
  
  # Fixed opacity for the MCP fills; the track lines stay solid.
  layer_opacity <- 0.4
  
  # Groups offered in the layers control. Kept in variables because the
  # downloads need to tell a basemap apart from an overlay when they read the
  # visible groups back from the live map.
  base_groups    <- c("TopoMap", "Aerial", "OpenStreetMap")
  overlay_groups <- c("Tracks", "MCPs")
  
  # The map is built in two parts. base_map() holds everything that never
  # depends on the inputs (tiles, scale bar, layers control); add_data_layers()
  # holds everything that does. renderLeaflet() below draws the base only, so
  # moving the slider updates the data layers through leafletProxy() instead of
  # rebuilding the widget -- rebuilding is what reset the chosen basemap, the
  # overlay checkboxes and the zoom on every slider move.
  base_map <- function() {
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addScaleBar(position = "topleft") %>%
      addLayersControl(
        baseGroups = base_groups,
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # layerId on the legend so the proxy can replace it instead of stacking one
  # legend per redraw.
  add_data_layers <- function(map, mcp_dat) {
    track_lines <- mcp_dat$track_lines
    sf_mcp <- mcp_dat$data_mcp
    ids <- unique(c(sf_mcp$track_id, track_lines$track_id))
    pal <- colorFactor(palette = pals::glasbey(), domain = ids)
    
    map %>%
      addPolylines(data = track_lines, color = ~pal(track_lines$track_id),
                   weight = 3, opacity = 1, group = "Tracks") %>%
      addPolygons(data = sf_mcp, fillColor = ~pal(track_id),color = "black",fillOpacity = layer_opacity,
                  weight = 2,label = ~track_id,group = "MCPs") %>%
      
      addLegend(position = "bottomright",pal = pal,values = ids,title = "Track",
                layerId = "track_legend")
  }
  
  # Keeps the legend scrollable when there are more tracks than fit on screen.
  legend_scroll_js <- "function(el, x) {
        function cap(lg) {
          lg.style.maxHeight = '55vh';
          lg.style.overflowY = 'auto';
          L.DomEvent.disableScrollPropagation(lg);
        }
        function capAll(root) {
          Array.prototype.forEach.call(root.querySelectorAll('.info.legend'), cap);
        }
        capAll(el);
        new MutationObserver(function(muts) {
          Array.prototype.forEach.call(muts, function(m) {
            Array.prototype.forEach.call(m.addedNodes, function(n) {
              if (n.nodeType !== 1) return;
              if (n.classList.contains('legend')) cap(n); else capAll(n);
            });
          });
        }).observe(el, { childList: true, subtree: true });
      }"
  
  # What the user is actually looking at. Leaflet reports the currently visible
  # groups and the current view back to Shiny as input$leafmap_groups /
  # _center / _zoom, so the downloads can reproduce the basemap picked in the
  # layers control, the overlays still ticked and the pan/zoom, instead of
  # always writing out the defaults.
  map_view <- reactive({
    groups <- input$leafmap_groups
    picked <- intersect(base_groups, groups)
    
    list(
      base     = if (length(picked) > 0) picked[1] else base_groups[1],
      overlays = if (is.null(groups)) overlay_groups else intersect(overlay_groups, groups),
      center   = input$leafmap_center,
      zoom     = input$leafmap_zoom
    )
  })
  
  # Full standalone widget, used by the HTML and PNG downloads only.
  mmap <- reactive({
    mcp_dat <- mcp_cal()
    view <- map_view()
    
    m <- base_map() %>% add_data_layers(mcp_dat)
    
    # Same section of the world as on screen. Before the live map has reported
    # a view (nothing rendered yet) fall back to the extent of the selection.
    m <- if (!is.null(view$center) && !is.null(view$zoom)) {
      m %>% setView(lng = view$center$lng, lat = view$center$lat, zoom = view$zoom)
    } else {
      bounds <- as.vector(st_bbox(selected_data()))
      m %>% fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    }
    
    # Drop the unticked overlays and switch to the chosen basemap. This has to
    # come after addLayersControl(), which on its own leaves the first basemap
    # of the list showing.
    m %>%
      hideGroup(c(setdiff(base_groups, view$base),
                  setdiff(overlay_groups, view$overlays))) %>%
      showGroup(view$base) %>%
      onRender(legend_scroll_js)
  })
  

  map_ready <- reactiveVal(FALSE)

  observe({
    req(!map_ready())
    req(nrow(selected_data()) >0)
    mcp_cal()
    map_ready(TRUE)
  })

  output$leafmap <- renderLeaflet({
    req(map_ready())
    isolate(mmap())
  })


  output$map_ready <- reactive(map_ready())
  outputOptions(output, "map_ready", suspendWhenHidden = FALSE)
  
  # Redraw the data layers whenever the percentage or the track selection
  # changes. The view is left alone here on purpose.
  observe({
    proxy <- leafletProxy("leafmap", session) %>%
      clearGroup("Tracks") %>%
      clearGroup("MCPs") %>%
      removeControl("track_legend")
    
    if (nrow(selected_data()) == 0) {
      proxy %>% addControl("Select at least one track.", position = "topright",
                           layerId = "empty_msg")
      return(invisible(NULL))
    }
    
    proxy %>%
      removeControl("empty_msg") %>%
      add_data_layers(mcp_cal())
  })
  
  # Re-frame only when the selected tracks change: the bounds do not depend on
  # the slider, so this keeps the user's pan/zoom while they drag it.
  observeEvent(selected_data(), {
    d <- selected_data()
    req(nrow(d) > 0)
    bounds <- as.vector(st_bbox(d))
    leafletProxy("leafmap", session) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  })
  
  
  ###download the table of mcp
  output$download_mcp_table <- downloadHandler(
    filename = function() paste0("MCPs_", input$perc, "_areas.csv"),
    content = function(file) {
      mcp_df <- as.data.frame(mcp_cal()$data_mcp)
      df <- data.frame(TrackID = mcp_df$track_id, Area_km2 = mcp_df$area, MCP_percent = input$perc)
      write.csv(df, file, row.names = FALSE) })
  
  
  
  ### save map as HTML
  output$save_html <- downloadHandler(
    filename = function() paste0("MCPs_", input$perc, ".html"),
    content = function(file) {
      saveWidget(widget = mmap(),file=file) })
  
  
  ### save map as PNG
  output$save_png <- downloadHandler(
    filename = function() paste0("MCPs_",input$perc,".png"),
    content = function(file) {
    
    shinybusy::show_modal_spinner(spin = "fading-circle", text = "Saving PNG…")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      # Render the map to an HTML file first. selfcontained = FALSE avoids the
      # pandoc dependency (the browser loads the local file + sidecar directly).
      html_file <- tempfile(fileext = ".html")
      saveWidget(mmap(), file = html_file, selfcontained = FALSE)
      html_file <- normalizePath(html_file, winslash = "/", mustWork = TRUE)

      # webshot2/chromote drive headless Chrome over the SAME global `later`
      # event loop that Shiny is already running. Calling it directly from a
      # Shiny handler re-enters that loop and deadlocks (the R process spins at
      # ~100% CPU, the screenshot never completes, and the download surfaces as
      # a gateway 500 / "connection prematurely closed"). Running it in a
      # separate R process via callr gives chromote its own event loop and
      # avoids the deadlock.
      ok <- tryCatch(
        callr::r(
          function(html_file, out_file) {
            # chromote only adds --no-sandbox / --disable-dev-shm-usage when it
            # can detect that it runs inside a container, and it detects that
            # by looking for /.dockerenv or the string "docker" in
            # /proc/self/cgroup. Neither marker exists on the MoveApps
            # runtime, so Chrome starts sandboxed and with the default 64 MB
            # /dev/shm; it is then killed as soon as it opens the page, which
            # surfaces as "Session and underlying target have been closed".
            # Passing the flags explicitly is harmless outside a container.
            chromote::set_chrome_args(unique(c(
              "--no-sandbox",
              "--disable-dev-shm-usage",
              "--disable-gpu",
              chromote::default_chrome_args()
            )))
            webshot2::webshot(url = html_file, file = out_file, vwidth = 1000, vheight = 800, delay = 2)
            file.exists(out_file) && file.size(out_file) > 0
          },
          args = list(html_file = html_file, out_file = file)
        ),
        error = function(e) {
          # The callr error text carries the chromote/Chrome cause; keep it in
          # the app log, but do not let the raw message be the only feedback.
          message("PNG export failed: ", conditionMessage(e))
          FALSE
        })

      if (!isTRUE(ok)) {
        showNotification(
          "Could not render the map to PNG on this server. Please use 'Download as HTML' instead.",
          type = "error", duration = 10)
        stop("PNG export failed: headless Chrome could not render the map.")
      }
    })
  
  
  ###download shape as kmz  
  output$download_kmz <- downloadHandler(
    filename = function() paste0("MCPs_", input$perc, ".kmz"),
    content = function(file) {
      temp_kmz <- tempdir()
      mcp_shape <- st_as_sf(mcp_cal()$data_mcp)
      kml_path <- file.path(temp_kmz, "mcp.kml")
      st_write(mcp_shape, kml_path, driver="KML", delete_dsn = TRUE)
      zip::zip(zipfile = file, files = kml_path, mode = "cherry-pick")})
  
  
  # ###download shape as GeoJSON###
  # output$download_geojson <- downloadHandler(
  #   filename = paste0("MCPs_",input$perc,".geojson"),
  #   content = function(file) {
  #     mcp_l <- mcp_cal()
  #     mcp_shape <- st_as_sf(mcp_l$data_mcp)
  #     track_lines <- mcp_l$track_lines
  #     ids <- unique(mcp_shape$individual_name_deployment_id)
  #     pal <- colorFactor(palette = pals::cols25(), domain = ids)
  #     mcp_shape$`fill` <- pal(mcp_shape$individual_name_deployment_id)
  #     st_write(mcp_shape, file, driver = "GeoJSON", delete_dsn = TRUE)  })
  
  
  ###download shape as GeoPackage (GPKG)
  output$download_gpkg <- downloadHandler(
    filename = function() paste0("MCPs_", input$perc, ".gpkg"),
    content = function(file) {
      mcp_shape <- st_as_sf(mcp_cal()$data_mcp)
      st_write(mcp_shape, file, driver = "GPKG", delete_dsn = TRUE)} )
  
  
  return(reactive({ current() }))
}