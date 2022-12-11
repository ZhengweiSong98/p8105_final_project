library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)

#Read in dataset
map_covid =
    read_csv("./data/map_data.csv")

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  sliderInput("range", "Total Cases", min(map_covid$cumulative_cases), max(map_covid$cumulative_cases),
                              value = range(map_covid$cumulative_cases), step = 0.1
                  ),
                  selectInput("colors", "Color Scheme",
                              rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                  ),
                  checkboxInput("legend", "Show legend", TRUE)
    )
)

server <- function(input, output, session) {

    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        map_covid[map_covid$cumulative_cases >= input$range[1] & map_covid$cumulative_cases <= input$range[2],]
    })

    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <- reactive({
        colorNumeric(input$colors, map_covid$cumulative_cases)
    })

    output$map <- renderLeaflet({


        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet(map_covid) %>% addTiles() %>%
            fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })

    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        # Prepare the text for the tooltip:
        mytext <- paste(
            "County: ", map_covid$county, "<br/>",
            "Unemployment Rate(%): ", map_covid$unemployment, "<br/>",
            "ICU Beds per 100 people: ", map_covid$icu_bed*100, "<br/>",
            "Death Rate(%): ", map_covid$death_rate, sep="") %>%
            lapply(htmltools::HTML)

        pal <- colorpal()

        leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(radius = ~cumulative_cases/10, weight = 1, color = "#777777",
                       fillColor = ~pal(cumulative_cases), fillOpacity = 0.7, popup = ~paste(cumulative_cases), label = mytext
            )
    })

    # Use a separate observer to recreate the legend as needed.
    observe({
        proxy <- leafletProxy("map", data = map_covid)

        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% addLegend(position = "bottomright",
                                pal = pal, values = ~cumulative_cases, title = "Toal Cases"
            )
        }
    })
}

shinyApp(ui, server)
