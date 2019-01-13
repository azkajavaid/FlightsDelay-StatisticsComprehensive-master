
library(shiny)
library(plyr)
library(dplyr)
library(mosaic)
library(base)
library(plotly)
library(ggplot2)
library(nycflights13)
library(lubridate)
library(igraph)
require(visNetwork)
library(gridExtra)
library(grid)
library(leaflet)
library(ggthemes)

load("fullDivision.Rda")
load("data2N.Rda")
load("USAirSum32N.Rda")
load("stateDelayFirstHalf.Rda")
load("stateDelaySecondHalf.Rda")
load("data3FirstHalf.Rda")
load("data3SecondHalf.Rda")

group18 <- rbind(stateDelayFHalf, stateDelaySHalf)
data32 <- rbind(data3FHalf, data3SHalf)

data(flights)
data(weather)

ui <- navbarPage(
  "Flights Analysis", inverse = TRUE,
  tabPanel(
    "Table Summary",
    sidebarLayout(
      sidebarPanel(
        tags$head(
          tags$style(HTML("
                                                body {
                                                background-color: #663399;
                                                color: #330033;
                                                }
                                                "))
        ),

        strong("Flights Analysis Application analyzes United States flights
                                    delays data from 2008-2016. The application analyzes flights with departure delay greater 
                                     than 90 minutes, assessing delays by state, division, region and 
                                     carrier status over time."),

        HTML("<br><br>"),

        strong("Table shows the average departure delay in minutes from 2008-2016 for all flights with departure delay over 90 minutes with the specified origin and destination airport. 
                                     Alternatively, origin and destination states can also be specified."),

        HTML("<br><br>"),
        selectInput(
          "origin", "Choose a origin airport:",
          choices = sort(unique(data2$OriginAirport)), selected = "John F. Kennedy International Airport"
        ),

        selectInput(
          "destination", "Choose a destination airport:",
          choices = sort(unique(data2$DestAirport)), selected = "San Francisco International Airport"
        ),

        actionButton("go1", "Show Airport Level Flights", style = "color: #ffffff;background-color: #663399;margin: 4px;"),

        selectInput(
          "originState", "Choose a origin state:",
          choices = sort(unique(data2$OriginFState)), selected = "New York"
        ),

        selectInput(
          "destState", "Choose a destination state:",
          choices = sort(unique(data2$DestFState)), selected = "California"
        ),

        actionButton("go2", "Show State Level Flights", style = "color: #ffffff;background-color: #663399;margin: 4px;"),

        width = 3
      ),
      mainPanel(
        dataTableOutput("view"),
        verbatimTextOutput("avgDelayState"),
        dataTableOutput("view2")
      )
    )
  ),

  tabPanel(
    "Paths Analysis",
    sidebarLayout(
      sidebarPanel(
        strong("Map shows the average departure delay in minutes from 2008-2016 for all United States flights with departure delay over 90 minutes. Click a
                                     point to display state and airport level departure delay."),
        HTML("<br><br>"),
        selectInput(
          "responseP", "Choose a response predictor:",
          choices = as.character(c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016))
        ),

        selectInput(
          "responseP2", "Choose an Origin state:",
          choices = sort(as.character(unique(group12$OriginFState))), "New York"
        ),
        selectInput(
          "responseP3", "Choose a Destination state:",
          choices = sort(as.character(unique(group12$DestFState))), "California"
        ),
        actionButton("go111", "Map Departure Delays", style = "color: #ffffff;background-color: #663399;margin: 4px;"),
        br(),
        width = 3
      ),
      mainPanel(
        leafletOutput("map4"),
        dataTableOutput("lat")
      )
    )
  ),
  navbarMenu(
    "Delay by State, Division and Region from 2008-2016",
    tabPanel(
      "State",
      sidebarLayout(
        sidebarPanel(
          strong("Top plot shows departure delay in minutes for all airports in the specified state from 2008-2016 for all flights with departure delay over 90 minutes.
                                               Bottom plot displays the aggregate departure delay for the specified state from 2008-2016."),
          HTML("<br><br>"),
          selectInput(
            "stateDest", "Choose a state:",
            choices = sort(as.character(unique(group12$OriginFState))), "New York"
          ),
          br(),
          width = 2
        ),
        mainPanel(
          plotlyOutput("plot31"),
          plotlyOutput("plot32")
        )
      )
    ),
    tabPanel(
      "Midwest",
      sidebarLayout(
        sidebarPanel(
          strong("Top pair of plots shows average departure delay in minutes for Midwestern United States over 2008-2016. 
                                 Bottom pair of plots shows aggregated departure delay from 2008 to 2016. 
                                  Left graph in both pairs corresponds to East North Central while right 
                                  graph corresponds to West North Central."), width = 0.5
        ),
        mainPanel(
          plotlyOutput("plot34"),
          plotlyOutput("plot35"), width = 12.5
        )
      )
    ),
    tabPanel(
      "Northeast",
      sidebarLayout(
        sidebarPanel(
          strong("Top pair of plots shows average departure delay in minutes for Northeastern United States over 2008-2016. 
                                      Bottom pair of plots shows aggregated departure delay from 2008 to 2016. 
                                      Left graph in both pairs corresponds to Middle Atlantic while right 
                                      graph corresponds to New England."), width = 0.5
        ),
        mainPanel(
          plotlyOutput("plot36"),
          plotlyOutput("plot37"), width = 12.5
        )
      )
    ),
    tabPanel(
      "South",
      sidebarLayout(
        sidebarPanel(
          strong("Top three plots shows average departure delay in minutes for Southern United States over 2008-2016. 
                                    Bottom three plots show aggregated departure delay from 2008 to 2016. 
                                    Left graph in both trios corresponds to East South Central, middle graph corresponds to 
                                    West South Central and right graph to South Atlantic."), width = 0.5
        ),
        mainPanel(
          plotlyOutput("plot38"),
          plotlyOutput("plot39"), width = 12.5
        )
      )
    ),
    tabPanel(
      "West",
      sidebarLayout(
        sidebarPanel(
          strong("Top pair of plots shows average departure delay in minutes for Western United States over 2008-2016. 
                                    Bottom pair of plots shows aggregated departure delay from 2008 to 2016. 
                                    Left graph in both pairs corresponds to Pacific region while right 
                                    graph corresponds to Mountain region."), width = 0.5
        ),
        mainPanel(
          plotlyOutput("plot40"),
          plotlyOutput("plot41"), width = 12.5
        )
      )
    )
  ),
  navbarMenu(
    "Delay by State and Region Over Time and Carrier",
    tabPanel(
      "Delay by Hour",
      sidebarLayout(
        sidebarPanel(
          strong("Specify state to observe departure delay by hour and region and aggregated departure delay for specified state from 2008 to 2016."),
          HTML("<br><br>"),
          selectInput(
            "stateDest21", "Choose a state:",
            choices = sort(as.character(unique(group18$OriginFState))), "New York"
          ),
          width = 2
        ),
        mainPanel(
          plotlyOutput("plot61"),
          plotlyOutput("plot42"), width = 10
        )
      )
    ),
    tabPanel(
      "Day of Week",
      sidebarLayout(
        sidebarPanel(
          strong("Departure delay by week."),
          HTML("<br><br>"),
          selectInput(
            "stateDest22", "Choose a state:",
            choices = sort(as.character(unique(group18$OriginFState))), "New York"
          ),
          width = 2
        ),
        mainPanel(
          plotlyOutput("plot43"),
          plotlyOutput("plot44")
        )
      )
    ),
    tabPanel(
      "Weekend Status",
      sidebarLayout(
        sidebarPanel(
          strong("Departure delay by weekend status."),
          HTML("<br><br>"),
          selectInput(
            "stateDest23", "Choose a state:",
            choices = sort(as.character(unique(group18$OriginFState))), "New York"
          ),
          width = 2
        ),
        mainPanel(
          plotlyOutput("plot45"),
          plotlyOutput("plot46")
        )
      )
    ),
    tabPanel(
      "Month",
      sidebarLayout(
        sidebarPanel(
          strong("Departure delay by month."),
          HTML("<br><br>"),
          selectInput(
            "stateDest24", "Choose a state:",
            choices = sort(as.character(unique(group18$OriginFState))), "New York"
          ),
          width = 2
        ),
        mainPanel(
          plotlyOutput("plot47"),
          plotlyOutput("plot48")
        )
      )
    ),
    tabPanel(
      "Season",
      sidebarLayout(
        sidebarPanel(
          strong("Departure delay by season."),
          HTML("<br><br>"),
          selectInput(
            "stateDest25", "Choose a state:",
            choices = sort(as.character(unique(group18$OriginFState))), "New York"
          ),
          width = 2
        ),
        mainPanel(
          plotlyOutput("plot49"),
          plotlyOutput("plot50")
        )
      )
    ),
    tabPanel(
      "Carrier",
      sidebarLayout(
        sidebarPanel(
          strong("Departure delay by carrier."),
          HTML("<br><br>"),
          selectInput(
            "stateDest26", "Choose a state:",
            choices = sort(as.character(unique(group18$OriginFState))), "New York"
          ),
          width = 0.5
        ),
        mainPanel(
          plotlyOutput("plot51"),
          plotlyOutput("plot52"), width = 12
        )
      )
    )
  ),
  tabPanel(
    "Aggregate Delay by Region",
    sidebarLayout(
      sidebarPanel(
        strong("Map shows the average departure delay over 90 minutes for United States from 2008 to 2016. Hover over
        a point to display state and airport level departure delay."),
        HTML("<br><br>"),

        selectInput(
          "responseM", "Choose a response predictor:",
          choices = as.character(c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016))
        ),

        selectInput(
          "responseM2", "Choose a US region:",
          choices = sort(as.character(c(
            "East North Central", "East South Central",
            "Middle Atlantic", "Mountain", "New England", "Pacific",
            "South Atlantic",
            "West North Central", "West South Central"
          )))
        ),

        br()
      ),
      mainPanel(
        plotlyOutput("plot3")
      )
    )
  ),
  tabPanel(
    "References",
    verbatimTextOutput("Reference")
  )
)

server <- shinyServer(function(input, output) {
  df_subset <- eventReactive(input$go1, {
    a <- data2 %>% filter(OriginAirport == input$origin & DestAirport == input$destination)
    return(a)
  })

  df_subset2 <- eventReactive(input$go2, {
    b <- data2 %>% filter(OriginFState == input$originState & DestFState == input$destState) %>% arrange(desc(meanDelay))
    return(b)
  })

  df_weekend <- eventReactive(input$go, {
    DatCarrier <- data3 %>%
      filter(year >= input$yearInitial & year <= input$yearEnd) %>%
      mutate(yearF = as.factor(year)) %>%
      dplyr::group_by_("yearF", input$response) %>%
      dplyr::summarise(MeanDep = mean(dep_delay))
    return(DatCarrier)
  })


  output$plot <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      dfweek <- df_weekend()
      incProgress(0.6, detail = "Building plot")
      p <- ggplot(dfweek, aes_string(x = "yearF", y = "MeanDep", group = input$response, color = input$response)) +
        geom_point() + geom_line(size = 1) + ggtitle(paste("Average departure delay overtime by", input$response))
      incProgress(0.4, detail = "Finishing...")
      ggplotly(p)
    })
  })

  output$plot3 <- renderPlotly({
    USAirSum32 <- subset(USAirSum32, year == input$responseM & Division == input$responseM2)
    USAirSum33 <- USAirSum32
    USAirSum33 <- plyr::rename(USAirSum33, replace = c("mean2" = "AverageDelay"))
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa"),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )

    USAirSum33$AverageDelay <- round(USAirSum33$AverageDelay, 2)
    p <- plot_geo(
      USAirSum33, locationmode = "USA-states",
      sizes = c(1, 250)
    ) %>%
      add_markers(
        x = ~ OriginLong, y = ~ OriginLat,
        color = ~ AverageDelay, alpha = 0.8,
        text = ~ paste(
          OriginFState, "<br />",
          AverageDelay, "<br />",
          OriginAirport
        )
      ) %>%
      layout(
        title = (paste(
          "Mean Departure Delay by Airport for",
          unique(USAirSum33$year), "and",
          unique(USAirSum33$Division)
        )),
        geo = g
      )
    ggplotly(p)
  })


  datMap2 <- eventReactive(input$go111, {
    group13 <- subset(group12, year == input$responseP & OriginFState == input$responseP2 & DestFState == input$responseP3)
    group13$OriginLong <- round(group13$OriginLong, 4)
    group13$OriginLat <- round(group13$OriginLat, 4)

    group13$DestLong <- round(group13$DestLong, 4)
    group13$DestLat <- round(group13$DestLat, 4)
    return(group13)
  })

  output$map4 <- renderLeaflet({
    withProgress(message = "Application loading", value = 0, {
      group13 <- datMap2()
      map4 <- leaflet() %>% addTiles()
      incProgress(0.7, detail = "Building plot")
      if (nrow(group13) != 0) {
        for (i in 1:nrow(group13))
        {
          long <- cbind(group13[i, "OriginLong"], group13[i, "DestLong"])
          lat <- cbind(group13[i, "OriginLat"], group13[i, "DestLat"])
          long1 <- as.list(data.frame(t(long)))
          lat1 <- as.list(data.frame(t(lat)))

          map4 <- map4 %>% addTiles(options = providerTileOptions(noWrap = TRUE)) %>% addCircleMarkers(
            lng = long1$t.long.,
            lat = lat1$t.lat., group = "circles", color = "#660099", fillColor = "black",
            weight = (group13[i, "meanDelay"]) / 20
          ) %>% addPolylines(
            lng = long1$t.long., lat = lat1$t.lat.,
            color = "#660099"
          )
        }
        return(map4)
      }
      else {
        return(map4)
      }
      incProgress(0.3, detail = "Finishing...")
    })
  })

  observeEvent(input$map4_marker_click, {
    group13 <- datMap2()
    p <- input$map4_marker_click
    clat <- p$lat
    clng <- p$lng
    clat <- round(clat, 4)
    clng <- round(clng, 4)
    group14 <- subset(group13, (OriginLong == clng & OriginLat == clat) | (DestLong == clng & DestLat == clat))
    output$latitude <- renderText({
      return(clat)
    })
    output$longitude <- renderText({
      return(clng)
    })
    group15 <- group14 %>% select(
      OriginAirport, OriginFState, DestAirport,
      DestFState, meanDelay
    )
    group15 <- plyr::rename(group15, replace = c("OriginFState" = "OriginState"))
    group15 <- plyr::rename(group15, replace = c("DestFState" = "DestState"))
    output$lat <- renderDataTable({
      return(group15)
    })
  })

  # Midwest plot
  output$plot34 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      M <- subset(group12, OrigRegion == "Midwest")
      MGroup <- M %>% group_by(OriginFState, year, OrigDivision) %>% summarise(AvgDelay = mean(meanDelay))

      MGroup <- plyr::rename(MGroup, replace = c("OriginFState" = "State"))

      MidAt <- subset(MGroup, OrigDivision == "East North Central")
      NewEng <- subset(MGroup, OrigDivision == "West North Central")

      incProgress(0.6, detail = "Building plot")

      gra <- ggplot(data = MidAt, aes(x = year, y = AvgDelay)) + geom_line(aes(colour = State), size = 1) +
        theme(legend.position = "right") + theme(legend.title = element_blank()) + theme_fivethirtyeight() + scale_colour_few()
      g <- ggplotly(gra)


      gra1 <- ggplot(data = NewEng, aes(x = year, y = AvgDelay)) + geom_line(aes(colour = State), size = 1) +
        theme(legend.position = "right") + ylab("Year") + theme(legend.title = element_blank()) + theme_fivethirtyeight() + scale_colour_few() +
        xlab("AvgDelay")
      g1 <- ggplotly(gra1)

      p <- subplot(g, g1)
      incProgress(0.3, detail = "Finishing...")
      p
    })
  })


  output$plot35 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      M <- subset(group12, OrigRegion == "Midwest")
      MGroup <- M %>% group_by(OriginFState, year, OrigDivision) %>% summarise(AvgDelay = mean(meanDelay))

      MidAt <- subset(MGroup, OrigDivision == "East North Central")
      NewEng <- subset(MGroup, OrigDivision == "West North Central")

      incProgress(0.7, detail = "Building plot")

      MidAt2 <- MidAt %>% group_by(OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))

      NewEng2 <- NewEng %>% group_by(OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))

      incProgress(0.3, detail = "Finishing...")
      g2 <- ggplot(MidAt2, aes(x = OriginFState, y = AvgDelay)) +
        geom_bar(fill = "#330033", stat = "identity", width = 0.4) + theme_fivethirtyeight() + scale_colour_few()
      g2 <- ggplotly(g2)
      g2

      g3 <- ggplot(NewEng2, aes(x = OriginFState, y = AvgDelay)) +
        geom_bar(fill = "#330033", stat = "identity", width = 0.5) + theme_fivethirtyeight() + scale_colour_few()
      g3 <- ggplotly(g3)
      g3

      incProgress(0.6, detail = "Finishing...")
      p2 <- subplot(g2, g3)
      p2
    })
  })

  output$plot36 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      NE <- subset(group12, OrigRegion == "Northeast")
      NEGroup <- NE %>% group_by(OriginFState, year, OrigDivision) %>% summarise(AvgDelay = mean(meanDelay))

      MidAt <- subset(NEGroup, OrigDivision == "Middle Atlantic")
      NewEng <- subset(NEGroup, OrigDivision == "New England")

      MidAt <- plyr::rename(MidAt, replace = c("OriginFState" = "State"))
      gra <- ggplot(data = MidAt, aes(x = year, y = AvgDelay)) + geom_line(aes(colour = State), size = 1) +
        theme(legend.position = "right") +
        theme(legend.title = element_blank()) + theme_fivethirtyeight() + scale_colour_few()

      incProgress(0.7, detail = "Building plot")

      g <- ggplotly(gra)
      g

      NewEng <- plyr::rename(NewEng, replace = c("OriginFState" = "State"))
      gra1 <- ggplot(data = NewEng, aes(x = year, y = AvgDelay)) + geom_line(aes(colour = State), size = 1) +
        theme(legend.position = "right") +
        theme(legend.title = element_blank()) + theme_fivethirtyeight() + scale_colour_few()

      g1 <- ggplotly(gra1)

      incProgress(0.3, detail = "Finishing...")
      subplot(g, g1)
    })
  })

  output$plot37 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      NE <- subset(group12, OrigRegion == "Northeast")
      NEGroup <- NE %>% group_by(OriginFState, year, OrigDivision) %>% summarise(AvgDelay = mean(meanDelay))

      MidAt <- subset(NEGroup, OrigDivision == "Middle Atlantic")
      NewEng <- subset(NEGroup, OrigDivision == "New England")

      MidAt2 <- MidAt %>% group_by(OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))

      NewEng2 <- NewEng %>% group_by(OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))

      incProgress(0.7, detail = "Building plot")

      g2 <- ggplot(MidAt2, aes(OriginFState)) +
        geom_bar(fill = "#330033", aes(weight = AvgDelay), width = 0.4) + theme_fivethirtyeight() + scale_colour_few()

      g2 <- ggplotly(g2)

      g3 <- ggplot(NewEng2, aes(OriginFState)) +
        geom_bar(fill = "#330033", aes(weight = AvgDelay), width = 0.4) + theme_fivethirtyeight() + scale_colour_few()
      g3 <- ggplotly(g3)

      incProgress(0.3, detail = "Finishing...")
      subplot(g2, g3)
    })
  })

  output$plot38 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      NE <- subset(group12, OrigRegion == "South")
      NEGroup <- NE %>% group_by(OriginFState, year, OrigDivision) %>% summarise(AvgDelay = mean(meanDelay))

      NEGroup <- plyr::rename(NEGroup, replace = c("OriginFState" = "State"))

      MidAt <- subset(NEGroup, OrigDivision == "East South Central")
      NewEng <- subset(NEGroup, OrigDivision == "West South Central")
      NewSouth <- subset(NEGroup, OrigDivision == "South Atlantic")

      incProgress(0.7, detail = "Building plot")

      gra <- ggplot(data = MidAt, aes(x = year, y = AvgDelay)) + geom_line(aes(colour = State), size = 1) +
        theme(legend.position = "right") + theme(legend.title = element_blank()) + theme_fivethirtyeight() + scale_colour_hue()

      g <- ggplotly(gra)

      gra1 <- ggplot(data = NewEng, aes(x = year, y = AvgDelay)) + geom_line(aes(colour = State), size = 1) +
        theme(legend.position = "right") + theme(legend.title = element_blank()) + theme_fivethirtyeight() + scale_colour_hue()

      g1 <- ggplotly(gra1)

      gra2 <- ggplot(data = NewSouth, aes(x = year, y = AvgDelay)) + geom_line(aes(colour = State), size = 1) +
        theme(legend.position = "right") + theme(legend.title = element_blank()) + theme_fivethirtyeight() + scale_colour_hue()
      g2 <- ggplotly(gra2)

      incProgress(0.3, detail = "Finishing...")
      subplot(g, g1, g2)
    })
  })

  output$plot39 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      NE <- subset(group12, OrigRegion == "South")
      NEGroup <- NE %>% group_by(OriginFState, year, OrigDivision) %>% summarise(AvgDelay = mean(meanDelay))

      MidAt <- subset(NEGroup, OrigDivision == "East South Central")
      NewEng <- subset(NEGroup, OrigDivision == "West South Central")
      NewSouth <- subset(NEGroup, OrigDivision == "South Atlantic")

      MidAt2 <- MidAt %>% group_by(OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))

      NewEng2 <- NewEng %>% group_by(OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))

      NewSouth2 <- NewSouth %>% group_by(OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))

      incProgress(0.7, detail = "Building plot")

      g2 <- ggplot(MidAt2, aes(OriginFState)) + geom_bar(fill = "#330033", aes(weight = AvgDelay), width = 0.4) + theme_fivethirtyeight() + scale_colour_few()
      g2 <- ggplotly(g2)

      g3 <- ggplot(NewEng2, aes(OriginFState)) + geom_bar(fill = "#330033", aes(weight = AvgDelay), width = 0.4) + theme_fivethirtyeight() + scale_colour_few()
      g3 <- ggplotly(g3)

      g4 <- ggplot(NewSouth2, aes(OriginFState)) + geom_bar(fill = "#330033", aes(weight = AvgDelay), width = 0.4) + theme_fivethirtyeight() + scale_colour_few()
      g4 <- ggplotly(g4)

      incProgress(0.3, detail = "Finishing...")

      subplot(g2, g3, g4)
    })
  })

  output$plot40 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      M <- subset(group12, OrigRegion == "West")
      MGroup <- M %>% group_by(OriginFState, year, OrigDivision) %>% summarise(AvgDelay = mean(meanDelay))

      MGroup <- plyr::rename(MGroup, replace = c("OriginFState" = "State"))

      MidAt <- subset(MGroup, OrigDivision == "Pacific")
      NewEng <- subset(MGroup, OrigDivision == "Mountain")

      incProgress(0.6, detail = "Building plot")

      gra <- ggplot(data = MidAt, aes(x = year, y = AvgDelay)) + geom_line(aes(colour = State), size = 1) +
        theme(legend.position = "right") + theme(legend.title = element_blank()) + theme_fivethirtyeight() + scale_colour_hue()
      g <- ggplotly(gra)

      gra1 <- ggplot(data = NewEng, aes(x = year, y = AvgDelay)) + geom_line(aes(colour = State), size = 1) +
        theme(legend.position = "right") + theme(legend.title = element_blank()) + theme_fivethirtyeight() + scale_colour_hue()
      g1 <- ggplotly(gra1)

      incProgress(0.3, detail = "Finishing...")
      subplot(g, g1)
    })
  })

  output$plot41 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      M <- subset(group12, OrigRegion == "West")
      MGroup <- M %>% group_by(OriginFState, year, OrigDivision) %>% summarise(AvgDelay = mean(meanDelay))

      MidAt <- subset(MGroup, OrigDivision == "Pacific")
      NewEng <- subset(MGroup, OrigDivision == "Mountain")

      MidAt2 <- MidAt %>% group_by(OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))

      NewEng2 <- NewEng %>% group_by(OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))

      incProgress(0.6, detail = "Building plot")

      g2 <- ggplot(MidAt2, aes(OriginFState)) + geom_bar(fill = "#330033", aes(weight = AvgDelay), width = 0.4) +
        theme_fivethirtyeight() + scale_colour_few()
      g2 <- ggplotly(g2)

      g3 <- ggplot(NewEng2, aes(OriginFState)) + geom_bar(fill = "#330033", aes(weight = AvgDelay), width = 0.4) +
        theme_fivethirtyeight() + scale_colour_few()
      g3 <- ggplotly(g3)

      incProgress(0.3, detail = "Finishing...")
      subplot(g2, g3)
    })
  })

  output$plot31 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group13 <- subset(group12, OriginFState == input$stateDest)
      group14 <- group13 %>% group_by(OriginAirport, year, OriginFState) %>% summarise(AvgDelay = mean(meanDelay))
      group14$OriginAirport <- as.factor(group14$OriginAirport)
      incProgress(0.6, detail = "Building plot")
      plot <- ggplot(data = group14, aes(x = year, y = AvgDelay)) + geom_line(size = 1) +
        aes(colour = OriginAirport) + theme(legend.position = "none") +
        # labs(title=paste("Departure Delay for Airports in", input$stateDest, sep = " ")) +
        theme(legend.position = "right") + theme_fivethirtyeight() + scale_colour_hue()
      # scale_fill_manual(values=topo.colors(13))
      incProgress(0.4, detail = "Finishing...")
      ggplotly(plot)
    })
  })


  output$plot32 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group13 <- subset(group12, OriginFState == input$stateDest)
      group14 <- group13 %>% group_by(OriginAirport, year, OriginFState) %>% summarise(AvgDelay = mean(meanDelay))
      group14$OriginAirport <- as.factor(group14$OriginAirport)
      incProgress(0.6, detail = "Building plot")
      group15 <- group14 %>% group_by(year, OriginFState) %>% summarise(AvgDelay = mean(AvgDelay))
      graph <- ggplot(data = group15, aes(x = year, y = AvgDelay)) + geom_line(color = "#330033", size = 1) +
        labs(title = paste("Departure Delay for", input$stateDest, sep = " ")) + theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.6, detail = "Finishing...")
      ggplotly(graph)
    })
  })

  output$view <- renderDataTable(df_subset())

  output$avgDelayState <- renderText({
    dataDel <- df_subset2()
    meanDepDelay <- mean(dataDel$meanDelay)
    text <- paste("Average Departure Delay from ", dataDel$OriginFState[1], " to ", dataDel$DestFState[1], ": ", round(meanDepDelay, 3), sep = "")
    return(text)
  })

  output$view2 <- renderDataTable(df_subset2())

  output$plot61 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      incProgress(0.6, detail = "Building plot")
      group19 <- group18 %>% group_by(hour, OrigRegion) %>% summarise(AvgDelay = mean(dep_delay))
      r1 <- ggplot(data = group19, aes(x = hour, y = AvgDelay, group = OrigRegion)) +
        geom_line(aes(colour = OrigRegion), size = 1) + theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.3, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot42 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group20 <- subset(group18, OriginFState == input$stateDest21)
      group20 <- subset(group18, OriginFState == "New York")
      group19 <- group20 %>% group_by(hour) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = hour, y = AvgDelay, group = 1)) +
        geom_line(size = 1, color = "#330033") +
        labs(title = paste("Departure Delay for", input$stateDest21, sep = " ")) + theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.4, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot43 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group18 <- plyr::rename(group18, replace = c("OrigRegion" = "Region"))
      group19 <- group18 %>% group_by(week, Region) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = week, y = AvgDelay, group = Region)) +
        geom_line(aes(colour = Region), size = 1) + theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.4, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot44 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group20 <- subset(group18, OriginFState == input$stateDest22)
      group19 <- group20 %>% group_by(week) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = week, y = AvgDelay, group = 1)) +
        geom_line(size = 1, color = "#330033") +
        labs(title = paste("Departure Delay for", input$stateDest22, sep = " ")) +
        theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.4, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot45 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group18 <- plyr::rename(group18, replace = c("OrigRegion" = "Region"))
      group19 <- group18 %>% group_by(weekend, Region) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.7, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = weekend, y = AvgDelay, group = Region)) +
        geom_line(aes(colour = Region), size = 1) + theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.3, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot46 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group20 <- subset(group18, OriginFState == input$stateDest23)
      group19 <- group20 %>% group_by(weekend) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = weekend, y = AvgDelay, group = 1)) +
        geom_line(size = 1, color = "#330033") +
        labs(title = paste("Departure Delay for", input$stateDest23, sep = " ")) +
        theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.6, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot47 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group18 <- plyr::rename(group18, replace = c("OrigRegion" = "Region"))
      group19 <- group18 %>% group_by(month, Region) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = month, y = AvgDelay, group = Region)) +
        geom_line(aes(colour = Region), size = 1) +
        theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.4, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot48 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group20 <- subset(group18, OriginFState == input$stateDest24)
      group19 <- group20 %>% group_by(month) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = month, y = AvgDelay, group = 1)) +
        geom_line(size = 1, color = "#330033") +
        labs(title = paste("Departure Delay for", input$stateDest24, sep = " ")) + theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.4, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot49 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group18 <- plyr::rename(group18, replace = c("OrigRegion" = "Region"))
      group19 <- group18 %>% group_by(season, Region) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = season, y = AvgDelay, group = Region)) +
        geom_line(aes(colour = Region), size = 1) + theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.4, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot50 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group20 <- subset(group18, OriginFState == input$stateDest25)
      group19 <- group20 %>% group_by(season) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = season, y = AvgDelay, group = 1)) +
        geom_line(size = 1, color = "#330033") +
        labs(title = paste("Departure Delay for", input$stateDest25, sep = " ")) +
        theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.4, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot51 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group18 <- plyr::rename(group18, replace = c("OrigRegion" = "Region"))
      group19 <- group18 %>% group_by(carrier, Region) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = carrier, y = AvgDelay, group = Region)) +
        geom_line(aes(colour = Region), size = 1) +
        theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.4, detail = "Finishing...")
      ggplotly(r1)
    })
  })

  output$plot52 <- renderPlotly({
    withProgress(message = "Application loading", value = 0, {
      group20 <- subset(group18, OriginFState == input$stateDest26)
      group19 <- group20 %>% group_by(carrier) %>% summarise(AvgDelay = mean(dep_delay))
      incProgress(0.6, detail = "Building plot")
      r1 <- ggplot(data = group19, aes(x = carrier, y = AvgDelay, group = 1)) +
        geom_line(size = 1, color = "#330033") +
        labs(title = paste("Departure Delay for", input$stateDest26, sep = " ")) +
        theme_fivethirtyeight() + scale_colour_few()
      incProgress(0.4, detail = "Finishing...")
      ggplotly(r1)
    })
  })
  output$Reference <- renderPrint({
    sessionInfo()
  })
})

shinyApp(ui = ui, server = server)