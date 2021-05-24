library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Importance of a Formula1 Engine"),
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                "yearInput",
                "Year Range:",
                start = '1950-01-01',
                end = '2020-01-01',
                min = '1950-01-01',
                max = '2020-12-31',
                format = 'yyyy',
                startview = 'decade'
            )
        ),
        
        mainPanel(
            plotOutput("heatmap.start.finish"),
            plotOutput("jitter.speed.finish")
        )
    )
)

server <- function(input, output) {
    library(tidyverse)
    library(lubridate)
    
    results <- read_csv("results.csv")
    constructors <- read_csv("constructors.csv")
    drivers <- read_csv("drivers.csv")
    races <- read_csv("races.csv")
    
    drivers %>%
        select(driverId, driverRef) -> driver.lookup
    constructors %>%
        select(constructorId, constructorRef, own_engine) -> constructor.lookup
    results %>%
        inner_join(., driver.lookup) %>%
        inner_join(., constructor.lookup) -> driver_constructor_results
    races %>%
        select("raceId", "year") -> race_year_lookup
    
    driver_constructor_results %>%
        inner_join(., race_year_lookup) -> driver_constructor_results_year
    
    driver_constructor_results_year %>%
        group_by(constructorRef, driverRef, own_engine) %>%
        filter(fastestLapSpeed != "\\N", position != "\\N") %>% 
        mutate(fastestLapSpeed = as.numeric(fastestLapSpeed),
               position = as.numeric(position)) -> jitter.speed.finish.data
    
    output$heatmap.start.finish <- renderPlot({
        driver_constructor_results_year %>%
            filter(year >= year(input$yearInput[1]),
                   year <= year(input$yearInput[2])) %>%
            group_by(constructorRef, driverRef, own_engine) %>%        
            filter(grid != "\\N", position != "\\N") %>%
            mutate(grid = as.numeric(grid)) %>%
            mutate(position = as.numeric(position)) -> dcr_2020
        dcr_2020 %>%
            ungroup() %>%
            select(grid, position, own_engine) %>%
            filter(grid > 0) %>%
            group_by(grid, position) %>%
            summarise(total_count = n()) -> dcr_totalgp
        dcr_2020 %>%
            ungroup() %>%
            select(grid, position, own_engine) %>%
            filter(grid > 0) %>%
            group_by(grid, position, own_engine) %>%
            summarise(count = n()) %>%
            ungroup() %>%
            full_join(., dcr_totalgp) %>%
            mutate(pct = count / total_count) %>%
            distinct(grid, position, .keep_all = T) -> dcr_pct
        
        dcr_pct$fixedpct <-
            if_else(dcr_pct$own_engine == "N", 1 - dcr_pct$pct, dcr_pct$pct)
        
        dcr_pct %>%
            ggplot() +
            geom_tile(aes(grid, position, fill = fixedpct)) +
            scale_fill_viridis_c() +
            theme_minimal() +
            xlab('Starting Position') +
            ylab('Finishing Position') +
            labs(fill = "% Built Engine", caption = 'Source: Vopani | Kaggle') +
            ggtitle("Starting vs Finishing Position", subtitle = '(available 1950-2020)')
    })
    
    output$jitter.speed.finish <- renderPlot({
        jitter.speed.finish.data %>%
            filter(year >= year(input$yearInput[1]),
                   year <= year(input$yearInput[2])) %>%
            ggplot() +
            geom_jitter(aes(position, fastestLapSpeed, color = own_engine)) +
            ggtitle('Finishing Position vs Fastest Lap Speed', subtitle = '(available 2004-2020)') +
            theme_minimal() +
            xlab('Finishing Position') +
            ylab('Fastest Lap Speed') +
            scale_color_viridis_d() +
            labs(caption = 'Source: Vopani | Kaggle')
    })
    

}

# Run the application
shinyApp(ui = ui, server = server)
