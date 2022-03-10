library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(dplyr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# Read in the raw data and subset the data for analysis
df <- readr::read_csv(here::here('data', 'world-data-gapminder_raw.csv')) %>% 
    select(country, year, population, region, income)

# Define constant values
YEAR_MIN <- 1901
YEAR_MAX <- 2018
YEAR_INTERVAL <- 10
INCOME_UNIT <- 1000000
REGIONS <- unique(df$region)
COUNTRIES <- unique(df$country)

app$layout(
    dbcContainer(
        list(
            dccGraph(id='plot-area'),
            dccDropdown(
                id='region_dropdown',
                options = REGIONS, 
                value='')
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('region_dropdown', 'value')),
    function(region) {
        p <- df %>% 
            group_by(year) %>% 
            summarise(income = sum(income),
                      pop = sum(population)) %>% 
            mutate(income_per_capita = income * INCOME_UNIT / pop) %>% 
            ggplot(aes(x = year, y = income_per_capita)) +
            geom_line() 
        ggplotly(p)
    }
)

app$run_server(host = '0.0.0.0')
