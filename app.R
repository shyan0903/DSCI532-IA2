library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

# Read in the raw data and subset the data for analysis
df <- readr::read_csv(here::here('data', 'combined-data-for-GDP-per-cap.csv'))

# Define constant values
years <- list("1960" = "1960",
              "1968" = "1968",
              "1978" = "1978",
              "1988" = "1988",
              "1998" = "1998",
              "2008" = "2008",
              "2018" = "2018"
              )
regions <- list(list(label = "All", value = ''),
                list(label = "Asia", value = "Asia"),
                list(label = "Europe", value = "Europe"),
                list(label = "Africa", value = "Africa"),
                list(label = "Americas", value = "Americas"),
                list(label = "Oceania", value = "Oceania"))
    
COUNTRIES <- unique(df$country)

app$layout(
    dbcContainer(
        list(
            dccGraph(id='plot-area'),
            htmlLabel("Zoom in Years: "),
            dccRangeSlider(
                id = 'year_slider',
                min = 1960, 
                max = 2018,
                step = 1,
                value = list(1960, 2018),
                marks = years,
                tooltip = 'placement'
            ),
            htmlLabel("Filter by Geographic Region: "),
            dccDropdown(
                id = 'region_dropdown',
                options = regions,
                value = '',
                multi = TRUE)
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('year_slider', 'value'),
         input('region_dropdown', 'value')),
    
    function(year_range, region_filter) {
        df_sub <- df %>%
            filter(year >= year_range[1] & year <= year_range[2])
        
        if ('' %in% region_filter) {
            p <- df_sub %>%
                group_by(year) %>%
                summarise(income = sum(`GDP total`),
                          pop = sum(population)) %>%
                mutate(income_per_capita = income / pop) %>%
                ggplot(aes(x = year, y = income_per_capita)) +
                geom_line() +
                labs(x = "Year",
                     y = "income Per Capita (in US$)",
                     title = "income Per Capita Has Been Rising") +
                ggthemes::scale_color_tableau()
        } else {
            df_sub <- df_sub %>%
                filter(region %in% region_filter)
            p <- df_sub %>%
                group_by(year, region) %>%
                summarise(income = sum(`GDP total`),
                          pop = sum(population)) %>%
                mutate(income_per_capita = income / pop) %>%
                ggplot(aes(x = year, y = income_per_capita, color=region)) +
                geom_line() +
                labs(x = "Year",
                     y = "income Per Capita (in US$)",
                     title = "Income Per Capita Has Been Rising") +
                ggthemes::scale_color_tableau()
        }
        ggplotly(p)

    }
)

app$run_server(host = '0.0.0.0')
