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
years <- list("1901" = "1901",
              "1908" = "1908",
              "1918" = "1918", 
              "1928" = "1928",
              "1938" = "1938",
              "1948" = "1948",
              "1958" = "1958",
              "1968" = "1968",
              "1978" = "1978",
              "1988" = "1988",
              "1998" = "1998",
              "2008" = "2008",
              "2018" = "2018"
              )
INCOME_UNIT <- 10000000
regions <- list("Asia" = "Asia",
                "Europe" = "Europe",
                "Africa" = "Africa",
                "Americas" = "Americas",
                "Oceania" = "Oceania"
                )
    
COUNTRIES <- unique(df$country)

app$layout(
    dbcContainer(
        list(
            dccGraph(id='plot-area'),
            htmlLabel("Zoom in Years: "),
            dccRangeSlider(
                id = 'year_slider',
                min = 1901, 
                max = 2018,
                value = list(1901, 2018),
                marks = years
            )
            # ,
            # htmlLabel("Filter by Geographic Region: "),
            # dccDropdown(
            #     id = 'region_dropdown',
            #     options = regions , 
            #     value = '')
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('year_slider', 'value')
         # ,
         # input('region_dropdown', 'value')
         ),
    
    function(year_range) {
    
        p <- df %>%
            filter(year >= year_range[1] & year <= year_range[2]) %>%
            group_by(year) %>%
            summarise(income = sum(income),
                      pop = sum(population)) %>%
            mutate(income_per_capita = income * INCOME_UNIT / pop) %>%
            ggplot(aes(x = year, y = income_per_capita)) +
            geom_line() +
            labs(x = "Year",
                 y = "Income Per Capita (in US$)",
                 title = "Income Per Capita Has Been Rising") +
            ggthemes::scale_color_tableau()

        ggplotly(p)
        

    }
    # function(time) {
    #     df_filter_year <- df %>% 
    #         filter(year >= years[time[1] + 1] & year <= years[time[2] + 1])
    #     
    #     if (region == '') {

    #     } else {
    #         p <- df_filter_year %>%
    #             filter(region == region) %>%
    #             group_by(year) %>%
    #             summarise(income = sum(income),
    #                       pop = sum(population)) %>%
    #             mutate(income_per_capita = income * INCOME_UNIT / pop) %>%
    #             ggplot(aes(x = year, y = income_per_capita)) +
    #             geom_line()
    #         ggplotly(p)
    #     }
    # }
)

app$run_server(host = '0.0.0.0')
