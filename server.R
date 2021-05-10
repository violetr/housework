library(shiny)
library(tidyverse)
library(here)
library(magrittr)
library(lubridate)
library(magrittr)
library(plotly)
#library(reticulate) # don't load it
library(rintrojs)
library(viridis)
library(gt)
library(DT)



median_week <- function(month_i, data_wm) {
    median_week <-
        data_wm %>%
        dplyr::filter(month_name == month_i) %>%
        pull(week_num) %>%
        unique() %>%
        as.integer() %>%
        median() %>%
        round() %>%
        as.integer()
    
    median_week
}


compute_hover <- function(matrix_heatmap_ij, date_ij) {
    hover <- ""
    if (is.na(matrix_heatmap_ij)) {
        hover <- "No data, on"
    } else {
        if (matrix_heatmap_ij == 0) {
            hover <- "No hours, on "
        } else if (matrix_heatmap_ij == 1) {
            hover <- "1 hour, on "
        } else if (matrix_heatmap_ij == -1) {
            hover <- "No data, on"
        } else {
            hover <- paste0(round(matrix_heatmap_ij, 2), " hours, on")
        }
    }
    return(paste0(hover, " ", month(date_ij, TRUE), " ", day(date_ij), ", 2021"))
}

# For flags in table
# answered Dec 14 '17 at 2:49
# hrbrmstr
img_uri <- function(x) { sprintf('<img src="%s" height="25"/>', knitr::image_uri(x)) }

PYTHON_DEPENDENCIES = c('pip', 'numpy', 'scipy')

shinyServer(function(input, output, session) {
    Sys.setlocale("LC_TIME", "English") # for day names
    
    virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
    python_path = Sys.getenv('PYTHON_PATH')
    
    # Create virtual env and install dependencies
    reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
    reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=TRUE)
    reticulate::use_virtualenv(virtualenv_dir, required = T)
    
    #reticulate::use_python(python = 'C:/Users/viole/anaconda3/python.exe', required = T)
    
    scipy = reticulate::import("scipy.stats")
    
    Gender_StatsData <- read_csv("Gender_StatsData.csv")
    
    prop_gender_data <- Gender_StatsData %>%
        filter(`Indicator Code` %in% c("SG.TIM.UWRK.FE", "SG.TIM.UWRK.MA")) %>%
        pivot_longer(cols = 5:ncol(.),names_to = "year", values_to = "proportion of the day") %>%
        pivot_wider(names_from = `Indicator Code`, values_from = `proportion of the day`,
                    id_cols = c(`Country Name`, year))
    
    prop_by_country <- prop_gender_data %>%
        filter(! is.na(SG.TIM.UWRK.FE),
               ! is.na(SG.TIM.UWRK.MA)) %>%
        group_by(`Country Name`) %>%
        slice_max(year) %>%
        ungroup() 
    
    year = seq(ymd("2021-01-01"),ymd('2021-12-31'),by='days')
    
    options_selector <- as.list(prop_by_country$`Country Name`)
    
    
    output$out_country <- renderPrint(input$select_country)
    
    # Steps for tour introduction    
    steps <- data.frame(element = c(NA, 
                                    ".shiny-input-container",
                                    ".plotly",
                                    "body > nav > div > ul > li:nth-child(2) > a",
                                    "body > nav > div > ul > li:nth-child(3) > a"),
                         intro = c("<b>Welcome to Housework Center!</b> <br><br> This application is a proof-of-concept of a Github-like site for housework. An application about unpaid work (in principle same as Github), but this time the invisible work, usually done by women.<br><br> In this application you can visualize how a typical profile of a he/she/they from each country would look based on the average number of hours dedicated to housework. <br><br> The idea to make this app came around when reading the Data Feminism book.",
                                   "Choose a country and a set of pronouns.",
                                   "This is a heatmap that represents the hypothetical number of hours spent on unpaid domestic and care work by a typical person from the chosen country and pronouns. The number of hours for each day is simulated in order to respect the average number reported by World Bank.",
                                   "You can go to this page to compare the average numbers of hours spent on unpaid house and care work between different countries.<br><br> The identity line in the scatter plot represents an equity scenario. <br><br> You can click on any row of the table to highlight the data value in the scatterplot.",
                                   "Click here to see more info about the application, the data and the idea."))
    
    introjs(session ,options = list(steps=steps,
                                   "nextLabel"="Next",
                                   "prevLabel"="Prev",
                                   "skipLabel"="Skip"))
    
    output$country <- renderText({

        if (is.null(input$select_country) || input$select_country=="") {
            return("Argentina (she/her)")
        } else {
            return(input$select_country)
        }
    })
    
    flags_list <- list.files(path = "country-flags/png")
    
    table_comp <- prop_by_country %>%
        mutate(hours_female = SG.TIM.UWRK.FE * 24/100,
               hours_male = SG.TIM.UWRK.MA* 24/100,
               rate = round(hours_female/hours_male, digits = 3),
               hours_female = round(hours_female, 3),
               hours_male = round(hours_male, 3),
               country = purrr::map_chr(str_split(str_to_lower(`Country Name`), ","), 1)) %>%
        mutate(country = case_when(country == "korea" ~ "south korea",
                                   country == "russian federation" ~ "russia",
                                   country == "north macedonia" ~ "macedonia",
                                   country == "oman" ~ "261-oman",
                                   TRUE ~ country)) %>%
        filter(!country %in% c("bhutan", "hong kong sar",
                               "west bank and gaza", "lao pdr",
                               "kyrgyz republic")) %>%
        mutate(file_name = purrr::map_chr(country, ~ flags_list[which(str_detect(str_to_lower(flags_list), str_to_lower(.x)))[1]]),
               flag = paste0("country-flags/png/", file_name),
               flag = purrr::map_chr(flag, img_uri),
               country = case_when(country == "261-oman" ~ "oman",
                                   TRUE ~ country)) %>%
        select(country, flag, hours_female, hours_male, rate) %>%
        arrange(desc(rate)) %>%
        as.data.frame()
    
    
    output$table <- DT::renderDT({
        
        dt = datatable(table_comp, escape = FALSE, colnames = c("country", "flag", "hours her", "hours him", "rate her/him"))
        
        return(dt)

    })
    
    # highlight selected rows in the scatterplot
    output$x2 = renderPlotly({
        s = input$table_rows_selected
        pp = ggplot() +
            geom_point(data = table_comp,
                       mapping = aes(x = hours_female, 
                           y = hours_male,
                           text=sprintf("Country: %s", country))) +
            geom_point(data = table_comp[s, c(1, 3, 4)], 
                       aes(x = hours_female, 
                           y = hours_male,
                           text = sprintf("Country: %s", country)), 
                       color = viridis::magma(5, direction = -1)[2]) +
            geom_line(data = NULL, mapping =
                          aes(x = seq(0, 7, length.out = 100),
                              y = seq(0, 7, length.out = 100)),
                      color = viridis::magma(5, direction = -1)[3]) + 
            scale_x_continuous(limits = c(0, 7), expand = c(0, 0)) +
            scale_y_continuous(limits = c(0, 7), expand = c(0, 0)) +
            coord_fixed() +
            labs(x = "hours her", y = "hours him") +
            theme_minimal()
        ggplotly(pp, tooltip = c("text"))
    })
    
    output$countryprop <- renderText({
        
        if (is.null(input$select_country) || input$select_country=="") {

            country = "Argentina"
            gender = "she/her"
        } else {
            country <- str_trim(str_split(input$select_country, "\\(")[[1]][1])
            gender <- str_remove(str_split(input$select_country, "\\(")[[1]][2],
                                 "\\)")
        }
        if(gender == "she/her") {
            prop_selected = prop_by_country %>%
                filter(`Country Name` == country) %>%
                pull(SG.TIM.UWRK.FE)

            return(paste0("The average number of hours used for these activities is <b>", 
                          round(prop_selected * 24/100, 2),
                          "</b> (World Bank)"))
        } else if (gender == "he/him") {
            prop_selected = prop_by_country %>%
                filter(`Country Name` == country) %>%
                pull(SG.TIM.UWRK.MA)
            return(paste0("The average number of hours used for these activities is <b>", 
                          round(prop_selected * 24/100, 2),
                          "</b> (World Bank)"))
        } else {
            return("No data")
        }
    })
    
    output$countryurl <- renderText({
        
        if (is.null(input$select_country) || input$select_country=="") {
            return(paste0("<a href='https://en.wikipedia.org/wiki/", 
                          "Argentina",
                          "' target='_blank'> https://en.wikipedia.org/wiki/", 
                          "Argentina",
                          "</a>"))
        } else {
            country <- str_trim(str_split(input$select_country, "\\(")[[1]][1])
            return(paste0("<a href='https://en.wikipedia.org/wiki/", 
                          country,
                          "' target='_blank'> https://en.wikipedia.org/wiki/", 
                          country,
                          "</a>"))
        }
    })
    
    output$flagcountry <- renderImage({
        
        if (is.null(input$select_country) || input$select_country=="") {
             country = "Argentina"
        } else {
            country <- str_trim(str_split(input$select_country, "\\(")[[1]][1])
        } 
        
        country = str_split(str_to_lower(country), ",")[[1]][1]
        country = case_when(country == "korea" ~ "south korea",
                            country == "russian federation" ~ "russia",
                            country == "north macedonia" ~ "macedonia",
                            country == "oman" ~ "261-oman",
                            TRUE ~ country)
        
        flags_list <- list.files(path = "country-flags/png")
        file_name <- flags_list[str_detect(str_to_lower(flags_list), country)]
        return(list(src = paste0("country-flags/png/",
                                 file_name),
                    width = "70%",
                    alt = "Flag of the country"))
    }, deleteFile = FALSE)
    

    output$distPlot <- renderPlotly({
        
        
        filter_by <- ""
        if (is.null(input$select_country) || input$select_country=="") {
            filter_by <- "Argentina"
            gender <- "she/her"
        } else {
            filter_by <- str_trim(str_split(input$select_country, "\\(")[[1]][1])
            gender <- str_remove(str_split(input$select_country, "\\(")[[1]][2], "\\)")
        }
        all_dates_period <-
            tibble(gen_date = as_date(as_date("2021-01-03"):as_date("2021-12-25"))) %>%
            mutate(
                month_name = month(gen_date, TRUE),
                week_num = epiweek(gen_date), # to  start on Sun
                week_day = wday(gen_date, TRUE)
            )
        
        base_to_heatmap <- all_dates_period %>%
            select(-month_name, -gen_date)
        
        random_datis = scipy$cosine$rvs(size = 365L)
        if(gender == "she/her") {

            prop_selected = prop_by_country %>%
                filter(`Country Name` == filter_by) %>%
                pull(SG.TIM.UWRK.FE)
        } else if (gender == "he/him") {
            prop_selected = prop_by_country %>%
                filter(`Country Name` == filter_by) %>%
                pull(SG.TIM.UWRK.MA)
            
        } else {
            prop_selected = rep(NA, 365)
        }

        
        data_country_gender = tibble(date = year,
                                     prop_hw = ((random_datis + pi)/(pi/prop_selected*100))[1:365])
        
        data_calendar <- data_country_gender %>%
            mutate(
                month_name = month(date, TRUE),
                week_num = epiweek(date), # to  start on Sun
                week_day = wday(date, TRUE)
            ) %>%
            arrange(date) 
        
        data_to_heatmap <- data_calendar %>%
            select(-month_name, -date) %>%
            mutate(hours = 24 * prop_hw) %>%
            select(-prop_hw)
        
        to_heatmap <-
            base_to_heatmap %>%
            left_join(data_to_heatmap, by = c(
                "week_num" = "week_num",
                "week_day" = "week_day"
            )) %>%
            spread(key = week_num, value = hours) %>%
            as.data.frame() %>%
            column_to_rownames("week_day") %>%
            replace(., is.na(.), -1) # for they/them
        
        # axis labels ####
        
        median_weeks <- purrr::map_int(month.abb, median_week, data_wm = all_dates_period)
        
        setNames(median_weeks, month.abb)
        
        row_days <- rownames(to_heatmap)
        lab_row_days <- rev(rownames(to_heatmap))
        lab_row_days[c(1, 3, 5, 7)] <- rep(" ", 4)
        
        data_wm_worep <-
            all_dates_period %>%
            select(week_num, month_name) %>%
            distinct() %>%
            group_by(week_num) %>%
            summarize(month_name = last(month_name))
        
        column_months <- as.character(data_wm_worep$month_name)

        lab_column_months <- as.character(data_wm_worep$month_name)

        lab_column_months[!data_wm_worep$week_num %in% median_weeks] <- " "
        
        matrix_heatmap <- as.matrix(to_heatmap)
        matrix_heatmap <- matrix_heatmap[ nrow(matrix_heatmap):1, ]
        
        get_date_weeknum_daynum_year <- function(dataset, weeknum, weekday) {
            fechas <- dataset %>%
                dplyr::filter(
                    week_num == weeknum,
                    week_day == weekday
                ) %$%
                gen_date
            return(fechas)
        }
        
        
        week_numbers <- unique(all_dates_period$week_num)
        matrix_dates <- matrix(rep(NA, 7 * ncol(to_heatmap)), nrow = 7)
        for (i in 1:7) {
            for (j in 1:ncol(to_heatmap)) {
                matrix_dates[i, j] <- get_date_weeknum_daynum_year(
                    all_dates_period,
                    week_numbers[j],
                    row_days[i]
                )
            }
        }
        
        matrix_dates <- matrix_dates[nrow(matrix_dates):1, ]
        
        dates_vec <- (lubridate::as_date(as.vector(matrix_dates), origin = lubridate::origin))

        hover_heatmap <- matrix(purrr::map2_chr(
                                                as.vector(matrix_heatmap),
                                                dates_vec,
                                                ~ compute_hover(.x, .y)
                                            ),
                                nrow = 7
                                )

        if (gender == "they/them") {
            mypalette <- c("#e3e3e3") # No data
        } else {
            mypalette <- viridis::magma(5, direction = -1)    
        }
        
        if (gender == "they/them") {
            
            plot_ly(
                z = matrix_heatmap,
                type = "heatmap",
                zmin = 0,
                zmax = 11,
                colors = mypalette, showlegend = FALSE,
                xgap = 0.5, ygap = 0.5,
                hoverinfo = "text",
                text = hover_heatmap
            ) %>%
                layout(
                    yaxis = list(
                        ticktext = lab_row_days, ticks = "",
                        tickmode = "array",
                        tickfont = list(color = "86888A"),
                        tickvals = 0:6,
                        zeroline = FALSE
                    ),
                    xaxis = list(
                        side = "top", ticktext = lab_column_months, ticks = "",
                        tickmode = "array", tickangle = 0,
                        tickfont = list(color = "86888A"),
                        tickvals = 0:51,
                        zeroline = FALSE
                    ),
                    margin = list(
                        r = 67
                    )
                ) %>% 
                hide_colorbar() %>%
                config(displayModeBar = FALSE) 
            
        } else {
        
            plot_ly(
                z = matrix_heatmap,
                type = "heatmap",
                zmin = 0,
                zmax = 11,
                colors = mypalette, xgap = 0.5, ygap = 0.5,
                hoverinfo = "text",
                text = hover_heatmap,
                colorbar = list(thickness =10, len = 1)
            ) %>%
                layout(
                    yaxis = list(
                        ticktext = lab_row_days, ticks = "",
                        tickmode = "array",
                        tickfont = list(color = "86888A"),
                        tickvals = 0:6,
                        zeroline = FALSE
                    ),
                    xaxis = list(
                        side = "top", ticktext = lab_column_months, ticks = "",
                        tickmode = "array", tickangle = 0,
                        tickfont = list(color = "86888A"),
                        tickvals = 0:51,
                        zeroline = FALSE
                    )
                ) %>%
                config(displayModeBar = FALSE)
            }
    })
})

