library(tidyverse)
library(shiny)
library(plotly)
library(htmltools)
library(glue)
library(rintrojs)
library(viridis)
library(DT)
library(fresh)

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

options_selector <- setdiff(as.list(prop_by_country$`Country Name`),
                            c("Bhutan", "Hong Kong SAR, China", 
                              "West Bank and Gaza", "Lao PDR",
                              "Kyrgyz Republic")) # No flags


options_selector <- sort(c(outer(options_selector, 
                                 c(" (she/her)", 
                                   " (he/him)", 
                                   " (they/them)"), 
                                 FUN=paste0)))


shinyUI(
    navbarPage(
        h4(icon("broom"), "Housework Center"),
        tabPanel(
            selectInput("select_country", label = NULL,
                        choices = c("Choose a country"='',options_selector),
                        multiple = FALSE, selectize = TRUE),
            fluidPage(
                use_googlefont("Lato"),
                use_theme(create_theme(
                    theme = "default",
                    bs_vars_font(
                        family_sans_serif = "Lato, Helvetica"
                    )
                )),
                column(4, 
                        style = "display: inline-block; vertical-align: top;",
                        fluidPage(
                            plotOutput("flagcountry",inline = TRUE),
                            br(),
                            br(),
                            textOutput("country"),
                            br(),
                            uiOutput("countryurl")
                        )
                     ),
                     column(6,
                         fluidRow(
                             introjsUI(),
                             h4("Projects"),
                             fluidRow(
                                 shiny::column(5, 
                                               wellPanel(
                                               shiny::a("Washing dishes",
                                                        href = "https://en.wikipedia.org/wiki/Dishwashing",
                                                        target="_blank"))),
                                 shiny::column(5,
                                               wellPanel(
                                                   shiny::a("Cooking",
                                                          href = "https://en.wikipedia.org/wiki/Cooking",
                                                          target="_blank")))
                             ),
                             fluidRow(
                                 shiny::column(5, 
                                               wellPanel(shiny::a("Child care",
                                                           href = "https://en.wikipedia.org/wiki/Child_care",
                                                           target="_blank"))),
                                 shiny::column(5,
                                               wellPanel(shiny::a("Laundry",
                                                          href = "https://en.wikipedia.org/wiki/Laundry",
                                                          target="_blank")))
                             ),
                         h4("Number of hours spent on unpaid domestic and care work"),
                         plotlyOutput("distPlot", height = "160px", width = "550px"),
                         uiOutput("countryprop"),
                         hr()
                    )
                 )),
            tags$head(tags$style(HTML(".selectize-input {height: 8px; width: 300px; font-size: 12px; margin-bottom: 0px} .navbar-nav{height: 15px; width: 600px;  margin-bottom: 15px} .nav>li>a{padding-top: 10px; padding-bottom: 0px} .nav>li>.active>a {padding-top: 10px; padding-bottom: 0px; background-color: black}    .container-fluid>li>.active {padding-top: 10px; padding-bottom: 0px; background-color: black} .navbar-default .navbar-nav>.active>a {padding-top: 10px; padding-bottom: 0px; background-color: #f8f8f8} .navbar-default .navbar-nav>.active>a:focus {padding-top: 10px; padding-bottom: 0px; background-color: #f8f8f8} .navbar-default .navbar-nav>.active>a:hover {padding-top: 10px; padding-bottom: 0px; background-color: #f8f8f8} .h4{margin-top: 0px; vertical-align: middle;} .navbar{vertical-align: middle;} .navbar-brand{vertical-align: middle;} #country {font-size:20px;
                   color:black;
                   font-weight: bold;
                   display:block; } h4 {color:black;
                   font-weight: bold;} .navbar-brand{padding-top: 5px;} #countryprop {color:#777;
                   } .row {align-items: center;} .li:active {background-color: #f8f8f8;} .well{border-radius: 0; background-color: white} a {color: black} .well>a:hover {font-weight: bold; color: black} .shiny-bound-output>a {color: #777} .shiny-bound-output>a:hover {color: black}" )))
        ),
        tabPanel("COMPARE", 
                 fluidRow(
                     HTML("&nbsp;&nbsp;&nbsp;&nbsp; The source of the data is World Bank and the reported value is the one of the last year found in the Gender Stats data set.<br><br>")
                 ),
                 fluidRow(
                     column(6, DTOutput("table")),
                     column(6, plotlyOutput('x2', height = 500))
                 )
                 ),
        tabPanel("LEARN MORE", 
                 
                 fluidRow(
                     shiny::column(6, 
                 wellPanel(style = "height:390px",
                     icon("broom"),
                     HTML("<b> ABOUT</b> <br><br> <b>Housework Center</b> is an exercise to imagine a GitHub-like platform related to housework and care. <br><br> It is inpired in the 'Show your work' chapter of the <a href = 'https://data-feminism.mitpress.mit.edu/' target = '_blank'>Data Feminism</a> book where they discuss unpaid work and proper attribution to all the chain of the processes.<br><br> GitHub is an open source platform, a huge repository of (usually) unpaid work. As discussed in the book, all this free work has to be visible in some way. Therefore, the site is full of statistics about coders: the how much, the who and the when is depicted in multiple heatmaps, area and line charts. Even more, each project has a ranking of contributors. <br><br> We mimic this concept this time with unpaid house and care work, usually done by women.<br><br>") 
                 )),
                 shiny::column(6, 
                 wellPanel(style = "height:390px",
                     icon("github"),
                     HTML("<b> CODE</b> <br><br> This app was written in R with the <code>shiny</code> package. <br><br> The source code of this app is available in this (ironic) Github repository. <br><br> The application uses a library of Python through the <code>reticulate</code> package. You can check this resource in order to setup your <code>.Rprofile</code> to have access to this feature.<br><br> The tour from the beginning is generated with the <code>rintrojs</code> package that is a wrapper of the one of javascript.<br><br> The interactive plots are generated using the <code>plotly</code> package.<br><br>")
                 )
                 )),
                 fluidRow(
                     shiny::column(6, 
                 wellPanel(style = "height:140px",
                     icon("thumbs-up"),
                     HTML("<b> ATTRIBUTIONS</b> <br><br> <ul> <li></b> <b>Data:</b> the data is simulated based on average values from the Gender Stats World Bank data <a href = 'https://data.worldbank.org/' target = '_blank'>https://data.worldbank.org/</a></li> <li><b>Flag pictures:</b> icons made by <a href='https://www.freepik.com' title='Freepik'>Freepik</a> from <a href='https://www.flaticon.com/' title='Flaticon'>www.flaticon.com</a></li></ul>")    
                 )),
                 shiny::column(6, 
                               wellPanel(style = "height:140px",
                                   icon("twitter"),
                                   HTML("<b> CONTACT</b> <br><br> <b>Hi!</b> My name is Violeta and you can reach me out on <a href = 'https://twitter.com/violetrzn' target = '_blank'>twitter</a>.<br>")    
                               ))
                 ),
                 
                 tags$head(tags$style(HTML(".selectize-input {height: 8px; width: 300px; font-size: 12px; margin-bottom: 0px} .navbar-nav{height: 15px; width: 600px;  margin-bottom: 15px} .nav>li>a{padding-top: 10px; padding-bottom: 0px} .nav>li>.active>a {padding-top: 10px; padding-bottom: 0px; background-color: black}    .container-fluid>li>.active {padding-top: 10px; padding-bottom: 0px; background-color: black} .navbar-default .navbar-nav>.active>a {padding-top: 10px; padding-bottom: 0px; background-color: #f8f8f8} .navbar-default .navbar-nav>.active>a:focus {padding-top: 10px; padding-bottom: 0px; background-color: #f8f8f8} .navbar-default .navbar-nav>.active>a:hover {padding-top: 10px; padding-bottom: 0px; background-color: #f8f8f8} .h4{margin-top: 0px; vertical-align: middle;} .navbar{vertical-align: middle;} .navbar-brand{vertical-align: middle;} #country {font-size:20px;
                   color:black;
                   font-weight: bold;
                   display:block; } .row {align-items: center;}" )))), 
        windowTitle = HTML("Housework Center")
    )
)

