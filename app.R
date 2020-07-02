
# param -------------------------------------------------------------------
path <- "D:\\OneDrive\\R_packages\\psbc_risk_shiny"

setwd(path)
options(shiny.reactlog=TRUE)
# library -----------------------------------------------------------------


library(shiny)
library(shinyFiles)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(DT)
library(fs)
library(logging)
library(scorecard)
library(tools)
library(openxlsx)
library(stringr)
library(plotly)
# config object -----------------------------------------------------------
source("config.R", encoding = "UTF-8")
source("tooltips_ui.R", encoding = "UTF-8")
# UI ----------------------------------------------------------------------
# source navbar's tabPanel
source("tab_project_ui.R", encoding = "UTF-8") #项目
source("tab_data_ui.R", encoding = "UTF-8") #数据
source("tab_var_ui.R", encoding = "UTF-8") #变量
source("tab_psi_ui.R", encoding = "UTF-8") #PSI
source("tab_bin_ui.R", encoding = "UTF-8") #项目
source("tab_scorecard_ui.R", encoding = "UTF-8") #评分卡



ui <- tagList(
    useShinyjs(),
    includeCSS("styles.css"),
    extendShinyjs(script = "script.js"),
    tags$head(
        tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                 .inline .form-group{display: table-row;}")
    ),
    navbarPage(
        "风险管理部",
        tab_project_ui,
        tab_data_ui,
        tab_var_ui,
        tab_psi_ui,
        tab_bin_ui,
        tab_scorecard_ui,
        tooltips_taglist,
        id = "navbar"
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    source("general_op.R", local = T, encoding = "UTF-8")
    
    source("tab_project_op.R",
           local = T,
           encoding = "UTF-8")
    source("tab_data_op.R",
           local = T,
           encoding = "UTF-8")
    source("tooltips_op.R", local = T, encoding = "UTF-8") # conditionPanel必须在server中加tooltips
}

# Run the application
shinyApp(ui = ui, server = server)
