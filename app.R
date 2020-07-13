
# param -------------------------------------------------------------------
path <- "D:\\OneDrive\\R_packages\\psbc_risk_shiny"

setwd(path)
options(shiny.reactlog=TRUE)
# library -----------------------------------------------------------------
library(shiny)
library(shinyFiles)
library(shinyjs)
library(shinyBS)
library(DT)
library(tidyverse)
library(readxl)
library(fs)
library(logging)
library(scorecard)
library(tools)
library(openxlsx)
library(plotly)
library(rlang)
library(data.table)
# config object -----------------------------------------------------------
for (file in list.files("functions",full.names=TRUE)) {
    source(file,local=TRUE, encoding = "UTF-8")
}
source("config.R",local=TRUE, encoding = "UTF-8")
source("tooltips_ui.R",local=TRUE, encoding = "UTF-8")
# UI ----------------------------------------------------------------------
# source navbar's tabPanel
source("tab_project_ui.R",local=TRUE, encoding = "UTF-8") 
source("tab_data_ui.R",local=TRUE, encoding = "UTF-8")
source("tab_setting_ui.R",local=TRUE, encoding = "UTF-8")
source("tab_var_ui.R",local=TRUE, encoding = "UTF-8")
source("tab_psi_ui.R",local=TRUE, encoding = "UTF-8") 
source("tab_bin_ui.R",local=TRUE, encoding = "UTF-8")
source("tab_ml_ui.R",local=TRUE, encoding = "UTF-8")
source("tab_report_ui.R",local=TRUE, encoding = "UTF-8")
source("tab_scorecard_ui.R",local=TRUE, encoding = "UTF-8")



ui <- tagList(
    useShinyjs(),
    includeCSS("styles.css"),
    extendShinyjs(script = "script.js"),
    tags$head(
        tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                 .inline .form-group{display: table-row;}")
    ),
    tags$style(type="text/css", "body {padding-top: 35px;}"),
    navbarPage(
        "风险管理部",
        tab_project_ui,
        tab_data_ui,
        tab_setting_ui,
        tab_var_ui,
        tab_bin_ui,
        tab_scorecard_ui,
        tab_psi_ui,
        tab_ml_ui,
        tab_report_ui,
        tooltips_taglist,
        id = "navbar",
        position="fixed-top"
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
    source("tab_setting_op.R",
           local = T,
           encoding = "UTF-8")
    source("tab_var_op.R",
           local = T,
           encoding = "UTF-8")
    source("tab_bin_op.R",
           local = T,
           encoding = "UTF-8")
    source("tooltips_op.R", local = T, encoding = "UTF-8") # conditionPanel cannot addtooltips by ui?
}

# Run the application
shinyApp(ui = ui, server = server)
