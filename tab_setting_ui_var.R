# part 1 ------------------------------------------------------------------
tab_setting_ui_var <- fluidRow(
  fluidRow(column(2,h2("vars")),
           ),
  fluidRow(
    DTOutput("tab_setting_var_table")
  )
  
  
)