tab_project_ui <- tabPanel(
  "项目",
  fluidRow(
    column(1, div("当前项目:")),
    column(
      3,
      verbatimTextOutput('tab_project_text_current', placeholder = TRUE)
    ),
    column(1, disabled(
      actionButton("tab_project_btn_select", "更换项目为")
    )),
    column(4, verbatimTextOutput('tab_project_text_select'))
    
  ),
  hr(),
  DTOutput("tab_project_table"),
  hr(),
  fluidRow(
    column(2, div(
      class = "inline", textInput("tab_project_add_name", label = "英文名")
    )),
    column(2, div(
      class = "inline", textInput("tab_project_add_name_cn", label = "中文名")
    )),
    column(2, div(
      class = "inline", textInput("tab_project_add_owner", label = "Owner")
    )),
    column(1, shinyDirButton("tab_project_add_path", "选择目录", "选择项目目录")),
    column(4, verbatimTextOutput('tab_project_add_path_text')),
    column(1, actionButton("tab_project_btn_add", "新建项目")),
  ),
  fluidRow(
    selectInput(
      "tab_project_select_debug",
      "log level",
      choices = c(
        "INFO" = "INFO",
        "WARNING" =
          "WARNING",
        "ERROR" =
          "ERROR",
        "DEBUG" =
          "DEBUG"
      ),
      selected = "INFO"
    )
    
    
  )
  
)