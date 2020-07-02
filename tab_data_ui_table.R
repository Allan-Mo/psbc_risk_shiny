

tab_data_ui_table_rdo <- radioButtons(
  "tab_data_rdo_tbltype",
  label = NULL,
  choices = list("多数据集" = "split", "单数据集" = "single"),
  selected = "split"
  ,
  inline = TRUE
)

tab_data_ui_table_cond1 <-
  conditionalPanel(condition = "input.tab_data_rdo_tbltype == 'single' ",
                   fluidRow(column(
                     4,
                     shinyFilesButton(
                       "tab_data_btn_tbl_single",
                       "单数据集",
                       title = NULL,
                       multiple = FALSE
                     )
                     
                   ),
                   column(
                     8, verbatimTextOutput("tab_data_text_tbl_single",placeholder = TRUE)
                   )))
tab_data_ui_table_cond2 <-
  conditionalPanel(
    condition = "input.tab_data_rdo_tbltype == 'split' ",
    fluidRow(column(
      4,
      shinyFilesButton(
        "tab_data_btn_tbl_train",
        "训练集",
        title = NULL,
        multiple = FALSE
      )
    )
    , column(
      8, verbatimTextOutput("tab_data_text_tbl_train",placeholder = TRUE)
    )),
    fluidRow(column(
      4,
      shinyFilesButton(
        "tab_data_btn_tbl_test",
        "测试集",
        title = NULL,
        multiple = FALSE
      )
    )
    , column(8, verbatimTextOutput(
      "tab_data_text_tbl_test",placeholder = TRUE
    ))),
    fluidRow(column(
      4,
      shinyFilesButton(
        "tab_data_btn_tbl_validate",
        "验证集",
        title = NULL,
        multiple = FALSE
      )
    )
    , column(
      8, verbatimTextOutput("tab_data_text_tbl_validate",placeholder = TRUE)
    ))
  )



tab_data_ui_table1 <- fluidRow(
  column(
    4,
    tab_data_ui_table_rdo,
    tab_data_ui_table_cond1,
    tab_data_ui_table_cond2 ,
    fluidRow(column(6,actionButton("tab_data_btn_load_allfile", "load所有")),
             column(6,actionButton("tab_data_btn_load_file", "load文件")))
  ),
  column(5, dataTableOutput("tab_data_tbl_stat")),
  column(3,plotlyOutput("tab_data_tbl_plot"))
)
tab_data_ui_table2 <- fluidRow(
  
  
  column(
    1,
    shinyFilesButton(
      "tab_data_btn_special",
      "特殊值",
      title = NULL,
      multiple = FALSE
    )
  ),
  column(2, verbatimTextOutput("tab_data_text_special",placeholder = TRUE)),
  column(2, verbatimTextOutput("tab_data_text_special_time",placeholder = TRUE)),
  column(
    1,
    shinyFilesButton(
      "tab_data_btn_setting",
      "参数文件",
      title = NULL,
      multiple = FALSE
    )
  ),
  column(2, verbatimTextOutput("tab_data_text_setting",placeholder = TRUE)),
  column(2, verbatimTextOutput("tab_data_text_setting_time",placeholder = TRUE)),
  column(2,actionButton("tab_data_btn_special_view","查看特殊值"))
)