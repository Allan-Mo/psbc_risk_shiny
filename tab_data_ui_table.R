


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
                     8,
                     verbatimTextOutput("tab_data_text_tbl_single", placeholder = TRUE)
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
      8,
      verbatimTextOutput("tab_data_text_tbl_train", placeholder = TRUE)
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
    , column(
      8,
      verbatimTextOutput("tab_data_text_tbl_test", placeholder = TRUE)
    )),
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
      8,
      verbatimTextOutput("tab_data_text_tbl_validate", placeholder = TRUE)
    ))
  )



tab_data_ui_table <- fluidRow(
  column(
    3,
    tab_data_ui_table_rdo,
    tab_data_ui_table_cond1,
    tab_data_ui_table_cond2 ,
    fluidRow(column(12,actionButton("tab_data_btn_load_alltable", "load数据"))),
    hr(),
    fluidRow(column(4,shinyFilesButton(
      "tab_data_btn_tbl_setting",
      "参数文件",
      title = NULL,
      multiple = FALSE
    )),
             column(8,verbatimTextOutput("tab_data_text_tbl_setting", placeholder = TRUE))),
    fluidRow(column(4,actionButton("tab_data_btn_load_setting", "load参数文件"))),
    hr(),
    fluidRow(column(4,actionButton("tab_data_btn_load_allfile", "load所有")),
             column(4,actionButton("tab_data_btn_cancel_allfile", "清除选择")),
             column(4,actionButton("tab_data_table_tbn_confirm_all_remove","删除数据")))
    
  ),
  column(5,
         dataTableOutput("tab_data_tbl_stat")),
  column(4, plotlyOutput("tab_data_tbl_plot"))
)