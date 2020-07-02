tab_data_ui_mutate_left <- column(4,
                                  fluidRow(
                                    column(8, DTOutput("tab_data_mutate_tbl")),
                                    column(
                                      4,
                                      actionButton("tab_data_mutate_btn_up", "上升"),
                                      actionButton("tab_data_mutate_btn_down", "下降"),
                                      actionButton("tab_data_mutate_btn_delete", "删除"),
                                      actionButton("tab_data_mutate_btn_view", "查看>>"),
                                      actionButton("tab_data_mutate_btn_edit", "修改>>"),
                                      actionButton("tab_data_mutate_btn_edit_confirm", "确定修改"),
                                      actionButton("tab_data_mutate_btn_edit_cancel", "取消修改"),
                                      
                                    )
                                  ),
                                  fluidRow(actionButton("tab_data_mutate_btn_apply", "执行计算")))
tab_data_ui_mutate_right <- column(8,
                                   fluidRow(
                                     radioButtons(
                                       "tab_data_mutate_rdo_type",
                                       label = NULL,
                                       choices = list(
                                         "手动代码" = "manual"
                                         ,
                                         "all" = "all",
                                         "int" =
                                           "int",
                                         "double" =
                                           "double",
                                         "factor" =
                                           "factor",
                                         "string" =
                                           "string",
                                         "范围或列表" =
                                           "range",
                                         "高级" =
                                           "advanced"
                                       ),
                                       inline = TRUE,selected="manual"
                                     )
                                   ),
                                   fluidRow(
                                     column(
                                       2,
                                       textInput("tab_data_mutate_input_note", "备注"),
                                       actionButton("tab_data_mutate_btn_calculate", "试算"),
                                       actionButton("tab_data_mutate_btn_add", "<<添加")
                                     ),
                                     column(
                                       10,
                                       verbatimTextOutput("tab_data_mutate_text_desc", placeholder = TRUE)
                                       ,
                                       textAreaInput(
                                         "tab_data_mutate_input_code",
                                         label = NULL,
                                         width = "800px",
                                         height = "150px"
                                       )
                                     )
                                   ))

tab_data_ui_mutate <-
  fluidRow(tab_data_ui_mutate_left, tab_data_ui_mutate_right)