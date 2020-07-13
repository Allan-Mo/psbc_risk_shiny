tab_data_ui_mutate_left <- column(4,
                                  fluidRow(
                                    column(8, verbatimTextOutput("tab_data_mutate_text_select",placeholder = TRUE),
                                           DTOutput("tab_data_mutate_tbl")),
                                    column(
                                      4,
                                      actionButton("tab_data_mutate_btn_select", "选择"),
                                      conditionalPanel("output.mutate_status == 'select' ",actionButton("tab_data_mutate_btn_up", "上升")),
                                      conditionalPanel("output.mutate_status == 'select' ",actionButton("tab_data_mutate_btn_down", "下降")),
                                      conditionalPanel("output.mutate_status == 'select' ",actionButton("tab_data_mutate_btn_delete", "删除")),
                                      conditionalPanel("output.mutate_status == 'select' ",actionButton("tab_data_mutate_btn_view", "查看>>")),
                                      conditionalPanel("output.mutate_status == 'select' || output.mutate_status == 'view'",actionButton("tab_data_mutate_btn_cancel", "取消")),
                                      conditionalPanel("output.mutate_status == 'view' ",actionButton("tab_data_mutate_btn_edit", "修改>>")),
                                      conditionalPanel("output.mutate_status == 'update' ",actionButton("tab_data_mutate_btn_edit_confirm", "确定修改")),
                                      conditionalPanel("output.mutate_status == 'update' ",actionButton("tab_data_mutate_btn_edit_cancel", "取消修改")),
                                      
                                    )
                                  ),
                                  fluidRow(actionButton("tab_data_mutate_btn_apply", "执行计算",width="300px")))
tab_data_ui_mutate_right <- column(8,
                                   fluidRow(
                                     radioButtons(
                                       "tab_data_mutate_rdo_type",
                                       label = NULL,
                                       choices = list(
                                         "手动代码" = "manual"
                                         ,
                                         "数值型"="real",
                                         "字符型" =
                                           "character",
                                         "factor" =
                                           "factor",
                                         "int" =
                                           "integer",
                                         "numeric" =
                                           "numeric",
                                         "全部" = "all",
                                         "factor_real"="factor_real",
                                         "factor_char"="factor_character",
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
                                       conditionalPanel("output.mutate_status == 'calc' ",actionButton("tab_data_mutate_btn_add", "<<添加"))
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