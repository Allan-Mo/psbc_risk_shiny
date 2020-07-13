tab_report_ui <- tabPanel("报告",
                          value = "report",
                          fluidRow(column(
                            3, selectInput(
                              "tab_rpt_select_model",
                              "选择模板",
                              choices = list(
                                "评分卡" = "scorecard",
                                "XGBoost" = "xgboost",
                                "GBDT" = "gbdt",
                                "CARD" = "card"
                              )
                            )
                          ),
                          column(
                            2, downloadButton("tab_rpt_select_download", "生成报告")
                          )))