tab_scorecard_ui <- tabPanel(
  title = "评分卡",
  value = "scorecard",
  h4("候选集合"),
  DTOutput("tab_sc_table_candidate"),
  h4("LR model"),
  fluidRow(column(
    3,
    radioButtons(
      "tab_sc_rdo_method",
      "变量筛选",
      choices = list("AIC" = "aic", "lasso" =
                       "lasso"),
      selected = "aic",
      inline = TRUE
    )
  ),
  column(
    3,
    radioButtons(
      "tab_sc_rdo_direct",
      "方向",
      choices = list(
        "both" = "both",
        "forward" = "forward",
        "backward" = "backward"
      ),
      selected = "aic",
      inline = TRUE
    )
  ),column(3,actionButton("tab_sc_btn_run","运行"))),
  h4("KS & ROC plot"),
  hr(),
  h4("PSI")
)