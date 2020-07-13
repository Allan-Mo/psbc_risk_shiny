tab_data_ui_stat <-
  fluidRow(
    # div(style = "display: inline-block;vertical-align:top; width: 70px;", actionButton("tab_data_btn_stat_all", "全新统计"))
    # ,
    column(
      2,
      actionButton("tab_data_stat_btn_all", "全新统计")
    ),
    column(
      2,
      actionButton("tab_data_stat_btn_new", "增量统计")
    ),
    column(
      2,
      actionButton("tab_data_stat_btn_bin_all", "全新分箱")
    ),
    column(
      2,
      actionButton("tab_data_stat_btn_bin_new", "增量分箱")
    )
  )
