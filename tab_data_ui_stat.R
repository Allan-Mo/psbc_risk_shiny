tab_data_ui_stat <- fluidRow(
  div(style = "display: inline-block;vertical-align:top; width: 70px;", actionButton("tab_data_btn_stat_all", "全新统计"))
  ,
  div(style = "display: inline-block;vertical-align:top; width: 70px;", actionButton("tab_data_btn_stat_new", "增量统计"))
  ,
  div(style = "display: inline-block;vertical-align:top; width: 70px;", actionButton("tab_data_btn_bin_new", "增量分箱"))
  ,
  div(
    style = "display: inline-block;vertical-align:top; width: 150px;",
    sliderInput("tab_data_param_bin_num_limit", "bin_num_limit",min=2,max=20,value = 8,step=1)
  )
  ,
  div(
    style = "display: inline-block;vertical-align:top; width: 150px;",
    sliderInput("stop_limit", "stop_limit", value = 0.1,min=0.01,max=0.5,step=0.01)
  ),
  div(
    style = "display: inline-block;vertical-align:top; width: 150px;",
    sliderInput("count_distr_limit", "count_distr_limit", value = 8,min=0.01,max=0.20,step=0.01)
  ),
  div(
    style = "display: inline-block;vertical-align:top; width: 150px;",
    numericInput("no_cores", "bin_num_limit", value = 8)
  )
)
