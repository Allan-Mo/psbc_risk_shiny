

tab_var_ui <- tabPanel(
  "变量",
  value = "var",
  fluidRow(verbatimTextOutput("tab_var_text_var")),
  h4("时间检查"),
  DTOutput("tab_var_tbl_stat_p1"),
  h4("基础统计"),
  DTOutput("tab_var_tbl_stat_p2"),
  fluidRow(column(4,plotlyOutput("tab_var_plot_miss")),
           column(4,plotlyOutput("tab_var_plot_unique")),
           column(4,plotlyOutput("tab_var_plot_special"))),
  h4("TOP值"),
  DTOutput("tab_var_tbl_stat_p3"),
  fluidRow(column(10,plotlyOutput("tab_var_plot_rank")),
            column(2,sliderInput("tab_var_input_rank_num","个数",min=3L,max=20L,step=1L,value=20L))),
  conditionalPanel(
    "output.onevar_type=='numeric' ",
    h4("百分位"),
    DTOutput("tab_var_tbl_stat_p4"),
    h4("百分位(详细)"),
    DTOutput("tab_var_tbl_stat_p5"),
    plotlyOutput("tab_var_plot_quantile")
  ),
  h4("PSI(以train为期望)"),
  DTOutput("tab_var_tbl_stat_p6"),
  h4("分箱"),
  DTOutput("tab_var_tbl_stat_p7"),
  plotlyOutput("tab_var_plot_bin_iv_train"),
  plotlyOutput("tab_var_plot_bin_iv_all"),
  plotlyOutput("tab_var_plot_bin_psi_train"),
  plotlyOutput("tab_var_plot_bin_psi_all"),
  conditionalPanel(
    "output.onevar_type=='numeric' ",
    fluidRow(column(6, plotlyOutput("tab_var_plot_box")),
             column(6, plotlyOutput(
               "tab_var_plot_violin"
             ))),
    fluidRow(column(6, plotlyOutput(
      "tab_var_plot_density"
    ))
    ,column(6, plotlyOutput("tab_var_plot_hist"))
    )
    ,div(class="inline",sliderInput("tab_var_plot_hist_bins","直方图个数",min=5,max=50,value=20,step=1))
    ,fluidRow(column(6,plotOutput("tab_var_plot_hist_train_l",brush = "tab_var_s_train_brush")),
              column(6,plotOutput("tab_var_plot_hist_train_r")))
    ,fluidRow(column(6,plotOutput("tab_var_plot_hist_test_l",brush = "tab_var_s_test_brush")),
              column(6,plotOutput("tab_var_plot_hist_test_r")))
    ,fluidRow(column(6,plotOutput("tab_var_plot_hist_validate_l",brush = "tab_var_s_validate_brush")),
              column(6,plotOutput("tab_var_plot_hist_validate_r")))
    ,fluidRow(column(6,plotOutput("tab_var_plot_hist_all_l",brush = "tab_var_s_all_brush")),
              column(6,plotOutput("tab_var_plot_hist_all_r")))
  )
)
