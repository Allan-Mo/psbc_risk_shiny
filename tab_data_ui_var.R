tab_data_ui_var <- fluidRow(
  column(5, fluidRow(
    column(4, actionButton("tbl_data_btn_chkgroup_tables", "选择数据")) ,
    column(
      8,
      checkboxGroupInput(
        "tbl_data_var_chkgroup_tables",
        label = NULL
        ,
        choices = list(
          "全部" = "all",
          "训练集" = "train",
          "测试集" = "test",
          "验证集" = "validate"
        ),
        selected = c("all", "train", "test", "validate"),
        inline = TRUE
      )
    )
    
  ),
  fluidRow(
    column(4, actionButton(
      "tbl_data_btn_chkgroup_binsrc", "选择分箱来源"
    )),
    column(
      8,
      checkboxGroupInput(
        "tbl_data_var_chkgroup_binsrc",
        label = NULL
        ,
        choices = list("训练" = "train",
                       "全部" = "all"),
        selected = c("all"),
        inline = TRUE
      )
    )
    
    
  )),
  
  column(
    7,
    checkboxGroupInput(
      "tbl_data_var_chkgroup_cols",
      label = NULL
      ,
      choices = list(
        "缺失值" = "miss",
        "特殊值" = "special",
        "唯一值" = "unique",
        "众数" = "rank",
        "均值" = "mean",
        "百分位" = "quantile",
        "百分位(详细)" = "quantile_detail",
        "PSI" = "psi",
        "分箱" = "bin",
        "时间" = "dt"
      ),
      selected = c("miss", "special", "unique", "rank", "mean", "psi", "bin"),
      inline = TRUE
    )
    ,
    
    column(4, actionButton("tbl_data_btn_chkgroup_cols", "选择列"))
    
  ),
  
  DTOutput("tab_data_var_tbl")
  
)