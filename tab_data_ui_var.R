tab_data_ui_var <- fluidRow(
  checkboxGroupInput(
    "tbl_data_chk_group",
    label = NULL
    ,
    choices = list(
      "训练集" = "train",
      "测试集" = "test",
      "验证集" = "validate"
      ,
      "百分位" = "quantile",
      "百分位(详细)" = "quantile_detail",
      "PSI" = "psi",
      "分箱" = "bin"
    ),
    selected = c("train"),
    inline = TRUE
  ),
  
  DTOutput("tab_data_tbl_var")
  
)