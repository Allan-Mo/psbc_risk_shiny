source("tab_setting_ui_chk.R",
       local = TRUE,
       encoding = "UTF-8")

source("tab_setting_ui_special.R",
       local = TRUE,
       encoding = "UTF-8")
# source("tab_setting_ui_bin.R",
#        local = TRUE,
#        encoding = "UTF-8")
source("tab_setting_ui_var.R",
       local = TRUE,
       encoding = "UTF-8")
tab_setting_ui <- tabPanel(
  "setting",
  value = "setting",
  tab_setting_ui_chk,
  hr(),
  tab_setting_ui_special_p1,
  tab_setting_ui_special_p2,
  h6("说明:name=default_num用于integer/integer64/numeric类型的默认特殊值,name=default_char用于其他类型的默认特殊值。type=numeric请会使用as.numeric(value_num)强制转换，NA值会被忽略。"),
  # hr(),
  # tab_setting_ui_bin_p1,
  # tab_setting_ui_bin_p2,
  hr(),
  tab_setting_ui_var,
  h6("列special_value_ref对应上表的name，如果不使用特殊值，明确写NULL，否则根据数据类型选择default_num/default_char(如果有)。")
)