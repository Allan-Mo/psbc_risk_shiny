




# tooltips_taglist <- tagList(
#   bsTooltip("tab_data_btn_stat_all", "The wait times will be broken into this many equally spaced bins",
#             "right", options = list(container = "body")))

tooltips_taglist <- tagList(
  
  # 特殊值文件
  bsTooltip(
    "tab_data_btn_special",    
    "1.csv/txt文件使用read.csv读入;\n2.xlsx/xls读取special/第一个sheet的第一列(无列名)"
  ),
  # 参数文件
  bsTooltip(
    "tab_data_btn_setting",    
    "支持xlsx/xls,该文件会写入"
  ),
  # 所有文件
  bsTooltip(
    "tab_data_btn_allfile",    
    "完成后请检查右侧的更新时间dt"
  )
  
)