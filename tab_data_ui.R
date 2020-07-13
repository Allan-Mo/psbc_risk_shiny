source("tab_data_ui_table.R",local=TRUE, encoding = "UTF-8") #数据
source("tab_data_ui_mutate.R",local=TRUE, encoding = "UTF-8") #数据
source("tab_data_ui_stat.R",local=TRUE, encoding = "UTF-8") #数据
source("tab_data_ui_var.R",local=TRUE, encoding = "UTF-8") #数据


tab_data_ui <- tabPanel(
  title="数据",
  value="data",
  # 数据集
  tab_data_ui_table,
  hr(),
  tab_data_ui_mutate,
  hr(),
  # 开始统计、增量统计、开始分箱、增量分箱
  tab_data_ui_stat
  ,
  hr(),
  tab_data_ui_var
)