
# 系统配置 --------------------------------------------------------------------

ra_config <- reactiveValues(
  df_project = config$df_project,
  select_project = NULL,
  log_level = config$df_config
)



observeEvent({
  ra_config$df_project
  ra_config$select_project
  ra_config$log_level
  1
}, {
  config <- reactiveValuesToList(ra_config, all.names = FALSE)
  save(config, file = "config.Rdata")
})


# 项目配置 --------------------------------------------------------------------


ra_project <- reactiveValues(
  tbl_type = "split",
  file_single = NULL,
  file_train = NULL,
  file_test = NULL,
  file_validate = NULL,
  file_special = NULL,
  file_setting = NULL,
  special_values = NULL,
  setting_tbls = NULL,
  tbl_stat = NULL,
  tbl_mutate=NULL,
  tbl_varstat=NULL,
  mutate_status="new" #new新增,edit=编辑,update提交
)

observeEvent({
  ra_project$tbl_type
  ra_project$file_single
  ra_project$file_train
  ra_project$file_test
  ra_project$file_validate
  ra_project$file_special
  ra_project$file_setting
  ra_project$special_values
  ra_project$setting_tbls
  ra_project$tbl_stat
  ra_project$tbl_mutate
  ra_project$mutate_status
  ra_project$tbl_varstat
  1
}, {
  if (!is.null(ra_config$select_project)) {
    project <- reactiveValuesToList(ra_project, all.names = FALSE)
    save(project,
         file = path(ra_config$select_project$path, "project.Rdata"))
  }
  
})

# 数据 ----------------------------------------------------------------------
ra_tbl <- reactiveValues(tbl_train = NULL,
                         tbl_test = NULL,
                         tbl_validate = NULL)

