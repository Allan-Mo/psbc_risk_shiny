



file <- path(s$path, "project.Rdata")
if (file.exists(file)) {
  my_log("INFO",ra_config$debug_level,"project init - loading project.Rdata")
  load(file)
  for (name in names(project)) {
    ra_project[[name]] <- project[[name]]
  }
}

# 部分表清空
ra_project$tbl_stat <- NULL

ra_tbl$tbl_train <- NULL
ra_tbl$tbl_test <- NULL
ra_tbl$tbl_validate <- NULL
ra_tbl$tbl_modal <- NULL

ra_data$mutate_select <- NULL
ra_data$mutate_status <- "add"
ra_data$mutate_selected <- NULL
ra_data$varlist_mutate_new <- list(train=character(0),test=character(0),validate=character(0))
ra_data$file_setting_time <- NULL

ra_data$setting_special_status <- "view"
ra_data$setting_bin_status <- "view"
ra_data$setting_var_status <- "view"

gc()

# 初始化

# 表类型单次变更
if (!is.null(ra_project$tbl_type)) {
  updateRadioButtons(session,"tab_data_rdo_tbltype",selected=ra_project$tbl_type)
}

# 空表结构（初始化显示）
if (is.null(ra_project$tbl_stat)) {
  my_log("DEBUG",ra_config$debug_level,"project init - tbl_stat structure")
  ra_project$tbl_stat <- psbc_risk_template$tbl_stat
}

if (is.null(ra_project$tbl_mutate)) {
  my_log("DEBUG",ra_config$debug_level,"project init - tbl_mutate structure")
  ra_project$tbl_mutate <- psbc_risk_template$tbl_mutate
}

if (is.null(ra_project$tbl_special_values)) {
  my_log("DEBUG",ra_config$debug_level,"project init - tbl_special_values structure")
  ra_project$tbl_special_values <- psbc_risk_template$tbl_special_values
}
if (is.null(ra_project$tbl_vars)) {
  my_log("DEBUG",ra_config$debug_level,"project init - tbl_vars structure")
  ra_project$tbl_vars <- psbc_risk_template$tbl_vars
}
if (is.null(ra_project$tbl_setting_chk)) {
  my_log("DEBUG",ra_config$debug_level,"project init - tbl_setting_chk structure")
  ra_project$tbl_setting_chk <- psbc_risk_template$tbl_setting_chk
}



if (is.null(ra_project$tbl_varstat)) {
  my_log("DEBUG",ra_config$debug_level,"project init - tbl_varstat structure")
  ra_project$tbl_varstat <- psbc_risk_template$tbl_varstat
}

if (is.null(ra_project$tbl_stat_cat)) {
  my_log("DEBUG",ra_config$debug_level,"project init - tbl_stat_cat structure")
  ra_project$tbl_stat_cat <- psbc_risk_template$tab_stat_cat
}


# 模板文件

relative_file <- "setting_template.xlsx"
file <- path(ra_config$select_project$path, relative_file)
if (!file.exists(file)) {
  my_log("INFO",ra_config$debug_level,"project init - copy",relative_file)
  file.copy(relative_file,file,overwrite = TRUE)
}