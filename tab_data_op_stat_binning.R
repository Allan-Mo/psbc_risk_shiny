# 只需要传入vars和type,不需要sample_type
observeEvent(ra_data$stat_bin, {
  my_log("DEBUG",
         ra_config$log_level,
         "(tab_data_stat) stat_bin trigger")
  req(
    sum(
      ra_data$has_train_tbl,
      ra_data$has_test_tbl,
      ra_data$has_validate_tbl
    ) > 0 && length(ra_data$stat_bin$vars) > 0
  )
  my_log("DEBUG",
         ra_config$log_level,
         "(tab_data_stat) stat_bin start")
  progress <- shiny::Progress$new(max=8)
  on.exit(progress$close())
  progress$set(message = "Binning", value = 0)
  
  vars <- ra_data$stat_bin$vars
  type <- ra_data$stat_bin$type
  tbl_vars <- ra_project$tbl_vars
  
  
  source("tab_data_op_stat_binning_train.R",local = TRUE,encoding = "UTF-8")
  # source("tab_data_op_stat_binning_traintest.R",local = TRUE,encoding = "UTF-8")
  source("tab_data_op_stat_binning_all.R",local = TRUE,encoding = "UTF-8")
  
  
  
  
  
  
  # 数据合并
  ra_project$tbl_stat_bin <- bind_rows(ra_project$tbl_bin_train_for_train,
                                       ra_project$tbl_bin_train_for_test,
                                       ra_project$tbl_bin_train_for_validate,
                                       ra_project$tbl_bin_train_for_all,
                                       ra_project$tbl_bin_all_for_train,
                                       ra_project$tbl_bin_all_for_test,
                                       ra_project$tbl_bin_all_for_validate,
                                       ra_project$tbl_bin_all_for_all,)
  ra_data$stat_bin <- list()
  my_log("DEBUG",
         ra_config$log_level,
         "(tab_data_stat) stat_bin finish")
})