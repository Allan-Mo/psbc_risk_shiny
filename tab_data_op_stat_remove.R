
observeEvent(input$tab_data_stat_confirm_remove_train_stat, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_stat)",
    "remove train stat table "
  )
  ra_project$tbl_stat_general_train <-
    get_tbl_stat_general(df = NULL)  # 不直接置空，由function确定统一的数据格式
  ra_project$tbl_stat_special_train <-
    get_tbl_stat_special(df = NULL)
  ra_project$tbl_stat_bin_train <- get_tbl_stat_bin(df = NULL)
  ra_project$lst_special_train <- list()
  ra_project$lst_bin_train <- list()
  
})

observeEvent(input$tab_data_stat_confirm_remove_test_stat, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_stat)",
    "remove test stat table "
  )
  ra_project$tbl_stat_general_test <-
    get_tbl_stat_general(df = NULL)  # 不直接置空，由function确定统一的数据格式
  ra_project$tbl_stat_special_test <-
    get_tbl_stat_special(df = NULL)
  ra_project$tbl_stat_bin_test <- get_tbl_stat_bin(df = NULL)
  ra_project$lst_special_test <- list()
  ra_project$lst_bin_test <- list()
  
})
observeEvent(input$tab_data_stat_confirm_remove_validate_stat, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_stat)",
    "remove validate stat table "
  )
  ra_project$tbl_stat_general_validate <-
    get_tbl_stat_general(df = NULL)  # 不直接置空，由function确定统一的数据格式
  ra_project$tbl_stat_special_validate <-
    get_tbl_stat_special(df = NULL)
  ra_project$tbl_stat_bin_validate <- get_tbl_stat_bin(df = NULL)
  ra_project$lst_special_validate <- list()
  ra_project$lst_bin_validate <- list()
  
})

observeEvent(input$tab_data_stat_confirm_remove_all_stat, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_stat)",
    "remove all stat table "
  )
  ra_project$tbl_stat_general_all <-
    get_tbl_stat_general(df = NULL)  # 不直接置空，由function确定统一的数据格式
  ra_project$tbl_stat_special_all <-
    get_tbl_stat_special(df = NULL)
  ra_project$tbl_stat_bin_all <- get_tbl_stat_bin(df = NULL)
  ra_project$lst_special_all <- list()
  ra_project$lst_bin_all <- list()
  
})