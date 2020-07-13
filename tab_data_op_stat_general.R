observeEvent(ra_data$stat_general, {
  my_log("DEBUG",
         ra_config$log_level,
         "(tab_data_stat) stat_general trigger")
  req(ra_data$stat_general$sample_type)
  sample_type <- ra_data$stat_general$sample_type
  req((sample_type=="train" & ra_data$has_train_tbl) ||
        (sample_type=="test" & ra_data$has_test_tbl) ||
        (sample_type=="validate" & ra_data$has_validate_tbl) ||
        (sample_type=="all" & ra_data$n_data>0))
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = sprintf("general stat for %s table",sample_type), value = 0)
  my_log(
    "DEBUG",
    ra_config$log_level,
    "(tab_data_stat) stat_general ",
    ra_data$stat_general$sample_type,
    "start"
  )
  stat_general <- ra_data$stat_general
  if (stat_general$sample_type == "train") {
    df <- ra_tbl$tbl_train
    lst_setting <- ra_project$lst_setting_train
  } else if (stat_general$sample_type == "test") {
    df <- ra_tbl$tbl_test
    lst_setting <- ra_project$lst_setting_test
  } else if (stat_general$sample_type == "validate") {
    df <- ra_tbl$tbl_validate
    lst_setting <- ra_project$lst_setting_validate
  } else {
    df <-
      bind_rows(ra_tbl$tbl_train, ra_tbl$tbl_test, ra_tbl$tbl_validate)
    lst_setting <- ra_project$lst_setting_all
  }
  
  stat_rst <-
    get_tbl_stat_general(
      df = df,
      vars = stat_general$vars,
      sample_type = stat_general$sample_type
      ,lst_setting=lst_setting
    )
  exclude_var <- unique(stat_rst$var)
  if (stat_general$sample_type == "train") {
    if (stat_general$type == "update") {
      ra_project$tbl_stat_general_train <-
        ra_project$tbl_stat_general_train %>%
        filter(!var %in% exclude_var) %>%
        bind_rows(stat_rst)
      
    } else {
      ra_project$tbl_stat_general_train <- stat_rst
    }
    
  } else if (stat_general$sample_type == "test") {
    if (stat_general$type == "update") {
      ra_project$tbl_stat_general_test <-
        ra_project$tbl_stat_general_test %>%
        filter(!var %in% exclude_var) %>%
        bind_rows(stat_rst)
      
    } else {
      ra_project$tbl_stat_general_test <- stat_rst
    }
  } else if (stat_general$sample_type == "validate") {
    if (stat_general$type == "update") {
      ra_project$tbl_stat_general_validate <-
        ra_project$tbl_stat_general_validate %>%
        filter(!var %in% exclude_var) %>%
        bind_rows(stat_rst)
      
    } else {
      ra_project$tbl_stat_general_validate <- stat_rst
    }
  } else {
    if (stat_general$type == "update") {
      ra_project$tbl_stat_general_all <-
        ra_project$tbl_stat_general_all %>%
        filter(!var %in% exclude_var) %>%
        bind_rows(stat_rst)
      
    } else {
      ra_project$tbl_stat_general_all <- stat_rst
    }
  }
  
  my_log(
    "DEBUG",
    ra_config$log_level,
    "(tab_data_stat) stat_general ",
    ra_data$stat_general$sample_type,
    "finish with nrow = ",nrow(stat_rst)
  )
  ra_data$stat_general <- list()
},priority=10)