observeEvent(ra_data$stat_special, {
  my_log("DEBUG",
         ra_config$log_level,
         "(tab_data_stat) stat_special trigger")
  sample_type <- ra_data$stat_special$sample_type
  req(sample_type)
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
    "(tab_data_stat) stat_special ",
    sample_type,
    "start"
  )
  stat_special <- ra_data$stat_special
  if (stat_special$sample_type == "train") {
    df <- ra_tbl$tbl_train
    lst_setting <- ra_project$lst_setting_train[names(ra_project$lst_setting_train) %in% stat_special$vars]
  } else if (stat_special$sample_type == "test") {
    df <- ra_tbl$tbl_test
    lst_setting <- ra_project$lst_setting_test[names(ra_project$lst_setting_test) %in% stat_special$vars]
  } else if (stat_special$sample_type == "validate") {
    df <- ra_tbl$tbl_validate
    lst_setting <- ra_project$lst_setting_validate[names(ra_project$lst_setting_validate) %in% stat_special$vars]
  } else {
    df <-
      bind_rows(ra_tbl$tbl_train, ra_tbl$tbl_test, ra_tbl$tbl_validate)
    lst_setting <- ra_project$lst_setting_all[names(ra_project$lst_setting_all) %in% stat_special$vars]
  }
  
  
  stat_rst <- get_tbl_stat_special(
    df = df,
    vars = stat_special$vars
    ,
    lst_setting = lst_setting,
    sample_type = stat_special$sample_type
  )
  
  
  exclude_var <- unique(stat_rst$var)
  if (stat_special$sample_type == "train") {
    if (stat_special$type == "update") {
      ra_project$tbl_stat_special_train <-
        ra_project$tbl_stat_special_train %>%
        filter(!var %in% exclude_var) %>%
        bind_rows(stat_rst)
      
      for (name in stat_special$vars) {
        ra_project$lst_special_train[[name]] <- lst_setting[[name]]
      }
      
    } else {
      ra_project$tbl_stat_special_train <- stat_rst
      ra_project$lst_special_train <- lst_setting
    }
    
  } else if (stat_special$sample_type == "test") {
    if (stat_special$type == "update") {
      ra_project$tbl_stat_special_test <-
        ra_project$tbl_stat_special_test %>%
        filter(!var %in% exclude_var) %>%
        bind_rows(stat_rst)
      for (name in stat_special$vars) {
        ra_project$lst_special_test[[name]] <- lst_setting[[name]]
      }
      
    } else {
      ra_project$tbl_stat_special_test <- stat_rst
      ra_project$lst_special_test <- lst_setting
    }
  } else if (stat_special$sample_type == "validate") {
    if (stat_special$type == "update") {
      ra_project$tbl_stat_special_validate <-
        ra_project$tbl_stat_special_validate %>%
        filter(!var %in% exclude_var) %>%
        bind_rows(stat_rst)
      
      for (name in stat_special$vars) {
        ra_project$lst_special_validate[[name]] <- lst_setting[[name]]
      }
      
    } else {
      ra_project$tbl_stat_special_validate <- stat_rst
      ra_project$lst_special_validate <- lst_setting
    }
  } else {
    if (stat_special$type == "update") {
      ra_project$tbl_stat_special_all <-
        ra_project$tbl_stat_special_all %>%
        filter(!var %in% exclude_var) %>%
        bind_rows(stat_rst)
      for (name in stat_special$vars) {
        ra_project$lst_special_all[[name]] <- lst_setting[[name]]
      }
      
    } else {
      ra_project$tbl_stat_special_all <- stat_rst
      ra_project$lst_special_all <- lst_setting
    }
  }
  my_log(
    "DEBUG",
    ra_config$log_level,
    "(tab_data_stat) stat_special ",
    sample_type,
    "finish with nrow=",nrow(stat_rst)
  )
  
  ra_data$stat_special <- list()
  
  
},priority = 11)  # priority need to be higher than binning