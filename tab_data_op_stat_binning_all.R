# use all data for binning ----------------------------------------------
progress$inc(amount = 1, detail = "binng for all tables")
if (ra_data$n_data > 0) {
  # list(lst_bin=list(),lst_breaks=list())
  my_log("DEBUG",
         ra_config$log_level,
         "(tab_data_stat) stat_bin all_for_all start")
  setting_all_for_all <-
    ra_project$lst_setting_all[names(ra_project$lst_setting_all) %in% vars]
  rst_all_for_all <-
    get_binning(
      df = bind_rows(ra_tbl$tbl_train, ra_tbl$tbl_test, ra_tbl$tbl_validate)  ,
      vars = vars,
      lst_setting = setting_train_for_train,
      sample_type = "train"
    )
  rst_all_for_all_stat <-
    get_tbl_stat_bin(
      vars = vars
      ,
      rst = rst_all_for_all,
      sample_type = "all",
      bin_src = "all"
    )
  if (type == "update") {
    # to avoid save when every item assigns
    rst_all_for_all2 <- ra_project$lst_bin_all_for_all
    for (var in vars) {
      rst_all_for_all2$lst_bin[[var]] <-
        rst_all_for_all$lst_bin[[var]]
    }
    for (var in vars) {
      rst_all_for_all2$lst_breaks[[var]] <-
        rst_all_for_all$lst_breaks[[var]]
    }
    ra_project$lst_bin_all_for_all <- rst_all_for_all2
    ra_project$tbl_bin_all_for_all <-
      ra_project$tbl_bin_all_for_all %>%
      filter(!var %in% vars) %>%
      bind_rows(rst_all_for_all_stat)
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin all_for_all update")
    
  } else {
    ra_project$lst_bin_all_for_all <- rst_all_for_all
    ra_project$tbl_bin_all_for_all <- rst_all_for_all_stat
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin all_for_all replace")
  }
}
# apply binning to train ---------------------------------------------------
progress$inc(amount = 1, detail = "apply all tables breaks to train")
if (ra_data$has_train_tbl && ra_data$has_train_tbl) {
  rst_all_for_train <-
    get_binning_from_rst(
      df = ra_tbl$tbl_train,
      vars = vars,
      src_rst = rst_all_for_all,
      src = "all"
    )
  rst_all_for_train_stat <-
    get_tbl_stat_bin(
      vars = vars
      ,
      rst = rst_all_for_train,
      exp_rst = NULL,
      #------------------------------ the psi base is train not all tables
      sample_type = "train",
      bin_src = "all"
    )
  if (type == "update") {
    # to avoid save when every item assigns
    rst_all_for_train2 <- ra_project$lst_bin_all_for_train
    for (var in vars) {
      rst_all_for_train2$lst_bin[[var]] <-
        rst_all_for_train$lst_bin[[var]]
    }
    for (var in vars) {
      rst_all_for_train2$lst_breaks[[var]] <-
        rst_all_for_train$lst_breaks[[var]]
    }
    ra_project$lst_bin_all_for_train <- rst_all_for_train2
    ra_project$tbl_bin_all_for_train <-
      ra_project$tbl_bin_all_for_train %>%
      filter(!var %in% vars) %>%
      bind_rows(rst_all_for_train_stat)
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin all_for_train update")
    
  } else {
    ra_project$lst_bin_all_for_train <- rst_all_for_train
    ra_project$tbl_bin_all_for_train <- rst_all_for_train_stat
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin all_for_train replace")
  }
}

# apply binning to test ---------------------------------------------------
progress$inc(amount = 1, detail = "apply all tables breaks to test")
if (ra_data$n_data > 0 && ra_data$has_test_tbl) {
  rst_all_for_test <-
    get_binning_from_rst(
      df = ra_tbl$tbl_test,
      vars = vars,
      src_rst = rst_all_for_all,
      src = "all"
    )
  rst_all_for_test_stat <-
    get_tbl_stat_bin(
      vars = vars
      ,
      rst = rst_all_for_test,
      exp_rst = rst_all_for_train,
      #------------------------------ the psi base is train not all tables
      sample_type = "test",
      bin_src = "all"
    )
  if (type == "update") {
    # to avoid save when every item assigns
    rst_all_for_test2 <- ra_project$lst_bin_all_for_test
    for (var in vars) {
      rst_all_for_test2$lst_bin[[var]] <-
        rst_all_for_test$lst_bin[[var]]
    }
    for (var in vars) {
      rst_all_for_test2$lst_breaks[[var]] <-
        rst_all_for_test$lst_breaks[[var]]
    }
    ra_project$lst_bin_all_for_test <- rst_all_for_test2
    ra_project$tbl_bin_all_for_test <-
      ra_project$tbl_bin_all_for_test %>%
      filter(!var %in% vars) %>%
      bind_rows(rst_all_for_test_stat)
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin all_for_test update")
    
  } else {
    ra_project$lst_bin_all_for_test <- rst_all_for_test
    ra_project$tbl_bin_all_for_test <- rst_all_for_test_stat
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin all_for_test replace")
  }
}

# apply binning to validate ---------------------------------------------------
progress$inc(amount = 1, detail = "apply all tables breaks to validate")
if (ra_data$has_train_tbl && ra_data$has_validate_tbl) {
  rst_all_for_validate <-
    get_binning_from_rst(
      df = ra_tbl$tbl_validate,
      vars = vars,
      src_rst = rst_all_for_all,
      src = "all"
    )
  rst_all_for_validate_stat <-
    get_tbl_stat_bin(
      vars = vars
      ,
      rst = rst_all_for_validate,
      exp_rst = rst_all_for_train,
      #------------------------------ the psi base is train not all tables
      sample_type = "validate",
      bin_src = "all"
    )
  if (type == "update") {
    # to avoid save when every item assigns
    rst_all_for_validate2 <- ra_project$lst_bin_all_for_validate
    for (var in vars) {
      rst_all_for_validate2$lst_bin[[var]] <-
        rst_all_for_validate$lst_bin[[var]]
    }
    for (var in vars) {
      rst_all_for_validate2$lst_breaks[[var]] <-
        rst_all_for_validate$lst_breaks[[var]]
    }
    ra_project$lst_bin_all_for_validate <- rst_all_for_validate2
    ra_project$tbl_bin_all_for_validate <-
      ra_project$tbl_bin_all_for_validate %>%
      filter(!var %in% vars) %>%
      bind_rows(rst_all_for_validate_stat)
    my_log(
      "DEBUG",
      ra_config$log_level,
      "(tab_data_stat) stat_bin all_for_validate update"
    )
    
  } else {
    ra_project$lst_bin_all_for_validate <- rst_all_for_validate
    ra_project$tbl_bin_all_for_validate <- rst_all_for_validate_stat
    my_log(
      "DEBUG",
      ra_config$log_level,
      "(tab_data_stat) stat_bin all_for_validate replace"
    )
  }
}
