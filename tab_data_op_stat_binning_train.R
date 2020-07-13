# use train data for binning ----------------------------------------------

progress$inc(amount=1,detail="binng for train table")
if (ra_data$has_train_tbl) {
  # list(lst_bin=list(),lst_setting=list())
  setting_train_for_train <- ra_project$lst_setting_train[names(ra_project$lst_setting_train) %in% vars]
  rst_train_for_train <-
    get_binning(df = ra_tbl$tbl_train,
                vars = vars,
                lst_setting=setting_train_for_train
                ,sample_type="train"
                )
  rst_train_for_train_stat <-
    get_tbl_stat_bin(
      vars = vars
      ,
      rst = rst_train_for_train,
      sample_type = "train",
      bin_src = "train"
    )
  if (type == "update") {
    # to avoid save when every item assigns
    rst_train_for_train2 <- ra_project$lst_bin_train_for_train
    for (var in vars) {
      rst_train_for_train2$lst_bin[[var]] <-
        rst_train_for_train$lst_bin[[var]]
    }
    for (var in vars) {
      rst_train_for_train2$lst_setting[[var]] <-
        rst_train_for_train$lst_setting[[var]]
    }
    ra_project$lst_bin_train_for_train <- rst_train_for_train2
    ra_project$tbl_bin_train_for_train <-
      ra_project$tbl_bin_train_for_train %>%
      filter(!var %in% vars) %>%
      bind_rows(rst_train_for_train_stat)
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin train_for_train update")
    
  } else {
    ra_project$lst_bin_train_for_train <- rst_train_for_train
    ra_project$tbl_bin_train_for_train <- rst_train_for_train_stat
    my_log(
      "DEBUG",
      ra_config$log_level,
      "(tab_data_stat) stat_bin train_for_train replace"
    )
  }
}


# apply binning to test ---------------------------------------------------
progress$inc(amount=1,detail="apply train tables breaks to test")
if (ra_data$has_train_tbl && ra_data$has_test_tbl) {
  my_log("DEBUG",
         ra_config$log_level,
         "(tab_data_stat) stat_bin train for_test start")
  rst_train_for_test <-
    get_binning_from_rst(df = ra_tbl$tbl_test,
                         vars = vars,
                         src_rst = rst_train_for_train,
                         src="train")
  rst_train_for_test_stat <-
    get_tbl_stat_bin(
      vars = vars,
      rst = rst_train_for_test,
      exp_rst = rst_train_for_train,
      sample_type = "test",
      bin_src = "train"
    )
  if (type == "update") {
    # to avoid save when every item assigns
    rst_train_for_test2 <- ra_project$lst_bin_train_for_test
    for (var in vars) {
      rst_train_for_test2$lst_bin[[var]] <-
        rst_train_for_test$lst_bin[[var]]
    }
    for (var in vars) {
      rst_train_for_test2$lst_setting[[var]] <-
        rst_train_for_test$lst_setting[[var]]
    }
    ra_project$lst_bin_train_for_test <- rst_train_for_test2
    ra_project$tbl_bin_train_for_test <-
      ra_project$tbl_bin_train_for_test %>%
      filter(!var %in% vars) %>%
      bind_rows(rst_train_for_test_stat)
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin train_for_test update")
    
  } else {
    ra_project$lst_bin_train_for_test <- rst_train_for_test
    ra_project$tbl_bin_train_for_test <- rst_train_for_test_stat
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin train_for_test replace")
  }
}

# apply binning to validate ---------------------------------------------------
progress$inc(amount=1,detail="apply train tables breaks to validate")
if (ra_data$has_train_tbl && ra_data$has_validate_tbl) {
  rst_train_for_validate <-
    get_binning_from_rst(df = ra_tbl$tbl_validate,
                         vars = vars,
                         src_rst = rst_train_for_train,
                         src="train")
  rst_train_for_validate_stat <-
    get_tbl_stat_bin(
      vars = vars
      ,
      rst = rst_train_for_validate,
      exp_rst = rst_train_for_train,
      sample_type = "validate",
      bin_src = "train"
    )
  if (type == "update") {
    # to avoid save when every item assigns
    rst_train_for_validate2 <- ra_project$lst_bin_train_for_validate
    for (var in vars) {
      rst_train_for_validate2$lst_bin[[var]] <-
        rst_train_for_validate$lst_bin[[var]]
    }
    for (var in vars) {
      rst_train_for_validate2$lst_setting[[var]] <-
        rst_train_for_validate$lst_setting[[var]]
    }
    ra_project$lst_bin_train_for_validate <- rst_train_for_validate2
    ra_project$tbl_bin_train_for_validate <-
      ra_project$tbl_bin_train_for_validate %>%
      filter(!var %in% vars) %>%
      bind_rows(rst_train_for_validate_stat)
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin train_for_validate update")
    
  } else {
    ra_project$lst_bin_train_for_validate <- rst_train_for_validate
    ra_project$tbl_bin_train_for_validate <- rst_train_for_validate_stat
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin train_for_validate replace")
  }
}

# apply binning to all ---------------------------------------------------
progress$inc(amount=1,detail="apply train tables breaks to all")
if (ra_data$n_data>0) {
  rst_train_for_all <-
    get_binning_from_rst(df = bind_rows(ra_tbl$tbl_train,ra_tbl$tbl_test,ra_tbl$tbl_test) ,
                         vars = vars,
                         src_rst = rst_train_for_train,
                         src="train")
  rst_train_for_all_stat <-
    get_tbl_stat_bin(
      vars = vars
      ,
      rst = rst_train_for_all,
      exp_rst = rst_train_for_train,
      sample_type = "all",
      bin_src = "train"
    )
  if (type == "update") {
    # to avoid save when every item assigns
    rst_train_for_all2 <- ra_project$lst_bin_train_for_all
    for (var in vars) {
      rst_train_for_all2$lst_bin[[var]] <-
        rst_train_for_all$lst_bin[[var]]
    }
    for (var in vars) {
      rst_train_for_all2$lst_setting[[var]] <-
        rst_train_for_all$lst_setting[[var]]
    }
    ra_project$lst_bin_train_for_all <- rst_train_for_all2
    ra_project$tbl_bin_train_for_all <-
      ra_project$tbl_bin_train_for_all %>%
      filter(!var %in% vars) %>%
      bind_rows(rst_train_for_all_stat)
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin train_for_all update")
    
  } else {
    ra_project$lst_bin_train_for_all <- rst_train_for_all
    ra_project$tbl_bin_train_for_all <- rst_train_for_all_stat
    my_log("DEBUG",
           ra_config$log_level,
           "(tab_data_stat) stat_bin train_for_all replace")
  }
}
