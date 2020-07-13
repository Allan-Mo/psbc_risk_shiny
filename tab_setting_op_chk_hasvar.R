if (ra_data$n_data > 0) {
  has_default_num <- "default_num" %in%  names(lst_special)
  n_default_num <- length(lst_special$default_num$values)
  has_default_char <- "default_char" %in%  names(lst_special)
  n_default_char <- length(lst_special$default_char$values)
  
  
  # lapply(c("train", "test", "validate", "all"), function(sample_type) {
  for (sample_type in c("train", "test", "validate", "all")) {
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_setting_chk)",
      "vars=yes sample_type=",sample_type
    )
    col_types <- switch(
      sample_type
      ,
      "train" = train_col_types
      ,
      "test" = test_col_types
      ,
      "validate" = validate_col_types
      ,
      "all" = all_col_types
    )
    rst <- lst_setting_default
    # lapply(names(col_types), function(var) {
    for (var in names(col_types)) {
      if (var %in% names(lst_vars)) {
        # check special_value match
        if (col_types[[var]] %in% c("integer", "integer64", "numeric")) {
          if ((!is.null(lst_vars[[var]]$special_type)) && lst_vars[[var]]$special_type == "character") {
            tbl_setting_chk <- tbl_setting_chk %>%
              add_row(
                level = "error",
                var = var,
                sample_type = sample_type,
                chk = sprintf(
                  "表中的类型%s与vars中指定special_value_ref=%s的类型%s不符.",
                  col_types[[var]],
                  lst_vars[[var]]$special_name,
                  lst_vars[[var]]$special_type
                )
              )
          }
        } else {
          if ((!is.null(lst_vars[[var]]$special_type)) && lst_vars[[var]]$special_type == "numeric") {
            tbl_setting_chk <- tbl_setting_chk %>%
              add_row(
                level = "error",
                var = var,
                sample_type = sample_type,
                chk = sprintf(
                  "表中的类型%s与vars中指定special_value_ref=%s的类型%s不符.",
                  col_types[[var]],
                  lst_vars[[var]]$special_name,
                  lst_vars[[var]]$special_type
                )
              )
          }
        }
        
        item <- lst_vars[[var]]
      } else {
        item <- item_default
        if (col_types[[var]] %in% c("integer", "integer64", "numeric")) {
          if (has_default_num) {
            item$special_name <- "default_num"
            item$special_type <- "numeric"
            item$special_values <- lst_special$default_num$values
            item$special_n <- n_default_num
          }
          
        } else {
          if (has_default_char) {
            item$special_name <- "default_char"
            item$special_type <- "chareric"
            item$special_values <- lst_special$default_char$values
            item$special_n <- n_default_char
          }
        }
      }
      
      
      rst[[var]] <- item
      # }) # end of cols
    } # end of cols
    ra_project$tbl_setting_chk <- tbl_setting_chk
    if (sample_type == "train") {
      lst_setting_train <- rst
    } else if (sample_type == "test") {
      lst_setting_test <- rst
    } else if (sample_type == "validate") {
      lst_setting_validate <- rst
    } else  {
      lst_setting_all <- rst
    }
    # }) # end of sample_type
  } # end of sample_type
} else {
  # 没有数据,do nothing
  
}