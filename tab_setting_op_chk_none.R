if (ra_data$n_data > 0) {
  lapply(c("train", "test", "validate", "all"), function(sample_type) {
    col_types <- switch(
      sample_type
      ,
      "train" = train_col_types
      ,
      "test" = all_col_types
      ,
      "validate" = all_col_types
      ,
      "all" = all_col_types
    )
    rst <- lst_setting_default
    lapply(names(col_types), function(var) {
      rst[[var]] <- item_default
    })
    
    if (sample_type == "train") {
      lst_setting_train <<- rst
    } else if (sample_type == "test") {
      lst_setting_test <<- rst
    } else if (sample_type == "validate") {
      lst_setting_validate <<- rst
    } else  {
      lst_setting_all <<- rst
    }
  })
} else {
  # 没有数据,do nothing
  
}