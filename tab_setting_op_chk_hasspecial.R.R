if (ra_data$n_data > 0) {
  has_default_num <- "default_num" %in%  names(lst_special)
  n_default_num <- length(lst_special$default_num$values)
  has_default_char <- "default_char" %in%  names(lst_special)
  n_default_char <- length(lst_special$default_char$values)
  
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
      item <- item_default
      if (col_types[[var]] %in% c("integer","integer64","numeric")) {
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
      rst[[var]] <<- item
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