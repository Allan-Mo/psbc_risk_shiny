if (has_vars) {
  tbl_vars <- ra_project$tbl_vars %>%
    ungroup() %>%
    mutate(rn = row_number())
  # 检查vars:空行
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "vars check : empty var"
  )
  if (sum(is.na(tbl_vars$var)) > 0) {
    tbl_setting_chk <- tbl_vars %>%
      filter(is.na(name)) %>%
      mutate(
        level = "wanning"
        ,
        sample_type = character(0)
        ,
        var = character(0)
        ,
        chk = sprintf("special_values中第%d行的name为空"
                      , name, rn)
      ) %>%
      select(all_of(colnames(tbl_setting_chk))) %>%
      bind_rows(tbl_setting_chk, .)
    tbl_vars <- tbl_vars %>%
      filter(!is.na(name))
  }
  
  # 检查vars:var多个值
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "vars check : var multiple times"
  )
  tbl_setting_chk <- tbl_vars %>%
    group_by(var) %>%
    tally() %>%
    filter(n > 1) %>%
    mutate(
      level = "error"
      ,
      sample_type = character(0)
      ,
      chk = sprintf("vars中出现%d的var=%s", n, var)
    ) %>%
    select(all_of(colnames(tbl_setting_chk))) %>%
    bind_rows(tbl_setting_chk, .)
  
  # 检查vars:special_value_ref不存在
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "vars check : special_value_ref exists"
  )
  tbl_setting_chk <- tbl_vars %>%
    filter(
      is.na(special_value_ref) &
        special_value_ref != 'NULL' &
        (!special_value_ref %in% names(lst_special))
    ) %>%
    mutate(
      level = "error"
      ,
      sample_type = character(0)
      ,
      chk = sprintf(
        "vars中第%d行var=%s的special_value_ref：%未出现在special_values中",
        rn,
        var,
        special_value_ref
      )
    ) %>%
    select(all_of(colnames(tbl_setting_chk))) %>%
    bind_rows(tbl_setting_chk, .)
  
  # 检查vars:value无法转成数字
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "vars check : bin param not number"
  )
  tbl_setting_chk <- tbl_vars %>%
    mutate(
      ok1 = bin_num_limit == "NULL" |
        is.na(bin_num_limit) |
        is.numeric(bin_num_limit) | (!is.na(as.numeric(
          bin_num_limit
        ))),
      ok2 = stop_limit == "NULL" |
        is.na(stop_limit) |
        is.numeric(stop_limit) | (!is.na(as.numeric(stop_limit))),
      ok3 = count_distr_limit == "NULL" |
        is.na(count_distr_limit) |
        is.numeric(count_distr_limit) |
        (!is.na(as.numeric(
          count_distr_limit
        )))
    ) %>%
    filter(ok1 + ok2 + ok3 < 3) %>%
    mutate(
      level = "error"
      ,
      sample_type = character(0)
      ,
      chk = sprintf(
        "vars中第%d行bin_num_limit或stop_limit或count_distr_limit，不是NULL或空，也无法转换成numeric",
        rn
      )
    ) %>%
    select(all_of(colnames(tbl_setting_chk))) %>%
    bind_rows(tbl_setting_chk, .)
  
  # 检查 breaks
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "vars check : breaks code error"
  )
  if (sum(!is.na(tbl_vars$breaks))>0) {
    tbl_vars_breaks <- tbl_vars %>% 
      filter(!is.na(breaks))
    for (i in 1:nrow(tbl_vars_breaks)) {
      tryCatch({
        x <- eval(parse(text=tbl_vars_breaks$breaks[i]))
      },error=function(e) {
        tbl_setting_chk <<- tbl_setting_chk %>% 
          add_row(level="error",var=tbl_vars_breaks$var[i],
                  chk=sprintf("vars中第%d行var=%s的breaks报错:%s",tbl_vars_breaks$rn[i],tbl_vars_breaks$var[i],tbl_vars_breaks$breaks[i]))
      })
    }
    
  }
  
  # 转换成lst
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "vars check : generate lst_vars"
  )
  lapply(1:nrow(tbl_vars),function(i) {
    item <- item_default
    var <- tbl_vars$var[i]
    special_value_ref <- tbl_vars$special_value_ref[i]
    bin_num_limit <- tbl_vars$bin_num_limit[i]
    bin_num_limit <- ifelse(is.na(bin_num_limit),item$bin_num_limit,bin_num_limit)
    stop_limit <- tbl_vars$stop_limit[i]
    stop_limit <- ifelse(is.na(stop_limit),item$stop_limit,stop_limit)
    count_distr_limit <- tbl_vars$count_distr_limit[i]
    count_distr_limit <- ifelse(is.na(count_distr_limit),item$count_distr_limit,count_distr_limit)
    breaks_raw <- tbl_vars$breaks[i]
    if ((!is.null(special_value_ref)) && (!is.na(special_value_ref)) && special_value_ref=="NULL") {
      special_forceNULL <<- c(special_forceNULL,var)
      item$special_forceNULL <- TRUE
    } else if (!is.na(special_value_ref)) {
      item$special_name <- special_value_ref
      item$special_type <- lst_special[[special_value_ref]]$type
      item$special_values <- lst_special[[special_value_ref]]$values
      item$special_n <- length(lst_special[[special_value_ref]]$values)
    }
    
    item$breaks_raw <- breaks_raw
    
    if ((!is.null(bin_num_limit)) && (!is.na(bin_num_limit)) && bin_num_limit=="NULL") {
      special_forceNULL <<- c(special_forceNULL,var)
      item$bin_forceNULL <- TRUE
      item$bin_num_limit <- NULL
      item$stop_limit <- NULL
      item$count_distr_limit <- NULL
      
    } else {
      item$bin_num_limit <- bin_num_limit
      item$stop_limit <- stop_limit
      item$count_distr_limit <- count_distr_limit
    }
    
    if (!is.na(breaks_raw)) {
      tryCatch({
        breaks <- eval(parse(text=breaks_raw))
        item$breaks <- breaks
        item$breaks_n <- length(breaks)
      },error=function(e) {})
    }
    
    lst_vars[[var]] <<- item
    
  })
  
} # end of vars checking