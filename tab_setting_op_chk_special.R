if (has_specials) {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "special check : empty name"
  )
  tbl_special_values <- ra_project$tbl_special_values %>%
    ungroup() %>%
    mutate(rn = row_number())
  
  if (sum(is.na(tbl_special_values$name)) > 0) {
    tbl_setting_chk <- tbl_special_values %>%
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
    tbl_special_values <- tbl_special_values %>%
      filter(!is.na(name))
  }
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "special check : other type"
  )
  
  # 检查special:type其他值
  tbl_setting_chk <- tbl_special_values %>%
    filter(!is.na(name) &
             (is.na(type) | (
               !type %in% c("numeric", "character")
             ))) %>%
    mutate(
      level = "error"
      ,
      sample_type = character(0)
      ,
      var = character(0)
      ,
      chk = sprintf(
        "special_values中第%d行name=%s的type只能为character/numeric,出现值'%s'"
        ,
        rn,
        name,
        type
      )
    ) %>%
    select(all_of(colnames(tbl_setting_chk))) %>%
    bind_rows(tbl_setting_chk, .)
  
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "special check : multiple types"
  )
  # 检查special:type多个值
  tbl_setting_chk <- tbl_special_values %>%
    group_by(name) %>%
    summarise(n_dist = n_distinct(type)) %>%
    filter(n_dist > 1) %>%
    mutate(
      level = "error"
      ,
      sample_type = character(0)
      ,
      var = character(0)
      ,
      chk = sprintf("special_values中name=%s包含%d中type"
                    , name, n_dist)
    ) %>%
    select(all_of(colnames(tbl_setting_chk))) %>%
    bind_rows(tbl_setting_chk, .)
  # 生成list
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "special check : generate lst_special"
  )
  lapply(unique(tbl_special_values$name), function(x) {
    sub <- tbl_special_values %>%
      filter(name == x)
    lst_special[[x]] <<-
      list(type = sub$type[1], values = if (sub$type[1] == "numeric") {
        sub$value_num
      } else {
        sub$value_char
      })
  })
  
} # end of special checking