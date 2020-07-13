observeEvent(input$tab_data_mutate_btn_calculate, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_calculate "
  )
  type <- input$tab_data_mutate_rdo_type
  code <- input$tab_data_mutate_input_code
  
  if (is.null(code) | str_trim(code)=="") {
    showModal(modalDialog(title = "未提供代码",
                          footer = tagList(modalButton("返回"))))
    return()
  }
  
  if (is.null(ra_tbl$tbl_train) &
      is.null(ra_tbl$tbl_test) & is.null(ra_tbl$tbl_validate)) {
    showModal(modalDialog(title = "数据未导入",
                          footer = tagList(modalButton("返回"))))
    return()
  }
  # 获取数据
  if ((!is.null(ra_tbl$tbl_train)) & nrow(ra_tbl$tbl_train) > 0) {
    df <- ra_tbl$tbl_train %>%
      head(10)
  } else if ((!is.null(ra_tbl$tbl_test)) &
             nrow(ra_tbl$tbl_test) > 0) {
    df <- ra_tbl$tbl_test %>%
      head(10)
  } else if ((!is.null(ra_tbl$tbl_validate)) &
             nrow(ra_tbl$tbl_validate) > 0) {
    df <- ra_tbl$tbl_validate %>%
      head(10)
  } else {
    showModal(modalDialog(title = "数据空行",
                          footer = tagList(modalButton("返回"))))
    return()
  }
  
  # 返回使用的字段列表
  
  
  if (!type %in% c("all","manual",
                   "real",
                   "character",
                   "factor",
                   "integer",
                   "numeric",
                   "advanced")) {
    showModal(modalDialog(title = "该类型尚未支持",
                          footer = tagList(modalButton("返回"))))
    return()
  }
  
  vars_list <- get_vars_by_code(df = df, type = type, code = code)
  
  df <- df %>%
    dplyr::select(any_of(vars_list$change))
  
  # 执行计算
  s <- tryCatch({
    if (type == "manual") {
      for (x in str_split(code, "\n(?=[_a-zA-Z])")[[1]]) {
        x <- gsub("(^\\s*)|(,\\s*$)", "", x)
        pattern <- "^(\\w+)\\s*=(.*)"
        name <- sub(pattern, "\\1", x)
        exp <- sub(pattern, "\\2", x)
        df <- df %>%
          mutate(!!name := !!parse_expr(exp))
      }
    } else if (type %in% c("all")) {
      df <- df %>%
        mutate_all(.funs = funs(eval(parse(text = code))))
    } else if (type %in% c("advanced")) {
      df <- df %>%
        mutate_at(.vars = vars_list$change,
                  .funs = eval(parse(text = gsub("^.*?\\n","",code))))
    } 
    else if (type %in% c("real",
                         "character",
                         "factor",
                         "integer",
                         "numeric"))
    {
      df <- df %>%
        mutate_at(.vars = vars_list$change,
                  .funs = eval(parse(text = code)))
      
    }
    ra_tbl$tbl_modal <- df
    showModal(modalDialog(
      title = "试算结果",
      DTOutput("tbl_modal"),
      footer = tagList(modalButton("返回"))
    ))
    
    if (ra_data$mutate_status == "edit") {
      ra_data$mutate_status = "update"
    } else if (ra_data$mutate_status == "add") {
      ra_data$mutate_status = "calc"
    }
  }, error = function(e) {
    if (ra_data$mutate_status == "update") {
      ra_data$mutate_status = "edit"
    } else if (ra_data$mutate_status == "calc") {
      ra_data$mutate_status = "add"
    }
    return(e)
    
    # showModal(modalDialog(title = "试算错误",
    #                       file,
    #                       footer = tagList(modalButton("返回修改"))))
    
  })
  
  if (is.list(s)) {
    showNotification(s$message)
  }
  
  
})