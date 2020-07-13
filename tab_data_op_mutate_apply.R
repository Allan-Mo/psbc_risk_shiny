observeEvent(input$tab_data_mutate_btn_apply, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_apply_calc "
  )
  # 数据未导入
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "calculating", value = 0)
  
  my_log("INFO",
         ra_config$debug_level,
         "(tab_data_mutate) apply starts")
  ok <- TRUE
  if (is.null(ra_tbl$tbl_train) &
      is.null(ra_tbl$tbl_test) & is.null(ra_tbl$tbl_validate)) {
    showModal(modalDialog(title = "数据未导入",
                          footer = tagList(modalButton("返回"))))
    my_log("ERROR",
           ra_config$debug_level,
           "(tab_data_mutate) apply withoud data")
    ok <- FALSE
  }
  req(ok)
  # 数据全部空行
  if (is.null(ra_tbl$tbl_train) & nrow(ra_tbl$tbl_train) == 0 &
      is.null(ra_tbl$tbl_test) &
      nrow(ra_tbl$tbl_test) == 0 &
      is.null(ra_tbl$tbl_validate)  &
      nrow(ra_tbl$tbl_validate == 0)) {
    showModal(modalDialog(title = "数据全部空行",
                          footer = tagList(modalButton("返回"))))
    my_log("ERROR",
           ra_config$debug_level,
           "(tab_data_mutate) apply withoud data")
    ok <- FALSE
  }
  
  # 无可执行代码
  if (nrow(ra_project$tbl_mutate) == 0) {
    showModal(modalDialog(title = "无代码需要计算",
                          footer = tagList(modalButton("返回"))))
    my_log("ERROR",
           ra_config$debug_level,
           "(tab_data_mutate) apply withoud code")
    ok <- FALSE
  }
  req(ok)
  
  # 置空
  varlist_mutate_new <- list(train = character(0),
                             test = character(0),
                             validate = character(0))
  ra_data$varlist_mutate_new <- varlist_mutate_new
  
  
  # 代码缩写，只读
  tbl_mutate <- ra_project$tbl_mutate
  
  
  
  for (sample_type in c("train", "test", "validate")) {
    progress$inc(amount=0.2,detail=sprintf(" for %s table",sample_type))
    varlist_new <- character(0)
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_data_mutate) apply ",
      sample_type,
      "starts"
    )
    df <- switch(
      sample_type,
      train = ra_tbl$tbl_train,
      test = ra_tbl$tbl_test,
      validate = ra_tbl$tbl_validate
    )
    
    if (nrow(df) == 0) {
      my_log(
        "DEBUG",
        ra_config$debug_level,
        "(tab_data_mutate) apply ",
        sample_type,
        " exit without data"
      )
      next
    }
    
    for (i in 1:nrow(tbl_mutate)) {
      row <- tbl_mutate[i, "id"]
      type <- tbl_mutate[i, "type"]
      code <- tbl_mutate[i, "code"]
      note <- tbl_mutate[i, "note"]
      if (!type %in% c(
        "all",
        "manual",
        "real",
        "character",
        "factor",
        "integer",
        "numeric",
        "advanced"
      )) {
        showModal(modalDialog(
          title = "该类型尚未支持",
          sprintf("第%d个:%s", row, type),
          footer = tagList(modalButton("返回"))
        ))
        my_log(
          "ERROR",
          ra_config$debug_level,
          "(tab_data_mutate) apply ",
          sample_type,
          " not support type -",
          type
        )
        req(FALSE)
      }
      s <- tryCatch({
        vars_list <- get_vars_by_code(df = df,
                                      type = type,
                                      code = code)
        
        varlist_new <- c(varlist_new,vars_list$new)
        # 执行计算
        
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
                      .funs = eval(parse(text = gsub(
                        "^.*?\\n", "", code
                      ))))
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
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "计算错误",
          sprintf("第%d个type=%s,note=%s,code=%s", row, type, note, code),
          ifelse(sample_type != "train", "数据已经改变，建议重新load数据"),
          footer = tagList(modalButton("返回修改"))
        ))
        my_log(
          "ERROR",
          ra_config$debug_level,
          "(tab_data_mutate) apply ",
          sample_type,
          " code error"
        )
        req(FALSE)
      }) # end of tryCatch
      
      if (is.list(s) & !is.null(s$message)) {
        showNotification(s$message)
      }
      
    } # end of code loop
    
    # 所有code执行成功才会执行
    newvars <-  varlist_new %>% unique()
    print("newvars===")
    print(newvars)
    if (sample_type == "train") {
      ra_tbl$tbl_train <- df
      ra_data$mutate_newcol_train <- newvars
    } else if (sample_type == "test") {
      ra_tbl$tbl_test <- df
      ra_data$mutate_newcol_test <- newvars
    } else if (sample_type == "validate") {
      ra_tbl$tbl_validate <- df
      ra_data$mutate_newcol_validate <- newvars
      ra_data$mutate_newcol_all <- newvars  # 需要确保for里面validate写在最后
      ra_data$stat_bin <-
        list(type = "update",
             vars = newvars)
    }
    
    
    
    rm(df)
    gc()
    
    my_log(
      "INFO",
      ra_config$debug_level,
      "(tab_data_mutate) apply ",
      sample_type,
      " sucessfully"
    )
    
    
    
    
    
    
  } # end of data loop
  # 所有data执行成功才会执行
  
  gc()
  showNotification("计算完成\n请检查数据统计表的dt时时间戳。")
  my_log("INFO",
         ra_config$debug_level,
         "(tab_data_mutate) apply all sucessfully")
  
})


# 统计新列 --------------------------------------------------------------------
observeEvent(ra_data$mutate_newcol_train,{
  req(ra_data$mutate_newcol_train)
  ra_data$stat_general <-
    list(sample_type = "train",
         type = "update",
         vars = ra_data$mutate_newcol_train)
  ra_data$stat_special <-
    list(sample_type = "train",
         type = "update",
         vars = ra_data$mutate_newcol_train)
  ra_data$mutate_newcol_train <- NULL
})

observeEvent(ra_data$mutate_newcol_test,{
  req(ra_data$mutate_newcol_test)
  ra_data$stat_general <-
    list(sample_type = "test",
         type = "update",
         vars = ra_data$mutate_newcol_test)
  ra_data$stat_special <-
    list(sample_type = "test",
         type = "update",
         vars = ra_data$mutate_newcol_test)
  ra_data$mutate_newcol_test <- NULL
})

observeEvent(ra_data$mutate_newcol_validate,{
  req(ra_data$mutate_newcol_validate)
  ra_data$stat_general <-
    list(sample_type = "validate",
         type = "update",
         vars = ra_data$mutate_newcol_validate)
  ra_data$stat_special <-
    list(sample_type = "validate",
         type = "update",
         vars = ra_data$mutate_newcol_validate)
  ra_data$mutate_newcol_validate <- NULL
})

observeEvent(ra_data$mutate_newcol_all,{
  req(ra_data$mutate_newcol_all)
  ra_data$stat_general <-
    list(sample_type = "all",
         type = "update",
         vars = ra_data$mutate_newcol_all)
  ra_data$stat_special <-
    list(sample_type = "all",
         type = "update",
         vars = ra_data$mutate_newcol_all)
  ra_data$mutate_newcol_all <- NULL
})
