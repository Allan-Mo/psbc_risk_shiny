




# debug -------------------------------------------------------------------

#
# reactive({
#   print(input$tab_data_btn_tbl_single)
# })

# 选择文件 --------------------------------------------------------------------
ra_tab_data_path = reactive({
  ra_config$select_project$path
})

tab_data_roots <-
  exprToFunction(c(chosenFolder = ra_tab_data_path()))

shinyFileChoose(input, 'tab_data_btn_tbl_single', roots = tab_data_roots)
shinyFileChoose(input, 'tab_data_btn_tbl_train', roots = tab_data_roots)
shinyFileChoose(input, 'tab_data_btn_tbl_test', roots = tab_data_roots)
shinyFileChoose(input, 'tab_data_btn_tbl_validate', roots = tab_data_roots)
shinyFileChoose(input, 'tab_data_btn_special', roots = tab_data_roots)
shinyFileChoose(input, 'tab_data_btn_setting', roots = tab_data_roots)



# 响应文件点击 ------------------------------------------------------------------

observeEvent(input$tab_data_btn_tbl_single, {
  if (!is.integer(input$tab_data_btn_tbl_single)) {
    ra_project$file_single <- NULL
    ra_project$file_single <-
      as.character(paste0(input$tab_data_btn_tbl_single$files[[1]], collapse = "/"))
  }
})

observeEvent(input$tab_data_btn_tbl_train, {
  if (!is.integer(input$tab_data_btn_tbl_train)) {
    ra_project$file_train <- NULL
    ra_project$file_train <-
      as.character(paste0(input$tab_data_btn_tbl_train$files[[1]], collapse = "/"))
  }
})

observeEvent(input$tab_data_btn_tbl_test, {
  if (!is.integer(input$tab_data_btn_tbl_test)) {
    ra_project$file_test <- NULL
    ra_project$file_test <-
      as.character(paste0(input$tab_data_btn_tbl_test$files[[1]], collapse = "/"))
  }
})

observeEvent(input$tab_data_btn_tbl_validate, {
  if (!is.integer(input$tab_data_btn_tbl_validate)) {
    ra_project$file_validate <- NULL
    ra_project$file_validate <-
      as.character(paste0(input$tab_data_btn_tbl_validate$files[[1]], collapse = "/"))
  }
})

observeEvent(input$tab_data_btn_special, {
  if (!is.integer(input$tab_data_btn_special)) {
    ra_project$file_special <- NULL
    ra_project$file_special <-
      as.character(paste0(input$tab_data_btn_special$files[[1]], collapse = "/"))
  }
})

observeEvent(input$tab_data_btn_setting, {
  if (!is.integer(input$tab_data_btn_setting)) {
    ra_project$file_setting <- NULL
    ra_project$file_setting <-
      as.character(paste0(input$tab_data_btn_setting$files[[1]], collapse = "/"))
  }
})


# 导入single ----------------------------------------------------------------
observeEvent({
  ra_project$file_single
  input$tab_data_btn_load_allfile
  1
}, {
  if (input$tab_data_rdo_tbltype == 'split' |
      is.null(ra_project$file_single)) {
    return()
  }
  
  file <-
    path(ra_config$select_project$path, ra_project$file_single)
  base_name <- basename(file)
  obj_name <- gsub("(\\.\\w+)$", "", base_name)
  
  chk <- list()
  tryCatch({
    load(file)
  }, error = function(e) {
    showModal(modalDialog(title = "single数据无法load()",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    chk$no_file <- TRUE
  })
  if (isTRUE(chk$no_file)) {
    return()
  }
  
  chk <- character(0)
  if (!(obj_name %in% ls())) {
    chk <- c(chk, "文件未包含同名object")
  } else if (!("sample_type" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无sample_type字段")
  } else if (!("label" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无label字段")
  }
  if (length(chk) > 0) {
    showModal(
      modalDialog(
        title = "single数据格式不标准",
        "要求：1.文件包含同名object；\n2.data.frame包含sample_type和label字段；\n3.sample_type的取值包括train/test/validate",
        paste(chk, collapse = "\n"),
        footer = tagList(modalButton("返回修改"))
      )
    )
  } else {
    ra_tbl$tbl_train <- NULL   #应该是比较了数据值，如果值不变，不触发重新统计
    ra_tbl$tbl_test <- NULL
    ra_tbl$tbl_validate <- NULL
    
    ra_tbl$tbl_train <- get(obj_name) %>%
      filter(sample_type == "train")
    ra_tbl$tbl_test <- get(obj_name) %>%
      filter(sample_type == "test")
    ra_tbl$tbl_validate <- get(obj_name) %>%
      filter(sample_type == "validate")
    omit_nrow <- get(obj_name) %>%
      filter(!(sample_type %in% c("train", "test", "validate"))) %>%
      nrow()
    if (omit_nrow > 0) {
      showModal(modalDialog(
        title = "sample_type包含其他取值",
        sprintf(
          "sample_type包括%d行train/test/validate之外的取值，这些行将被忽略！",
          omit_nrow
        ),
        footer = tagList(modalButton("确定"))
      ))
    }
    rm(list = c(obj_name))
    gc()
  }
})

# 训练文件 & 全部导入
observeEvent({
  ra_project$file_train
  input$tab_data_btn_load_allfile
  1
}, {
  if (input$tab_data_rdo_tbltype == 'single'  |
      is.null(ra_project$file_train)) {
    return()
  }
  
  file <-
    path(ra_config$select_project$path, ra_project$file_train)
  base_name <- basename(file)
  obj_name <- gsub("(\\.\\w+)$", "", base_name)
  chk <- list()
  tryCatch({
    load(file)
  }, error = function(e) {
    showModal(modalDialog(title = "train数据无法load()",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    chk$no_file <- TRUE
  })
  if (isTRUE(chk$no_file)) {
    return()
  }
  chk <- character(0)
  if (!(obj_name %in% ls())) {
    chk <- c(chk, "文件未包含同名object")
  } else if (!("label" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无label字段")
  }
  if (length(chk) > 0) {
    showModal(
      modalDialog(
        title = "train数据格式不标准",
        "要求：1.文件包含同名object；\n2.data.frame包含label字段；",
        paste(chk, collapse = "\n"),
        footer = tagList(modalButton("返回修改"))
      )
    )
  } else {
    ra_tbl$tbl_train <- NULL
    ra_tbl$tbl_train <- get(obj_name)
    rm(list = c(obj_name))
    gc()
  }
  
})


# 测试文件 & 全部导入
observeEvent({
  ra_project$file_test
  input$tab_data_btn_load_allfile
  1
}, {
  if (input$tab_data_rdo_tbltype == 'single'  |
      is.null(ra_project$file_test)) {
    return()
  }
  
  file <-
    path(ra_config$select_project$path, ra_project$file_test)
  base_name <- basename(file)
  obj_name <- gsub("(\\.\\w+)$", "", base_name)
  chk <- list()
  tryCatch({
    load(file)
  }, error = function(e) {
    showModal(modalDialog(title = "test数据无法load()",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    chk$no_file <- TRUE
  })
  if (isTRUE(chk$no_file)) {
    return()
  }
  chk <- character(0)
  if (!(obj_name %in% ls())) {
    chk <- c(chk, "文件未包含同名object")
  } else if (!("label" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无label字段")
  }
  if (length(chk) > 0) {
    showModal(
      modalDialog(
        title = "test数据格式不标准",
        "要求：1.文件包含同名object；\n2.data.frame包含label字段；",
        paste(chk, collapse = "\n"),
        footer = tagList(modalButton("返回修改"))
      )
    )
  } else {
    ra_tbl$tbl_test <- NULL
    ra_tbl$tbl_test <- get(obj_name)
    rm(list = c(obj_name))
    gc()
    
  }
})

# 验证文件 & 全部导入
observeEvent({
  ra_project$file_validate
  input$tab_data_btn_load_allfile
  1
}, {
  if (input$tab_data_rdo_tbltype == 'single'  |
      is.null(ra_project$file_validate)) {
    return()
  }
  file <-
    path(ra_config$select_project$path, ra_project$file_validate)
  base_name <- basename(file)
  obj_name <- gsub("(\\.\\w+)$", "", base_name)
  chk <- list()
  tryCatch({
    load(file)
  }, error = function(e) {
    showModal(modalDialog(title = "validate数据无法load()",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    chk$no_file <- TRUE
  })
  if (isTRUE(chk$no_file)) {
    return()
  }
  chk <- character(0)
  if (!(obj_name %in% ls())) {
    chk <- c(chk, "文件未包含同名object")
  } else if (!("label" %in% colnames(get(obj_name)))) {
    chk <- c(chk, "data.frame无label字段")
  }
  if (length(chk) > 0) {
    showModal(
      modalDialog(
        title = "validate数据格式不标准",
        "要求：1.文件包含同名object；\n2.data.frame包含label字段；",
        paste(chk, collapse = "\n"),
        footer = tagList(modalButton("返回修改"))
      )
    )
  } else {
    ra_tbl$tbl_validate <- NULL
    ra_tbl$tbl_validate <- get(obj_name)
    rm(list = c(obj_name))
    gc()
    
  }
})


# 特殊值导入
observeEvent({
  ra_project$file_special
  input$tab_data_btn_load_allfile
  input$tab_data_btn_load_file
  1
}, {
  if (is.null(ra_project$file_special)) {
    return()
  }
  file <-
    path(ra_config$select_project$path, ra_project$file_special)
  
  base_name <- basename(file)
  ext <- tolower(tools::file_ext(base_name))
  
  if (!(ext %in% c("txt", "csv", "xlsx", "xls"))) {
    showModal(modalDialog(
      title = "特殊值文件扩展名不符",
      sprintf("文件扩展名%s不在支持列表中，仅支持txt/csv/xls/xlsx"),
      footer = tagList(modalButton("返回修改"))
    ))
    return()
  }
  tryCatch({
    values <- if (ext %in% c("txt", "csv")) {
      read.csv(file, header = FALSE) %>%
        "[["(1)
    } else  {
      openxlsx::read.xlsx(file, sheet = "special", colNames = FALSE) %>%
        "[["(1)
    }
    dt <- Sys.time()
    ra_project$special_values <- list(values = values, dt = dt)
  }, error = function(e) {
    showModal(
      modalDialog(
        title = "特殊值文件读取错误",
        "1.csv/txt文件使用read.csv读入;\n2.xlsx/xls读取special/第一个sheet的第一列(无列名)",
        file,
        footer = tagList(modalButton("返回修改"))
      )
    )
    
  })
  gc()
  
})


# 数据集改变 -------------------------------------------------------------------


observeEvent(ra_tbl$tbl_train, {
  if (!is.null(ra_tbl$tbl_train)) {
    tbl_stat <- ra_tbl$tbl_train %>%
      sapply(class) %>%
      stack() %>%
      summarise(
        n = n()
        ,
        factor = sum(values %in% c("factor"))
        ,
        int = sum(values %in% c("integer", "integer64"))
        ,
        num = sum(values %in% c("numeric"))
        ,
        str = sum(values %in% c("character"))
      )
    tbl_stat <- ra_tbl$tbl_train %>%
      ungroup() %>%
      summarise(
        n = n(),
        label_miss = sum(is.na(label), na.rm = T)
        ,
        pos = sum(label, na.rm = T)
      ) %>%
      mutate(rate = pos / n) %>%
      mutate(
        sample_type = "train",
        pct = NA_real_,
        ncol = tbl_stat$n,
        factor = tbl_stat$factor,
        int = tbl_stat$int,
        num = tbl_stat$num,
        str = tbl_stat$str,
        dt = as.character(Sys.time())
      ) %>%
      select(dt, sample_type, n, pct, pos, rate, ncol, factor, int, num, str)
    
    ra_project$tbl_stat <- ra_project$tbl_stat %>%
      filter(sample_type != "train") %>%
      bind_rows(tbl_stat) %>%
      ungroup() %>%
      mutate(tot = sum(ifelse(
        sample_type %in% c("train", "test", "validate"), n, 0
      ))) %>%
      mutate(pct = ifelse(sample_type %in% c("train", "test", "validate"), n /
                            tot, NA)) %>%
      select(dt,
             sample_type,
             n,
             pct,
             pos,
             rate,
             ncol,
             factor,
             int,
             num,
             str)
    
  }
})



observeEvent(ra_tbl$tbl_test, {
  if (!is.null(ra_tbl$tbl_test)) {
    tbl_stat <- ra_tbl$tbl_test %>%
      sapply(class) %>%
      stack() %>%
      summarise(
        n = n()
        ,
        factor = sum(values %in% c("factor"))
        ,
        int = sum(values %in% c("integer", "integer64"))
        ,
        num = sum(values %in% c("numeric"))
        ,
        str = sum(values %in% c("character"))
      )
    tbl_stat <- ra_tbl$tbl_test %>%
      ungroup() %>%
      summarise(
        n = n(),
        label_miss = sum(is.na(label), na.rm = T)
        ,
        pos = sum(label, na.rm = T)
      ) %>%
      mutate(rate = pos / n) %>%
      mutate(
        sample_type = "test",
        pct = NA_real_,
        ncol = tbl_stat$n,
        factor = tbl_stat$factor,
        int = tbl_stat$int,
        num = tbl_stat$num,
        str = tbl_stat$str,
        dt = as.character(Sys.time())
      ) %>%
      select(dt, sample_type, n, pct, pos, rate, ncol, factor, int, num, str)
    ra_project$tbl_stat <- ra_project$tbl_stat %>%
      filter(sample_type != "test") %>%
      bind_rows(tbl_stat) %>%
      ungroup() %>%
      mutate(tot = sum(ifelse(
        sample_type %in% c("train", "test", "validate"), n, 0
      ))) %>%
      mutate(pct = ifelse(sample_type %in% c("train", "test", "validate"), n /
                            tot, NA)) %>%
      select(dt,
             sample_type,
             n,
             pct,
             pos,
             rate,
             ncol,
             factor,
             int,
             num,
             str)
    
  }
})


observeEvent(ra_tbl$tbl_validate, {
  if (!is.null(ra_tbl$tbl_validate)) {
    tbl_stat <- ra_tbl$tbl_validate %>%
      sapply(class) %>%
      stack() %>%
      summarise(
        n = n()
        ,
        factor = sum(values %in% c("factor"))
        ,
        int = sum(values %in% c("integer", "integer64"))
        ,
        num = sum(values %in% c("numeric"))
        ,
        str = sum(values %in% c("character"))
      )
    tbl_stat <- ra_tbl$tbl_validate %>%
      ungroup() %>%
      summarise(
        n = n(),
        label_miss = sum(is.na(label), na.rm = T)
        ,
        pos = sum(label, na.rm = T)
      ) %>%
      mutate(rate = pos / n) %>%
      mutate(
        sample_type = "validate",
        pct = NA_real_,
        ncol = tbl_stat$n,
        factor = tbl_stat$factor,
        int = tbl_stat$int,
        num = tbl_stat$num,
        str = tbl_stat$str,
        dt = as.character(Sys.time())
      ) %>%
      select(dt, sample_type, n, pct, pos, rate, ncol, factor, int, num, str)
    
    ra_project$tbl_stat <- ra_project$tbl_stat %>%
      filter(sample_type != "validate") %>%
      bind_rows(tbl_stat) %>%
      ungroup() %>%
      mutate(tot = sum(ifelse(
        sample_type %in% c("train", "test", "validate"), n, 0
      ))) %>%
      mutate(pct = ifelse(sample_type %in% c("train", "test", "validate"), n /
                            tot, NA)) %>%
      select(dt,
             sample_type,
             n,
             pct,
             pos,
             rate,
             ncol,
             factor,
             int,
             num,
             str)
    
  }
})

# 显示 -------------------------------------------------------------------

output$tab_data_text_tbl_single <-
  renderText({
    if (is.null(ra_project$file_single)) {
      return()
    } else
      (return(ra_project$file_single %>% basename()))
    
  })

output$tab_data_text_tbl_train <-
  renderText({
    if (is.null(ra_project$file_train)) {
      return()
    } else
      (return(ra_project$file_train %>% basename()))
    
  })

output$tab_data_text_tbl_test <-
  renderText({
    if (is.null(ra_project$file_test)) {
      return()
    } else
      (return(ra_project$file_test %>% basename()))
    
  })

output$tab_data_text_tbl_validate <-
  renderText({
    if (is.null(ra_project$file_validate)) {
      return()
    } else
      (return(ra_project$file_validate %>% basename()))
    
  })

output$tab_data_text_tbl_special <-
  renderText({
    if (is.null(ra_project$file_special)) {
      return()
    } else
      (return(ra_project$file_special %>% basename()))
    
  })

output$tab_data_text_tbl_setting <-
  renderText({
    if (is.null(ra_project$file_setting)) {
      return()
    } else
      (return(ra_project$file_setting %>% basename()))
    
  })


# 统计表 ---------------------------------------------------------------------

output$tab_data_tbl_stat <- DT::renderDT({
  df <- if (is.null(ra_project$tbl_stat)) {
    data.frame(
      dt = character(0),
      sample_type = character(0),
      n = integer(0),
      pct = numeric(0),
      pos = integer(0),
      rate = numeric(0),
      ncol = integer(0),
      factor = integer(0),
      int = integer(0),
      num = integer(0),
      str = integer(0)
    )
    
  } else {
    ra_project$tbl_stat
  }
  
  
  df %>%
    datatable(
      selection = "single",
      rownames = FALSE,
      options = list(
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE
      )
    ) %>%
    formatPercentage(c("pct", "rate"), 1)
  
}
,
server = TRUE)


# 统计图 ---------------------------------------------------------------------
output$tab_data_tbl_plot <- renderPlotly({
  if (is.null(ra_project$tbl_stat) | nrow(ra_project$tbl_stat)==0) {
    return()
  }
  ra_project$tbl_stat %>% 
    mutate(sample_type=factor(sample_type,levels=c("train","test","validate"))) %>% 
    plot_ly(x = ~sample_type, type = 'bar') %>% 
    add_bars(y = ~n) %>% 
    add_lines( y = ~rate, yaxis = "y2") %>% 
    add_markers(y = ~rate, yaxis = "y2") %>% 
    layout(showlegend=FALSE,yaxis2 = list(overlaying="y",side = "right",showgrid = FALSE,linewidth=0.5,tickformat = "%",tick0=0,dtick=0.1,title=""),
           yaxis = list(showgrid = FALSE,linewidth=0.5,tickformat = ",",title=""),
           xaxis = list(showgrid = FALSE,title="")
           
    )
})


# 特殊值响应 -------------------------------------------------------------------

output$tab_data_text_special <- renderText({
  if (is.null(ra_project$file_special)) {
    return()
  } else
    (ra_project$file_special %>% basename() %>% return())
})

output$tab_data_text_special_time <- renderText({
  if (is.null(ra_project$special_values)) {
    return()
  } else
    (ra_project$special_values$dt %>% as.character() %>% return())
})


# 查看特殊值 -------------------------------------------------------------------
observeEvent(input$tab_data_btn_special_view, {
  showModal(modalDialog(
    title = ifelse(
      is.null(ra_project$special_values),
      "特殊值为空",
      sprintf("共有%d个特殊值", length(ra_project$special_values$values))
    ),ifelse(
      is.null(ra_project$special_values),"",
    HTML(paste0(
      paste(
        1:length(ra_project$special_values$values),
        ra_project$special_values$values,
        sep = ":"
      ),
      collapse = "<br/>"
    ))),
    footer = tagList(modalButton("返回"))
  ))
})
