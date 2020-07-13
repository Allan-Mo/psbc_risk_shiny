

# 响应选择数据集类型 -----------------------------------------------------------------
observeEvent(input$tab_data_rdo_tbltype, {
  req(input$tab_data_rdo_tbltype)
  ra_project$tbl_type <- input$tab_data_rdo_tbltype
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_table)",
    "tbl_type change to ",
    ra_project$tbl_type
  )
})


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
shinyFileChoose(input, 'tab_data_btn_tbl_setting', roots = tab_data_roots)



# 响应文件点击 ------------------------------------------------------------------

observeEvent(input$tab_data_btn_tbl_single, {
  if (!is.integer(input$tab_data_btn_tbl_single)) {
    ra_project$file_single <- NULL
    ra_project$file_single <-
      as.character(paste0(input$tab_data_btn_tbl_single$files[[1]], collapse = "/"))
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_data_table)",
      "file_single change to ",
      ra_project$file_single
    )
  }
})

observeEvent(input$tab_data_btn_tbl_train, {
  if (!is.integer(input$tab_data_btn_tbl_train)) {
    ra_project$file_train <- NULL
    ra_project$file_train <-
      as.character(paste0(input$tab_data_btn_tbl_train$files[[1]], collapse = "/"))
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_data_table)",
      "file_train change to ",
      ra_project$file_train
    )
  }
})

observeEvent(input$tab_data_btn_tbl_test, {
  if (!is.integer(input$tab_data_btn_tbl_test)) {
    ra_project$file_test <- NULL
    ra_project$file_test <-
      as.character(paste0(input$tab_data_btn_tbl_test$files[[1]], collapse = "/"))
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_data_table)",
      "file_test change to ",
      ra_project$file_test
    )
  }
})

observeEvent(input$tab_data_btn_tbl_validate, {
  if (!is.integer(input$tab_data_btn_tbl_validate)) {
    ra_project$file_validate <- NULL
    ra_project$file_validate <-
      as.character(paste0(input$tab_data_btn_tbl_validate$files[[1]], collapse = "/"))
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_data_table)",
      "file_validate change to ",
      ra_project$file_validate
    )
  }
})



observeEvent(input$tab_data_btn_tbl_setting, {
  if (!is.integer(input$tab_data_btn_tbl_setting)) {
    ra_project$file_setting <- NULL
    ra_project$file_setting <-
      as.character(paste0(input$tab_data_btn_tbl_setting$files[[1]], collapse = "/"))
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_data_table)",
      "file_setting change to ",
      ra_project$file_setting
    )
  }
})



# 导入single ----------------------------------------------------------------
source("tab_data_op_table_single.R",
       local = TRUE,
       encoding = "UTF-8")


# 训练文件 & 全部导入
source("tab_data_op_table_multiple.R",
       local = TRUE,
       encoding = "UTF-8")



# 配置文件 & 全部导入
source("tab_data_op_table_setting.R",
       local = TRUE,
       encoding = "UTF-8")

# 数据集改变 -------------------------------------------------------------------
source("tab_data_op_table_stat.R",
       local = TRUE,
       encoding = "UTF-8")


# 数据集改变触发新列统计 -------------------------------------------------------------
# source("tab_data_op_table_update.R",
#        local = TRUE,
#        encoding = "UTF-8")



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

output$tab_data_text_tbl_setting <-
  renderText({
    if (is.null(ra_project$file_setting)) {
      return()
    } else
      (return(ra_project$file_setting %>% basename()))
    
  })

output$tab_data_text_tbl_setting_time <- renderText({
  if (is.null(ra_data$file_setting_time)) {
    return()
  } else
    (return(ra_data$file_setting_time))
})



# 统计表 ---------------------------------------------------------------------

output$tab_data_tbl_stat <- DT::renderDT({
  req(ra_project$tbl_stat)
  ra_project$tbl_stat %>%
    datatable(
      selection = "single",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE
      )
    ) %>%
    formatPercentage(c("pct", "rate"), 1)
  
}
,
server = TRUE)


# 统计图 ---------------------------------------------------------------------
output$tab_data_tbl_plot <- renderPlotly({
  if (is.null(ra_project$tbl_stat) | nrow(ra_project$tbl_stat) == 0) {
    return()
  }
  ra_project$tbl_stat %>%
    mutate(sample_type = factor(sample_type, levels = c("train", "test", "validate"))) %>%
    plot_ly(x = ~ sample_type, type = 'bar') %>%
    add_bars(y = ~ n) %>%
    add_lines(y = ~ rate, yaxis = "y2") %>%
    add_markers(y = ~ rate, yaxis = "y2") %>%
    layout(
      title=NULL,
      showlegend = FALSE,
      yaxis2 = list(
        overlaying = "y",
        side = "right",
        tickformat = "%",
        title = ""
      ),
      yaxis = list(
        showgrid = FALSE,
        tickformat = ",",
        title = ""
      ),
      xaxis = list(showgrid = FALSE, title = ""),
      margin=list(l = 50,r = 50,b = 50,t = 50,pad = 4)
    )
})


# 清除选择 --------------------------------------------------------------------

observeEvent(input$tab_data_btn_cancel_allfile, {
  ra_project$file_single <- NULL
  ra_project$file_train <- NULL
  ra_project$file_test <- NULL
  ra_project$file_validate <- NULL
  ra_project$file_setting <- NULL
})



# 删除数据 --------------------------------------------------------------------

observeEvent(input$tab_data_btn_remove_data, {
  showModal(modalDialog(
    title = "数据将被删除",
    "请确认",
    footer = tagList(
      actionButton("tab_data_table_tbn_confirm_all_remove", "确定删除"),
      modalButton("取消")
    )
  ))
})

observeEvent(input$tab_data_table_tbn_confirm_all_remove, {
  ra_tbl$tbl_train <- NULL
  ra_tbl$tbl_test <- NULL
  ra_tbl$tbl_validate <- NULL
  ra_project$tbl_stat <- psbc_risk_template$tbl_stat
  if ((!is.null(ra_project$tbl_vars) &&
       nrow(ra_project$tbl_vars) > 0) ||
      (
        !is.null(ra_project$tbl_special_values) &&
        nrow(ra_project$tbl_special_values) > 0
      ))  {
    showModal(modalDialog(
      title = "参数文件将被删除",
      "请再次确认",
      footer = tagList(
        actionButton("tab_data_table_tbn_confirm_setting_remove", "确认删除"),
        modalButton("取消")
      )
    ))
  }
})

observeEvent(input$tab_data_table_tbn_confirm_setting_remove, {
  ra_project$tbl_vars <- psbc_risk_template$tbl_vars
  ra_project$tbl_special_values <-
    psbc_risk_template$tbl_special_values
  removeModal()
  my_log("INFO",
         ra_config$debug_level,
         "(tab_data_table)",
         "all ")
})