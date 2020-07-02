


# debug代码 -----------------------------------------------------------------


reactive({
  print(ra_project$mutate_status)
})

# 代码示例 --------------------------------------------------------------------
output$tab_data_mutate_text_desc <- renderPrint({
  switch(
    input$tab_data_mutate_rdo_type,
    "manual" = "muate(a)中的a，除外赋值所在行其他行需要缩进(用于检测变动的字段)。示例：\nx2=a+b,\nx3=case_when(\n a>5 | b>3 ~ 1,\n a>2 | b>1 ~ 2,\n TRUE ~ NA_integer_)",
    "all" = "应用至所有字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "int" = "应用至所有integer和interger64字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "double" = "应用至所有numeric但不是integer、interger64的字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "factor" = "应用至所有factor字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "string" = "应用至所有character字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "range" = "应用至字段列表或字段范围，第一行是列表或范围，示例：\na-b\nfunciton(a){ifelse(a>0,1,0)}",
    "advanced" = "暂不支持"
  ) %>%
    cat()
})


# 执行代码表 -------------------------------------------------------------------

output$tab_data_mutate_tbl <- DT::renderDT({
  ra_project$tbl_mutate %>%
    DT::datatable(
      selection = "single",
      rownames = FALSE,
      options = list(lengthChange = TRUE,searching=FALSE)
    )
},
server = TRUE)

# 新增和修改状态 -----------------------------------------------------------------
observeEvent(ra_project$mutate_status, {
  if (ra_project$mutate_status == "new") {
    enable("tab_data_mutate_rdo_type")
    enable("tab_data_mutate_btn_add")
    enable("tab_data_mutate_btn_edit")
    disable("tab_data_mutate_btn_edit_confirm")
    disable("tab_data_mutate_btn_edit_cancel")
  } else if (ra_project$mutate_status == "edit") {
    disable("tab_data_mutate_rdo_type")
    disable("tab_data_mutate_btn_add")
    disable("tab_data_mutate_btn_edit")
    disable("tab_data_mutate_btn_edit_confirm")
    disable("tab_data_mutate_btn_edit_cancel")
  } else if (ra_project$mutate_status == "update") {
    disable("tab_data_mutate_rdo_type")
    disable("tab_data_mutate_btn_add")
    disable("tab_data_mutate_btn_edit")
    disable("tab_data_mutate_btn_edit_confirm")
    disable("tab_data_mutate_btn_edit_cancel")
  }
})



# 修改按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_edit, {
  ra_project$mutate_status == "edit"
})

# 确定修改按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_edit_confirm, {
  ra_project$mutate_status == "add"
})

# 取消修改按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_edit_cancel, {
  ra_project$mutate_status == "add"
  updateTextAreaInput("tab_data_mutate_input_code", value = "")
})


# 试算按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_calculate, {
  if (is.null(ra_tbl$tbl_train) & is.null(ra_tbl$tbl_test) & is.null(ra_tbl$tbl_validate)) {
    return()
  }
  if ((!is.null(ra_tbl$tbl_train)) & nrow(ra_tbl$tbl_train)>0) {
    df <- ra_tbl$tbl_train %>% 
      top_n(10)
  } else if ((!is.null(ra_tbl$tbl_test)) & nrow(ra_tbl$tbl_test)>0) {
    df <- ra_tbl$tbl_test %>% 
      top_n(10)
  } else if((!is.null(ra_tbl$tbl_validate)) & nrow(ra_tbl$tbl_validate)>0) {
    df <- ra_tbl$tbl_validate %>% 
      top_n(10)
  } else {
    return()
  }
  
  tryCatch({
    
      
    if (ra_project$mutate_status == "edit") {
      ra_project$mutate_status = "update"
    }
  }, error = function(e) {
    if (ra_project$mutate_status == "update") {
      ra_project$mutate_status = "edit"
    }
    showModal(modalDialog(title = "试算错误",
                          file,
                          footer = tagList(modalButton("返回修改"))))
    
  })
  updateTextAreaInput(session,"tab_data_mutate_input_code", value = "")
})


# 试算后输入又有变动 -------------------------------------------------------------------
observeEvent(input$tab_data_mutate_input_code, {
  if (ra_project$mutate_status == "update") {
    ra_project$mutate_status = "edit"
  }
})
