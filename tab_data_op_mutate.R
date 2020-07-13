







# debug代码 -----------------------------------------------------------------



# 代码示例 --------------------------------------------------------------------
output$tab_data_mutate_text_desc <- renderPrint({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "rendering code tips "
  )
  switch(
    input$tab_data_mutate_rdo_type,
    "manual" = "mutate(a)中的a，除外赋值所在行其他行需要缩进(用于检测变动的字段)。示例：\nx2=a+b,\nx3=case_when(\n a>5 | b>3 ~ 1,\n a>2 | b>1 ~ 2,\n TRUE ~ NA_integer_)",
    "all" = "应用至字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "real" = "应用至数值,包括interget、integer64、numeric，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "integer" = "应用至integer、interger64字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "numeric" = "应用至numeric但不是integer、interger64的字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "factor" = "应用至factor字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "factor_real"= "应用至levels能转数值的factor字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "factor_character"= "应用至levels无法转数值的factor字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "character" = "应用至character字段，示例：\nfunciton(a){ifelse(a>0,1,0)}",
    "advanced" = "第一行是dplyr::select()中的表达，示例：应用至从a到b的所有字段\na:b\nfunciton(a){ifelse(a>0,1,0)}"
  ) %>%
    cat()
})


# 执行代码表 -------------------------------------------------------------------

output$tab_data_mutate_tbl <- DT::renderDT({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "rendering table "
  )
  ra_project$tbl_mutate %>%
    DT::datatable(
      selection = "single",
      rownames = FALSE,
      options = list(
        orderClasses = TRUE,
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollx = TRUE,
        scrolly = TRUE,
        target = "row",
        selected = ra_data$mutate_selected
      )
    )
},
server = TRUE)

# 输出mutate_status给conditionalPanel -----------------------------------------------------------------
output$mutate_status <- reactive({
  ra_data$mutate_status
})

outputOptions(output, "mutate_status", suspendWhenHidden = FALSE)

# 监控mutate_status变化 -------------------------------------------------------
observeEvent(ra_data$mutate_status, {
  if (ra_data$mutate_status %in% c("view", "select")) {
    disable("tab_data_mutate_rdo_type")
    disable("tab_data_mutate_input_note")
    disable("tab_data_mutate_input_code")
  }
  else   {
    enable("tab_data_mutate_rdo_type")
    enable("tab_data_mutate_input_note")
    enable("tab_data_mutate_input_code")
  }
  # 试算
  if (ra_data$mutate_status %in% c("select")) {
    disable("tab_data_mutate_btn_calculate")
  }
  else   {
    enable("tab_data_mutate_btn_calculate")
  }
  # 执行计算
  if (ra_data$mutate_status %in% c("add")) {
    enable("tab_data_mutate_btn_apply")
  }
  else   {
    disable("tab_data_mutate_btn_apply")
  }
  
})



# 监控选择变化 ------------------------------------------------------------------

ra_data_mutate_select_record <- reactive({
  if (is.null(input$tab_data_mutate_tbl_rows_selected)) {
    return(NULL)
  } else {
    ra_project$tbl_mutate[input$tab_data_mutate_tbl_rows_selected,] %>%
      unlist() %>%
      as.list() %>%
      return()
  }
})


# 显示选择 --------------------------------------------------------------------

output$tab_data_mutate_text_select <- renderText({
  s <- ra_data$mutate_select
  if (is.null(s)) {
    ""
  } else {
    sprintf("第%s个:%s", s$id, s$note)
  }
})


# 选择按钮 --------------------------------------------------------------------



observeEvent({
  input$tab_data_mutate_tbl_rows_selected
  ra_data$mutate_status
  1
}, {
  if ((!is.null(input$tab_data_mutate_tbl_rows_selected)) &
      ra_data$mutate_status %in% c("add", "calc")) {
    enable("tab_data_mutate_btn_select")
  } else {
    disable("tab_data_mutate_btn_select")
  }
})

observeEvent(input$tab_data_mutate_btn_select, {
  if (!is.null(ra_data_mutate_select_record())) {
    ra_data$mutate_select <- ra_data_mutate_select_record()
    ra_data$mutate_status <- "select"
    ra_data$mutate_selected <-
      as.integer(ra_data_mutate_select_record()$id)
  }
})


# 上升按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_up, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_up "
  )
  req(ra_data$mutate_select)
  s <- ra_data$mutate_select
  id_change <- as.integer(s$id)
  if (id_change != 1) {
    ra_project$tbl_mutate <- isolate(ra_project$tbl_mutate) %>%
      mutate(id = ifelse(
        id == id_change - 1,
        id_change,
        ifelse(id == id_change, id_change - 1, id)
      )) %>%
      dplyr::arrange(id)
    s$id <- id_change -1
    ra_data$mutate_select <- s
    ra_data$mutate_selected <- id_change - 1
    
  }
})



# 下降按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_down, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_down "
  )
  req(ra_data$mutate_select)
  s <- ra_data$mutate_select
  id_change <- as.integer(s$id)
  if (id_change != nrow(ra_project$tbl_mutate)) {
    ra_project$tbl_mutate <- ra_project$tbl_mutate %>%
      mutate(id = ifelse(
        id == id_change,
        id_change + 1,
        ifelse(id == id_change + 1, id_change, id)
      )) %>%
      dplyr::arrange(id)
    s$id <- id_change +1
    ra_data$mutate_select <- s
    ra_data$mutate_selected <- id_change + 1
  }
})

# 删除 ----------------------------------------------------------------------

observeEvent(input$tab_data_mutate_btn_delete, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_delete"
  )
  req(ra_data$mutate_select)
  id_change <- as.integer(ra_data$mutate_select$id)
  ra_project$tbl_mutate <- ra_project$tbl_mutate %>%
    dplyr::filter(id != id_change) %>%
    mutate(id = row_number())
  ra_data$mutate_selected <- NULL
  ra_data$mutate_select <- NULL
  ra_data$mutate_status <- "add"
})

# 查看按钮 --------------------------------------------------------------------


observeEvent(input$tab_data_mutate_btn_view, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_view "
  )
  updateRadioButtons(session,
                     "tab_data_mutate_rdo_type",
                     selected = ra_data$mutate_select$type)
  updateTextInput(session,
                  "tab_data_mutate_input_note",
                  value = ra_data$mutate_select$note)
  updateTextAreaInput(session,
                      "tab_data_mutate_input_code",
                      value = ra_data$mutate_select$code)
  ra_data$mutate_status <-  "view"
})



# 取消按钮 --------------------------------------------------------------------


observeEvent(input$tab_data_mutate_btn_cancel, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_cancel "
  )
  ra_data$mutate_status <-  "add"
})

# 修改按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_edit, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_modify "
  )
  ra_data$mutate_status <-  "edit"
})

# 确定修改按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_edit_confirm, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_modify_confirm "
  )
  ra_project$tbl_mutate <- ra_project$tbl_mutate %>%
    filter(id != as.integer(ra_data$mutate_select$id)) %>%
    bind_rows(
      data.frame(
        id = as.integer(ra_data$mutate_select$id),
        type = ra_data$mutate_select$type,
        note = input$tab_data_mutate_input_note,
        code = input$tab_data_mutate_input_code,
        stringsAsFactors = FALSE
      )
    ) %>%
    dplyr::arrange(id)
  ra_data$mutate_status <-  "add"
  updateTextAreaInput(session, "tab_data_mutate_input_code", value = "")
  updateTextInput(session, "tab_data_mutate_input_note", value = "")
})

# 取消修改按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_edit_cancel, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_modify_cancel "
  )
  ra_data$mutate_status <-  "add"
  updateTextAreaInput(session, "tab_data_mutate_input_code", value = "")
  updateTextInput(session, "tab_data_mutate_input_note", value = "")
})


# 添加按钮 --------------------------------------------------------------------
observeEvent(input$tab_data_mutate_btn_add, {
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "click btn_add "
  )
  if (is.null(input$tab_data_mutate_input_note) |
      str_trim(input$tab_data_mutate_input_note) == ""
      |
      is.null(input$tab_data_mutate_input_code) |
      str_trim(input$tab_data_mutate_input_code) == "") {
    showModal(modalDialog(title = "信息不全，请填写备注和代码",
                          footer = tagList(modalButton("返回"))))
    return()
  }
  ra_project$tbl_mutate <- ra_project$tbl_mutate %>%
    bind_rows(
      data.frame(
        id = nrow(ra_project$tbl_mutate) + 1,
        type = input$tab_data_mutate_rdo_type,
        note = input$tab_data_mutate_input_note,
        code = input$tab_data_mutate_input_code,
        stringsAsFactors = FALSE
      )
    )
  
  updateTextAreaInput(session, "tab_data_mutate_input_code", value = "")
  updateTextInput(session, "tab_data_mutate_input_note", value = "")
  ra_data$mutate_status <-  "add"
})




# 试算按钮 --------------------------------------------------------------------
source("tab_data_op_mutate_calc.R",
       local = TRUE,
       encoding = "UTF-8")


# 试算后输入又有变动 -------------------------------------------------------------------
observeEvent({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_data_mutate)",
    "statuc change back "
  )
  input$tab_data_mutate_rdo_type
  input$tab_data_mutate_input_note
  1
}, {
  if (ra_data$mutate_status == "update") {
    ra_data$mutate_status = "edit"
  } else if (ra_data$mutate_status == "calc") {
    ra_data$mutate_status = "add"
  }
})


# 执行计算 --------------------------------------------------------------------
source("tab_data_op_mutate_apply.R",
       local = TRUE,
       encoding = "UTF-8")


