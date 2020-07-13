# part 1 ------------------------------------------------------------------
tab_setting_ui_bin_p1 <- fluidRow(
  column(9,
         DTOutput("tab_setting_bin_table")),
  column(
    2,
    h2("bin groups"),
    conditionalPanel(
      "output.setting_bin_status=='tblselect' "
      ,
      actionButton("tab_setting_bin_btn_select", "select")
      
    ),
    
    conditionalPanel(
      "output.setting_bin_status=='view' || output.setting_bin_status=='tblselect' || output.setting_bin_status=='select' "
      ,
      
      actionButton("tab_setting_bin_btn_addvalue", "add value")
      
    ),
    conditionalPanel(
      "output.setting_bin_status=='select' "
      ,
      actionButton("tab_setting_bin_btn_edit", "edit")
      
    ),
    conditionalPanel(
      "output.setting_bin_status=='select' "
      ,
      
      actionButton("tab_setting_bin_btn_delete", "delete value")
      
    ),
    conditionalPanel(
      "output.setting_bin_status=='add' "
      ,
      actionButton("tab_setting_bin_btn_add_confirm", "confirm")
      
    ),
    conditionalPanel(
      "output.setting_bin_status=='add' "
      ,
      
      actionButton("tab_setting_bin_btn_add_cancel", "cancel")
      
    ),
    conditionalPanel(
      "output.setting_bin_status=='edit' "
      ,
      
      actionButton("tab_setting_bin_btn_edit_confirm", "confirm")
      
    ),
    conditionalPanel(
      "output.setting_bin_status=='edit' "
      ,
      
      actionButton("tab_setting_bin_btn_edit_cancel", "cancel")
    )
    
  )
)




# input -------------------------------------------------------------------


tab_setting_ui_bin_p2 <- conditionalPanel(
  "output.setting_bin_status=='add' || output.setting_bin_status=='edit'",
  fluidRow(
    column(2,
           textInput("tab_setting_bin_input_name", label = "name")),
    column(
      3,
      sliderInput(
        "tab_setting_bin_input_bin_num_limit",
        "bin_num_limit",
        min = 2,
        max = 20,
        value = 8,
        step = 1
      )
    ),
    column(
      3,
      sliderInput(
        "tab_setting_bin_input_stop_limit",
        "stop_limit",
        value = 0.1,
        min = 0.01,
        max = 0.5,
        step = 0.01
      )
    ),
    column(
      3,
      sliderInput(
        "tab_setting_bin_input_count_distr_limit",
        "count_distr_limit",
        value = 8,
        min = 0.01,
        max = 0.20,
        step = 0.01
      )
    ),
    column(2,
           textInput("tab_setting_bin_input_notes", label = "note"))
  )
)