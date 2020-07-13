# part 1 ------------------------------------------------------------------
tab_setting_ui_special_p1 <- fluidRow(
  column(9,
         DTOutput("tab_setting_special_table")),
  column(
    2,
    h2("special values"),
    conditionalPanel(
      "output.setting_special_status=='tblselect' "
      ,
      actionButton("tab_setting_special_btn_select", "select")
      
    ),
    conditionalPanel(
      "output.setting_special_status=='view' || output.setting_special_status=='tblselect'  || output.setting_special_status=='select' "
      ,
      actionButton("tab_setting_special_btn_addname", "add name")
      
    ),
    conditionalPanel(
      "output.setting_special_status=='tblselect' || output.setting_special_status=='select' "
      ,
      
      actionButton("tab_setting_special_btn_addvalue", "add value")
      
    ),
    conditionalPanel(
      "output.setting_special_status=='select' "
      ,
      
      actionButton("tab_setting_special_btn_addvalue", "add value")
      
    ),
    conditionalPanel(
      "output.setting_special_status=='select' "
      ,
      actionButton("tab_setting_special_btn_edit", "edit")
      
    ),
    conditionalPanel(
      "output.setting_special_status=='select' "
      ,
      
      actionButton("tab_setting_special_btn_delete", "delete value")
      
    ),
    conditionalPanel(
      "output.setting_special_status=='add' "
      ,
      actionButton("tab_setting_special_btn_add_confirm", "confirm")
      
    ),
    conditionalPanel(
      "output.setting_special_status=='add' "
      ,
      
      actionButton("tab_setting_special_btn_add_cancel", "cancel")
      
    ),
    conditionalPanel(
      "output.setting_special_status=='edit' "
      ,
      
      actionButton("tab_setting_special_btn_edit_confirm", "confirm")
      
    ),
    conditionalPanel(
      "output.setting_special_status=='edit' "
      ,
      
      actionButton("tab_setting_special_btn_edit_cancel", "cancel")
    )
    
  )
)




# input -------------------------------------------------------------------


tab_setting_ui_special_p2 <- conditionalPanel(
  "output.setting_special_status=='add' || output.setting_special_status=='edit'",
  fluidRow(
    column(2, verbatimTextOutput("tab_setting_special_text_id")),
    column(2, div(
      class = "inline",
      textInput("tab_setting_special_input_name", label = "name")
    )),
    column(2, div(
      class = "inline",
      selectInput(
        "tab_setting_special_input_type",
        label = "type",
        choices = c("numeric", "character"),
        selected = "numeric"
      )
    )),
    column(2, div(
      class = "inline",
      selectInput(
        "tab_setting_special_input_value_type",
        label = "value type",
        choices = c("single value" = "single", "multiple values" = "multiple"),
        selected = "single"
      )
    )),
    column(2, div(
      class = "inline",
      textInput("tab_setting_special_input_values", label = "value")
    )),
    column(2, div(
      class = "inline",
      textInput("tab_setting_special_input_notes", label = "note")
    )),
  )
)