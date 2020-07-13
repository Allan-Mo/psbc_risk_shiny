tab_bin_ui <- tabPanel(
  title = "分箱",
  value = "bin",
  fluidRow(column(
    6,
    radioButtons(
      "tab_bin_rdo_binsrc",
      "分箱来源"
      ,
      choices = c("train" = "train", "all" = "all"),
      selected =
        "train"
    ),
    hr(),
    fluidRow(column(6,actionButton("tab_bin_btn_try","试算")),
             column(6,actionButton("tab_bin_btn_apply","修改"))),
    div(class="inline",textInput("tab_bin_input_breaks_1",label="1")),
    div(class="inline",textInput("tab_bin_input_breaks_2",label="2")),
    div(class="inline",textInput("tab_bin_input_breaks_3",label="3")),
    div(class="inline",textInput("tab_bin_input_breaks_4",label="4")),
    div(class="inline",textInput("tab_bin_input_breaks_5",label="5")),
    div(class="inline",textInput("tab_bin_input_breaks_6",label="6")),
    div(class="inline",textInput("tab_bin_input_breaks_7",label="7")),
    div(class="inline",textInput("tab_bin_input_breaks_8",label="8")),
    div(class="inline",textInput("tab_bin_input_breaks_9",label="9")),
    div(class="inline",textInput("tab_bin_input_breaks_10",label="10")),
    div(class="inline",textInput("tab_bin_input_breaks_11",label="11")),
    div(class="inline",textInput("tab_bin_input_breaks_12",label="12")),
    div(class="inline",textInput("tab_bin_input_breaks_13",label="13")),
    div(class="inline",textInput("tab_bin_input_breaks_14",label="14")),
    div(class="inline",textInput("tab_bin_input_breaks_15",label="15"))
  ),
  column(
    6, verbatimTextOutput("tab_bin_text_setting")
  )),
  h4("train"),
  fluidRow(column(6,plotOutput("tab_bin_plot_train_tree")),
           column(6,DTOutput("tab_bin_table_train_tree"))),
  h4("test"),
  fluidRow(column(6,plotOutput("tab_bin_plot_test_tree")),
           column(6,DTOutput("tab_bin_table_test_tree"))),
  h4("validate"),
  fluidRow(column(6,plotOutput("tab_bin_plot_validate_tree")),
           column(6,DTOutput("tab_bin_table_validate_tree"))),
  h4("all"),
  fluidRow(column(6,plotOutput("tab_bin_plot_all_tree")),
           column(6,DTOutput("tab_bin_table_all_tree"))),
  
)
