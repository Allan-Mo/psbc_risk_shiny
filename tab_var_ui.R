tab_var_ui <- tabPanel("变量",
                       tagList(
                         useShinyjs(),
                         navbarPage("hehe",
                           tabPanel("test"),
                           tabPanel("test2")
                         )),
                       fluidRow(
                         column(2,actionButton("tab_var_btn_restat","重新统计")),
                         column(2,actionButton("tab_var_btn_toint","转int")),
                         column(2,actionButton("tab_var_btn_tostr","转str")),
                         column(2,actionButton("tab_var_btn_tofac","转factor"))
                       ),
                       h3("数值型：变量、类型、缺失率、unique数量、特殊值、"))
