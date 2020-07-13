# stat all ----------------------------------------------------------------
# 分开写observeEvent，通过priority使得每次stat_general/stat_special的改变优先执行
# check data and stat status
observeEvent(input$tab_data_stat_btn_all,
             {
               ok <- TRUE
               my_log("DEBUG",
                      ra_config$log_level,
                      "(tab_data_stat) stat_all trigger ")
               
               if (ra_data$n_data == 0) {
                 showModal(modalDialog(
                   title = "数据全部为空",
                   "不进行任何处理",
                   footer = tagList(modalButton("返回"))
                 ))
                 ok <- FALSE
               }
               req(ok)
               # train
               if (!ra_data$has_train_tbl) {
                 if (ra_data$has_train_general) {
                   showModal(
                     modalDialog(
                       title = "train：无数据但有统计",
                       "返回先load数据，或删除统计，请选择。",
                       footer = tagList(
                         actionButton("tab_data_stat_confirm_remove_train_stat", "确认删除"),
                         modalButton("返回")
                       )
                     )
                   )
                 }
                 
               }
               # test
               if (!ra_data$has_test_tbl) {
                 if (ra_data$has_test_general) {
                   showModal(modalDialog(
                     title = "test：无数据但有统计",
                     "返回先load数据，或删除统计，请选择。",
                     footer = tagList(
                       actionButton("tab_data_stat_confirm_remove_test_stat", "确认删除"),
                       modalButton("返回")
                     )
                   ))
                 }
                 
               }
               # validate
               if (!ra_data$has_validate_tbl) {
                 if (ra_data$has_validate_general) {
                   showModal(
                     modalDialog(
                       title = "validate：无数据但有统计",
                       "返回先load数据，或删除统计，请选择。",
                       footer = tagList(
                         actionButton("tab_data_stat_confirm_remove_validate_stat", "确认删除"),
                         modalButton("返回")
                       )
                     )
                   )
                 }
                 
               }
               
               # all
               
               if (ra_data$n_data == 0) {
                 if (ra_data$has_all_general) {
                   showModal(modalDialog(
                     title = "all：无数据但有统计",
                     "返回先load数据，或删除统计，请选择。",
                     footer = tagList(
                       actionButton("tab_data_stat_confirm_remove_all_stat", "确认删除"),
                       modalButton("返回")
                     )
                   ))
                 }
                 
               }
               
             })

# train
observeEvent(input$tab_data_stat_btn_all,
             {
               my_log(
                 "DEBUG",
                 ra_config$debug_level,
                 "(tab_data_stat)",
                 "click btn_stat start train "
               )
               req(ra_data$has_train_tbl)
               my_log("DEBUG",
                      ra_config$log_level,
                      "(tab_data_stat) stat_all train: start")
               
               vars <- colnames(ra_tbl$tbl_train)
               ra_data$stat_general <-
                 list(sample_type = "train",
                      type = "all",
                      vars = vars)
               ra_data$stat_special <-
                 list(sample_type = "train",
                      type = "all",
                      vars = vars)
             })
# test
observeEvent(input$tab_data_stat_btn_all,
             {
               my_log(
                 "DEBUG",
                 ra_config$debug_level,
                 "(tab_data_stat)",
                 "click btn_stat start test "
               )
               req(ra_data$has_test_tbl)
               my_log("DEBUG",
                      ra_config$log_level,
                      "(tab_data_stat) stat_all test: start")
               
               vars <- colnames(ra_tbl$tbl_test)
               ra_data$stat_general <-
                 list(sample_type = "test",
                      type = "all",
                      vars = vars)
               ra_data$stat_special <-
                 list(sample_type = "test",
                      type = "all",
                      vars = vars)
             })
# validate

observeEvent(input$tab_data_stat_btn_all,
             {
               my_log(
                 "DEBUG",
                 ra_config$debug_level,
                 "(tab_data_stat)",
                 "click btn_stat start validate "
               )
               req(ra_data$has_validate_tbl)
               my_log("DEBUG",
                      ra_config$log_level,
                      "(tab_data_stat) stat_all validate: start")
               
               vars <- colnames(ra_tbl$tbl_validate)
               ra_data$stat_general <-
                 list(sample_type = "validate",
                      type = "all",
                      vars = vars)
               ra_data$stat_special <-
                 list(sample_type = "validate",
                      type = "all",
                      vars = vars)
             })
# all
observeEvent(input$tab_data_stat_btn_all,
             {
               my_log(
                 "DEBUG",
                 ra_config$debug_level,
                 "(tab_data_stat)",
                 "click btn_stat start all "
               )
               req(ra_data$n_data>0)
               my_log("DEBUG",
                      ra_config$log_level,
                      "(tab_data_stat) stat_all all: start")
               
               vars <- if (ra_data$has_train_tbl) {
                 colnames(ra_tbl$tbl_train)
               } else if (ra_data$has_test_tbl) {
                 colnames(ra_tbl$tbl_test)
               } else if (ra_data$has_validate_tbl) {
                 colnames(ra_tbl$tbl_validate)
               }
               ra_data$stat_general <-
                 list(sample_type = "all",
                      type = "all",
                      vars = vars)
               ra_data$stat_special <-
                 list(sample_type = "all",
                      type = "all",
                      vars = vars)
             })
# binning
observeEvent(input$tab_data_stat_btn_all,
             {
               my_log(
                 "DEBUG",
                 ra_config$debug_level,
                 "(tab_data_stat)",
                 "click btn_stat start bining "
               )
               vars <- if (ra_data$has_train_tbl) {
                 colnames(ra_tbl$tbl_train)
               } else if (ra_data$has_test_tbl) {
                 colnames(ra_tbl$tbl_test)
               } else if (ra_data$has_validate_tbl) {
                 colnames(ra_tbl$tbl_validate)
               }
               ra_data$stat_bin <-
                 list(type = "all",
                      vars = vars)
             })

# 删除统计确定 ------------------------------------------------------------------
source("tab_data_op_stat_remove.R",local = TRUE, encoding = "UTF-8")



# 统计响应 --------------------------------------------------------------------
source("tab_data_op_stat_general.R",local = TRUE, encoding = "UTF-8")


# special values响应 --------------------------------------------------------
source("tab_data_op_stat_special.R",local = TRUE, encoding = "UTF-8")



# binning -----------------------------------------------------------------
source("tab_data_op_stat_binning.R",local = TRUE, encoding = "UTF-8")




# tbl_stat_cat ------------------------------------------------------------

observeEvent(ra_project$tbl_vars,
             {
               my_log(
                 "DEBUG",
                 ra_config$debug_level,
                 "(tab_data_stat)",
                 "tbl_vars change "
               )
               req(ra_project$tbl_vars)
               
               if (is.null(ra_project$tbl_vars) ||
                   nrow(ra_project$tbl_vars) == 0)
               {
                 ra_project$tbl_stat_cat <- psbc_risk_template$tbl_stat_cat
                 
               } else {
                 ra_project$tbl_stat_cat <-
                   ra_project$tbl_vars %>%
                   select(all_of(colnames(psbc_risk_template$tbl_stat_cat)))
               }
             })


# 增量统计 --------------------------------------------------------------------

observeEvent(input$tab_data_stat_btn_new, {
  showModal(modalDialog(
    title = "功能未待开发",
    "功能未待开发",
    footer = tagList(modalButton("返回"))
  ))
})

# 增量分箱 --------------------------------------------------------------------


observeEvent(input$tab_data_stat_btn_bin_new, {
  showModal(modalDialog(
    title = "功能未待开发",
    "功能未待开发",
    footer = tagList(modalButton("返回"))
  ))
})