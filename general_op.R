




# 系统配置 --------------------------------------------------------------------

ra_config <- reactiveValues(
  tbl_project = config$tbl_project,
  select_project = NULL,
  log_level = config$log_level
)



observeEvent({
  ra_config$tbl_project
  ra_config$select_project
  ra_config$log_level
  1
}, {
  config <- reactiveValuesToList(ra_config, all.names = FALSE)
  save(config, file = "config.Rdata")
})


# 项目配置 --------------------------------------------------------------------


ra_project <- reactiveValues(
  tbl_type = "split",
  file_single = NULL,
  file_train = NULL,
  file_test = NULL,
  file_validate = NULL,
  file_setting = NULL,
  tbl_stat = NULL,
  tbl_mutate = NULL,
  tbl_varstat = NULL,
  tbl_varstat_filtered=NULL,
  tbl_special_values = NULL,
  tbl_vars = NULL,
  tbl_setting_chk = NULL,
  # list
  lst_special_values = list(),  
  lst_vars = list(),
  lst_setting_train=list(),
  lst_setting_test=list(),
  lst_setting_validate=list(),
  lst_setting_all=list(),
  # binning
  lst_bin_train_for_train=list(),
  lst_bin_train_for_test=list(),
  lst_bin_train_for_validate=list(),
  lst_bin_train_for_all=list(),
  lst_bin_all_for_train=list(),
  lst_bin_all_for_test=list(),
  lst_bin_all_for_validate=list(),
  lst_bin_all_for_all=list(),
  
  tbl_bin_train_for_train=psbc_risk_template$tbl_stat_bin,
  tbl_bin_train_for_test=psbc_risk_template$tbl_stat_bin,
  tbl_bin_train_for_validate=psbc_risk_template$tbl_stat_bin,
  tbl_bin_train_for_all=psbc_risk_template$tbl_stat_bin,
  tbl_bin_all_for_train=psbc_risk_template$tbl_stat_bin,
  tbl_bin_all_for_test=psbc_risk_template$tbl_stat_bin,
  tbl_bin_all_for_validate=psbc_risk_template$tbl_stat_bin,
  tbl_bin_all_for_all=psbc_risk_template$tbl_stat_bin,
  
  
  #  stat_table
  tbl_stat_general=psbc_risk_template$tbl_stat_general,
  tbl_stat_special=psbc_risk_template$tbl_stat_special,
  tbl_stat_bin=psbc_risk_template$tbl_stat_bin,
  tbl_stat_cat=psbc_risk_template$tbl_stat_cat,
  # stat_table - all
  tbl_stat_general_all = psbc_risk_template$tbl_stat_general,
  tbl_stat_special_all = psbc_risk_template$tbl_stat_special,
  lst_special_all = list(),
  # stat_table - train
  tbl_stat_general_train = psbc_risk_template$tbl_stat_general,
  tbl_stat_special_train = psbc_risk_template$tbl_stat_special,
  lst_special_train = list(),
  # stat_table - test
  tbl_stat_general_test = psbc_risk_template$tbl_stat_general,
  tbl_stat_special_test = psbc_risk_template$tbl_stat_special,
  lst_special_test = list(),
  # stat_table - validate
  tbl_stat_general_validate = psbc_risk_template$tbl_stat_general,
  tbl_stat_special_validate = psbc_risk_template$tbl_stat_special,
  lst_special_validate = list()
  
  # column list
  
)

observeEvent({
  ra_project$tbl_type
  ra_project$file_single
  ra_project$file_train
  ra_project$file_test
  ra_project$file_validate
  ra_project$file_setting
  ra_project$tbl_stat
  ra_project$tbl_mutate
  ra_project$tbl_varstat
  # ra_project$tbl_varstat_filtered
  ra_project$tbl_special_values
  ra_project$tbl_vars
  ra_project$lst_var_train_special
  ra_project$lst_var_train_col
  ra_project$lst_var_train_newcol
  ra_project$lst_var_test_special
  ra_project$lst_var_test_col
  ra_project$lst_var_test_newcol
  ra_project$lst_var_validate_special
  ra_project$lst_var_validate_col
  ra_project$lst_var_validate_newcol
  1
}, {
  if (!is.null(ra_config$select_project)) {
    project <- reactiveValuesToList(ra_project, all.names = FALSE)
    save(project,
         file = path(ra_config$select_project$path, "project.Rdata"))
  }
  
})

# 数据 ----------------------------------------------------------------------
ra_tbl <- reactiveValues(
  tbl_train = NULL,
  tbl_test = NULL,
  tbl_validate = NULL,
  tbl_modal = NULL,
  tbl_onevar=NULL,
  tbl_onevar_stat=NULL
)

ra_data <- reactiveValues(
  mutate_status = "add",
  mutate_select = NULL,
  mutate_selected = NULL,
  mutate_newcol_train=NULL,
  mutate_newcol_test=NULL,
  mutate_newcol_validate=NULL,
  mutate_newcol_all=NULL,
  file_setting_time = NULL,
  setting_special_status = "view",
  # status_info
  has_train_tbl=FALSE,
  has_validate_tbl=FALSE,
  has_test_tbl=FALSE,
  has_all_general=FALSE,
  has_train_general=FALSE,
  has_test_general=FALSE,
  has_validate_general=FALSE,
  has_all_special=FALSE,
  has_train_special=FALSE,
  has_test_special=FALSE,
  has_validate_special=FALSE,
  n_data=0L,
  stat_general=list(),
  stat_special=list(),
  stat_bin=list(),
  onevar_var=NULL,
  onevar_type=NULL
)


# 弹窗的table ----------------------------------------------------------------
output$tbl_modal <- renderDT({
  req(ra_tbl$tbl_modal)
  ra_tbl$tbl_modal %>%
    DT::datatable(
      selection = "none",
      rownames = TRUE,
      options = list(
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = TRUE,
        scrolly = TRUE
      )
    )
})



# logging -----------------------------------------------------------------
my_log <- function(level, level_threshold, ...) {
  if (length(level) != 1 || length(level_threshold) != 1) {return()} 
  level_n <- switch(
    level,
    "INFO" = 1,
    "WARNING" = 2,
    "ERROR" = 3,
    "DEBUG" = 0
  )
  level_threshold_n <-
    switch(
      level_threshold,
      "INFO" = 1,
      "WARNING" = 2,
      "ERROR" = 3,
      "DEBUG" = 0
    )
  if (level_n >= level_threshold_n) {
    level_c <- sprintf("[%s]", level) %>% str_pad(9, side = "right")
    cat(file = stderr(), format(Sys.time(), "%H:%M:%S"), level_c, ..., "\n")
  }
}


# table status ------------------------------------------------------------
observeEvent({ra_tbl$tbl_train
  ra_tbl$tbl_validate
  ra_tbl$tbl_test
  1
  },
              {
  # table
  ra_data$has_train_tbl <- ((!is.null(ra_tbl$tbl_train)) &&
                              nrow(ra_tbl$tbl_train) > 0)
  
  
  ra_data$has_test_tbl <- ((!is.null(ra_tbl$tbl_test)) &&
                             nrow(ra_tbl$tbl_test) > 0)
  ra_data$has_validate_tbl <- ((!is.null(ra_tbl$tbl_validate)) &&
                                 nrow(ra_tbl$tbl_validate) > 0)
  my_log("DEBUG",ra_config$log_level,"(general_op) has_train_tbl update as ",ra_data$has_train_tbl)
})

observe({
  # generel stat
  ra_data$has_all_general <-
    ((!is.null(ra_project$tbl_stat_general_all)) &&
       nrow(ra_project$tbl_stat_general_all) > 0)
  
  
  ra_data$has_train_general <-
    ((!is.null(ra_project$tbl_stat_general_train)) &&
       nrow(ra_project$tbl_stat_general_train) > 0)
  
  ra_data$has_test_general <-
    ((!is.null(ra_project$tbl_stat_general_test)) &&
       nrow(ra_project$tbl_stat_general_test) > 0)
  ra_data$has_validate_general <-
    ((!is.null(ra_project$tbl_stat_general_validate)) &&
       nrow(ra_project$tbl_stat_general_validate) > 0)
})

observe({
  # special values
  ra_data$has_all_special <-
    ((!is.null(ra_project$tbl_stat_special_all)) &&
       nrow(ra_project$tbl_stat_special_all) > 0)
  ra_data$has_train_special <-
    ((!is.null(ra_project$tbl_stat_special_train)) &&
       nrow(ra_project$tbl_stat_special_train) > 0)
  ra_data$has_test_special <-
    ((!is.null(ra_project$tbl_stat_special_test)) &&
       nrow(ra_project$tbl_stat_special_test) > 0)
  ra_data$has_validate_special <-
    ((!is.null(ra_project$tbl_stat_special_validate)) &&
       nrow(ra_project$tbl_stat_special_validate) > 0)
})

observe({
  # aggregate
  ra_data$n_data <- sum(ra_data$has_train_tbl,
                        ra_data$has_test_tbl,
                        ra_data$has_validate_tbl)
  my_log("DEBUG",ra_config$log_level,"(general_op) n_data update as ",ra_data$n_data)
  
},priority = 99)


# output for condition panel ----------------------------------------------

# 如果选择了label或者数据未准备好，图和显示对不上，需要在数据生成之后再赋值
output$onevar_var <- reactive({
  my_log("DEBUG",
         ra_config$debug_level,
         "(general_op)",
         "ra_data$onevar_var change to ",
         ra_data$onevar_var)  
  ra_data$onevar_var
})

output$onevar_type <- reactive({
  ra_data$onevar_type
})


outputOptions(output, "onevar_var", suspendWhenHidden = FALSE)
outputOptions(output, "onevar_type", suspendWhenHidden = FALSE)

