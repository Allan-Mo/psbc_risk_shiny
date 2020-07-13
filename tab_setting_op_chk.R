# tbl_special_values=name,type,value_num,value_char,note
# tbl_vars=var,type,special_value_ref,bin_num_limit,stop_limit,count_distr_limit,breaks
# tbl_setting_chk=sample_type,var,chk
# lst_setting_train=list(special_forceNULL=c(),bin_forceNULL=c()
# ,var_x=list(special_name='',special_value_type='',special_values=c(...),special_n=1L,
# bin_num_limit=,stop_limit=,count_distr_limit=,
# breaks=c(...),breaks_raw='',breaks_n=1L))

# 重新写一个 --------------------------------------------------------
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 设置了优先级，依赖的ra_data$n_data一定要设置更高优先级
observeEvent({
  ra_project$tbl_special_values
  ra_project$tbl_vars
  ra_tbl$tbl_train
  ra_tbl$tbl_test
  ra_tbl$tbl_validate
  1
}, {
  # 初始化
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "start"
  )
  
  lst_setting_default = list(special_forceNULL = character(0),
                             bin_forceNULL = character(0))
  special_forceNULL <- character(0)
  bin_forceNULL <- character(0)
  tbl_setting_chk <- psbc_risk_template$tbl_setting_chk
  lst_setting_train <- lst_setting_default
  lst_setting_test <- lst_setting_default
  lst_setting_validate <- lst_setting_default
  lst_setting_all <- lst_setting_default

  item_default = list(
    special_forceNULL=FALSE,
    special_name = NULL,
    special_type = NULL,
    special_values = NULL,
    special_n = 0L,
    bin_forceNULL=FALSE,
    bin_num_limit = 8,
    stop_limit = 0.1,
    count_distr_limit = 0.05,
    breaks = NULL,
    breaks_raw = NULL,
    breaks_n = 0L
  )
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "(tab_setting_chk)",
    "columns"
  )
  source("tab_setting_op_chk_col.R",local = TRUE,encoding = "UTF-8")


  has_specials <-
    ((!is.null(ra_project$tbl_special_values)) &&
       nrow(ra_project$tbl_special_values) > 0)
  has_vars <-
    ((!is.null(ra_project$tbl_vars)) && nrow(ra_project$tbl_vars) > 0)


  
  if ((!has_specials) && (!has_vars)) {
    # 无special，无vars，item返回默认值即可
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_setting_chk)",
      "special=no and vars=no : start"
    )
    source("tab_setting_op_chk_none.R",local = TRUE,encoding = "UTF-8")


  } else {
    # check special and generate lst
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_setting_chk)",
      "special check : start"
    )
    lst_special <- list() # list(name_x=list(type=,values=))
    source("tab_setting_op_chk_special.R",local = TRUE,encoding = "UTF-8")
    # check vars and generate lst
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_setting_chk)",
      "vars check : start"
    )
    lst_vars <- list() # list(var_x=list(special_value_ref=,bin_num_limit=,stop_limit=,count_distr_limit=,breaks))
    source("tab_setting_op_chk_var.R",local = TRUE,encoding = "UTF-8")


    if (has_specials && (!has_vars)) {
      # 有special，无vars，item设置默认的special即可
      my_log(
        "DEBUG",
        ra_config$debug_level,
        "(tab_setting_chk)",
        "special=yes and vars=no : start"
      )
      source("tab_setting_op_chk_hasspecial.R",local = TRUE,encoding = "UTF-8")

    } else  {
      # 无special，有vars
      # 有special，有vars
      # var在lst_vars直接取，否则lst_special中取default
      my_log(
        "DEBUG",
        ra_config$debug_level,
        "(tab_setting_chk)",
        "vars=yes : start"
      )
      source("tab_setting_op_chk_hasvar.R",local = TRUE,encoding = "UTF-8")
    }
  }
  # 输出结果
  if (sum(tbl_setting_chk$level=="error")>0) {
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_setting_chk)",
      "finish with error"
    )
    ra_project$lst_setting_train <- lst_setting_default
    ra_project$lst_setting_test <- lst_setting_default
    ra_project$lst_setting_validate <- lst_setting_default
    ra_project$lst_setting_all <- lst_setting_default
  } else {
    ra_project$lst_setting_train <- lst_setting_train
    ra_project$lst_setting_test <- lst_setting_test
    ra_project$lst_setting_validate <- lst_setting_validate
    ra_project$lst_setting_all <- lst_setting_all
    my_log(
      "DEBUG",
      ra_config$debug_level,
      "(tab_setting_chk)",
      "finish without error"
    )
  }
})







# tab_setting_table_chk ---------------------------------------------------
output$tab_setting_table_chk <- renderDT({
  my_log(
    "DEBUG",
    ra_config$debug_level,
    "((tab_setting_chk)) tab_setting_table_chk table rendering"
  )
  
  ra_project$tbl_setting_chk %>%
    datatable(
      selection = "none",
      rownames = TRUE,
      options = list(
        lengthChange = TRUE,
        searching = FALSE,
        paging = FALSE,
        scrollX = FALSE
      )
    )
}, server = TRUE)


# tab_setting_btn_download ------------------------------------------------

observeEvent(input$tab_setting_btn_download,
             {
               showModal(modalDialog(title = "功能未开发完成",
                                     footer = tagList(modalButton("返回"))))
             })



# =================以下废弃 ---------------------------------------------------

#
#
# # setting tbl to list -----------------------------------------------------
# # lst_special_values=list(var_x=list(type=numric/character,values=c()),var_y=list(...))
# # lst_vars=list(var_x=list(type=integer/numeric/character/facter,bin_num_limit=,stop_limit=,count_distr_limit=,breaks=,special_values=c(...)))
# # 特殊情况，上面lst_vars的special_values=list(forceNULL=TRUE)
# observeEvent({
#   ra_project$tbl_special_values
#   ra_project$tbl_vars
#   1
# }, {
#   ok <- TRUE
#   my_log("DEBUG",
#          ra_config$debug_level,
#          "((tab_setting_chk)) update setting list start")
#   # special value to list
#   lst_special_values <- list()
#   if (is.null(ra_project$tbl_special_values)) {
#
#   } else if (nrow(ra_project$tbl_special_values) == 0) {
#
#   } else {
#     df <- ra_project$tbl_special_values
#
#     for (x in unique(df$name)) {
#       sub <- df %>%
#         dplyr::filter(name == x)
#       lst_special_values[[x]] <-
#         list(type = sub[["type"]][1],
#              values = if (sub[["type"]][1] ==
#                           "numeric")
#              {
#                sub[["value_num"]]
#              } else {
#                sub[["value_char"]]
#              })
#     }
#
#   } # end of special values
#
#   # vars to list list(a=list(type=,values=),b=...)
#   lst_vars <- list()
#   if (is.null(ra_project$tbl_vars)) {
#
#   } else if (nrow(ra_project$tbl_vars) == 0) {
#
#   } else {
#     df <- ra_project$tbl_vars
#
#     for (x in df$var) {
#       # 转成data.table了....
#       sub <- df %>% filter(var == x) %>% as.list()
#       var <- sub$var
#       type <- sub$type
#       special_value_ref <- sub$special_value_ref
#       bin_num_limit <- sub$bin_num_limit
#       stop_limit <- sub$stop_limit
#       count_distr_limit <- sub$count_distr_limit
#       breaks <- sub$breaks
#       bin_num_limit <-
#         ifelse(is.na(bin_num_limit), 8, bin_num_limit)
#       stop_limit <- ifelse(is.na(stop_limit), 0.1, stop_limit)
#       count_distr_limit <-
#         ifelse(is.na(count_distr_limit), 0.05, count_distr_limit)
#       if (is.null(breaks) ||
#           is.na(breaks) || str_trim(breaks) == "") {
#         breaks <- NULL
#       } else {
#         tryCatch({
#           breaks <- eval(parse(text = breaks))
#         },
#         error = function(e) {
#           showModal(modalDialog(
#             title = "自定义分箱breaks错误",
#             sprintf("var=%s的breaks无法计算，请检查：%s", var, breaks),
#             footer = tagList(modalButton("返回"))
#           ))
#           req(FALSE)
#
#         })
#
#       }
#
#       lst <-
#         list(
#           type = type,
#           bin_num_limit = bin_num_limit,
#           stop_limit = stop_limit,
#           count_distr_limit = count_distr_limit,
#           breaks = breaks
#         )
#
#       if (is.na(special_value_ref)) {
#         lst$special_values <-
#           if (type %in% c("integer", "integer64", "numeric")) {
#             lst_special_values$default_num
#           } else {
#             lst_special_values$default_char
#           }
#       } else if (special_value_ref == "NULL") {
#         lst$special_values <-
#           list(forceNULL = TRUE)  #<<<<<<<<<<<<<<<<--------force null
#       } else {
#         lst$special_values <- lst_special_values[[special_value_ref]]
#       }
#
#       lst_vars[[var]] <- lst
#
#     }
#
#
#   } # end of vars to list
#
#   ra_project$lst_special_values <- lst_special_values
#   ra_project$lst_vars <- lst_vars
#   my_log(
#     "DEBUG",
#     ra_config$debug_level,
#     "((tab_setting_chk)) update setting list successfully"
#   )
# })
#
#
# # setting chk -------------------------------------------------------------
#
#
# observeEvent({
#   ra_tbl$tbl_train
#   ra_tbl$tbl_test
#   ra_tbl$tbl_validate
#   ra_project$lst_vars
#   1
# },
# {
#   my_log(
#     "DEBUG",
#     ra_config$debug_level,
#     "((tab_setting_chk)) tab_setting_table_chk table trigger"
#   )
#   ra_project$tbl_setting_chk <- psbc_risk_template$tbl_setting_chk
#   req((length(ra_project$lst_vars) > 0) &&
#         ra_data$n_data > 0)
#   my_log(
#     "DEBUG",
#     ra_config$debug_level,
#     "((tab_setting_chk)) tab_setting_table_chk table start"
#   )
#   lst_vars <- ra_project$lst_vars
#   chk_train <- NULL
#   chk_test <- NULL
#   chk_validate <- NULL
#   # train
#   if (ra_data$has_train_tbl) {
#     col_type <- lapply(ra_tbl$tbl_train, class)
#     col_chk <- base::intersect(names(col_type), names(lst_vars))
#     chk_train <- lapply(col_chk, function(col) {
#       sv <- lst_vars[[col]]$special_values
#       if (is.null(sv$type)) {
#         return(NULL)
#       } else if (sv$type == "numeric") {
#         if (!col_type[[col]] %in% c("integer", "integer64", "numeric")) {
#           return(data.frame(
#             sample_type = "train",
#             var = col,
#             chk = sprintf("字段类型为%s,但special value类型为numeric.", col_type[[col]])
#           ))
#         }
#       } else {
#         if (col_type[[col]] %in% c("integer", "integer64", "numeric")) {
#           return(data.frame(
#             sample_type = "train",
#             var = col,
#             chk = sprintf("字段类型为%s,但special value类型为character.", col_type[[col]])
#           ))
#         }
#       }
#     }) %>%
#       rbindlist()
#   } # end train
#
#   # test
#   if (ra_data$has_test_tbl) {
#     col_type <- lapply(ra_tbl$tbl_test, class)
#     col_chk <- base::intersect(names(col_type), names(lst_vars))
#     chk_test <- lapply(col_chk, function(col) {
#       sv <- lst_vars[[col]]$special_values
#       if (is.null(sv$type)) {
#         return(NULL)
#       } else if (sv$type == "numeric") {
#         if (!col_type[[col]] %in% c("integer", "integer64", "numeric")) {
#           return(data.frame(
#             sample_type = "test",
#             var = col,
#             chk = sprintf("字段类型为%s,但special value类型为numeric.", col_type[[col]])
#           ))
#         }
#       } else {
#         if (col_type[[col]] %in% c("integer", "integer64", "numeric")) {
#           return(data.frame(
#             sample_type = "test",
#             var = col,
#             chk = sprintf("字段类型为%s,但special value类型为character.", col_type[[col]])
#           ))
#         }
#       }
#     }) %>%
#       rbindlist()
#   } # end test
#
#   # validate
#   if (ra_data$has_validate_tbl) {
#     col_type <- lapply(ra_tbl$tbl_validate, class)
#     col_chk <- base::intersect(names(col_type), names(lst_vars))
#     chk_validate <- lapply(col_chk, function(col) {
#       sv <- lst_vars[[col]]$special_values
#       if (is.null(sv$type)) {
#         return(NULL)
#       } else if (sv$type == "numeric") {
#         if (!col_type[[col]] %in% c("integer", "integer64", "numeric")) {
#           return(data.frame(
#             sample_type = "validate",
#             var = col,
#             chk = sprintf("字段类型为%s,但special value类型为numeric.", col_type[[col]])
#           ))
#         }
#       } else {
#         if (col_type[[col]] %in% c("integer", "integer64", "numeric")) {
#           return(data.frame(
#             sample_type = "validate",
#             var = col,
#             chk = sprintf("字段类型为%s,但special value类型为character.", col_type[[col]])
#           ))
#         }
#       }
#     }) %>%
#       rbindlist()
#   } # end validate
#   chk <- bind_rows(chk_train, chk_test, chk_validate)
#   if (nrow(chk) > 0) {
#     ra_project$tbl_setting_chk <- chk
#   }
#
#
#
#
#   my_log(
#     "DEBUG",
#     ra_config$debug_level,
#     "((tab_setting_chk)) tab_setting_table_chk table successfully"
#   )
# })
