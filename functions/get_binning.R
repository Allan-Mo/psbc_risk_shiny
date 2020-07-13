# df <- tbl_single
# vars <- setdiff(colnames(tbl_single),c("sample_type","label"))[1:2]
# lst_setting <- project$lst_setting_train
# result <- get_binning(df,vars,lst_setting,"train")

get_binning <-
  function(df = NULL,
           vars = NULL,
           lst_setting = NULL,
           sample_type = NULL) {
    # save(df, vars, tbl_vars, file = "testing.Rdata")
    rst <- list(lst_bin = list(), lst_setting = list())
    for (var in vars) {
      # update to empty when no bin generated
      rst$lst_bin[[var]] <- list()
      rst$lst_setting[[var]] <- lst_setting[[var]]
    }
    if (length(vars) == 0) {
      return(rst)
    }
    
    lapply(vars, function(var) {
      if (isTRUE(lst_setting[[var]]$bin_forceNULL)) {
        return()
      }
      lapply(c("tree", "chimerge", "freq", "manual"), function(method) {
        special_values <- NULL
        if ((!isTRUE(lst_setting[[var]]$special_forceNULL)) &&
            length(lst_setting[[var]]$special_values) > 0) {
          special_values[[var]] <- lst_setting[[var]]$special_values
        }
        tryCatch({
          if (method == "manual") {
            if (length(lst_setting[[var]]$breaks) == 0) {
              return()
            }
            else {
              breaks_list <- NULL
              breaks_list[[var]] <- lst_setting[[var]]$breaks
              rst$lst_bin[[var]]$manual <<-
                woebin(
                  df,
                  y = "label",
                  x = var,
                  breaks_list = breaks_list,
                  special_values = special_values
                )[[var]]
            }
          } else if (method == "freq" &&
                     (!class(df[[var]]) %in% c("integer", "integer64", "numeric"))) {
            return()
          } else {
            file <- tempfile()
            rst$lst_bin[[var]][[method]] <<- woebin(
              df,
              y = "label",
              x = var,
              save_breaks_list = file,
              method = method,
              bin_num_limit = lst_setting[[var]]$bin_num_limit,
              stop_limit = lst_setting[[var]]$stop_limit,
              count_distr_limit = lst_setting[[var]]$count_distr_limit,
              special_values = special_values
            )[[var]]
            file <-
              list.files(dirname(file), basename(file), full.names = TRUE)
            breaks_name <- sprintf("breaks_%s", method)
            rst$lst_setting[[var]][[breaks_name]] <<- file %>%
              readLines() %>%
              paste0(collapse = "") %>%
              parse(text = .) %>%
              eval() %>% 
              "[["(var)
            file.remove(file)
          }
        }, error = function(e) {
          message("============[error]===============")
          message(
            sprintf(
              "Binning error for sample_type=%s var=%s method=%s",
              sample_type,
              var,
              method
            )
          )
          print(e)
          message("============[error]===============")
          
        }) # end of tryCatch
        
      }) # end of method
      
      # pick the best as manual
      if (length(lst_setting[[var]]$breaks) == 0) {
        methods <- names(rst$lst_bin[[var]])
        if (length(methods) == 1) {
          rst$lst_bin[[var]]$manual <- rst$lst_bin[[var]][[methods]]
          rst$lst_setting[[var]]$breaks <-
            rst$lst_setting[[sprintf("breaks_%s", methods)]]
          rst$lst_setting[[var]]$breaks_n <<- length(rst$lst_setting[[var]]$breaks)
          rst$lst_setting[[var]]$breaks_raw <<- sprintf("c(%s)",paste0(quote(rst$lst_setting[[var]]$breaks),collapse = ","))
        } else if (length(methods) > 1)  {
          total_iv <-
            sapply(methods, function(x) {
              rst$lst_bin[[var]][[x]]$total_iv[1]
            })
          method <- methods[which.max(total_iv)]
          rst$lst_bin[[var]]$manual <<- rst$lst_bin[[var]][[method]]
          rst$lst_setting[[var]]$breaks <<-
            rst$lst_setting[[var]][[sprintf("breaks_%s", method)]]
          rst$lst_setting[[var]]$breaks_n <<- length(rst$lst_setting[[var]]$breaks)
          rst$lst_setting[[var]]$breaks_raw <<- sprintf("c(%s)",paste0(quote(rst$lst_setting[[var]]$breaks),collapse = ","))
        }
        
      }
      
    }) # end of var
    
    
    return(rst)
    
  }

# load("testing.Rdata")
# get_binning(df, vars, tbl_vars)

# load("./examples/example_project/tbl_single.Rdata")
# tbl_vars <- readxl::read_excel("./examples/example_project/setting_template.xlsx",sheet="vars")
# df <- tbl_single
# vars <- setdiff(colnames(tbl_single),c("label","id","sample_type"))
#
# get_binning(df=df,vars=vars
#             ,tbl_vars=tbl_vars)
