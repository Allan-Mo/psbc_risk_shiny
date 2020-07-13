# return = list(lst_bin=list(x_var=list(tree=df,)),lst_breaks=list(x_var=list(tree=c(),chi=c())))


get_binning_from_rst <-
  function(df = NULL,
           vars = NULL,
           src_rst = NULL,
           sample_type = NULL,
           src = NULL) {
    rst <- list(lst_bin = list(), lst_setting = list())
    for (var in vars) {
      # update to empty when no bin generated
      rst$lst_bin[[var]] <- list()
      rst$lst_setting[[var]] <- src_rst$lst_setting[[var]]
    }
    if (length(vars) == 0 ||
        is.null(df) || nrow(df) == 0 || length(src_rst$lst_setting) == 0) {
      return(rst)
    }
    
    vars <- base::intersect(vars, names(src_rst$lst_setting))
    if (length(vars) == 0) {
      return(rst)
    }
    
    lapply(vars, function(var) {
      lapply(names(src_rst$lst_bin[[var]]), function(method) {
        special_values <- NULL
        if ((!isTRUE(src_rst$lst_setting[[var]]$special_forceNULL)) &&
            length(src_rst$lst_setting[[var]]$special_values) > 0) {
          special_values[[var]] <- src_rst$lst_setting[[var]]$special_values
        }
        breaks_list <- NULL
        if (method == "manual") {
          breaks_list[[var]] <-  src_rst$lst_setting[[var]]$breaks
        } else {
          breaks_name <- sprintf("breaks_%s", method)
          breaks_list[[var]] <-
            src_rst$lst_setting[[var]][[breaks_name]]
        }
        
        tryCatch({
          rst$lst_bin[[var]][[method]] <<-
            woebin(
              df,
              y = "label",
              x = var,
              breaks_list = breaks_list
              ,
              special_values = special_values
            )[[var]]
          
        }, error = function(e) {
          message("============[error]===============")
          message(
            sprintf(
              "[error] apply %s binning to sample_type=%s occurs error,  var=%s method=%s",
              src,
              sample_type,
              var,
              method
            )
          )
          print(e)
          message("============[error]===============")
          
        })
        return()
      })
      
    })
    return(rst)
    
  }



# testing -----------------------------------------------------------------
# load("./examples/example_project/project.Rdata")
# load("./examples/example_project/tbl_single.Rdata")
# df <- tbl_single
# vars <- setdiff(colnames(df),c("label"))
# src_rst <- project$lst_bin_train_for_train
# get_binning_from_rst(df, vars, src_rst)
