
load("./examples/example_project/project.Rdata")
load("./examples/example_project/tbl_single.Rdata")
project
project$lst_setting_all
project$lst_setting_train


project$lst_special_train
project$lst_setting_train

project$tbl_varstat %>% 
  filter(var=="duration_in_month") %>% 
  "[["("type") %>% 
  "["(1)

woebin(
  tbl_single,
  y = "label",
  x = "foreign_worker",
  # save_breaks_list = file,
  method = "tree",
  bin_num_limit = 8,
  stop_limit = 0.1,
  count_distr_limit = 0.05,
  special_values=list(foreign_worker=c("缺失","_NULL_","-SPECIAL_VALUE-")),
  no_cores=1
)

df = tbl_single
vars = var
src_rst = project$lst_bin_train_for_train
src="train"

rst_train_for_test <- get_binning_from_rst(df = df,
                     vars = var,
                     src_rst = project$lst_bin_train_for_train,
                     src="train")
rst_train_for_test_stat <-
  get_tbl_stat_bin(
    vars = var,
    rst = rst_train_for_test,
    exp_rst = project$lst_bin_train_for_train,
    sample_type = "test",
    bin_src = "train"
  )
