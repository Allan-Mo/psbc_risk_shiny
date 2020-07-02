tooltips_tbl <-
  "1.文件包含同名object；\n2.data.frame包含label字段；"


# 单个数据文件
addPopover(session,
           id = "tab_data_btn_tbl_single",
           title = "要求",
           content = "1.文件包含同名object；\n2.data.frame包含sample_type和label字段；\n3.sample_type的取值包括train/test/validate")
# 训练文件
addPopover(session,
           id = "tab_data_btn_tbl_train",
           title = "要求",
           content = tooltips_tbl)
# 测试文件
addPopover(session,
           id = "tab_data_btn_tbl_test",
           title = "要求",
           content = tooltips_tbl)
# 验证文件
addPopover(session,
           id = "tab_data_btn_tbl_validate",
           title = "要求",
           content = tooltips_tbl)