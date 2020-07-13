tooltips_tbl <-
  "1.文件包含同名object；\n2.data.frame包含label字段；"

# # 参数文件
# addPopover(session,
#            id = "tab_data_btn_tbl_setting",
#            title = "要求",
#            content = "模板参照setting_template,仅支持xls/xlsx，包含的sheet有special_values/bin_groups/vars")
# # load所有文件
# addPopover(session,
#            id = "tab_data_btn_load_allfile",
#            title = "警告",
#            content = "允许部分文件不提供，不检测文件是否存在，完成后请检查右侧的更新时间dt")
# # 执行计算
# addPopover(session,
#            id = "tab_data_mutate_btn_apply",
#            title = "警告",
#            content = "在内存中的数据执行计算，如需在原始数据执行计算，请重新读取数据")

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