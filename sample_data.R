data("germancredit")

tbl_single <- germancredit %>% 
  "names<-"(gsub("\\.","_",colnames(germancredit))) %>% 
  mutate(label=ifelse(creditability=="bad",1,0)) %>% 
  mutate(sample_type=ifelse(rnorm(nrow(.))<0.6,"train",ifelse(rnorm(nrow(.))<0.6,"test","validate"))) %>% 
  select(-creditability)
tbl_single %>% 
  group_by(sample_type) %>% 
  tally()

tbl_train <- tbl_single %>%
  filter(sample_type == "train")


tbl_test <- tbl_single %>%
  filter(sample_type == "test")


tbl_validate <- tbl_single %>%
  filter(sample_type == "validate")

save(tbl_train
     ,
     file = "./examples/example_project/tbl_train.Rdata",
     compress = TRUE)

save(tbl_test
     ,
     file = "./examples/example_project/tbl_test.Rdata",
     compress = TRUE)
save(tbl_validate
     ,
     file = "./examples/example_project/tbl_validate.Rdata",
     compress = TRUE)
save(tbl_single
     ,
     file = "./examples/example_project/tbl_single.Rdata",
     compress = TRUE)

# no-object
save(tbl_single
     ,
     file = "./examples/example_project/tbl_single_no_object.Rdata",
     compress = TRUE)

tbl_single_no_sample_type <- tbl_single %>% 
  select(-sample_type)
save(tbl_single_no_sample_type
     ,
     file = "./examples/example_project/tbl_single_no_sample_type.Rdata",
     compress = TRUE)

tbl_single_no_label <- tbl_single %>% 
  select(-label)
save(tbl_single_no_label
     ,
     file = "./examples/example_project/tbl_single_no_label.Rdata",
     compress = TRUE)

tbl_single_sample_type <- tbl_single %>% 
  mutate(sample_type=ifelse(runif(1)<0.1,NA,sample_type))
save(tbl_single_sample_type
     ,
     file = "./examples/example_project/tbl_single_sample_type.Rdata",
     compress = TRUE)
