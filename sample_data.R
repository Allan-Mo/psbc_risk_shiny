n <- 10000
rnorm_limit <- function(n, mean, sd, lwr, upr) {
  samp <- rnorm(n * 10, mean, sd)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  }
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}


zdt_single <- data.frame(
  id = 1:n,
  sample_type = sample(
    c("train", "test", "validate"),
    n,
    prob = c(0.6, 0.2, 0.2),
    replace = T
  ),
  
  label = sample(c(1, 0), n, prob = c(0.6, 0.4), replace = T),
  gender = sample(
    c("male", "female"),
    n,
    prob = c(0.65, 0.35),
    replace = T
  ),
  education = sample(
    c("高中及以下", "本科", "研究生"),
    n,
    prob = c(0.5, 0.35, 0.15),
    replace = T
  ),
  age = as.integer(rnorm_limit(n, 35, 10, 18, 50)),
  income = rnorm_limit(n, 5000, 100, 1000, 50000)
) %>%
  mutate(education = as.factor(education))

zdt_train <- zdt_single %>%
  filter(sample_type == "train")


zdt_test <- zdt_single %>%
  filter(sample_type == "test")


zdt_validate <- zdt_single %>%
  filter(sample_type == "validate")

save(zdt_train
     ,
     file = "./examples/zdt/zdt_train.Rdata",
     compress = TRUE)

save(zdt_test
     ,
     file = "./examples/zdt/zdt_test.Rdata",
     compress = TRUE)
save(zdt_validate
     ,
     file = "./examples/zdt/zdt_validate.Rdata",
     compress = TRUE)
save(zdt_single
     ,
     file = "./examples/zdt/zdt_single.Rdata",
     compress = TRUE)

# no-object
save(zdt_single
     ,
     file = "./examples/zdt/zdt_single_no_object.Rdata",
     compress = TRUE)

zdt_single_no_sample_type <- zdt_single %>% 
  select(-sample_type)
save(zdt_single_no_sample_type
     ,
     file = "./examples/zdt/zdt_single_no_sample_type.Rdata",
     compress = TRUE)

zdt_single_no_label <- zdt_single %>% 
  select(-label)
save(zdt_single_no_label
     ,
     file = "./examples/zdt/zdt_single_no_label.Rdata",
     compress = TRUE)

zdt_single_sample_type <- zdt_single %>% 
  mutate(sample_type=ifelse(runif(1)<0.1,NA,sample_type))
save(zdt_single_sample_type
     ,
     file = "./examples/zdt/zdt_single_sample_type.Rdata",
     compress = TRUE)
