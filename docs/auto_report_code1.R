# auto_report_code1.R

library(tidyverse)
library(stringr)
library(data.table)
# library(ggplot2)

dt <- ggplot2::diamonds %>% setDT %>% .[order(get("price"))] %>% setcolorder("price"); dt

SAMPLE_SIZE = 15
RANDOM_SEED = 99; set.seed(RANDOM_SEED)

dt1 <- dt[sample(.N, SAMPLE_SIZE)] [order(get("price"))]; dt1

strTitle <- sprintf("Effect of depth on price (size=%02g, seed=%02g).csv", SAMPLE_SIZE, RANDOM_SEED)

fwrite(dt1, strTitle)


ggplot(dt1) + theme_bw() +
  geom_point(aes_string(x="depth", y="price",col="color", size="carat", shape="cut")) +
  labs(title = strTitle)


