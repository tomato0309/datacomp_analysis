#############################################################################
#  モデリング用のスクリプト
#############################################################################

## ライブラリ読み込み
if(!require("data.table")){install.packages("data.table")}
if(!require("dplyr")){install.packages("dplyr")}
if(!require("xlsx")){install.packages("xlsx")}
if(!require("reshape2")){install.packages("reshape2")}
if(!require("foreach")){install.packages("foreach")}
if(!require("tcltk")){install.packages("tcltk")}
if(!require("readr")){install.packages("readr")}
if(!require("stringr")){install.packages("stringr")}

# データの読み込み
data <- data.frame(read_csv("~/Documents/データコンペ/middle_table/beer_test_data.csv",locale=locale(encoding = "CP932"))) %>% select(-X1)
demo <- data.frame(read_csv("~/Documents/データコンペ/middle_table/consumer_mart.csv",locale=locale(encoding = "CP932"))) %>% select(-X1)
summary(data)


## testの実施
# 第3のビールに限定しCM属性を入れて推定する

masta <-data.frame(
  product_id = c("752609","752608","752610","752611")
  ,登場回数 = c(2,1.5,6,4)
  ,発言回数 = c(1,1,2,1)
)
masta$product_id <- as.character(masta$product_id)
data$product_id <- as.character(data$product_id)

# 分析に使用するデータの抽出
use_data <-
  data %>% 
  filter(カテゴリー名 == "缶入りの発泡酒や第三のビール（購入回数）") %>% 
  left_join(masta,by=c("product_id")) %>%
  data.frame()
  





