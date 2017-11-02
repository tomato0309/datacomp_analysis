#############################################################################
#  モデリング用データセット作成
#############################################################################

## 
input <- "~/Documents/データコンペ/analysis_data"
place <- "~/Documents/データコンペ"
web <- paste(place,"Web",sep="/")
place <- "~/Documents/データコンペ"
questionare <- paste(place,"アンケートデータ/CSV",sep="/")

## loadファイルの読み込み
filename <- "middle_data.RData"
filepass <- paste("/Users/ryosuzuki/Documents/データコンペ/data_comp/",filename,sep="")
load(filepass)

## ライブラリ読み込み
if(!require("data.table")){install.packages("data.table")}
if(!require("dplyr")){install.packages("dplyr")}
if(!require("xlsx")){install.packages("xlsx")}
if(!require("reshape2")){install.packages("reshape2")}
if(!require("foreach")){install.packages("foreach")}
if(!require("tcltk")){install.packages("tcltk")}
if(!require("readr")){install.packages("readr")}
if(!require("stringr")){install.packages("stringr")}
options(java.parameters = "-Xmx8000m")

## 
setwd(questionare)
all_data <- data.frame(fread("メインデータ_2017.csv"))
m_data<-melt(all_data,id.vars = "SampleID")

setwd(input)
main_data <- read_csv("メインデータ分析用.csv",locale = locale(encoding = "cp932"))

## 変数名区分
# オブジェクトをmain_dataに指定すると任意のデータが得られる
train_variable <- colnames(main_data)[grep("利用路線",colnames(main_data))]
value_variable <- colnames(main_data)[grep("消費価値観",colnames(main_data))]
durable_variable <- colnames(main_data)[grep("耐久消費財",colnames(main_data))]
newspaper_variable <- colnames(main_data)[grep("新聞",colnames(main_data))]
hobby_variable <- colnames(main_data)[grep("趣味",colnames(main_data))]
symptom_variable <- colnames(main_data)[grep("気になる症状",colnames(main_data))]
station_variable <- colnames(main_data)[grep("駅利用",colnames(main_data))]
channel_variable <- colnames(main_data)[grep("チャネル利用頻度",colnames(main_data))]
cgm_variable <- colnames(main_data)[grep("CGM利用頻度",colnames(main_data))]
channelfreq_variable <- colnames(main_data)[grep("1ヶ月以内利用頻度",colnames(main_data))]
radio_variable <- colnames(main_data)[grep("ラジオ",colnames(main_data))]
pc_variable <- colnames(main_data)[grep("パソコン",colnames(main_data))]
mobile_variable <- colnames(main_data)[grep("携帯電話",colnames(main_data))]
PI_variable <- colnames(main_data)[grep("購入意向.",colnames(main_data))]
PS_variable <- colnames(main_data)[grep("購入実態.",colnames(main_data))]
PC_variable <- colnames(main_data)[grep("購入回数.",colnames(main_data))]
demografic_variable <- 
  c("性別",
    "年齢",
    "未既婚",
    "子供有無",
    "子供人数",
    "第１子の年齢",
    "第２子の年齢",
    "第３子の年齢",
    "第４子の年齢",
    "末子の年齢",
    "家族構成",
    "居住地",
    "職業",
    "あなたの現在のお住まいは.以下のどれにあたりますか.なお.親などの持ち家に同居している場合も.持ち家.としてお考えください..ひとつだけ.",
    "BS視聴可能有無.あなたのご家庭では.BS放送.例.NHK.BS1など.を見ることができますか.",
    "世帯保有金融資産",
    "世帯年収")

## 今回使用する変数の抽出
demo_data<-data.frame(main_data[,c("SampleID",demografic_variable)])
value_data<-data.frame(main_data[,c("SampleID",value_variable)])
comsumer_mart <- inner_join(demo_data,value_data,by=c("SampleID"))

## カテゴリ情報の紐付け
setwd(input)
cate<-read.xlsx("カテゴリーPS割り付け.xlsx",sheetIndex = 1)
cate$product_id <- substring(cate$製品PS,7,16)
cate$製品PS <- as.character(cate$製品PS)
m_data$variable <- as.character(m_data$variable)
cate$research_date <- gsub("）","",gsub("（","",apply(data.frame(cate[,"製品名"]),1,FUN = function(x){substring(x,(regexpr("（0",x)[1]),nchar(x))}),fixed = T),fixed = T)
res <- inner_join(m_data,cate,by=c("variable"="製品PS"))

## 文字列処理
res$製品名 <- as.character(res$製品名)
res$product_name <- apply(data.frame(res[,"製品名"]),1,FUN = function(x){substring(x,1,(regexpr("_",x)[1])-1)})

## マスタ作成
brand_category_masta <-
  res %>% 
  dplyr::select(
     product_id
    , product_name
    , カテゴリー名
    ) %>% 
  unique() %>% 
  data.frame()


## 態度と購入をブランド別消費者別に加工する

#brand_id <- unique(brand_category_masta$product_id)
## ひとまずビールに限定する
beer <-
  brand_category_masta %>% 
  filter(
    カテゴリー名=="缶やビン入りのビール（発泡酒や第三のビールを除く）（購入回数）" | 
    カテゴリー名=="缶入りの発泡酒や第三のビール（購入回数）"
    ) %>% 
  data.frame()
brand_id <- unique(beer$product_id)

#omit_dat <- na.omit(res[c("SampleID","variable","value")])
#test <- length(unique(omit_dat$SampleID))
cst_id <- unique(res$SampleID)[1:500]
result_data <- data.frame()
progress.bar <- paste(rep("-", length(cst_id)), collapse="")

for(i in cst_id){ # 消費者別
  
  cat(progress.bar, i, "\n")          # プログレスバーの表示
  progress.bar <- substring(progress.bar, 2) # プログレスバーを進める
  
  for(k in brand_id){ # ブランド別
    
    sample_per <-
      m_data %>% 
      filter(SampleID == i) %>%
      filter(str_count(variable,k)>=1) %>% 
      mutate(variable_name = 
        ifelse(str_count(variable,"PI_01")>=1,"第1期購入意向",
        ifelse(str_count(variable,"PS_01")>=1,"第1期購入実態",
        ifelse(str_count(variable,"PI_02")>=1,"第2期購入意向",
        ifelse(str_count(variable,"PS_02")>=1,"第2期購入実態","他"))))
      ) %>% 
      dcast(SampleID ~ variable_name,value.var = "value") %>%
      mutate(product_id = k) %>%
      inner_join(brand_category_masta,by=c("product_id")) %>% 
      data.frame()
    
    result_data <- rbind(result_data,sample_per)
    
  } # ブランド別
} # 消費者別

## 消費者別態度購入データを出力する(後ほど全消費者全カテゴリで実行)
write.csv(result_data,"consumer_questionare_data.csv",fileEncoding = "CP932")


### CM要因(番組とブランドの紐付け)
  cm_category <- read.xlsx("CMカテゴリー割り付け済み.xlsx",sheetIndex = 1,stringsAsFactors=FALSE)
  cm_category<-
    cm_category %>% 
    filter(is.na(番組ID)==F) %>%
    filter(is.na(広告主)==F) %>%
    data.frame()
  cm_category[is.na(cm_category)] <- 0
  cm_category$番組ID <- gsub("TVWatch.","",cm_category$番組ID)
  
  setwd(place)
  tv_masta <- read.xlsx("データ定義_2017.xlsx",sheetIndex=1,colIndex=c(5:12),startRow = 2)
  tv_masta<-tv_masta[-1,]
  tv_masta$番組ID <- as.character(tv_masta$番組ID)
  cm_data <- inner_join(tv_masta,cm_category)

## CMの順序と密度を計算
  CM <- read.csv("~/Documents/データコンペ/出稿データ/テレビCM出稿データ_2017.csv",fileEncoding = "CP932",stringsAsFactors = F)
  CM_adj <- # 1番組あたりの同一CM回数と合計秒数を算出
    CM %>% 
    group_by(番組ID,広告主,アイテム名) %>% 
    summarise(
      product_num = n()
      ,product_second = sum(秒数)
      ) %>% 
    data.frame()
  
  CM_order <- # CMの順番を計算
    CM %>% 
    group_by(番組ID) %>%
    mutate(row_number = row_number()) %>% 
    data.frame()
  
  CM_max<-
    CM_order %>% 
    group_by(番組ID) %>%
    summarise(max_num = max(row_number)) %>% 
    data.frame()
  
  CM_variable <- inner_join(CM_adj,CM_order)
  CM_variable <- inner_join(CM_variable,CM_max)
  CM_variable$番組ID <- gsub("TVWatch.","",CM_variable$番組ID)
  CM_variable <- CM_variable %>% arrange(番組ID,row_number)

## CMのカテゴリ変数と順番頻度を紐付け(全カテゴリ)
CM_variable <- inner_join(CM_variable,cm_data)
#write.csv(CM_variable,"cm_content_variable.csv")

## ファイルを一度保存しておく
filename <- "middle_data.RData"
filepass <- paste("/Users/ryosuzuki/Documents/データコンペ/data_comp/",filename,sep="")
save.image(file=filepass)
load(filepass)

## CM要因(SampleIDと番組の紐付け)
setwd(questionare)
tv <- data.frame(fread("テレビ番組別視聴状況_2017.csv"))
m_tv <- melt(tv,id.vars = "SampleID")
m_tv$variable <- gsub("TVWatch.","",m_tv$variable)

## CM視聴と消費者の紐付け
## カテゴリを指定
cm_watch <-
  result_data %>% 
  select(SampleID) %>% 
  inner_join(m_tv,by=c("SampleID")) %>%
  inner_join(CM_variable,by=c("variable"="番組ID")) %>% 
  filter(
    缶やビン入りのビール.発泡酒や第三のビールを除く..購入回数. != 0 |
    缶入りの発泡酒や第三のビール.購入回数.!= 0
    ) %>% 
  filter(アイテム名 != "本搾りチューハイ グレープフルーツ/レモン") %>%
  select(SampleID,variable,value,放送日,開始時,開始分,終了時,終了分,放送局,広告主,アイテム名,秒数,product_num,product_second,row_number,max_num) %>% 
  data.frame() 
head(cm_watch)

## 調査項目のブランド名とCMデータのブランド名を名寄せ
tvcm_name = as.character(unique(cm_watch$アイテム名))
ques_name = data.frame(ques_name = as.character(unique(result_data$product_name)))
cm_masta<-read.csv("/Users/ryosuzuki/Documents/データコンペ/analysis_data/cm_masta.csv",fileEncoding = "CP932",stringsAsFactors = F)
cm_masta<- cm_masta[,c(1,2,3)]

cm_watch$アイテム名 <- gsub(" ","",cm_watch$アイテム名)
cm_watch <- unique(cm_watch)
cm_masta <- cm_masta %>% rename(アイテム名 = tvcm_name)
cm_watch_data<-merge(cm_watch,cm_masta, by = "アイテム名", all.x = TRUE)



## 各productごとの調査日付の特定
setwd(place)
main_data_masta <- read.xlsx("データ定義_2017.xlsx",sheetIndex=3,colIndex=c(1,2),startRow = 2)
main_data_masta <- main_data_masta[-1,]
main_data_masta$変数名 <- as.character(main_data_masta$変数名)

brand_id_data <- data.frame()  
for(k in brand_id){
  
  main_data_masta$flg <- ifelse(str_count(main_data_masta$変数名,k) >= 1,1,0)
  main_data_masta_limit<- main_data_masta %>% filter(flg == 1) %>% select(ラベル) %>% data.frame()
  
  main_data_masta_limit$date <- gsub("）","",gsub("（","",apply(data.frame(main_data_masta_limit[,"ラベル"]),1,FUN = function(x){substring(x,(regexpr("（0",x)[1]),nchar(x))}),fixed = T),fixed = T)
  main_data_masta_limit$product_id <- k
  t1 <- unique(main_data_masta_limit$date)[1]
  t2 <- unique(main_data_masta_limit$date)[2]
  product_id <- unique(main_data_masta_limit$product_id)
  id_data <- data.frame(cbind(product_id,t1,t2))
  
  brand_id_data <- rbind(brand_id_data,id_data)
}
cols <- colnames(brand_id_data)
for(c in cols){
  brand_id_data[,c] <- as.character(brand_id_data[,c])  
}
brand_id_data$t1 <- paste('2017',substring(brand_id_data$t1,1,2),substring(brand_id_data$t1,4,5),sep="-")
brand_id_data$t2 <- paste('2017',substring(brand_id_data$t2,1,2),substring(brand_id_data$t2,4,5),sep="-")

## cmデータと紐付け
#cm_watch_data$cm_id <- as.character(cm_watch_data$cm_id)
#join<-left_join(cm_watch_data,brand_id_data,by=c("cm_id"="product_id"))

t1 <- unique(brand_id_data$t1)
t2 <- unique(brand_id_data$t2)

## 放送日がいつの調査時点なのかを識別
cm_watch_data$cm_term <- with(cm_watch_data, 
             ifelse(放送日 < t1 ,"第1期前",
                       ifelse(放送日 >= t1 & 放送日 < t2 ,"第1期-第2期中",
                                 ifelse(放送日 >= t2,"第2期後",'他')
                        )
                    )
             )
cm_watch_data$first_cm_flg <- ifelse(cm_watch_data$row_number == 1,1,0)
cm_watch_data$last_cm_flg <- ifelse(cm_watch_data$row_number == cm_watch_data$max_num,1,0)
cm_watch_data$middle_cm_flg <- ifelse(cm_watch_data$first_cm_flg == 0 & cm_watch_data$last_cm_flg == 0,1,0)

## ここまでで重要なデータフレームを中間テーブルとして出力
# result_data 
# cm_watch_data 
# consumer_mart
write.csv(result_data,"~/Documents/データコンペ/middle_table/result_data.csv",fileEncoding = "CP932",row.names = 1)
write.csv(cm_watch_data,"~/Documents/データコンペ/middle_table/cm_watch_data.csv",fileEncoding = "CP932",row.names = F)
write.csv(comsumer_mart,"~/Documents/データコンペ/middle_table/consumer_mart.csv",fileEncoding = "CP932",row.names = F)
save.image(file=filepass)



## cm_watch_dataを分析用に加工する
brand_cm_value <-
  cm_watch_data %>%
  group_by(
    SampleID,cm_id,cm_term
  ) %>% 
  summarise(
    brand_cm_value = sum(value)
    ,brand_cm_time = sum(ifelse(value==1,秒数,0))
    ,brand_cm_freq = mean(product_num) #1番組あたり同一ブランドCM数
    ,brand_cm_first = sum(first_cm_flg)
    ,brand_cm_middle = sum(middle_cm_flg)
    ,brand_cm_last = sum(last_cm_flg)
  ) %>% 
  data.frame()

sum_cm_value<-
  cm_watch_data %>%
  group_by(SampleID,cm_term) %>% 
  summarise(
    total_cm_value = sum(value)
    ,total_cm_time = sum(ifelse(value==1,秒数,0))
    ) %>% 
  data.frame()

cm_value <- inner_join(brand_cm_value,sum_cm_value,by=c("SampleID","cm_term"))
cm_value <- cm_value %>% 
  filter(cm_term != "第2期後") %>%
  rename(product_id = cm_id) %>%
  mutate(
    SOV_value =brand_cm_value / total_cm_value
    ,SOV_time = brand_cm_time / total_cm_time
  ) %>% 
  select(-brand_cm_value,-brand_cm_time,-total_cm_value,-total_cm_time) %>%
  data.frame()
cm_value[is.na(cm_value)] <- 0

melt_data <- melt(cm_value,id.vars = c("SampleID","product_id","cm_term"))
tmp <-acast(melt_data,SampleID+product_id ~ variable ~ cm_term)

list_names <- attributes(tmp)$dimnames[[3]]

#第1期-第2期中
  list_data<-data.frame(tmp[,,list_names[1]])
  list_data[is.na(list_data)] <- 0
  colnames(list_data) <- paste(list_names[1],colnames(list_data),sep="_")
  list_data$SampleID <- substring(rownames(list_data),1,6)
  list_data$product_id <- substring(rownames(list_data),8,13)
  rownames(list_data) <- 1:nrow(list_data)
  one_data<-list_data

# 第1期前
  list_data<-data.frame(tmp[,,list_names[2]])
  list_data[is.na(list_data)] <- 0
  colnames(list_data) <- paste(list_names[2],colnames(list_data),sep="_")
  list_data$SampleID <- substring(rownames(list_data),1,6)
  list_data$product_id <- substring(rownames(list_data),8,13)
  rownames(list_data) <- 1:nrow(list_data)
  two_data<-list_data

tmp_data <- inner_join(one_data,two_data)
  
## 
result_data$SampleID <- as.character(result_data$SampleID)
res_merge_data <- inner_join(result_data,tmp_data) %>% na.omit()





