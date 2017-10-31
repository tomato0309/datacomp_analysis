#############################################################################
#  モデリング用データセット作成
#############################################################################

## 
input <- "~/Documents/データコンペ/analysis_data"
place <- "~/Documents/データコンペ"
web <- paste(place,"Web",sep="/")
place <- "~/Documents/データコンペ"
questionare <- paste(place,"アンケートデータ/CSV",sep="/")

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

## 
demo_data<-data.frame(main_data[,c("SampleID",demografic_variable)])


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
#beer <- c("")
#brand_category_masta[brand_category_masta$カテゴリー名 == "ドリンク剤（購入意向）","カテゴリ名"]
#brand_id <- unique(brand_category_masta$product_id)

## ひとまずビールに限定する
beer <-
  brand_category_masta %>% 
  filter(カテゴリー名=="缶やビン入りのビール（発泡酒や第三のビールを除く）（購入回数）") %>% 
  data.frame()
brand_id <- unique(beer$product_id)

#brand_id <- brand_category_masta %>% filter(カテゴリー名==beer) %>% select(カテゴリー名) %>% unique()
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


## CM要因(SampleIDと番組の紐付け)
setwd(questionare)
tv <- data.frame(fread("テレビ番組別視聴状況_2017.csv"))
m_tv <- melt(tv,id.vars = "SampleID")
m_tv$variable <- gsub("TVWatch.","",m_tv$variable)

## CM視聴と消費者の紐付け
cm_watch <-
  result_data %>% 
  select(SampleID) %>% 
  inner_join(m_tv,by=c("SampleID")) %>%
  inner_join(cm_data,by=c("variable"="番組ID")) %>% 
  filter(缶やビン入りのビール.発泡酒や第三のビールを除く..購入回数. != 0) %>% 
  filter(アイテム名 != "本搾りチューハイ グレープフルーツ/レモン") %>%
  select(SampleID,variable,value,放送日,開始時,開始分,終了時,終了分,放送局,広告主,アイテム名,秒数) %>% 
  data.frame() 

## 
tvcm_name = as.character(unique(cm_watch$アイテム名))
ques_name = data.frame(ques_name = as.character(unique(result_data$product_name)))
beer_name <- c("ヱビスビール"
                ,"ヱビスビール"
                ,"アサヒ スーパードライ"
                ,"サッポロ 生ビール黒ラベル"
                ,"キリン 一番搾り生ビール"
                ,"なし"
                ,"アサヒ スーパードライ"
                ,"ヱビスビール"
                ,"サントリー ザ・プレミアム・モルツ"
                ,"アサヒ スーパードライ"
              )
masta <- data.frame(cbind(tvcm_name,beer_name))

masta$tvcm_name <- as.character(masta$tvcm_name)
masta$beer_name <- as.character(masta$beer_name)
ques_name$ques_name <- as.character(ques_name$ques_name)

full_masta <- dplyr::full_join(masta,ques_name,by=c("beer_name" ="ques_name")) %>% 
  filter(is.na(tvcm_name)==F | is.na(beer_name)==F) %>%
  data.frame()
full_masta[is.na(full_masta)] <- "なし"
colnames(full_masta)[2] <- "research_name"

join_data <- left_join(cm_watch,full_masta,by=c("アイテム名"="tvcm_name"))

setwd(place)
main_data_masta <- read.xlsx("データ定義_2017.xlsx",sheetIndex=3,colIndex=c(1,2),startRow = 2)
main_data_masta <- main_data_masta[-1,]
main_data_masta$変数名 <- as.character(main_data_masta$変数名)

## 
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

## 












M <- melt(main_data,id.vars = "SampleID")
M$variable <- as.character(M$variable)


res %>%
filter(カテゴリー名 == "缶やビン入りのビール（発泡酒や第三のビールを除く）（購入回数）") %>%
group_by(
  product_name
  ,research_date
  ) %>% 
summarise(N = n()) %>%
data.frame()









