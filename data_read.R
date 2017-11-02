#############################################################################
#
#############################################################################

## 環境設定
place <- "~/Documents/データコンペ"
questionare <- paste(place,"アンケートデータ/CSV",sep="/")
web <- paste(place,"Web",sep="/")
promotion <- paste(place,"出稿データ",sep="/")
output <- "~/Documents/データコンペ/analysis_data"

## ライブラリ読み込み
if(!require("data.table")){install.packages("data.table")}
if(!require("dplyr")){install.packages("dplyr")}
if(!require("xlsx")){install.packages("xlsx")}
if(!require("reshape2")){install.packages("reshape2")}
options(java.parameters = "-Xmx8000m")

# メモリ解放
 jgc <- function(){
  gc()
  .jcall("java/lang/System", method = "gc")
}

# アンケートデータ
setwd(questionare)
main_data <- fread("メインデータ_2017.csv")
tv <- fread("テレビ番組別視聴状況_2017.csv")
magazine <-fread("雑誌閲読状況_2017.csv")

# ログデータ
setwd(web)
Log1 <- fread("Webアクセスログ（20170128-20170228）.txt")
Log2 <- fread("Webアクセスログ（20170301-20170401）.txt")
colnames(Log1) <- c("サイト","SampleID","date","URL")
colnames(Log2) <- c("サイト","SampleID","date","URL")

freq1 <- fread("Webアクセス頻度（20170128-20170228）.txt")
freq2 <- fread("Webアクセス頻度（20170301-20170401）.txt")

# 出航データ
setwd(promotion)
CM <- read.csv("テレビCM出稿データ_2017.csv",fileEncoding = "CP932",stringsAsFactors = F)
zassi <- read.xlsx("雑誌・新聞出稿データ_2017.xlsx", sheetIndex=1)
newspaper <- read.xlsx("雑誌・新聞出稿データ_2017.xlsx", sheetIndex=2)

##
setwd(place)
# メインデータ
# label_join
main_data_masta <- read.xlsx("データ定義_2017.xlsx",sheetIndex=3,colIndex=c(1,2),startRow = 2)
main_data_masta <- main_data_masta[-1,]
main_data_masta$変数名 <- as.character(main_data_masta$変数名)
M <- melt(main_data,id.vars = "SampleID")
M$variable <- as.character(M$variable)
M_join <- inner_join(M,main_data_masta,by=c("variable"="変数名"))
label_join <- 
  M_join %>%
  dplyr::select(-variable) %>%
  dcast(SampleID ~ ラベル,value.var="value") %>%
  data.frame()
M <- NULL;M_join <- NULL

# CMデータ
# CM_useとwatch_use
tv_masta <- read.xlsx("データ定義_2017.xlsx",sheetIndex=1,colIndex=c(5:12),startRow = 2)
tv_masta<-tv_masta[-1,]
tv_masta$番組ID <- as.character(tv_masta$番組ID)
CM$番組ID <- as.character(CM$番組ID)
CM$ID <- gsub("TVWatch.","",CM$番組ID)
CM_data<-inner_join(tv_masta,CM,by=c("番組ID"="ID"))
CM_use <- CM_data %>% dplyr::select(-ID,-番組ID.y)
tv_melt <- melt(tv,id.vars = "SampleID") #%>% filter(value==1) 
tv_melt$variable <- as.character(tv_melt$variable)
tv_melt$番組ID <- gsub("TVWatch.","",tv_melt$variable)
watch_use <- tv_melt %>% select(-variable)

# 雑誌データ
# magazine_use
magazine_masta <- read.xlsx("データ定義_2017.xlsx",sheetIndex=2,colIndex=c(1,2,5,6),startRow = 2)
magazine_masta <- magazine_masta[-1,]
magazine_masta$変数 <- as.character(magazine_masta$変数)
magazine_melt <-melt(magazine,id.vars = "SampleID")
magazine_melt$variable <- as.character(magazine_melt$variable)
magazine_data <-inner_join(magazine_melt,magazine_masta,by=c("variable"="変数"))
magazine_melt <- NULL;
zassi$variable <- paste("MZ",zassi$雑誌コード,sep=".")
zassi$雑誌名 <- as.character(zassi$雑誌名)
magazine_data$雑誌名 <- as.character(magazine_data$雑誌名)
magazine_use<-inner_join(magazine_data,zassi,by=c("variable"="variable","雑誌名"="雑誌名"))

# 新聞データ
# newspaper_use
newspaper_variable <- colnames(label_join)[grep("新聞",colnames(label_join))]
news_melt<-melt(label_join[,c("SampleID",newspaper_variable)],id.vars = "SampleID")
names_variable <- data.frame(variable = unique(news_melt$variable))
names_variable$variable <- as.character(names_variable$variable)
names<-as.character(gsub("新聞_","",gsub(".02.03月","",names_variable$variable)))
names_variable$names <-as.character(gsub("新聞_","",gsub(".02.03月","",names_variable$variable)))
#news_paper_name <- gsub("新聞_会社などで閲読.02.03月.","",newspaper_variable[1:27])

for(i in 1:length(names)){
  names_variable[i,"形態"]<- strsplit(names[i], ".",fixed = T)[[1]][1]
  names_variable[i,"新聞"]<- strsplit(names[i], ".",fixed = T)[[1]][2]
}
newspaper_use <- inner_join(news_melt,names_variable[,c("variable","形態","新聞")],by=c("variable"))


# Webアクセス
# web_dataとLog1とLog2
web_masta <- read.xlsx("データ定義_2017.xlsx",sheetIndex=4,colIndex=c(1,2,3),startRow = 2)
web_masta <- web_masta[-1,]
web_masta$変数 <- as.character(web_masta$変数)
web_masta$variable <- gsub("@","site",web_masta$変数)
freq1_melt <- melt(freq1,id.vars = "SampleID")
freq2_melt <- melt(freq2,id.vars = "SampleID")
freq <- rbind(freq1_melt,freq2_melt)
web_data<-inner_join(freq,web_masta,by=c("variable"))

# カテゴリの紐付け
setwd("~/Documents/データコンペ/01データ受け渡し")
masta <-read.csv("PI→CAT.csv",header=F)
masta <- masta[,c(1:5)]

masta$V4 <- gsub("（購入意向）","",masta$V4)
masta$id <- substring(masta$V1,7,12)

category <- unique(masta$V4)
for(i in category){
  sub <- subset(masta,V4==i,c("id","V4","V2"))
  ids <- sub$id
  
  for(k in ids){
    <- as.character(main_data_masta$ラベル[grep(k,main_data_masta$変数名)])
    
    
  }
  
  
}
  




## データ出力
setwd(output)
write.csv(label_join,"メインデータ分析用.csv")
write.csv(CM_use,"CMマスタデータ.csv",fileEncoding = "CP932")
write.csv(watch_use,"CM閲覧データ.csv",fileEncoding = "CP932")
write.csv(magazine_use,"雑誌データ.csv",fileEncoding = "CP932")
write.csv(newspaper_use,"新聞データ.csv",fileEncoding = "CP932")
write.csv(web_data,"ウェブデータ.csv",fileEncoding = "CP932")





## 変数名区分
# オブジェクトをlabel_joinに指定すると任意のデータが得られる
train_variable <- colnames(label_join)[grep("利用路線",colnames(label_join))]
value_variable <- colnames(label_join)[grep("消費価値観",colnames(label_join))]
durable_variable <- colnames(label_join)[grep("耐久消費財",colnames(label_join))]
newspaper_variable <- colnames(label_join)[grep("新聞",colnames(label_join))]
hobby_variable <- colnames(label_join)[grep("趣味",colnames(label_join))]
symptom_variable <- colnames(label_join)[grep("気になる症状",colnames(label_join))]
station_variable <- colnames(label_join)[grep("駅利用",colnames(label_join))]
channel_variable <- colnames(label_join)[grep("チャネル利用頻度",colnames(label_join))]
cgm_variable <- colnames(label_join)[grep("CGM利用頻度",colnames(label_join))]
channelfreq_variable <- colnames(label_join)[grep("1ヶ月以内利用頻度",colnames(label_join))]
radio_variable <- colnames(label_join)[grep("ラジオ",colnames(label_join))]
pc_variable <- colnames(label_join)[grep("パソコン",colnames(label_join))]
mobile_variable <- colnames(label_join)[grep("携帯電話",colnames(label_join))]
PI_variable <- colnames(label_join)[grep("購入意向.",colnames(label_join))]
PS_variable <- colnames(label_join)[grep("購入実態.",colnames(label_join))]
PC_variable <- colnames(label_join)[grep("購入回数.",colnames(label_join))]
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




###########################################################################
## 利用例
###

## メインデータ
# 変数名_variable
# train,value,train,value,durable,newspaper,hobby,symptom,
# station,channel,cgm,channelfreq,radio,pc,mobile,PI,PS,PC,demografic,
# head(label_join[,PC_variable])





## 洋服
close_data <-
  main_data %>% 
  dplyr::select(
    SampleID
    ,SEX_CD
    ,AGE
    ,MARRIAGE
    ,CHILD_CD
    ,PREF_CD
    ,PI_01_753506 # earth music & ecology
    ,PS_01_753506
    ,PI_02_753506
    ,PS_02_753506
    ,PI_01_753507 # GU
    ,PS_01_753507
    ,PI_02_753507
    ,PS_02_753507
    ,PI_01_753508 # しまむら
    ,PS_01_753508
    ,PI_02_753508
    ,PS_02_753508
    ,PI_01_753509 # gloval work
    ,PS_01_753509
    ,PI_02_753509
    ,PS_02_753509
    ,PI_01_753510 # ユニクロ
    ,PS_01_753510
    ,PI_02_753510
    ,PS_02_753510
  )



