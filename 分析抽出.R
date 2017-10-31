

## 環境設定
input <- "~/Documents/データコンペ/analysis_data"
place <- "~/Documents/データコンペ"
web <- paste(place,"Web",sep="/")
setwd(input)

# ライブラリ
if(!require("data.table")){install.packages("data.table")}
if(!require("dplyr")){install.packages("dplyr")}
if(!require("xlsx")){install.packages("xlsx")}
if(!require("reshape2")){install.packages("reshape2")}
if(!require("readr")){install.packages("readr")}
library(stringr)


# 読み込み
main_data <- read_csv("メインデータ分析用.csv",locale = locale(encoding = "cp932"))

##
value_variable <- colnames(main_data)[grep("消費価値観",colnames(main_data))]
colnames(main_data[,value_variable])

## 
cols <- gsub("消費価値観.","",colnames(main_data[,value_variable]))
values <- main_data[,value_variable]
colnames(values) <- cols
values <- na.omit(values)

### 


### クラスタリング
#平均ベクトル（2次元上の中心）
m.dat<-colMeans(values)
#それぞれの分散共分散行列
v.dat<-var(values)
#マハラノビス距離の計算
m_dist <- mahalanobis(values,m.dat,v.dat)
m_dist <- vegdist(values,method="jaccard",binary=T)
h_c <- hclust(m_dist, method = "ward.D2")
plot(h_c,hang=-1,main="ウォード法")








######## 駅/衣料品/EC利用意向 のデータ
## 駅利用データの抽出
  train_variable <- colnames(main_data)[grep("利用路線",colnames(main_data))]
  station_variable <- colnames(main_data)[grep("駅利用",colnames(main_data))]
  train_data <- data.frame(main_data[,c("SampleID",train_variable)])
  station_data <- data.frame(main_data[,c("SampleID",station_variable)])

## melt形式でID別に利用駅を抽出
melt_train <- melt(train_data,id.vars = "SampleID") %>% filter(value>=1) %>% arrange(SampleID) %>% data.frame()
melt_station <- melt(station_data,id.vars = "SampleID") %>% filter(value>=1) %>% arrange(SampleID) %>% data.frame()

  # 一人あたり何路線使っているのか
  melt_train %>% group_by(SampleID) %>% summarise(NUM = length(unique(variable))) %>% arrange(desc(NUM))
  melt_station %>% group_by(SampleID) %>% summarise(NUM = length(unique(variable))) %>% arrange(desc(NUM))
  
  # 1197人が週に1回以上利用している路線はないと回答
  # 都道府県別では12千葉、13東京、14神奈川が以外と多い
  train_data_liv <- data.frame(main_data[,c("SampleID","居住地",train_variable)])
  melt(train_data_liv,id.vars = c("SampleID","居住地")) %>% 
  filter(value>=1) %>% 
  filter(variable == "利用路線.02.03月.週１回以上利用している路線なし.") %>% 
  group_by(居住地) %>% 
  summarise(num = length(unique(SampleID)))

melt_train$line <- apply(data.frame(as.character(melt_train$variable)),1,function(x){substring(x,as.numeric(str_locate(x, ".02.03月.")[,2]),nchar(x))})
melt_train$line <- gsub(".","",melt_train$line,fixed = T)
  
melt_station$station <- apply(data.frame(as.character(melt_station$variable)),1,function(x){substring(x,as.numeric(str_locate(x, ".02.03月.")[,2]),nchar(x))})
melt_station$station <- gsub(".","",melt_station$station,fixed = T)

# 横に複数路線と駅をつなげたい




## 衣料品購入変数
  cate_variable <- c("SampleID"
  ,"ユニクロ.しまむら.GAP.H.Mなどのファストファッションの店.購入意向."
  ,"ユニクロ.しまむら.GAP.H.Mなどのファストファッションの店.購入回数.")
  
  close_variable<- c("SampleID"
    ,"しまむら_購入意向.03.01."
    ,"しまむら_購入意向.03.22."
    ,"しまむら_購入実態.03.01."
    ,"しまむら_購入実態.03.22."
    ,"UNIQLO.ユニクロ._購入意向.03.01."
    ,"UNIQLO.ユニクロ._購入意向.03.21."
    ,"UNIQLO.ユニクロ._購入実態.03.01."
    ,"UNIQLO.ユニクロ._購入実態.03.21."
    ,"GLOBAL.WORK.グローバルワーク._購入意向.03.01."                                                                                     
    ,"GLOBAL.WORK.グローバルワーク._購入意向.03.22."                                                                                     
    ,"GLOBAL.WORK.グローバルワーク._購入実態.03.01."                                                                                     
    ,"GLOBAL.WORK.グローバルワーク._購入実態.03.22."                                                                                     
    ,"GU.ジーユー._購入意向.03.01."                                                                                                      
    ,"GU.ジーユー._購入意向.03.22."                                                                                                      
    ,"GU.ジーユー._購入実態.03.01."                                                                                                      
    ,"GU.ジーユー._購入実態.03.22." 
    ,"earth.music.ecology.アースミュージック.エコロジー._購入意向.03.01."                                                                
    ,"earth.music.ecology.アースミュージック.エコロジー._購入意向.03.21."                                                                
    ,"earth.music.ecology.アースミュージック.エコロジー._購入実態.03.01."                                                                
    ,"earth.music.ecology.アースミュージック.エコロジー._購入実態.03.21."    
  )
  close_data <- data.frame(main_data[,close_variable])

  melt_close <- melt(close_data,id.vars = "SampleID")

## 
# ブランド名を抽出
  melt_close$brand <- apply(data.frame(melt_close$variable),1,function(x){substring(x,1,regexpr("_", x)[1]-1)})
  melt_close$variable <- as.character(melt_close$variable)
  
# 調査期間を抽出
  elseifs<-function(x){ifelse(grep("03.01",x)==T,'1回目','2回目')}
  melt_close$term <- apply(data.frame(melt_close$variable),1,function(x){ifelse(grep("03.01",x)==T,'1回目','2回目')})
  melt_close$term <- apply(data.frame(melt_close$variable),1,function(x){substring(x,nchar(x)-5,nchar(x)-1)})
  melt_close$term <- ifelse(melt_close$term == '03.01','1回目','2回目')
  melt_close$var_name <- ifelse(str_detect(melt_close$variable,"購入意向")==T,'購入意向','購入実態')

  close_data <-dcast(melt_close,SampleID+brand+var_name ~ term,value.var = "value",mean)

  cate_data<-data.frame(main_data[,cate_variable])
close_data <- inner_join(cate_data,close_data,by=c("SampleID"))



## EC購買関連変数
#pc_variable <- colnames(main_data)[grep("パソコン利用方法",colnames(main_data))]
  pc_variable<-c("SampleID"
    ,"パソコン利用方法.商品.サービスの発注."
    ,"パソコン利用方法.商品.サービスの代金の支払い."
    ,"パソコン利用方法.どれも利用したことがない.")
  pc_data <- data.frame(main_data[,pc_variable])
res_data <- inner_join(pc_data,close_data,by=c("SampleID"))



## しゅつりょく
head(melt_train)
head(melt_station)
head(close_data)



####### CM閲覧データ
  CM_masta <- read_csv("CMマスタデータ.csv",locale = locale(encoding = "cp932"))
  CM_watch<- read_csv("CM閲覧データ.csv",locale = locale(encoding = "cp932"))
  
  #"ナノユニバース" 
  store_brand<-data.frame(stores =c(
    "ユニクロ/ファーストリテイリング"
    ,"ファッションセンターしまむら/しまむら"
    ,"ジーユー"
    ,"グローバルワーク/アダストリア"
    ,"アースミュージック&エコロジー/ストライプインターナショナル"))
  store_brand$stores <- as.character(store_brand$stores)
  
  CM_masta_lim <- inner_join(CM_masta,store_brand,by=c("広告主"="stores")) %>% dplyr::select(-X1)
  #cm_id <- data.frame(番組ID = CM_masta_lim$番組ID)
  
  CM_watch <- CM_watch %>% filter(value>=1) %>% select(-X1)%>% data.frame()
  cm_merge<-inner_join(CM_watch,CM_masta_lim,by=c("番組ID"))
  
  cm_attach_data <- cm_merge %>%
    group_by(SampleID,広告主,放送日) %>%
    summarise(
      count = n(),
      time = sum(秒数)
    ) %>% 
    data.frame()

# しゅつりょく  
head(cm_attach_data)


####### PCログを個人ごとに
web <- paste(place,"Web",sep="/")
setwd(web)

Log1 <- fread("Webアクセスログ（20170128-20170228）.txt")
Log2 <- fread("Webアクセスログ（20170301-20170401）.txt")

close_domain <- function(x){
  c(unique(x$V1)[grep('uniqlo',unique(x$V1))]
  ,unique(x$V1)[grep('shimamura',unique(x$V1))]
  ,unique(x$V1)[grep('gu-',unique(x$V1))]
  ,unique(x$V1)[grep('earth1999',unique(x$V1))]
  ,unique(x$V1)[grep('dot-st',unique(x$V1))]
  ,unique(x$V1)[grep('globalwork',unique(x$V1))])
}
each_domain <- close_domain(Log2)

close_site_log <- data.frame()

for(i in each_domain){
  close_site_log1<-subset(Log1,V1==i)
  close_site_log2<-subset(Log2,V1==i)
  close_site_bind<-rbind(close_site_log1,close_site_log2)
  close_site_log <-rbind(close_site_log,close_site_bind)
}
Log1 <- NA;Log2 <-NA
colnames(close_site_log) <- c("サイト","SampleID","date","URL")


#### 全データ分析用結合





