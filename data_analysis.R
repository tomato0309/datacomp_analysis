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
if(!require("bayesm")){install.packages("bayesm")}

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


## 練習
library(lme4)
install.packages("lmerTest")　#lmerTestインストール
library(lmerTest) #lmerTest読み込み

lmr_res <- lmer(第2期購入意向 ~ 
                   (1|SampleID) +
                   (1|product_id) +
                   第1期購入意向 +
                   第1期.第2期中_SOV_value +
                   第1期前_SOV_value +
                   第1期前_brand_cm_freq + 
                   登場回数 +
                   発言回数 +
                   第1期前_SOV_value:登場回数 +
                   第1期前_SOV_value:発言回数,
                 data=use_data)
summary(lmr_res)









data("BTdata")
data("BTped")

BTdata[1,]
head(BTped)

m1 <- MCMCglmm(cbind(tarsus, back) ~ 　trait:sex + 　trait:hatchdate - 1,
          random = ~ us(trait):animal + us(trait):fosternest, rcov = ~ us(trait):units
          , prior = prior
          , family = rep("cumulative", 2), nitt = 5000, burnin = 1000,
        　thin=25, data = BTdata, pedigree=BTped)





















## 階層ベイズの用意
## 現状のデータセットをbayesmでやるのは無理
data(camera)
length(camera)
str(camera[[1]]) #[[1]]がたまたま5を選んで無かっただけ
length(camera[[1]]$y)
head(camera[[1]]$X)


##
use_data$SampleID <- as.factor(use_data$SampleID)
N <- nlevels(use_data$SampleID)

choice_alternative <- length(unique(use_data$第2期購入意向)) #pに入力する

#消費者別ブランド別の変数
na_v <- c(
  "第2期購入意向"
  ,"第1期.第2期中_SOV_value"
  ,"第1期前_SOV_value")

nd_v <- c(
  "登場回数"
  ,"発言回数"
  #,"第1期.第2期中_brand_cm_freq"
  #,"第1期.第2期中_brand_cm_first"
  #,"第1期.第2期中_brand_cm_middle"
  #,"第1期.第2期中_brand_cm_last"
  #,"第1期前_brand_cm_freq"
  #,"第1期前_brand_cm_first"
  #,"第1期前_brand_cm_middle"
  #,"第1期前_brand_cm_last"
  )

## createx::
##  p = choice_alternative
##  na = alternative_specific vars (選択肢別属性の数)
##  nd = non-alternative_specific (個体属性の数)
##  Xa = n*p*na (個体別選択肢別属性のベクトル)
##  Xd = n * nd (個体別属性のベクトル)

dat <- vector(mode = "list", length = N)
for (i in 1:N) {
  dat[[i]]$y <- use_data[use_data$SampleID==levels(use_data$SampleID)[i], "第2期購入意向"]
  dat[[i]]$X <- createX(
      p = 4
    , na = 1
    , Xa = use_data[use_data$SampleID==levels(use_data$SampleID)[i],nd_v]
    , nd = 2
    , Xd = use_data[use_data$SampleID==levels(use_data$SampleID)[i],c("第1期購入意向","第1期.第2期中_SOV_value")] #選択肢数*Xの数が$Xの行
  )
}



set.seed(66)
nreg = 100
nobs = 100
nvar = 3
Vbeta = matrix(c(1, 0.5, 0, 0.5, 2, 0.7, 0, 0.7, 1), ncol=3)
Z = cbind(c(rep(1,nreg)), 3*runif(nreg))
Z[,2] = Z[,2] - mean(Z[,2])
nz = ncol(Z)
Delta = matrix(c(1,-1,2,0,1,0), ncol=2)
Delta = t(Delta) # first row of Delta is means of betas
Beta = matrix(rnorm(nreg*nvar),nrow=nreg)%*%chol(Vbeta) + Z%*%Delta
tau = 0.1
iota = c(rep(1,nobs))
regdata = NULL
for (reg in 1:nreg) {
  X = cbind(iota, matrix(runif(nobs*(nvar-1)),ncol=(nvar-1)))
  y = X%*%Beta[reg,] + sqrt(tau)*rnorm(nobs)
  regdata[[reg]] = list(y=y, X=X)
}
R=2000
Data1 = list(regdata=regdata, Z=Z)
Mcmc1 = list(R=R, keep=1)
out = rhierLinearModel(Data=Data1, Mcmc=Mcmc1)
cat("Summary of Delta draws", fill=TRUE)
summary(out$Deltadraw, tvalues=as.vector(Delta))






