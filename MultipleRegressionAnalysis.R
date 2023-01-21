# ニューヨークのルーズベルト島で観測したオゾン濃度の時系列データセットを使用
libary(datasets)
data(airquality)

#データの構造と項目の一覧を確認する
str(airquality)
DFsub <- na.omit(airquality)
str(DFsub)#欠損値行を削除しても111行であるためこのまま分析を進める
#時系列情報を削除
DFsub <- DFsub[, -5:-6]
#関数summary()を使って平均値,中央値などを表示
summary(DFsub)

#ライブラリGGallyのggpairs()で散布図マトリクスを描く
library(ggplot2)
library(GGally)
ggpairs(DFsub,                        #すべての列を含める
        aes(alpha=0.5),                    #透明度
        upper=list(continuous=wrap("cor", size=9))) +
  #相関係数の文字サイズ
  theme(axis.text =element_text(size=9),  #軸の文字サイズ
        strip.text=element_text(size=9))  #項目の文字サイズ

library(car)
#3要素用いた重回帰分析
LM3 <- lm(Ozone ~ Solar.R + Wind + Temp, data=DFsub)
vif(LM3)

#2要素用いた重回帰分析
LM2 <- lm( Ozone ~ Solar.R + Wind, data=DFsub)

summary(LM3)
summary(LM2)

#AIC(赤池情報量基準)を使ってモデルを比較
#モデルの複雑さとデータへの適合のバランスを見る指標
#小さい方がよい
AIC(LM3)
AIC(LM2)
#参考：BIC(ベイズ情報量基準)を使った比較
#指標値の見方はAICと同じ
BIC(LM3)
BIC(LM2)

#目的変数の値について、モデル上の予測値を図示する
#モデル上の予測値
#　＝モデルをDF の説明変数の値に当てはめた場合の予測値

#予測値はモデル作成の際にfitted.valuesとして格納されている
#実測値との差（残差）は同様にresidualsとして格納されている
head(DFsub$Ozone)       #実測値
head(LM3$fitted.values) #予測値
head(LM3$residuals)     #残差（予測値－実測値）

#実測値を横軸に、予測値を縦軸にプロットする
#　モデルが完璧にデータに適合していれば、点は対角線（y=x）
#　の上に並ぶはず（対角線からの縦方向のずれが残差を表す）
ggplot()+
  geom_point(aes(x=DFsub$Ozone, y=LM3$fitted.values),
             colour="orange", size=4, #LM3をオレンジで表示
             shape=16, alpha=.6 ) +　 #shape=16：●
  geom_point(aes(x=DFsub$Ozone, y=LM2$fitted.values),
             colour="brown",  size=3, #LM2をブラウンで表示
             shape=17, alpha=.6 ) +   #shape=17：▲
  xlab("実測値") +                    #軸ラベル
  ylab("モデル上の予測値(元のデータに基づく予測値)") +
  stat_function(colour="black", fun=function(x)x)   #y=Xに沿って線を引く

#残差の分布を密度プロットで比較
ggplot()+
  geom_density( aes(x=LM3$residuals), #LM3の残差
                color="orange",       #枠線をオレンジ
                fill ="orange",       #塗りもオレンジ
                alpha=0.2  ) +        #透明度を指定
  geom_density( aes(x=LM2$residuals), #LM2の残差
                color="brown",        #枠線をブラウン
                fill ="brown",        #塗りもブラウン
                alpha=0.2  ) +        #透明度を指定
  xlab("残差(モデル上の予測値－実測値)")  #軸ラベル

#残差の2乗の分布を密度プロットで比較
ggplot()+
  geom_density( aes(x=LM3$residuals^2), #LM3の残差(2乗)
                color="orange",         #枠線をオレンジ
                fill ="orange",         #塗りもオレンジ
                alpha=0.2  ) +          #透明度を指定
  geom_density( aes(x=LM2$residuals^2), #LM2の残差(2乗)
                color="brown",          #枠線をブラウン
                fill ="brown",          #塗りもブラウン
                alpha=0.2  ) +          #透明度を指定
  xlab("残差の2乗")  #軸ラベル

