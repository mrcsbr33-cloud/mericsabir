# kullanılacak paketleri yükleme ve çağırma
install.packages("tidyverse")
install.packages("caret")
library(tidyverse)
library(caret)
str(data)  # Veri yapısını incelemek için kullanıyoruz
summary(data)  # özet istatistikleri görüntülemeye yarıyor

#Eksik Veri tespiti ve Temizlenmesi
data <- na.omit(data)  # Eksik verileri bu kod ile siliyoruz
#yeni isimlerin atammasD1 iC'in 1den 90 a dizi oluEturma
new_names= paste("m_", 1:20, sep="")

#verilerin yedeklenmesi
data_rn <- data
#yeni kopyalanan verinin degisken isimlerinin atanmas1
names(data_rn)  <- new_names


#data frame olusturma ve tibble 
data_df = data.frame(data_rn)
data_df_tib <- as_tibble(data_df)

#baslıklarin ve siniflarin kontrol edilmesi
head(data_df_tib)

#y nin atanmasD1 yalnD1zca 7. sutun
y=data_df_tib[7]
#3 tane y atamasi
y3=select(data_df_tib, m_6, m_7, m_10)

#m lerin olusturulmasi 
m <- subset(data_df_tib, select = -c(m_3, m_4, m_6, m_7, m_10 ))
#ayni m leri değişik sekilde oluşturulmasi
m4 <- data_df_tib[ , c(3:6, 11:20)]

#m lerin dataframe yapılmasi
m_df = data.frame(m)

#Numeric degerlerin filtrelenmesi
all_num <- Filter(is.numeric, data_df_tib)
m_num <- Filter(is.numeric, m)

#Numeric ve NA degerleri eleyen sabit bir fonksiyon. 
is_numeric_nu_na <- function(col){
  is_numeric <- is.numeric(col)
  no_na <- !anyNA(col)
  return(is_numeric & no_na)
}

#tüm ddegiskenler ve m ler olusturulan fonksiyona girilmesi
num_colu_no_na_all <- Filter(is_numeric_nu_na, all_num)
num_colu_no_na_m <- Filter(is_numeric_nu_na, m_num)

#tum degiskenlerin tanimlayici istatistiklerinin elde edilmesi
summ_stat <- summary(num_colu_no_na_all)

library("glmnet")
library(corrplot)
#data verisinin korelasyon grafigi
veri <- all_num[,1:7]
corr_mat <- cor(veri)
corrplot(corr_mat, method = "color")

# veri grafik
data(all_num)

ggplot(all_num,
       aes(x=m_10,
           y=m_6)) +
  geom_point() +
  facet_wrap(~m_1)




