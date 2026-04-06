# Rastgele veri yaratmak için indirmemiz gereken paket için aşağıdaki kodu calistiriyoruz
install.packages("MASS")
# aktifleştirmemiz gerken kütüphane için kodu calistiriyoruz
library(MASS)

# Veri setini yaratma

set.seed(987) # Tekrarlanabilirlik amaciyla seed belirliyoruz
n <- 150 # gozlem sayisi
x1 <- rnorm(n, mean = 21, sd = 3) # BaDD1msD1z deDiEken 1
x2 <- rnorm(n, mean = 15, sd = 2.5) # BaDD1msD1z deDiEken 2
x3 <- rnorm(n, mean = 18, sd = 3.5) # BaDD1msD1z deDiEken 3
x4 <- rnorm(n, mean = 49, sd = 2.1) # BaDD1msD1z deDiEken 4
x5 <- rnorm(n, mean = 16, sd = 4) # BaDD1msD1z deDiEken 5
y <- 9 + 3*x1 - 2.5*x2 + 3.5*x3 + 2.1*x4 - 4*x5 + rnorm(n, mean = 0, sd = 3) # bagimsiz degisken

#veri setini bir data frame yapısına cevirmek için kodu calistiriyoruz
data <- data.frame(y, x1, x2, x3, x4, x5)

#  veri setini gosterme
head(data)

# x1 degiskenini faktorel degiskene donusturme
x1_factor <- as.factor(x1)

# x2 degiskenini sayisal degiskene donusturme
x2_numeric <- as.numeric(x2)

# degisken türlerini  kontrol etme
str(x1_factor)
str(x2_numeric)

#Veri kalitesi
#eksik veri tespiti ve doldurma

# ornek veri setini olusturma
set.seed(987)
n <- 150
x1_ev <- rnorm(n, mean = 21, sd = 2)
x2_ev <- rnorm(n, mean = 15, sd = 1)
y_ev <- rnorm(n, mean = 9 + 3*x1 - 2.5*x2, sd = 2)

# Eksik veri tespiti
is_na_x1 <- is.na(x1_ev)
is_na_x2 <- is.na(x2_ev)


# Eksik verileri ortalama ile doldurma
mean_x1_ev <- mean(x1, na.rm = TRUE)
x1_filled <- ifelse(is.na(x1_ev), mean_x1, x1)

install.packages("zoo")
library(zoo)

# ornek vektCor olusturma
vec <- c(1, NA, NA, 4, NA, 6, NA, 8, NA)

# Eksik deDerleri LOCF yontemiyle doldurma
vec_filled <- na.locf(vec)

print(vec_filled)

# Eksik verileri en yakin degerle doldurma
x2_filled <- na.locf(x2_ev)


#out lier tespiti
# ornek veri setini olusturma
set.seed(987)
data <- rnorm(150)

# Kutu grafigi olusturma
boxplot(data)
sort(data)

# Z-puanının hesaplanmasi
z_scores <- scale(data)

# out lier belirleme
outliers <- abs(z_scores) > 3


# Alt ve ust Ceyreklikleri hesaplama
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)

# Alt ve C<st sD1nD1rD1 hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR


#---
#dagilimlarin kesfi
# ornek veri setini olusturma
set.seed(987)
data <- rnorm(150)

# Histogram olusturma
hist(data)

# Kutu grafigi olusturma
boxplot(data)

# Q-Q plot olusturma
qqnorm(data)
qqline(data)

# Kantillerden yararlanma
summary(data)


#multicollinearity
# Crnek veri setini oluEturma
set.seed(987)
x1 <- rnorm(150) # BaDD1msD1z deDiEken 1
x2 <- rnorm(150) # BaDD1msD1z deDiEken 2
x3 <- rnorm(150) # BaDD1msD1z deDiEken 3
y <- 3*x1 + 2.5*x2 - 3.5*x3 + rnorm(150, mean = 0, sd = 3) # bagımlı degisken

# Coklu dogrusal regresyon modelini olusturma
model <- lm(y ~ x1 + x2 + x3)

# Model ozetini alma
summary(model)

# bagımsız degiskenler arasD1ndaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(x1, x2, x3))
print(correlation_matrix)
# corrplot paketini yukleme
library(corrplot)
# Korelasyon matrisini gorsellestirme
corrplot(correlation_matrix, method = "color")


# bagımsız degiskenlerin grafigini cizme
par(mfrow=c(1,3)) # Grafiklerin yan yana yerleEtirilmesi iC'in ayar
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "green", pch = 16)


# Crnek veri setini oluEturma
set.seed(987)
x1 <- rnorm(150) # BaDD1msD1z deDiEken 1
x2 <- rnorm(150) # BaDD1msD1z deDiEken 2
x3 <- rnorm(150) # BaDD1msD1z deDiEken 3
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(150, mean = 0, sd = 0.5) # BaDD1mlD1 deDiEken

# Veriyi standartlaEtD1rma
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
x3_standardized <- scale(x3)
y_standardized <- scale(y)

# Standartlastırılan veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(x3_standardized)
summary(y_standardized)

#---

#---
#kategorik keşif

# caTools paketini yukleme
install.packages("caTools")
library(caTools)

# Veri setini oluEturma (C6rnek veri)
set.seed(987)
x1 <- rnorm(150) # BaDD1msD1z deDiEken 1
x2 <- rnorm(150) # BaDD1msD1z deDiEken 2
x3 <- rnorm(150) # BaDD1msD1z deDiEken 3
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(150, mean = 0, sd = 0.5) # BaDD1mlD1 deDiEken

# Veriyi test-egitim ve  alt kümelere bolme
split <- sample.split(y, SplitRatio = 0.7) # 70% eDitim, 30% test
train_data <- subset(data.frame(x1, x2, x3, y), split == TRUE)
test_data <- subset(data.frame(x1, x2, x3, y), split == FALSE)

# Verinin boyutunu kontrol etme
dim(train_data)
dim(test_data)


# hazır veri seti ile yapacağımız analize buradan başlıyoruz.



# gerekli kütüphanelerin etkinleştirilmesi

library(MASS)
library(zoo)
library(corrplot)
library(caTools)

# Veri setinin kontrolu
head(Performans)
dim(Performans)
names(Performans)
str(Performans)
summary(Performans)


# EKSIK VERI KONTROLU


# Her degiskende eksik veri sayisi
colSums(is.na(Performans))




# TANIMLAYICI ISTATISTIKLER

summary(data)



# KATEGORIK DEGISKENLER ICIN FREKANS TABLOLARI

table(Performans$Cinsiyet)
table(Performans$Ebeveyn_Katilimi)
table(Performans$Kaynaklara_Erisim)
table(Performans$Motivasyon_Seviyesi)
table(Performans$Aile_Geliri)
table(Performans$Okul_Turu)
table(Performans$Ogrenme_Guclukleri)


# DAGILIMLARIN KESFI (SADECE NUMERIC DEGISKENLER)

# Grafik paneli
par(mfrow = c(2, 2))

# Histogramlar
hist(Performans$Calisilan_Saatler,
     main = "Histogram - Calisilan Saatler",
     xlab = "Calisilan_Saatler",
     col = "lightblue")

hist(Performans$Devam_Durumu,
     main = "Histogram - Devam Durumu",
     xlab = "Devam_Durumu",
     col = "lightgreen")

hist(Performans$Onceki_Notlar,
     main = "Histogram - Onceki Notlar",
     xlab = "Onceki_Notlar",
     col = "lightpink")

hist(Performans$Sinav_Notu,
     main = "Histogram - Sinav Notu",
     xlab = "Sinav_Notu",
     col = "lightgray")


#  KUTU GRAFIKLERI (OUTLIER KONTROLU)

par(mfrow = c(2, 2))

boxplot(Performans$Calisilan_Saatler,
        main = "Kutu Grafigi - Calisilan Saatler",
        col = "lightblue")

boxplot(Performans$Devam_Durumu,
        main = "Kutu Grafigi - Devam Durumu",
        col = "lightgreen")

boxplot(Performans$Onceki_Notlar,
        main = "Kutu Grafigi - Onceki Notlar",
        col = "lightpink")

boxplot(Performans$Sinav_Notu,
        main = "Kutu Grafigi - Sinav Notu",
        col = "lightgray")

par(mfrow = c(1, 1))

#  Q-Q PLOT 

par(mfrow = c(2, 2))

qqnorm(Performans$Calisilan_Saatler, main = "Q-Q Plot - Calisilan Saatler")
qqline(Performans$Calisilan_Saatler, col = "red")

qqnorm(Performans$Devam_Durumu, main = "Q-Q Plot - Devam Durumu")
qqline(Performans$Devam_Durumu, col = "red")

qqnorm(Performans$Onceki_Notlar, main = "Q-Q Plot - Onceki Notlar")
qqline(Performans$Onceki_Notlar, col = "red")

qqnorm(Performans$Sinav_Notu, main = "Q-Q Plot - Sinav Notu")
qqline(Performans$Sinav_Notu, col = "red")

par(mfrow = c(1, 1))


#  Z PUANI ILE outlier belirlenmesi


z_scores_sinav <- scale(Performans$Sinav_Notu)
outliers_z_sinav <- abs(z_scores_sinav) > 3

# IQR ILE AYKIRI DEGER TESPITI (SINAV_NOTU)

Q1_sinav <- quantile(Performans$Sinav_Notu, 0.25, na.rm = TRUE)
Q3_sinav <- quantile(Performans$Sinav_Notu, 0.75, na.rm = TRUE)
IQR_sinav <- Q3_sinav - Q1_sinav

lower_bound_sinav <- Q1_sinav - 1.5 * IQR_sinav
upper_bound_sinav <- Q3_sinav + 1.5 * IQR_sinav


# KORELASYON ANALIZI (SADECE NUMERIC DEGISKENLER)

numeric_performans <- Performans[, c("Calisilan_Saatler",
                         "Devam_Durumu",
                         "Uyku_Saatleri",
                         "Onceki_Notlar",
                         "Ozel_Ders_Oturumlari",
                         "Fiziksel_Aktivite",
                         "Sinav_Notu")]

correlation_matrix <- cor(numeric_performans)
print(correlation_matrix)

corrplot(correlation_matrix, method = "color")

# Veriyi test-egitim ve  alt kümelere bolme

split <- sample.split(Performans$Sinav_Notu, SplitRatio = 0.7)

train_performans <- subset(Performans, split == TRUE)
test_performans <- subset(Performans, split == FALSE)

dim(train_performans)
dim(test_performans)

head(train_performans)
head(test_performans)
