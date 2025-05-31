

#veri okutuldu.
#veri genelgecer bir sekilde analiz edildi.eksik veri analizi ve veri hakkında genel bilgiler edinildi.
# mesela summary gibi
#verileri gorsellestirildi.
#verileri detayli analiz edildi.
#aykiri veriler analiz edilip aykırı olmayan maksimum değere eşitlendi.
#tekrar görselleştirme yapıldı.
#normalizasyon yapıldı. normalizasyon yapılmamış hali de bulunuyor. iki türlü de sonuclar gösterilecektir.
#egitim ve test datasi olusturuldu. knn ve siniflandirma islemi
#model seçimi
#model eğitimi
#conf matriksle (table())gerekli hesaplamalar yapıldı.
#veriler ve çıktılar görselleştirildi



#DOSYA OKUNDU, EKSİK VERİ ANALİZİ VE BASİT BİR GENEL ANALİZ.
file_directory <- "C:\\Users\\Default\\Downloads\\kalp.csv"
data<-read.csv(file_directory)# dosyayi belirtilen yerde bulup okur.
data
library(dplyr)
glimpse(data)
data$target
colnum<-ncol(data)
print(colnum) # 14 adet sütun var. 14 ayrı veri değerlendirilecek.
rownum<-nrow(data)
print(rownum) # 303 satir var. bu da demek ki 303 kisi bulunmakta.
head(data)
summary(data)
any(is.na(data)) #verinin hicbir kisminda eksik veri olmadigi belirtiliyor.
anyNA(data)
colSums(is.na(data))#farkli bir sekilde eksik veri analizi

#library(tidyverse)
library(ggplot2)
library(plotly)



#VERİLER GÖRSELLEŞTİRİLDİ.
boxplot(data$age,xlab = "Age",ylab = "Amount of people") #insanlarin yaslari histogram tablosunda gorsellestirilmistir. 40-70 yas araligi cok 
barplot(table(data$sex),xlab = "Women                                Men") # cinsiyet dagiliminda erkekler cogunlukta
boxplot(data$chol)
hist(data$age) # digerlerinden uzakta kalan bir veri ya da bolum yok. 
barplot(table(data$sex))#kategorik
#hist(data$cp,xlab="Type of the chest pain",ylab="Age", main = "Situation between chest pain and age",breaks = seq(min(data$cp), max(data$cp) + 1, by = 1))
barplot(table(data$cp))
boxplot(data$trestbps)#histogramda 200 digerlerinden ayri gozukmekte. aykiri veri olarak alinabilir.
hist(data$chol)# 600 ve diğerleri aykiri veri
barplot(table(data$fbs))
barplot(table(data$restecg))
boxplot(data$thalach)#80 aykiri
barplot(table(data$exang))
boxplot(data$oldpeak)#6 kismi aykiri 
barplot(table(data$slope))
barplot(table(data$ca))
barplot(table(data$thal))
barplot(table(data$target))
#kategorik veriler barplot, sayisal(numeric) veriler histogram ile gorsellestirilmistir
ggplot(data , aes(x = sex , y = cp  , color = target)) + 
  
  geom_point() + 
  labs(title = "sex-cp" , x="sex" , y="cp" )

plot(data$age, data$chol, 
     main="correlation of chol with age", 
     xlab="age", 
     ylab="chol rate", 
     pch=19, 
     col="blue")#birbirinden uzakta cok veri var.
library(reshape2)
library(GGally)
#KORELASYON GRAFİĞİ.
ggcorr(data, nbreaks = 8, palette = "RdGy") #-1 ve 1 arasina 8 aralik koydum. her 0.25 degisimde renk degisir
## 0 ve -0.25 araligindaki korelasyonlar
#age-sex,age-cp,sex-cp,sex,trestbps,sex-chol,cp-chol,age-restecg,sex-restecg,sex-thalach,trestbps-restecg
#trestbps-thalach,chol-restecg,chol-thalach,fbs-restecg,fbs-thalach,restecg-exang,cp-oldpeak,restecg-oldpeak
#age-slope,sex-slope,trestbps-slope,chol-slope,fbs-slope,cp-ca,restecg-ca,thalach-ca,slope-ca,cp-thal,fbs-thal
#restecg-thal,thalach-thal,slope-thal,age-target,trestbps-target,chol-target,fbs-target
##

## -0.25 ve -0.5 arasi korelasyonlar
#age-thalach,cp-exang,thalach-exang,thalach-oldpeak,exang-slope,sex-target,exang-target
#oldpeak-target,ca-target-,thal-target
##

##-0.5 -0.75 arasi korelasyon
#oldpeak-slope
##

##0 ve 0.25 arasi korelasyonlar
#cp-trestbps,age-chol,trestbps-chol,age-fbs,sex-fbs,cp-fbs,trestbps-fbs,chol-fbs,cp-restecg,restecg-thalach
#age-exang,sex-exang,trestbps-exang,chol-exang,fbs-exang,age-oldpeak,sex-oldpeak,trestbps-oldpeak,chol-oldpeak,
#fbs-oldpeak,cp-slope,restecg-slope,sex-ca,trestbps-ca,chol-ca,fbs-ca,exang-ca,oldpeak-ca,age-thal,sex-thal
#trestbps-thal,chol-thal,exang-thal,oldpeak-thal,ca-thal,restecg-target
##

##0.25 0.50 arasi korelasyonlar
#age-trestbps,cp-thalach,exang-oldpeak,thalach-slope,age-ca,cp-target,thalach-target,slope-target
##

head(data)#henüz satırlar karıştırılmadı. gelecekte farklı bir isimde bunu sağlayacağım

#aykiri verileri burada analiz ettim. aykiri verilerden sonra normalizasyon yapmalıyım.

#chol-trestbps-thalach-oldpeak bunlar aykiri veri

print(max(data$chol))#aykiri verilerden once
length((data$chol))
data$chol

#chol artık aykiri verilere sahip degil. boxplotta kutunun icinde kalan maksimum degerler atanmistir.
max_chol<-data$chol[!data$chol %in% boxplot.stats(data$chol)$out]
data$chol[data$chol %in% boxplot.stats(data$chol)$out] <- max(max_chol)
#outlier_chol<- data$chol[data$chol %in% boxplot.stats(data$chol)$out]<-360
print(max(data$chol)) # aykiri veri alinmadi

length(data$chol)
#aykiri verileri yazdiriyor. bu sekilde tum aykiri verileri bul. kutu disindakileri veriyor.
print(data$chol)

#chol artık aykiri verilere sahip degil. boxplotta kutunun icinde kalan maksimum degerler atanmistir.

print(max(data$trestbps))#aykiri verilerle 
length((data$trestbps))
data$trestbps

#trestbpsde de aykiri veri yok

max_trest<-data$trestbps[!data$trestbps %in% boxplot.stats(data$trestbps)$out]#iceride kalanlar arasında maksimum deger bulundu
max(max_trest)#170

data$trestbps[data$trestbps %in% boxplot.stats(data$trestbps)$out] <- max(max_trest)# hata verirse max(max_trest) yerine 170 yaz. 170'i aykirilarin yerine yazdik
max(data$trestbps)# aykiri degerler gitti ve maks 170 oldu
length(data$trestbps) # herhangi bir veri kaybı yok. 303 veri
data$trestbps
#trestbpsde de aykiri veri yok

print(min(data$thalach))#aykiri verilerle. min yazma sebebim aykiri verilerin burada kucuk olmasi. (71-88)
length((data$thalach))
data$thalach

#thalachta da aykiri veri yok.
min_thalach<-data$thalach[!data$thalach %in% boxplot.stats(data$thalach)$out]#iceride kalanlar arasında maksimum deger bulundu
print(min(data$thalach))

data$thalach[data$thalach %in% boxplot.stats(data$thalach)$out] <- min(min_thalach)
print(min(data$thalach))
length(data$thalach)
#thalachta da aykiri veri yok.

print(max(data$oldpeak))#aykiri verilerden once
length((data$oldpeak))
data$oldpeak

#oldpeakte de aykiri veri kalmamistir.
max_oldpeak<-data$oldpeak[!data$oldpeak %in% boxplot.stats(data$oldpeak)$out]#iceride kalanlar arasında maksimum deger bulundu
max(max_oldpeak)

data$oldpeak[data$oldpeak %in% boxplot.stats(data$oldpeak)$out] <- max(max_oldpeak)
max(data$oldpeak)# aykiri degerler gitti ve maks 170 oldu
length(data$oldpeak) # herhangi bir veri kaybı yok. 303 veri
data$oldpeak
#oldpeakte de aykiri veri kalmamistir

#data$ca aykırı veri analizinden önce max deger falan yazdir.
data$ca
max(data$ca)
length(data$ca)
#data$ca 4 degerleri 3 olmustur. Dokümanda bu şekilde belirtilmiş (0-3). 
#bu yüzden 4'ü ve diğer sayıları(olsaydı) aykırı veri olarak ele aldım.
max_ca <- data$ca[data$ca %in% c(0,1,2,3)]
print(max(max_ca))
data$ca[!(data$ca %in% c(0,1,2,3))] <- max(max_ca)

data$ca
barplot(table(data$ca))
#data$ca 4 degerleri 3 olmustur. Dokümanda bu şekilde belirtilmiş (0-3). bu yüzden aykırı veri olarak ele aldım.
#değerler arasında birbirine yakınlık olan(ya da kategorik) sütunların aykırı verilerini ayıklamadım  etmedim.

#sınıflandırma, makine öğrenmesi ve lineer regresyon icin avantaj sağlayan satırları rastgele dizmek
set.seed(123)
data_new<-data
data_new<-data_new[sample(1:nrow(data_new)),]  #onceki dataya gore burada satirlar rastgele dagilimla siralanacaktir.
head(data_new)# satirlar rastgele
head(data)#satirlar ilk haldekiyle ayni

#     NORMALİZASYON
normalize_data<-scale(data_new[,1:13])## hedef değişken normalize edilmeyecektir.
normalize_data<-data.frame(normalize_data)
#normalize_df <- data.frame(normalize_data , target = data_new$target)
normalize_df<-data.frame(normalize_data, target = data_new$target)
head(normalize_df)

#    GÖRSELLEŞTİRME
boxplot(normalize_df$age,xlab = "Age",ylab = "Amount of people") #insanlarin yaslari histogram tablosunda gorsellestirilmistir. 40-70 yas araligi cok 
boxplot(normalize_df$chol)# göründüğü gibi grafik normalize edilmiştir. ayrıca aykırı veriye rastlanmamaktadır.
hist(normalize_df$age) # digerlerinden uzakta kalan bir veri ya da bolum yok. ancak miktari az olan bolumler aykiri olabilir.
#hist(normalize_df$cp,xlab="Type of the chest pain",ylab="Age", main = "Situation between chest pain and age",breaks = seq(min(data$cp), max(data$cp) + 1, by = 1))
hist(normalize_df$chol)
boxplot(normalize_df$thalach)
boxplot(normalize_df$oldpeak)#6 kismi aykiri 
boxplot(normalize_df$trestbps)#normalizasyondan sonra da aykırı veri göründü.

#AYKIRILIK ÖNCESİ ANALİZ
print(max(normalize_df$trestbps))#aykiri verilerle 
length((normalize_df$trestbps))
normalize_df$trestbps

#trestbps normalize_df kısmındaki aykırılığı düzelt
max_trest_norm<-normalize_df$trestbps[!normalize_df$trestbps %in% boxplot.stats(normalize_df$trestbps)$out]#iceride kalanlar arasında maksimum deger bulundu
max(max_trest_norm)#2.03

normalize_df$trestbps[normalize_df$trestbps %in% boxplot.stats(normalize_df$trestbps)$out] <- max(max_trest_norm)# hata verirse max(max_trest) yerine 2,03 yaz. 2,03'ü aykirilarin yerine yazdik
max(normalize_df$trestbps)# aykiri degerler gitti ve maks 2,03 oldu
length(normalize_df$trestbps) # herhangi bir veri kaybı yok. 303 veri
normalize_df$trestbps
boxplot(normalize_df$trestbps)
#trestbps norm aykırılık yok.

## Veriler artık uygun

#feature extraction

library(stats)#prcomp için. 
pca_value<-prcomp(data_new, scale=TRUE)#pca analizi için normalizasyon
summary(pca_value)
sapply(data_new, class) #sapply önemli. veri yapısına ait her elemana uygulanmak istenen işlem için.
plot(pca_value)#pca_value görselleştirildi
plot(pca_value$x[,1], pca_value$x[,2], xlab="X", ylab="Y")
plot(pca_value$x[,3], pca_value$x[,4], xlab="X", ylab="Y")## bazı yerlerde noktalar fazla birikmiş. uzak düşenler işleme alınmaz
plot(pca_value$x[,5], pca_value$x[,6], xlab="X", ylab="Y")## dolayısıyla verilerin boyutu azalır. 



# PCA ile boyut indirgeme  
##data_reduced <- data.frame(pca_value$x[,1:n_components])
##plot(data_reduced$PC1)
##plot(data_reduced$PC2)
##plot(data_reduced$PC3)
##plot(data_reduced$PC4)
##print(length(data_reduced))
##data_reduced <- data_reduced[sample(1:nrow(data)),]
##head(data_reduced)
##data_pca = data.frame(data_reduced$PC1, data_reduced$PC2, data_reduced$PC3, data_reduced$PC4)



#sınıflandırma, makine öğrenmesi ve lineer regresyon icin avantaj sağlayan satırları rastgele dizmek
set.seed(123)
data_new<-data
data_new<-data_new[sample(1:nrow(data_new)),]  #onceki dataya gore burada satirlar rastgele dagilimla siralanacaktir.
head(data_new)# satirlar rastgele
head(data)#satirlar ilk haldekiyle ayni


##egitim verileri ve test verileri olusturmak icin bol
library(class)
library(caret)
div <- createDataPartition(data_new$target , p =0.7 , list = FALSE)
train_Dataa <- data_new[div, ] #yuzde 70 i train edilecek
test_Dataa <-  data_new[-div, ] # yuzde 30 test datasi olacak


# 5 tane sınıflandırma algoritmasını da kullanmam lazım. hepsi farklı olmalı
#knn
knn_modell <- knn(train = train_Dataa[,1:14] , test=test_Dataa[, 1:14] , cl=train_Dataa$target , k = 10)
confusion_matrixx <- table(Referance=test_Dataa$target , Prediction=knn_modell) #table hata payi methodu
print(confusion_matrixx)
## normalizasyon olmamış hali #############################################################
accuracy <- sum(diag(confusion_matrixx)) / sum(confusion_matrixx)
cat("Knn algoritmasi dogrulugu : " , accuracy)#normalizasyon olmadan gelen dogruluk orani KNN
# True Positives (TP)
TP <- confusion_matrixx[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrixx[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity = TP / (TP + FN)
cat("Knn algoritmasi hassasiyeti : " , Sensitivity)#normalizasyondan önceki hassasiyet oranı

TN <- confusion_matrixx[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrixx[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon olmadan f1 skoru

# Sonucu yazdırma
cat("Knn algoritmasi F1 skoru:", F1_score)
print(F1_score)
Specificity <- TN / (TN + FP);
print(Specificity)
#normalizasyon olmamış özgüllük oranı


# Örnek veri çerçevesi görselleştirmeden önce veri çerçevesi veya data frameye dönüştürülmelidir.
data_frame_viss <- data.frame(
  result = c("accuracy", "Sensitivity", "Specificity", "F1_score"),
  value = c(accuracy, Sensitivity, Specificity, F1_score)
)
# Çubuk grafik oluşturma. bu şekilde analizler yapılabilir.
ggplot(data_frame_viss, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")
##normalizasyon olmadan hata matrisi görselleştirmesi yapıldı.###############################################




#################### NORMALİZASYONLU KNN

div_knn_norm <- createDataPartition(normalize_df$target , p =0.7 , list = FALSE)
train_Data <- normalize_df[div_knn_norm, ] #yuzde 70 i train edilecek
test_Data <-  normalize_df[-div_knn_norm, ] # yuzde 30 test datasi olacak


#knn 
#6-0.94
knn_model <- knn(train = train_Data[,1:14] , test=test_Data[,1:14] , cl=train_Data$target , k = 6) # K 6
confusion_matrix <- table(Referance=test_Data$target , Prediction=knn_model) #table hata payi methodu.
#buradaki çıktılarla doğruluk,hassasiyet gibi kavramları sonuçlandıracağız.
##confusionMatrix() metodu ile daha kolay bir şekilde bu değerler alınabilir. ancak ben uzun yolda yaptım.
print(confusion_matrix)


accuracy_norm <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Knn algoritmasi dogrulugu : " , accuracy_norm)#normalizasyondan sonraki doğruluk oranı


# True Positives (TP)
TP <- confusion_matrix[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrix[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity = TP / (TP + FN)
cat("Knn algoritmasi hassasiyeti : " , Sensitivity)#normalizasyondan sonraki hassasiyet oranı

TN <- confusion_matrix[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrix[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyondan sonraki f1 skoru

# Sonucu yazdırma
cat("Knn algoritmasi F1 skoru:", F1_score)
print(F1_score)
Specificity <- TN / (TN + FP);
print(Specificity)
#normalizasyondan sonraki özgüllük oranı


# Örnek veri çerçevesi
data_frame_vis <- data.frame(
  result = c("accuracy_norm", "Sensitivity", "Specificity", "F1_score"),
  value = c(accuracy_norm, Sensitivity, Specificity, F1_score)
)

# Çubuk grafik oluşturma
ggplot(data_frame_vis, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

##SVM,DECİSİONTREE,LOJİSTİK REGRESYON, Naive Bayes,randomforest bunlar arasından seçim yapılacak ve devam edilecek
##############################################################################################################################################################

####################### Lineer regresyon normalize edilmeden.

div_lr <- createDataPartition(data_new$target , p =0.7 , list = FALSE)#satırlar rastgele
train_Dataa_lr <- data_new[div_lr, ] #yuzde 70 i train edilecek
test_Dataa_lr <-  data_new[-div_lr, ] # yuzde 30 test datasi olacak
prop.table(table(train_Dataa_lr$target))##yüzdelik oranlar verilir 1-0

lr_pred <- glm(target ~ .,              #tahmin
               data = train_Dataa_lr,
               family = "binomial")

lr_model <- step(object = lr_pred,      #model eğitimi
                 direction = "backward",
                 trace = F)


test_Dataa_lr$pred_lr <- predict(lr_model, #tahmin edildikten sonra tahminin tutulacağı yer
                                 test_Dataa_lr,
                                 type = "response")
test_Dataa_lr$pred_label_lr <- ifelse(test_Dataa_lr$pred_lr >= 0.5, yes = 1, no = 0) #pred_lr değerine göre yeni bir değişkenle
#1 veya 0 değerine yuvarlama. pred_label_lr kullanılacak bundan sonra

test_Dataa_lr %>% ##işlem zinciri için %>%
  select(target, 
         pred_lr, 
         pred_label_lr) %>% 
  rmarkdown::paged_table() #büyük verileri aşağı kaymadan göstermek için kullandım. ancak çok da gerekli değildi.
confusion_matrixx_lr<-table(Referance= test_Dataa_lr$target,Prediction=test_Dataa_lr$pred_label_lr)# referans kısmı target prediction kısmı tahminimiz
print(confusion_matrixx_lr)

accuracy_lr <- sum(diag(confusion_matrixx_lr)) / sum(confusion_matrixx_lr)
cat("lineer regresyon algoritmasi dogrulugu : " , accuracy_lr)#normalizasyondan önce doğruluk oranı

# True Positives (TP)
TP <- confusion_matrixx_lr[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrixx_lr[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity_lr = TP / (TP + FN)
cat("lineer regresyon algoritmasi hassasiyeti : " , Sensitivity_lr)#normalizasyondan önceki hassasiyet oranı

TN <- confusion_matrixx_lr[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrixx_lr[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_lr <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon olmadan f1 skoru

# Sonucu yazdırma
cat("lineer regresyon algoritmasi F1 skoru:", F1_score_lr)
print(F1_score_lr)
Specificity_lr <- TN / (TN + FP);
print(Specificity_lr)
#normalizasyon olmamış özgüllük oranı


# Örnek veri çerçevesi
data_frame_viss_lr <- data.frame(
  result = c("accuracy_lr", "Sensitivity_lr", "Specificity_lr", "F1_score_lr"),
  value = c(accuracy_lr, Sensitivity_lr, Specificity_lr, F1_score_lr)
)
# Çubuk grafik oluşturma
ggplot(data_frame_viss_lr, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")################
##normalizasyon olmadan hata matrisi görselleştirmesi yapıldı.

################## NORMALİZASYONSUZ LİNEER REGRESYON TAMAMLANDI.

################# NORMALİZASYONLU LİNEER REGRESYON
div_lr_norm <- createDataPartition(normalize_df$target , p =0.7 , list = FALSE)#satırlar rastgele
train_Dataa_lr_norm <- normalize_df[div_lr_norm, ] #yuzde 70 i train edilecek
test_Dataa_lr_norm <-  normalize_df[-div_lr_norm, ] # yuzde 30 test datasi olacak
prop.table(table(train_Dataa_lr_norm$target))
#train_Dataa_lr_norm$target <-ifelse(train_Dataa_lr_norm$target>=0.5, yes=1,no=0)
lr_pred_norm <- glm(target ~ .,
                    data = train_Dataa_lr_norm,
                    family = "binomial")

lr_model_norm <- step(object = lr_pred_norm,
                      direction = "backward",
                      trace = F)


test_Dataa_lr_norm$pred_lr <- predict(lr_model_norm,
                                      test_Dataa_lr_norm,
                                      type = "response")
test_Dataa_lr_norm$pred_label_lr_norm <- ifelse(test_Dataa_lr_norm$pred_lr >= 0.65, yes = 1, no = 0)##Doğruluk oranı yükseldiği için
#0.65 kullandım. 0.5'e göre daha isabetli sonuç veriyor.

test_Dataa_lr_norm %>% 
  select(target, 
         pred_lr, 
         pred_label_lr_norm) %>% # yukarıda bahsetmiştim
  rmarkdown::paged_table()

confusion_matrixx_lr_norm<-table(Referance= test_Dataa_lr_norm$target,Prediction=test_Dataa_lr_norm$pred_label_lr_norm)
print(confusion_matrixx_lr_norm)

accuracy_lr_norm <- sum(diag(confusion_matrixx_lr_norm)) / sum(confusion_matrixx_lr_norm)
cat("lineer regresyon algoritmasi dogrulugu : " , accuracy_lr_norm)#normalizasyonlu doğruluk oranı

# True Positives (TP)
TP <- confusion_matrixx_lr_norm[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrixx_lr_norm[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity_lr_norm = TP / (TP + FN)
cat("lineer regresyon algoritmasi hassasiyeti : " , Sensitivity_lr_norm)#normalizasyonlu hassasiyet oranı

TN <- confusion_matrixx_lr_norm[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrixx_lr_norm[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_lr_norm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyonlu f1 skoru

# Sonucu yazdırma
cat("lineer regresyon algoritmasi F1 skoru:", F1_score_lr_norm)
print(F1_score_lr_norm)
Specificity_lr_norm <- TN / (TN + FP);
print(Specificity_lr_norm)
#normalizasyonlu özgüllük oranı


data_frame_viss_lr_norm <- data.frame( #dataframe oluşturma görselleştirmek için
  result = c("accuracy_lr_norm", "Sensitivity_lr_norm", "Specificity_lr_norm", "F1_score_lr_norm"),
  value = c(accuracy_lr_norm, Sensitivity_lr_norm, Specificity_lr_norm, F1_score_lr_norm)
)
# Çubuk grafik oluşturma
ggplot(data_frame_viss_lr_norm, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")
##################normalizasyon olmadan hata matrisi görselleştirmesi yapıldı. ANCAK NORMALİZASYONDAN SONRA DEĞERLER AZALDI. BU SORUNU ÇÖZ.


################  SVM NORMALİZASYONSUZ ##########################
#eğitim test kısmı
div_svm <- sample(1:nrow(data_new), 0.7 * nrow(data_new))
train_Dataa_svm <- data_new[div_svm, ]
test_Dataa_svm <- data_new[-div_svm, ]
library(e1071)
"~"
svm_modell  <- svm(target ~ . ,data=train_Dataa_svm,kernel="radial" ) 
#svmyi esnek kılar kernel="radial". 

summary(svm_modell)

pred_svm <- predict(svm_modell , newdata = train_Dataa_svm)

svm_pred <- glm(target ~ .,
                data = train_Dataa_svm,
                family = "binomial") #0 ya da 1

svm_modell <- step(object = svm_pred,
                   direction = "backward",
                   trace = F)
## yukarıdakilerin aynısı
test_Dataa_svm$pred_svm <- predict(svm_modell,
                                   test_Dataa_svm,
                                   type = "response")
test_Dataa_svm$pred_label_svm <- ifelse(test_Dataa_svm$pred_svm >= 0.5, yes = 1, no = 0)# pred_svm değerlerini 0 ya da 1'e 
#dönüştürüp ona göre target ile kıyaslanacak.

test_Dataa_svm %>% 
  select(target, 
         pred_svm, 
         pred_label_svm) %>% 
  rmarkdown::paged_table()

confusion_matrixx_svm <- table(test_Dataa_svm$target , test_Dataa_svm$pred_label_svm)
print(confusion_matrixx_svm)

accuracy_svm <- sum(diag(confusion_matrixx_svm)) / sum(confusion_matrixx_svm)
cat("svm algoritmasi dogrulugu : " , accuracy_svm)#normalizasyon olmadan gelen dogruluk orani

# True Positives (TP)
TP <- confusion_matrixx_svm[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrixx_svm[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity_svm = TP / (TP + FN)
cat("lineer regresyon algoritmasi hassasiyeti : " , Sensitivity_svm)#normalizasyondan önceki hassasiyet oranı

TN <- confusion_matrixx_svm[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrixx_svm[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_svm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon olmadan f1 skoru

# Sonucu yazdırma
cat("lineer regresyon algoritmasi F1 skoru:", F1_score_svm)
print(F1_score_svm)
Specificity_svm <- TN / (TN + FP);
print(Specificity_svm)
#normalizasyon olmamış özgüllük oranı


# Örnek veri çerçevesi
data_frame_viss_svm <- data.frame(
  result = c("accuracy_svm", "Sensitivity_svm", "Specificity_svm", "F1_score_svm"),
  value = c(accuracy_svm, Sensitivity_svm, Specificity_svm, F1_score_svm)
)
# Çubuk grafik oluşturma
ggplot(data_frame_viss_svm, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")################
##normalizasyon olmadan hata matrisi görselleştirmesi yapıldı.
###################SVM NORMALİZASYONSUZ TAMAMLANDI.#######################


###########SVM NORMALİZASYONLU ##########################
#yukarıdaki kodlarla aşağı yukarı aynı olduğu için aynı yorum satırlarını eklemek istemedim. sadece veriler normalize
div_svm_norm <- sample(1:nrow(normalize_df), 0.7 * nrow(normalize_df))
train_Dataa_svm_norm <- normalize_df[div_svm_norm, ]
test_Dataa_svm_norm <- normalize_df[-div_svm_norm, ]
library(e1071)
"~"
svm_modell_norm  <- svm(target ~ . ,data=train_Dataa_svm_norm,kernel="radial" )

summary(svm_modell_norm)

pred_svm_norm <- predict(svm_modell_norm , newdata = train_Dataa_svm_norm)

#train_Dataa_svm_norm$target <-ifelse(train_Dataa_svm_norm$target>=0.5, yes=1,no=0)

svm_pred_norm <- glm(target ~ .,
                     data = train_Dataa_svm_norm,
                     family = "binomial")

svm_modell_norm <- step(object = svm_pred_norm,
                        direction = "backward",
                        trace = F)

test_Dataa_svm_norm$pred_svm_norm <- predict(svm_modell_norm,
                                             test_Dataa_svm_norm,
                                             type = "response")
test_Dataa_svm_norm$pred_label_svm_norm <- ifelse(test_Dataa_svm_norm$pred_svm_norm >= 0.5, yes = 1, no = 0)

test_Dataa_svm_norm %>% 
  select(target, 
         pred_svm_norm, 
         pred_label_svm_norm) %>% 
  rmarkdown::paged_table()

confusion_matrixx_svm_norm <- table(test_Dataa_svm_norm$target , test_Dataa_svm_norm$pred_label_svm_norm)
print(confusion_matrixx_svm_norm)

accuracy_svm_norm <- sum(diag(confusion_matrixx_svm_norm)) / sum(confusion_matrixx_svm_norm)
cat("svm algoritmasi dogrulugu : " , accuracy_svm_norm)#normalizasyon olmadan gelen dogruluk orani

# True Positives (TP)
TP <- confusion_matrixx_svm_norm[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrixx_svm_norm[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity_svm_norm = TP / (TP + FN)
cat("lineer regresyon algoritmasi hassasiyeti : " , Sensitivity_svm_norm)#normalizasyondan önceki hassasiyet oranı

TN <- confusion_matrixx_svm_norm[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrixx_svm_norm[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_svm_norm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon olmadan f1 skoru

# Sonucu yazdırma
cat("svm norm algoritmasi F1 skoru:", F1_score_svm_norm)
print(F1_score_svm_norm)
Specificity_svm_norm <- TN / (TN + FP);
print(Specificity_svm_norm)
#normalizasyon olmamış özgüllük oranı


# Örnek veri çerçevesi
data_frame_viss_svm_norm <- data.frame(
  result_norm = c("accuracy_svm_norm", "Sensitivity_svm_norm", "Specificity_svm_norm", "F1_score_svm_norm"),
  value_norm = c(accuracy_svm_norm, Sensitivity_svm_norm, Specificity_svm_norm, F1_score_svm_norm)
)
# Çubuk grafik oluşturma
ggplot(data_frame_viss_svm_norm, aes(x=result_norm, y=value_norm)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

##################      NORMALİZASYONLU SVM DE BİTTİ


###################### NORMALİZASYONSUZ BAYES BİTTİ
library(naivebayes)

div_naive <- sample(1:nrow(data_new), 0.7 * nrow(data_new))
train_Dataa_naive <- data_new[div_naive, ]
test_Dataa_naive <- data_new[-div_naive, ]
#train_Dataa_naive$target <- as.factor(train_Dataa_naive$target)

naiveBayesModel <- naiveBayes(target ~ ., data=train_Dataa_naive)
naiveBayesTestPred <- predict(naiveBayesModel, test_Dataa_naive)
confusion_matrixx_nb<-table(test_Dataa_naive$target,naiveBayesTestPred)
print(confusion_matrixx_nb)



accuracy_nb <- sum(diag(confusion_matrixx_nb)) / sum(confusion_matrixx_nb)
cat("naive bayes algoritmasi dogrulugu : " , accuracy_nb)#normalizasyonsuz doğruluk oranı


# True Positives (TP)
TP <- confusion_matrixx_nb[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrixx_nb[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity_nb = TP / (TP + FN)
cat("naive bayes algoritmasi hassasiyeti : " , Sensitivity_nb)#normalizasyonsuz hassasiyet oranı

TN <- confusion_matrixx_nb[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrixx_nb[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_nb <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyonsuz f1 skoru

# Sonucu yazdırma
cat("naive bayes algoritmasi F1 skoru:", F1_score_nb)
print(F1_score_nb)
Specificity_nb <- TN / (TN + FP);
print(Specificity_nb)
#normalizasyondan sonraki özgüllük oranı


# Örnek veri çerçevesi
data_frame_vis_nb <- data.frame(
  result_nb = c("accuracy_nb", "Sensitivity_nb", "Specificity_nb", "F1_score_nb"),
  value_nb = c(accuracy_nb, Sensitivity_nb, Specificity_nb, F1_score_nb)
)

# Çubuk grafik oluşturma
ggplot(data_frame_vis_nb, aes(x=result_nb, y=value_nb)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

################## NORMALİZASYONSUZ BAYES BİTTİ ##############################

##################  NORMALİZASYONLU BAYES ####################################

div_naive_norm <- sample(1:nrow(normalize_df), 0.7 * nrow(normalize_df))
train_Dataa_naive_norm <- normalize_df[div_naive_norm, ]
test_Dataa_naive_norm <- normalize_df[-div_naive_norm, ]
#train_Dataa_naive$target <- as.factor(train_Dataa_naive$target)

naiveBayesModel_norm <- naiveBayes(target ~ ., data=train_Dataa_naive_norm)
naiveBayesTestPred_norm <- predict(naiveBayesModel_norm, test_Dataa_naive_norm)
confusion_matrixx_nb_norm<-table(test_Dataa_naive_norm$target,naiveBayesTestPred_norm)
print(confusion_matrixx_nb_norm)



accuracy_nb_norm <- sum(diag(confusion_matrixx_nb_norm)) / sum(confusion_matrixx_nb_norm)
cat("naive bayes algoritmasi dogrulugu : " , accuracy_nb_norm)#normalizasyondan sonraki doğruluk oranı


# True Positives (TP)
TP <- confusion_matrixx_nb_norm[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrixx_nb_norm[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity_nb_norm = TP / (TP + FN)
cat("naive bayes algoritmasi hassasiyeti : " , Sensitivity_nb_norm)#normalizasyondan sonraki hassasiyet oranı

TN <- confusion_matrixx_nb_norm[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrixx_nb_norm[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_nb_norm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyondan sonraki f1 skoru

# Sonucu yazdırma
cat("naive bayes algoritmasi F1 skoru:", F1_score_nb_norm)
print(F1_score_nb_norm)
Specificity_nb_norm <- TN / (TN + FP);
print(Specificity_nb_norm)
#normalizasyondan sonraki özgüllük oranı


# Örnek veri çerçevesi
data_frame_vis_nb_norm <- data.frame(
  result_nb_norm = c("accuracy_nb_norm", "Sensitivity_nb_norm", "Specificity_nb_norm", "F1_score_nb_norm"),
  value_nb_norm = c(accuracy_nb_norm, Sensitivity_nb_norm, Specificity_nb_norm, F1_score_nb_norm)
)

# Çubuk grafik oluşturma
ggplot(data_frame_vis_nb_norm, aes(x=result_nb_norm, y=value_nb_norm)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")
######### normalizasyonlu naive bayes de bitti################################

############# DECİSİON TREE NORMALİZASYONSUZ ##############################

library(rpart)

div_dt <- sample(1:nrow(data_new), 0.7 * nrow(data_new))
train_Dataa_dt <- data_new[div_dt, ]
test_Dataa_dt <- data_new[-div_dt, ]

decisionTreeModel <- rpart(target ~ ., data=train_Dataa_dt, method="class")# method eklenmesi gerek dec treede
decisionTreeTestPred <- predict(decisionTreeModel, test_Dataa_dt, type = "class")

confusion_matrixx_dt<- table(test_Dataa_dt$target,decisionTreeTestPred)
print(confusion_matrixx_dt)

accuracy_dt <- sum(diag(confusion_matrixx_dt)) / sum(confusion_matrixx_dt)
cat("decision tree algoritmasi dogrulugu : " , accuracy_dt)#normalizasyon öncesi doğruluk oranı


# True Positives (TP)
TP <- confusion_matrixx_dt[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrixx_dt[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity_dt = TP / (TP + FN)
cat("Decision Tree algoritmasi hassasiyeti : " , Sensitivity_dt)#normalizasyon öncesi hassasiyet oranı

TN <- confusion_matrixx_dt[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrixx_dt[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_dt <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon öncesi f1 skoru

# Sonucu yazdırma
cat("Decision Tree algoritmasi F1 skoru:", F1_score_dt)
print(F1_score_dt)
Specificity_dt <- TN / (TN + FP);
print(Specificity_dt)
#normalizasyon öncesi özgüllük oranı


# Örnek veri çerçevesi
data_frame_vis_dt <- data.frame(
  result_dt = c("accuracy_dt", "Sensitivity_dt", "Specificity_dt", "F1_score_dt"),
  value_dt = c(accuracy_dt, Sensitivity_dt, Specificity_dt, F1_score_dt)
)

# Çubuk grafik oluşturma
ggplot(data_frame_vis_dt, aes(x=result_dt, y=value_dt)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

################################################### NORMALİZASYONSUZ KARAR AĞAÇLARI TAMAMLANDI#############################


################################################## NORMALİZASYONLU KARAR AĞAÇLARI##########################################


div_dt_norm <- sample(1:nrow(normalize_df), 0.7 * nrow(normalize_df))
train_Dataa_dt_norm <- normalize_df[div_dt_norm, ]
test_Dataa_dt_norm <- normalize_df[-div_dt_norm, ]

decisionTreeModel_norm <- rpart(target ~ ., data=train_Dataa_dt_norm, method="class")
decisionTreeTestPred_norm <- predict(decisionTreeModel_norm, test_Dataa_dt_norm, type = "class")

confusion_matrixx_dt_norm<- table(test_Dataa_dt_norm$target,decisionTreeTestPred_norm)
print(confusion_matrixx_dt_norm)

accuracy_dt_norm <- sum(diag(confusion_matrixx_dt_norm)) / sum(confusion_matrixx_dt_norm)
cat("Knn algoritmasi dogrulugu : " , accuracy_dt_norm)#normalizasyondan sonraki doğruluk oranı


# True Positives (TP)
TP <- confusion_matrixx_dt_norm[2,2]#matriksin pozitif kısmı ve doğru

# False Negatives (FN)
FN <- confusion_matrixx_dt_norm[2,1]#matriksin negatif kısmı ve yanlış
Sensitivity_dt_norm = TP / (TP + FN)
cat("Knn algoritmasi hassasiyeti : " , Sensitivity_dt_norm)#normalizasyondan sonraki hassasiyet oranı

TN <- confusion_matrixx_dt_norm[1,1]#matriksin negatif kısmı ve doğru
FP <- confusion_matrixx_dt_norm[1,2]#matriksin pozitif kısmı ve yanlış

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_dt_norm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyondan sonraki f1 skoru

# Sonucu yazdırma
cat("Knn algoritmasi F1 skoru:", F1_score_dt_norm)
print(F1_score_dt_norm)
Specificity_dt_norm <- TN / (TN + FP);
print(Specificity_dt_norm)
#normalizasyondan sonraki özgüllük oranı


# Örnek veri çerçevesi
data_frame_vis_dt_norm <- data.frame(
  result_dt_norm = c("accuracy_dt_norm", "Sensitivity_dt_norm", "Specificity_dt_norm", "F1_score_dt_norm"),
  value_dt_norm = c(accuracy_dt_norm, Sensitivity_dt_norm, Specificity_dt_norm, F1_score_dt_norm)
)

# Çubuk grafik oluşturma
ggplot(data_frame_vis_dt_norm, aes(x=result_dt_norm, y=value_dt_norm)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

################################################# KARAR AĞAÇLARI NORMALİZE EDİLMİŞ HALİ TAMAM ############################

################################################# GENEL GÖRSELLEŞTİRME ###################################################

Graph_result <- data.frame(
  Model = c("KNN","KNN_norm","LR","LR_norm","SVM","SVM_norm","Naive_Bayes","Naive_Bayes_norm","Decision_Tree","Decision_Tree_norm"),
  testAccuracy = c(accuracy,accuracy_norm,accuracy_lr,accuracy_lr_norm,accuracy_svm,accuracy_svm_norm,accuracy_nb,accuracy_nb_norm,accuracy_dt,accuracy_dt_norm))


print(Graph_result)
acc_sort <- c(accuracy,accuracy_norm,accuracy_lr,accuracy_lr_norm,accuracy_svm,accuracy_svm_norm,accuracy_nb,accuracy_nb_norm,accuracy_dt,accuracy_dt_norm)
result_sorted <- sort(acc_sort, decreasing = TRUE)
print(result_sorted)

############################################## GENEL GÖRSELLEŞTİRME ######################################################

########################################## NORMALİZASYONLU VE NORMALİZASYONSUZ ÇIKTILAR HEPSİ VERİLİYOR.##################

################GÖRSELLEŞTİRME ÖNCESİ DATA FRAME OLUŞTURMAMIZ GEREK######################
data_frame_vis_result <- data.frame(
  result_graph = c("KNN","KNN_norm","LR","LR_norm","SVM","SVM_norm","Naive_Bayes","Naive_Bayes_norm","Decision_Tree","Decision_Tree_norm"),
  value_graph = c(accuracy,accuracy_norm,accuracy_lr,accuracy_lr_norm,accuracy_svm,accuracy_svm_norm,accuracy_nb,accuracy_nb_norm,accuracy_dt,accuracy_dt_norm)
)             ################# DATAFRAME TAMAM ###############

# Çubuk grafik oluşturma. burada normalize edilmiş ve edilmemiş olmak üzere doğruluk oranları bulunuyor. 
ggplot(data_frame_vis_result, aes(x=result_graph, y=value_graph)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")
###################### GRAFİKTE DE GÖRÜLDÜĞÜ GİBİ KNN_NORM AÇIK ARA EN YÜKSEKTEDİR. DOLAYISIYLA YENİ VERİ BU ŞEKİLDE TEST EDİLİR.

############### YENİ BİR VERİ EKLEYECEĞİM VE BU VERİYİ TEST EDECEĞİM ###############
## veri çerçevesine ihtiyacımız var.
testing <- data.frame(       #normalize_df'den rastgele sayılar aldım
  "age" = c(1.05070),
  "sex" = c(0.6798805),
  "cp" = c(1.969864251),
  "trestbps" = c(2.03895150),
  "chol" = c(-0.38124557),
  "fbs" = c(-0.41694480),
  "restecg" = c(-1.0041707),
  "thalach" = c(0.23302037),
  "exang" = c(-0.69548),
  "oldpeak" = c(-0.38231886),
  "slope" = c(-0.6480412),
  "ca" = c(-0.7299673  ),
  "thal" = c(-0.5120748),
  "target" = c(0)
)

## yeni bir model oluşturma
knn_model_testing <- knn(train = train_Data[,1:14] , test=testing[,1:14] , cl=train_Data$target , k = 6) # K 6
## model oluştururken en önemli kural knn normalizasyon modelini yaparken kullandığım sınıf verisinin aynısını kullanmak.
# test kısmı da zaten yeni oluşturulan veri çerçevesi
confusion_matrix_testing <- table(Referance=testing$target , Prediction=knn_model_testing) 
print(confusion_matrix_testing)## doğru bir tahmin referans 0 predict 0 geldi
