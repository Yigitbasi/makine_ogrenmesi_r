####################A??IKLAMA SATIRLARI EKLE VE GEREKS??ZLER?? S??L. SAKIN UNUTMA. DOSYANIN ADINI DA FORMATA UYGUN DE??????

#veri okutuldu.
#veri genelgecer bir sekilde analiz edildi.eksik veri analizi ve veri hakk??nda genel bilgiler edinildi.
# mesela summary gibi
#verileri gorsellestirildi.
#verileri detayli analiz edildi.
#aykiri veriler analiz edilip ayk??r?? olmayan maksimum de??ere e??itlendi.
#tekrar g??rselle??tirme yap??ld??.
#normalizasyon yap??ld??. normalizasyon yap??lmam???? hali de bulunuyor. iki t??rl?? de sonuclar g??sterilecektir.
#egitim ve test datasi olusturuldu. knn ve siniflandirma islemi
#model se??imi
#model e??itimi
#conf matriksle (table())gerekli hesaplamalar yap??ld??.
#veriler ve ????kt??lar g??rselle??tirildi



#DOSYA OKUNDU, EKS??K VER?? ANAL??Z?? VE BAS??T B??R GENEL ANAL??Z.
file_directory <- "C:\\Users\\Default\\Downloads\\kalp.csv"
data<-read.csv(file_directory)# dosyayi belirtilen yerde bulup okur.
data
library(dplyr)
glimpse(data)
data$target
colnum<-ncol(data)
print(colnum) # 14 adet s??tun var. 14 ayr?? veri de??erlendirilecek.
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



#VER??LER G??RSELLE??T??R??LD??.
boxplot(data$age,xlab = "Age",ylab = "Amount of people") #insanlarin yaslari histogram tablosunda gorsellestirilmistir. 40-70 yas araligi cok 
barplot(table(data$sex),xlab = "Women                                Men") # cinsiyet dagiliminda erkekler cogunlukta
boxplot(data$chol)
hist(data$age) # digerlerinden uzakta kalan bir veri ya da bolum yok. 
barplot(table(data$sex))#kategorik
#hist(data$cp,xlab="Type of the chest pain",ylab="Age", main = "Situation between chest pain and age",breaks = seq(min(data$cp), max(data$cp) + 1, by = 1))
barplot(table(data$cp))
boxplot(data$trestbps)#histogramda 200 digerlerinden ayri gozukmekte. aykiri veri olarak alinabilir.
hist(data$chol)# 600 ve di??erleri aykiri veri
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
#KORELASYON GRAF??????.
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

head(data)#hen??z sat??rlar kar????t??r??lmad??. gelecekte farkl?? bir isimde bunu sa??layaca????m

#aykiri verileri burada analiz ettim. aykiri verilerden sonra normalizasyon yapmal??y??m.

#chol-trestbps-thalach-oldpeak bunlar aykiri veri

print(max(data$chol))#aykiri verilerden once
length((data$chol))
data$chol

#chol art??k aykiri verilere sahip degil. boxplotta kutunun icinde kalan maksimum degerler atanmistir.
max_chol<-data$chol[!data$chol %in% boxplot.stats(data$chol)$out]
data$chol[data$chol %in% boxplot.stats(data$chol)$out] <- max(max_chol)
#outlier_chol<- data$chol[data$chol %in% boxplot.stats(data$chol)$out]<-360
print(max(data$chol)) # aykiri veri alinmadi

length(data$chol)
#aykiri verileri yazdiriyor. bu sekilde tum aykiri verileri bul. kutu disindakileri veriyor.
print(data$chol)

#chol art??k aykiri verilere sahip degil. boxplotta kutunun icinde kalan maksimum degerler atanmistir.

print(max(data$trestbps))#aykiri verilerle 
length((data$trestbps))
data$trestbps

#trestbpsde de aykiri veri yok

max_trest<-data$trestbps[!data$trestbps %in% boxplot.stats(data$trestbps)$out]#iceride kalanlar aras??nda maksimum deger bulundu
max(max_trest)#170

data$trestbps[data$trestbps %in% boxplot.stats(data$trestbps)$out] <- max(max_trest)# hata verirse max(max_trest) yerine 170 yaz. 170'i aykirilarin yerine yazdik
max(data$trestbps)# aykiri degerler gitti ve maks 170 oldu
length(data$trestbps) # herhangi bir veri kayb?? yok. 303 veri
data$trestbps
#trestbpsde de aykiri veri yok

print(min(data$thalach))#aykiri verilerle. min yazma sebebim aykiri verilerin burada kucuk olmasi. (71-88)
length((data$thalach))
data$thalach

#thalachta da aykiri veri yok.
min_thalach<-data$thalach[!data$thalach %in% boxplot.stats(data$thalach)$out]#iceride kalanlar aras??nda maksimum deger bulundu
print(min(data$thalach))

data$thalach[data$thalach %in% boxplot.stats(data$thalach)$out] <- min(min_thalach)
print(min(data$thalach))
length(data$thalach)
#thalachta da aykiri veri yok.

print(max(data$oldpeak))#aykiri verilerden once
length((data$oldpeak))
data$oldpeak

#oldpeakte de aykiri veri kalmamistir.
max_oldpeak<-data$oldpeak[!data$oldpeak %in% boxplot.stats(data$oldpeak)$out]#iceride kalanlar aras??nda maksimum deger bulundu
max(max_oldpeak)

data$oldpeak[data$oldpeak %in% boxplot.stats(data$oldpeak)$out] <- max(max_oldpeak)
max(data$oldpeak)# aykiri degerler gitti ve maks 170 oldu
length(data$oldpeak) # herhangi bir veri kayb?? yok. 303 veri
data$oldpeak
#oldpeakte de aykiri veri kalmamistir

#data$ca ayk??r?? veri analizinden ??nce max deger falan yazdir.
data$ca
max(data$ca)
length(data$ca)
#data$ca 4 degerleri 3 olmustur. Dok??manda bu ??ekilde belirtilmi?? (0-3). 
#bu y??zden 4'?? ve di??er say??lar??(olsayd??) ayk??r?? veri olarak ele ald??m.
max_ca <- data$ca[data$ca %in% c(0,1,2,3)]
print(max(max_ca))
data$ca[!(data$ca %in% c(0,1,2,3))] <- max(max_ca)

data$ca
barplot(table(data$ca))
#data$ca 4 degerleri 3 olmustur. Dok??manda bu ??ekilde belirtilmi?? (0-3). bu y??zden ayk??r?? veri olarak ele ald??m.
#de??erler aras??nda birbirine yak??nl??k olan(ya da kategorik) s??tunlar??n ayk??r?? verilerini ay??klamad??m  etmedim.

#s??n??fland??rma, makine ????renmesi ve lineer regresyon icin avantaj sa??layan sat??rlar?? rastgele dizmek
set.seed(123)
data_new<-data
data_new<-data_new[sample(1:nrow(data_new)),]  #onceki dataya gore burada satirlar rastgele dagilimla siralanacaktir.
head(data_new)# satirlar rastgele
head(data)#satirlar ilk haldekiyle ayni

#     NORMAL??ZASYON
normalize_data<-scale(data_new[,1:13])## hedef de??i??ken normalize edilmeyecektir.
normalize_data<-data.frame(normalize_data)
#normalize_df <- data.frame(normalize_data , target = data_new$target)
normalize_df<-data.frame(normalize_data, target = data_new$target)
head(normalize_df)

#    G??RSELLE??T??RME
boxplot(normalize_df$age,xlab = "Age",ylab = "Amount of people") #insanlarin yaslari histogram tablosunda gorsellestirilmistir. 40-70 yas araligi cok 
boxplot(normalize_df$chol)# g??r??nd?????? gibi grafik normalize edilmi??tir. ayr??ca ayk??r?? veriye rastlanmamaktad??r.
hist(normalize_df$age) # digerlerinden uzakta kalan bir veri ya da bolum yok. ancak miktari az olan bolumler aykiri olabilir.
#hist(normalize_df$cp,xlab="Type of the chest pain",ylab="Age", main = "Situation between chest pain and age",breaks = seq(min(data$cp), max(data$cp) + 1, by = 1))
hist(normalize_df$chol)
boxplot(normalize_df$thalach)
boxplot(normalize_df$oldpeak)#6 kismi aykiri 
boxplot(normalize_df$trestbps)#normalizasyondan sonra da ayk??r?? veri g??r??nd??.

#AYKIRILIK ??NCES?? ANAL??Z
print(max(normalize_df$trestbps))#aykiri verilerle 
length((normalize_df$trestbps))
normalize_df$trestbps

#trestbps normalize_df k??sm??ndaki ayk??r??l?????? d??zelt
max_trest_norm<-normalize_df$trestbps[!normalize_df$trestbps %in% boxplot.stats(normalize_df$trestbps)$out]#iceride kalanlar aras??nda maksimum deger bulundu
max(max_trest_norm)#2.03

normalize_df$trestbps[normalize_df$trestbps %in% boxplot.stats(normalize_df$trestbps)$out] <- max(max_trest_norm)# hata verirse max(max_trest) yerine 2,03 yaz. 2,03'?? aykirilarin yerine yazdik
max(normalize_df$trestbps)# aykiri degerler gitti ve maks 2,03 oldu
length(normalize_df$trestbps) # herhangi bir veri kayb?? yok. 303 veri
normalize_df$trestbps
boxplot(normalize_df$trestbps)
#trestbps norm ayk??r??l??k yok.

## Veriler art??k uygun

#feature extraction

library(stats)#prcomp i??in. 
pca_value<-prcomp(data_new, scale=TRUE)#pca analizi i??in normalizasyon
summary(pca_value)
sapply(data_new, class) #sapply ??nemli. veri yap??s??na ait her elemana uygulanmak istenen i??lem i??in.
plot(pca_value)#pca_value g??rselle??tirildi
plot(pca_value$x[,1], pca_value$x[,2], xlab="X", ylab="Y")
plot(pca_value$x[,3], pca_value$x[,4], xlab="X", ylab="Y")## baz?? yerlerde noktalar fazla birikmi??. uzak d????enler i??leme al??nmaz
plot(pca_value$x[,5], pca_value$x[,6], xlab="X", ylab="Y")## dolay??s??yla verilerin boyutu azal??r. 



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



#s??n??fland??rma, makine ????renmesi ve lineer regresyon icin avantaj sa??layan sat??rlar?? rastgele dizmek
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


# 5 tane s??n??fland??rma algoritmas??n?? da kullanmam laz??m. hepsi farkl?? olmal??
#knn
knn_modell <- knn(train = train_Dataa[,1:14] , test=test_Dataa[, 1:14] , cl=train_Dataa$target , k = 10)
confusion_matrixx <- table(Referance=test_Dataa$target , Prediction=knn_modell) #table hata payi methodu
print(confusion_matrixx)
## normalizasyon olmam???? hali #############################################################
accuracy <- sum(diag(confusion_matrixx)) / sum(confusion_matrixx)
cat("Knn algoritmasi dogrulugu : " , accuracy)#normalizasyon olmadan gelen dogruluk orani KNN
# True Positives (TP)
TP <- confusion_matrixx[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrixx[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity = TP / (TP + FN)
cat("Knn algoritmasi hassasiyeti : " , Sensitivity)#normalizasyondan ??nceki hassasiyet oran??

TN <- confusion_matrixx[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrixx[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon olmadan f1 skoru

# Sonucu yazd??rma
cat("Knn algoritmasi F1 skoru:", F1_score)
print(F1_score)
Specificity <- TN / (TN + FP);
print(Specificity)
#normalizasyon olmam???? ??zg??ll??k oran??


# ??rnek veri ??er??evesi g??rselle??tirmeden ??nce veri ??er??evesi veya data frameye d??n????t??r??lmelidir.
data_frame_viss <- data.frame(
  result = c("accuracy", "Sensitivity", "Specificity", "F1_score"),
  value = c(accuracy, Sensitivity, Specificity, F1_score)
)
# ??ubuk grafik olu??turma. bu ??ekilde analizler yap??labilir.
ggplot(data_frame_viss, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")
##normalizasyon olmadan hata matrisi g??rselle??tirmesi yap??ld??.###############################################




#################### NORMAL??ZASYONLU KNN

div_knn_norm <- createDataPartition(normalize_df$target , p =0.7 , list = FALSE)
train_Data <- normalize_df[div_knn_norm, ] #yuzde 70 i train edilecek
test_Data <-  normalize_df[-div_knn_norm, ] # yuzde 30 test datasi olacak


#knn 
#6-0.94
knn_model <- knn(train = train_Data[,1:14] , test=test_Data[,1:14] , cl=train_Data$target , k = 6) # K 6
confusion_matrix <- table(Referance=test_Data$target , Prediction=knn_model) #table hata payi methodu.
#buradaki ????kt??larla do??ruluk,hassasiyet gibi kavramlar?? sonu??land??raca????z.
##confusionMatrix() metodu ile daha kolay bir ??ekilde bu de??erler al??nabilir. ancak ben uzun yolda yapt??m.
print(confusion_matrix)


accuracy_norm <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Knn algoritmasi dogrulugu : " , accuracy_norm)#normalizasyondan sonraki do??ruluk oran??


# True Positives (TP)
TP <- confusion_matrix[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrix[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity = TP / (TP + FN)
cat("Knn algoritmasi hassasiyeti : " , Sensitivity)#normalizasyondan sonraki hassasiyet oran??

TN <- confusion_matrix[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrix[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyondan sonraki f1 skoru

# Sonucu yazd??rma
cat("Knn algoritmasi F1 skoru:", F1_score)
print(F1_score)
Specificity <- TN / (TN + FP);
print(Specificity)
#normalizasyondan sonraki ??zg??ll??k oran??


# ??rnek veri ??er??evesi
data_frame_vis <- data.frame(
  result = c("accuracy_norm", "Sensitivity", "Specificity", "F1_score"),
  value = c(accuracy_norm, Sensitivity, Specificity, F1_score)
)

# ??ubuk grafik olu??turma
ggplot(data_frame_vis, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

##SVM,DEC??S??ONTREE,LOJ??ST??K REGRESYON, Naive Bayes,randomforest bunlar aras??ndan se??im yap??lacak ve devam edilecek
##############################################################################################################################################################

####################### Lineer regresyon normalize edilmeden.

div_lr <- createDataPartition(data_new$target , p =0.7 , list = FALSE)#sat??rlar rastgele
train_Dataa_lr <- data_new[div_lr, ] #yuzde 70 i train edilecek
test_Dataa_lr <-  data_new[-div_lr, ] # yuzde 30 test datasi olacak
prop.table(table(train_Dataa_lr$target))##y??zdelik oranlar verilir 1-0

lr_pred <- glm(target ~ .,              #tahmin
               data = train_Dataa_lr,
               family = "binomial")

lr_model <- step(object = lr_pred,      #model e??itimi
                 direction = "backward",
                 trace = F)


test_Dataa_lr$pred_lr <- predict(lr_model, #tahmin edildikten sonra tahminin tutulaca???? yer
                                 test_Dataa_lr,
                                 type = "response")
test_Dataa_lr$pred_label_lr <- ifelse(test_Dataa_lr$pred_lr >= 0.5, yes = 1, no = 0) #pred_lr de??erine g??re yeni bir de??i??kenle
#1 veya 0 de??erine yuvarlama. pred_label_lr kullan??lacak bundan sonra

test_Dataa_lr %>% ##i??lem zinciri i??in %>%
  select(target, 
         pred_lr, 
         pred_label_lr) %>% 
  rmarkdown::paged_table() #b??y??k verileri a??a???? kaymadan g??stermek i??in kulland??m. ancak ??ok da gerekli de??ildi.
confusion_matrixx_lr<-table(Referance= test_Dataa_lr$target,Prediction=test_Dataa_lr$pred_label_lr)# referans k??sm?? target prediction k??sm?? tahminimiz
print(confusion_matrixx_lr)

accuracy_lr <- sum(diag(confusion_matrixx_lr)) / sum(confusion_matrixx_lr)
cat("lineer regresyon algoritmasi dogrulugu : " , accuracy_lr)#normalizasyondan ??nce do??ruluk oran??

# True Positives (TP)
TP <- confusion_matrixx_lr[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrixx_lr[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity_lr = TP / (TP + FN)
cat("lineer regresyon algoritmasi hassasiyeti : " , Sensitivity_lr)#normalizasyondan ??nceki hassasiyet oran??

TN <- confusion_matrixx_lr[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrixx_lr[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_lr <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon olmadan f1 skoru

# Sonucu yazd??rma
cat("lineer regresyon algoritmasi F1 skoru:", F1_score_lr)
print(F1_score_lr)
Specificity_lr <- TN / (TN + FP);
print(Specificity_lr)
#normalizasyon olmam???? ??zg??ll??k oran??


# ??rnek veri ??er??evesi
data_frame_viss_lr <- data.frame(
  result = c("accuracy_lr", "Sensitivity_lr", "Specificity_lr", "F1_score_lr"),
  value = c(accuracy_lr, Sensitivity_lr, Specificity_lr, F1_score_lr)
)
# ??ubuk grafik olu??turma
ggplot(data_frame_viss_lr, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")################
##normalizasyon olmadan hata matrisi g??rselle??tirmesi yap??ld??.

################## NORMAL??ZASYONSUZ L??NEER REGRESYON TAMAMLANDI.

################# NORMAL??ZASYONLU L??NEER REGRESYON
div_lr_norm <- createDataPartition(normalize_df$target , p =0.7 , list = FALSE)#sat??rlar rastgele
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
test_Dataa_lr_norm$pred_label_lr_norm <- ifelse(test_Dataa_lr_norm$pred_lr >= 0.65, yes = 1, no = 0)##Do??ruluk oran?? y??kseldi??i i??in
#0.65 kulland??m. 0.5'e g??re daha isabetli sonu?? veriyor.

test_Dataa_lr_norm %>% 
  select(target, 
         pred_lr, 
         pred_label_lr_norm) %>% # yukar??da bahsetmi??tim
  rmarkdown::paged_table()

confusion_matrixx_lr_norm<-table(Referance= test_Dataa_lr_norm$target,Prediction=test_Dataa_lr_norm$pred_label_lr_norm)
print(confusion_matrixx_lr_norm)

accuracy_lr_norm <- sum(diag(confusion_matrixx_lr_norm)) / sum(confusion_matrixx_lr_norm)
cat("lineer regresyon algoritmasi dogrulugu : " , accuracy_lr_norm)#normalizasyonlu do??ruluk oran??

# True Positives (TP)
TP <- confusion_matrixx_lr_norm[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrixx_lr_norm[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity_lr_norm = TP / (TP + FN)
cat("lineer regresyon algoritmasi hassasiyeti : " , Sensitivity_lr_norm)#normalizasyonlu hassasiyet oran??

TN <- confusion_matrixx_lr_norm[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrixx_lr_norm[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_lr_norm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyonlu f1 skoru

# Sonucu yazd??rma
cat("lineer regresyon algoritmasi F1 skoru:", F1_score_lr_norm)
print(F1_score_lr_norm)
Specificity_lr_norm <- TN / (TN + FP);
print(Specificity_lr_norm)
#normalizasyonlu ??zg??ll??k oran??


data_frame_viss_lr_norm <- data.frame( #dataframe olu??turma g??rselle??tirmek i??in
  result = c("accuracy_lr_norm", "Sensitivity_lr_norm", "Specificity_lr_norm", "F1_score_lr_norm"),
  value = c(accuracy_lr_norm, Sensitivity_lr_norm, Specificity_lr_norm, F1_score_lr_norm)
)
# ??ubuk grafik olu??turma
ggplot(data_frame_viss_lr_norm, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")
##################normalizasyon olmadan hata matrisi g??rselle??tirmesi yap??ld??. ANCAK NORMAL??ZASYONDAN SONRA DE??ERLER AZALDI. BU SORUNU ????Z.


################  SVM NORMAL??ZASYONSUZ ##########################
#e??itim test k??sm??
div_svm <- sample(1:nrow(data_new), 0.7 * nrow(data_new))
train_Dataa_svm <- data_new[div_svm, ]
test_Dataa_svm <- data_new[-div_svm, ]
library(e1071)
"~"
svm_modell  <- svm(target ~ . ,data=train_Dataa_svm,kernel="radial" ) 
#svmyi esnek k??lar kernel="radial". 

summary(svm_modell)

pred_svm <- predict(svm_modell , newdata = train_Dataa_svm)

svm_pred <- glm(target ~ .,
                data = train_Dataa_svm,
                family = "binomial") #0 ya da 1

svm_modell <- step(object = svm_pred,
                   direction = "backward",
                   trace = F)
## yukar??dakilerin ayn??s??
test_Dataa_svm$pred_svm <- predict(svm_modell,
                                   test_Dataa_svm,
                                   type = "response")
test_Dataa_svm$pred_label_svm <- ifelse(test_Dataa_svm$pred_svm >= 0.5, yes = 1, no = 0)# pred_svm de??erlerini 0 ya da 1'e 
#d??n????t??r??p ona g??re target ile k??yaslanacak.

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
TP <- confusion_matrixx_svm[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrixx_svm[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity_svm = TP / (TP + FN)
cat("lineer regresyon algoritmasi hassasiyeti : " , Sensitivity_svm)#normalizasyondan ??nceki hassasiyet oran??

TN <- confusion_matrixx_svm[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrixx_svm[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_svm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon olmadan f1 skoru

# Sonucu yazd??rma
cat("lineer regresyon algoritmasi F1 skoru:", F1_score_svm)
print(F1_score_svm)
Specificity_svm <- TN / (TN + FP);
print(Specificity_svm)
#normalizasyon olmam???? ??zg??ll??k oran??


# ??rnek veri ??er??evesi
data_frame_viss_svm <- data.frame(
  result = c("accuracy_svm", "Sensitivity_svm", "Specificity_svm", "F1_score_svm"),
  value = c(accuracy_svm, Sensitivity_svm, Specificity_svm, F1_score_svm)
)
# ??ubuk grafik olu??turma
ggplot(data_frame_viss_svm, aes(x=result, y=value)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")################
##normalizasyon olmadan hata matrisi g??rselle??tirmesi yap??ld??.
###################SVM NORMAL??ZASYONSUZ TAMAMLANDI.#######################


###########SVM NORMAL??ZASYONLU ##########################
#yukar??daki kodlarla a??a???? yukar?? ayn?? oldu??u i??in ayn?? yorum sat??rlar??n?? eklemek istemedim. sadece veriler normalize
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
TP <- confusion_matrixx_svm_norm[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrixx_svm_norm[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity_svm_norm = TP / (TP + FN)
cat("lineer regresyon algoritmasi hassasiyeti : " , Sensitivity_svm_norm)#normalizasyondan ??nceki hassasiyet oran??

TN <- confusion_matrixx_svm_norm[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrixx_svm_norm[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_svm_norm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon olmadan f1 skoru

# Sonucu yazd??rma
cat("svm norm algoritmasi F1 skoru:", F1_score_svm_norm)
print(F1_score_svm_norm)
Specificity_svm_norm <- TN / (TN + FP);
print(Specificity_svm_norm)
#normalizasyon olmam???? ??zg??ll??k oran??


# ??rnek veri ??er??evesi
data_frame_viss_svm_norm <- data.frame(
  result_norm = c("accuracy_svm_norm", "Sensitivity_svm_norm", "Specificity_svm_norm", "F1_score_svm_norm"),
  value_norm = c(accuracy_svm_norm, Sensitivity_svm_norm, Specificity_svm_norm, F1_score_svm_norm)
)
# ??ubuk grafik olu??turma
ggplot(data_frame_viss_svm_norm, aes(x=result_norm, y=value_norm)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

##################      NORMAL??ZASYONLU SVM DE B??TT??


###################### NORMAL??ZASYONSUZ BAYES B??TT??
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
cat("naive bayes algoritmasi dogrulugu : " , accuracy_nb)#normalizasyonsuz do??ruluk oran??


# True Positives (TP)
TP <- confusion_matrixx_nb[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrixx_nb[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity_nb = TP / (TP + FN)
cat("naive bayes algoritmasi hassasiyeti : " , Sensitivity_nb)#normalizasyonsuz hassasiyet oran??

TN <- confusion_matrixx_nb[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrixx_nb[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_nb <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyonsuz f1 skoru

# Sonucu yazd??rma
cat("naive bayes algoritmasi F1 skoru:", F1_score_nb)
print(F1_score_nb)
Specificity_nb <- TN / (TN + FP);
print(Specificity_nb)
#normalizasyondan sonraki ??zg??ll??k oran??


# ??rnek veri ??er??evesi
data_frame_vis_nb <- data.frame(
  result_nb = c("accuracy_nb", "Sensitivity_nb", "Specificity_nb", "F1_score_nb"),
  value_nb = c(accuracy_nb, Sensitivity_nb, Specificity_nb, F1_score_nb)
)

# ??ubuk grafik olu??turma
ggplot(data_frame_vis_nb, aes(x=result_nb, y=value_nb)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

################## NORMAL??ZASYONSUZ BAYES B??TT?? ##############################

##################  NORMAL??ZASYONLU BAYES ####################################

div_naive_norm <- sample(1:nrow(normalize_df), 0.7 * nrow(normalize_df))
train_Dataa_naive_norm <- normalize_df[div_naive_norm, ]
test_Dataa_naive_norm <- normalize_df[-div_naive_norm, ]
#train_Dataa_naive$target <- as.factor(train_Dataa_naive$target)

naiveBayesModel_norm <- naiveBayes(target ~ ., data=train_Dataa_naive_norm)
naiveBayesTestPred_norm <- predict(naiveBayesModel_norm, test_Dataa_naive_norm)
confusion_matrixx_nb_norm<-table(test_Dataa_naive_norm$target,naiveBayesTestPred_norm)
print(confusion_matrixx_nb_norm)



accuracy_nb_norm <- sum(diag(confusion_matrixx_nb_norm)) / sum(confusion_matrixx_nb_norm)
cat("naive bayes algoritmasi dogrulugu : " , accuracy_nb_norm)#normalizasyondan sonraki do??ruluk oran??


# True Positives (TP)
TP <- confusion_matrixx_nb_norm[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrixx_nb_norm[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity_nb_norm = TP / (TP + FN)
cat("naive bayes algoritmasi hassasiyeti : " , Sensitivity_nb_norm)#normalizasyondan sonraki hassasiyet oran??

TN <- confusion_matrixx_nb_norm[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrixx_nb_norm[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_nb_norm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyondan sonraki f1 skoru

# Sonucu yazd??rma
cat("naive bayes algoritmasi F1 skoru:", F1_score_nb_norm)
print(F1_score_nb_norm)
Specificity_nb_norm <- TN / (TN + FP);
print(Specificity_nb_norm)
#normalizasyondan sonraki ??zg??ll??k oran??


# ??rnek veri ??er??evesi
data_frame_vis_nb_norm <- data.frame(
  result_nb_norm = c("accuracy_nb_norm", "Sensitivity_nb_norm", "Specificity_nb_norm", "F1_score_nb_norm"),
  value_nb_norm = c(accuracy_nb_norm, Sensitivity_nb_norm, Specificity_nb_norm, F1_score_nb_norm)
)

# ??ubuk grafik olu??turma
ggplot(data_frame_vis_nb_norm, aes(x=result_nb_norm, y=value_nb_norm)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")
######### normalizasyonlu naive bayes de bitti################################

############# DEC??S??ON TREE NORMAL??ZASYONSUZ ##############################

library(rpart)

div_dt <- sample(1:nrow(data_new), 0.7 * nrow(data_new))
train_Dataa_dt <- data_new[div_dt, ]
test_Dataa_dt <- data_new[-div_dt, ]

decisionTreeModel <- rpart(target ~ ., data=train_Dataa_dt, method="class")# method eklenmesi gerek dec treede
decisionTreeTestPred <- predict(decisionTreeModel, test_Dataa_dt, type = "class")

confusion_matrixx_dt<- table(test_Dataa_dt$target,decisionTreeTestPred)
print(confusion_matrixx_dt)

accuracy_dt <- sum(diag(confusion_matrixx_dt)) / sum(confusion_matrixx_dt)
cat("decision tree algoritmasi dogrulugu : " , accuracy_dt)#normalizasyon ??ncesi do??ruluk oran??


# True Positives (TP)
TP <- confusion_matrixx_dt[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrixx_dt[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity_dt = TP / (TP + FN)
cat("Decision Tree algoritmasi hassasiyeti : " , Sensitivity_dt)#normalizasyon ??ncesi hassasiyet oran??

TN <- confusion_matrixx_dt[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrixx_dt[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_dt <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyon ??ncesi f1 skoru

# Sonucu yazd??rma
cat("Decision Tree algoritmasi F1 skoru:", F1_score_dt)
print(F1_score_dt)
Specificity_dt <- TN / (TN + FP);
print(Specificity_dt)
#normalizasyon ??ncesi ??zg??ll??k oran??


# ??rnek veri ??er??evesi
data_frame_vis_dt <- data.frame(
  result_dt = c("accuracy_dt", "Sensitivity_dt", "Specificity_dt", "F1_score_dt"),
  value_dt = c(accuracy_dt, Sensitivity_dt, Specificity_dt, F1_score_dt)
)

# ??ubuk grafik olu??turma
ggplot(data_frame_vis_dt, aes(x=result_dt, y=value_dt)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

################################################### NORMAL??ZASYONSUZ KARAR A??A??LARI TAMAMLANDI#############################


################################################## NORMAL??ZASYONLU KARAR A??A??LARI##########################################


div_dt_norm <- sample(1:nrow(normalize_df), 0.7 * nrow(normalize_df))
train_Dataa_dt_norm <- normalize_df[div_dt_norm, ]
test_Dataa_dt_norm <- normalize_df[-div_dt_norm, ]

decisionTreeModel_norm <- rpart(target ~ ., data=train_Dataa_dt_norm, method="class")
decisionTreeTestPred_norm <- predict(decisionTreeModel_norm, test_Dataa_dt_norm, type = "class")

confusion_matrixx_dt_norm<- table(test_Dataa_dt_norm$target,decisionTreeTestPred_norm)
print(confusion_matrixx_dt_norm)

accuracy_dt_norm <- sum(diag(confusion_matrixx_dt_norm)) / sum(confusion_matrixx_dt_norm)
cat("Knn algoritmasi dogrulugu : " , accuracy_dt_norm)#normalizasyondan sonraki do??ruluk oran??


# True Positives (TP)
TP <- confusion_matrixx_dt_norm[2,2]#matriksin pozitif k??sm?? ve do??ru

# False Negatives (FN)
FN <- confusion_matrixx_dt_norm[2,1]#matriksin negatif k??sm?? ve yanl????
Sensitivity_dt_norm = TP / (TP + FN)
cat("Knn algoritmasi hassasiyeti : " , Sensitivity_dt_norm)#normalizasyondan sonraki hassasiyet oran??

TN <- confusion_matrixx_dt_norm[1,1]#matriksin negatif k??sm?? ve do??ru
FP <- confusion_matrixx_dt_norm[1,2]#matriksin pozitif k??sm?? ve yanl????

Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN)

# f1 skoru
F1_score_dt_norm <- 2 * (Precision * Recall) / (Precision + Recall)#normalizasyondan sonraki f1 skoru

# Sonucu yazd??rma
cat("Knn algoritmasi F1 skoru:", F1_score_dt_norm)
print(F1_score_dt_norm)
Specificity_dt_norm <- TN / (TN + FP);
print(Specificity_dt_norm)
#normalizasyondan sonraki ??zg??ll??k oran??


# ??rnek veri ??er??evesi
data_frame_vis_dt_norm <- data.frame(
  result_dt_norm = c("accuracy_dt_norm", "Sensitivity_dt_norm", "Specificity_dt_norm", "F1_score_dt_norm"),
  value_dt_norm = c(accuracy_dt_norm, Sensitivity_dt_norm, Specificity_dt_norm, F1_score_dt_norm)
)

# ??ubuk grafik olu??turma
ggplot(data_frame_vis_dt_norm, aes(x=result_dt_norm, y=value_dt_norm)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")

################################################# KARAR A??A??LARI NORMAL??ZE ED??LM???? HAL?? TAMAM ############################

################################################# GENEL G??RSELLE??T??RME ###################################################

Graph_result <- data.frame(
  Model = c("KNN","KNN_norm","LR","LR_norm","SVM","SVM_norm","Naive_Bayes","Naive_Bayes_norm","Decision_Tree","Decision_Tree_norm"),
  testAccuracy = c(accuracy,accuracy_norm,accuracy_lr,accuracy_lr_norm,accuracy_svm,accuracy_svm_norm,accuracy_nb,accuracy_nb_norm,accuracy_dt,accuracy_dt_norm))


print(Graph_result)
acc_sort <- c(accuracy,accuracy_norm,accuracy_lr,accuracy_lr_norm,accuracy_svm,accuracy_svm_norm,accuracy_nb,accuracy_nb_norm,accuracy_dt,accuracy_dt_norm)
result_sorted <- sort(acc_sort, decreasing = TRUE)
print(result_sorted)

############################################## GENEL G??RSELLE??T??RME ######################################################

########################################## NORMAL??ZASYONLU VE NORMAL??ZASYONSUZ ??IKTILAR HEPS?? VER??L??YOR.##################

################G??RSELLE??T??RME ??NCES?? DATA FRAME OLU??TURMAMIZ GEREK######################
data_frame_vis_result <- data.frame(
  result_graph = c("KNN","KNN_norm","LR","LR_norm","SVM","SVM_norm","Naive_Bayes","Naive_Bayes_norm","Decision_Tree","Decision_Tree_norm"),
  value_graph = c(accuracy,accuracy_norm,accuracy_lr,accuracy_lr_norm,accuracy_svm,accuracy_svm_norm,accuracy_nb,accuracy_nb_norm,accuracy_dt,accuracy_dt_norm)
)             ################# DATAFRAME TAMAM ###############

# ??ubuk grafik olu??turma. burada normalize edilmi?? ve edilmemi?? olmak ??zere do??ruluk oranlar?? bulunuyor. 
ggplot(data_frame_vis_result, aes(x=result_graph, y=value_graph)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="performans sonuclari", x="sonuc tipleri", y="deger")
###################### GRAF??KTE DE G??R??LD?????? G??B?? KNN_NORM A??IK ARA EN Y??KSEKTED??R. DOLAYISIYLA YEN?? VER?? BU ??EK??LDE TEST ED??L??R.

############### YEN?? B??R VER?? EKLEYECE????M VE BU VER??Y?? TEST EDECE????M ###############
## veri ??er??evesine ihtiyac??m??z var.
testing <- data.frame(       #normalize_df'den rastgele say??lar ald??m
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

## yeni bir model olu??turma
knn_model_testing <- knn(train = train_Data[,1:14] , test=testing[,1:14] , cl=train_Data$target , k = 6) # K 6
## model olu??tururken en ??nemli kural knn normalizasyon modelini yaparken kulland??????m s??n??f verisinin ayn??s??n?? kullanmak.
# test k??sm?? da zaten yeni olu??turulan veri ??er??evesi
confusion_matrix_testing <- table(Referance=testing$target , Prediction=knn_model_testing) 
print(confusion_matrix_testing)## do??ru bir tahmin referans 0 predict 0 geldi
