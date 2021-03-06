#Hasar say�lar� i�in binom da��l�m�ndan yard�m ald�m.rasgele 6 tane say� �retmesi i�in komut girdim.
#Tekrar �al��t�rd���mda farkl� de�erler �retece�inden dolay� ilk �rettiklerini esas ald�m.
#�devimde ise g�zlem y�l�m� 7 olarak belirledim.

yil_1=rbinom(6,100,0.5)
yil_2=rbinom(6,100,0.5)
yil_3=rbinom(6,100,0.5)
yil_4=rbinom(6,100,0.5)
yil_5=rbinom(6,100,0.5)
yil_6=rbinom(6,100,0.5)
yil_7=rbinom(6,100,0.5)
sehir <- c(1:6)

#�retti�im hasar say�lar�n� y�llara aktard�m.


#Data frame yard�m�yla y�llar�m� hasar say�s� ad�nda olu�turdu�um de�i�kene tan�mlad�m. 

hasar_sayisi <- data.frame(sehir,yil_1,yil_2,yil_3,yil_4,yil_5,yil_6,yil_7)
hasar_sayisi

#�ehir s�tununda �retti�im 6 say�n�n ismini hasar g�zlemlenen illere g�re de�i�tirdim.
rownames(hasar_sayisi) <- rownames(hasar_sayisi, do.NULL = FALSE)
rownames(hasar_sayisi) <- c("Kocaeli","Ankara","�stanbul","Bursa","�zmir","Antalya")
hasar_sayisi

#Hasar tutarlar� i�in normal da��l�m kulland�m. 6 farkl� �ehir i�in 7 y�l g�zlemlenen hasar
#tutarlar� �rettim. h1= 1. y�l g�zlemlenen hasar tutar� olarak d���n�ld�.

h1= rnorm(6,mean=10000, sd=4000)
h2= rnorm(6,mean=10000, sd=4000)
h3= rnorm(6,mean=10000, sd=4000)
h4= rnorm(6,mean=10000, sd=4000)
h5= rnorm(6,mean=10000, sd=4000)
h6= rnorm(6,mean=10000, sd=4000)
h7= rnorm(6,mean=10000, sd=4000)

#Rasgele �retti�im de�erleri s�tunlar�ma tan�mlad�m.


#Data frame yard�m�yla hasarlar�m�, hasar tutar� ad�nda olu�turdu�um de�i�kene tan�mlad�m.
hasar_tutari <- data.frame(h1,h2,h3,h4,h5,h6,h7)
hasar_tutari

#Ayn� �ekilde il isimleri tan�mlad�m.

rownames(hasar_tutari) <- rownames(hasar_tutari, do.NULL = FALSE)
rownames(hasar_tutari) <- c("Kocaeli","Ankara","�stanbul","Bursa","�zmir","Antalya")
hasar_tutari

#Data frame yard�m�yla, olu�turmu� oldu�um iki matrisi "analiz" ad�nda birle�tirdim.
analiz = data.frame(hasar_sayisi,hasar_tutari)
analiz

#Analiz verimin sat�r ve s�tun sat�lar�n� g�rmek i�in "dim" komutu kulland�m.
dim(analiz)

#Temel istatistikleri yorumlamak i�in "basicStats" komutu kulland�m.
basicStats(analiz)

#Analiz verimi numeric yap�ya �evirmek i�in unlist() komutu kulland�m. 
analiz_yeni <- unlist(analiz)
analiz_yeni

#analiz_yeni de�i�kenimin numeric tipe d�n��t���n� teyit ettim.
is.numeric(analiz_yeni)

#Verimin yo�unlu�unu garfik yard�m�yla g�zlemledim.
m1=density (analiz_yeni)
plot(m1, col="purple")

#verimin histogram grafi�ini �izdirdim.
hist(analiz_yeni, breaks =10, col="green")
plot(analiz_yeni, type='l')   


#Buhlmann Kredibilite Analizini yapmak i�in "cm (credibility models)" kulland�m.
H <- cm(~sehir, analiz, yil = yil_1:h7)  
H

#�ehirlere g�re prim da��l�m�n� buldum.
summary(H)

H$means
H$weights
predict(H)

#BUHLMANN STRAUB MODEL

H1 <- cm(~sehir, analiz, yil = yil_1:yil_7, h = h1:h7, method = "iterative")
H1

H$means
H$weights

summary(H1)
predict(H1)

#BAYESC� KRED�B�L�TE

x <- c(44,49,44,43,51,48)
H2 <- cm("bayes", x, likelihood = "poisson",shape = 40, rate = 30)
H2
predict(H2)
summary(H2)