#Hasar sayýlarý için binom daðýlýmýndan yardým aldým.rasgele 6 tane sayý üretmesi için komut girdim.
#Tekrar çalýþtýrdýðýmda farklý deðerler üreteceðinden dolayý ilk ürettiklerini esas aldým.
#Ödevimde ise gözlem yýlýmý 7 olarak belirledim.

yil_1=rbinom(6,100,0.5)
yil_2=rbinom(6,100,0.5)
yil_3=rbinom(6,100,0.5)
yil_4=rbinom(6,100,0.5)
yil_5=rbinom(6,100,0.5)
yil_6=rbinom(6,100,0.5)
yil_7=rbinom(6,100,0.5)
sehir <- c(1:6)

#Ürettiðim hasar sayýlarýný yýllara aktardým.


#Data frame yardýmýyla yýllarýmý hasar sayýsý adýnda oluþturduðum deðiþkene tanýmladým. 

hasar_sayisi <- data.frame(sehir,yil_1,yil_2,yil_3,yil_4,yil_5,yil_6,yil_7)
hasar_sayisi

#Þehir sütununda ürettiðim 6 sayýnýn ismini hasar gözlemlenen illere göre deðiþtirdim.
rownames(hasar_sayisi) <- rownames(hasar_sayisi, do.NULL = FALSE)
rownames(hasar_sayisi) <- c("Kocaeli","Ankara","Ýstanbul","Bursa","Ýzmir","Antalya")
hasar_sayisi

#Hasar tutarlarý için normal daðýlým kullandým. 6 farklý þehir için 7 yýl gözlemlenen hasar
#tutarlarý ürettim. h1= 1. yýl gözlemlenen hasar tutarý olarak düþünüldü.

h1= rnorm(6,mean=10000, sd=4000)
h2= rnorm(6,mean=10000, sd=4000)
h3= rnorm(6,mean=10000, sd=4000)
h4= rnorm(6,mean=10000, sd=4000)
h5= rnorm(6,mean=10000, sd=4000)
h6= rnorm(6,mean=10000, sd=4000)
h7= rnorm(6,mean=10000, sd=4000)

#Rasgele ürettiðim deðerleri sütunlarýma tanýmladým.


#Data frame yardýmýyla hasarlarýmý, hasar tutarý adýnda oluþturduðum deðiþkene tanýmladým.
hasar_tutari <- data.frame(h1,h2,h3,h4,h5,h6,h7)
hasar_tutari

#Ayný þekilde il isimleri tanýmladým.

rownames(hasar_tutari) <- rownames(hasar_tutari, do.NULL = FALSE)
rownames(hasar_tutari) <- c("Kocaeli","Ankara","Ýstanbul","Bursa","Ýzmir","Antalya")
hasar_tutari

#Data frame yardýmýyla, oluþturmuþ olduðum iki matrisi "analiz" adýnda birleþtirdim.
analiz = data.frame(hasar_sayisi,hasar_tutari)
analiz

#Analiz verimin satýr ve sütun satýlarýný görmek için "dim" komutu kullandým.
dim(analiz)

#Temel istatistikleri yorumlamak için "basicStats" komutu kullandým.
basicStats(analiz)

#Analiz verimi numeric yapýya çevirmek için unlist() komutu kullandým. 
analiz_yeni <- unlist(analiz)
analiz_yeni

#analiz_yeni deðiþkenimin numeric tipe dönüþtüðünü teyit ettim.
is.numeric(analiz_yeni)

#Verimin yoðunluðunu garfik yardýmýyla gözlemledim.
m1=density (analiz_yeni)
plot(m1, col="purple")

#verimin histogram grafiðini çizdirdim.
hist(analiz_yeni, breaks =10, col="green")
plot(analiz_yeni, type='l')   


#Buhlmann Kredibilite Analizini yapmak için "cm (credibility models)" kullandým.
H <- cm(~sehir, analiz, yil = yil_1:h7)  
H

#þehirlere göre prim daðýlýmýný buldum.
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

#BAYESCÝ KREDÝBÝLÝTE

x <- c(44,49,44,43,51,48)
H2 <- cm("bayes", x, likelihood = "poisson",shape = 40, rate = 30)
H2
predict(H2)
summary(H2)