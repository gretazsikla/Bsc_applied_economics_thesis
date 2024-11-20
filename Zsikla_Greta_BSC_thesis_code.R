setwd("G:/egyetem/6.félév/projekt3")

#szükséges package-ek
library(readxl)
#install.packages("forecast")
library(forecast)
library(zoo)
library(ggplot2)
library(urca)
library(aTSA)
library(lmtest)
library(aTSA)
library(urca)
library(vars)
library(tsDyn)
library(frequencyConnectedness)
library(stargazer)
library(strucchange)


#adatok beolvasása
adatok<- read_excel("bázisolt_adatok.xlsx", sheet="végleges")
str(adatok) #ido2 posix formában van --> legyen dátum
adatok$ido2<- as.Date(adatok$ido2)
str(adatok) #minden adat jó formátumban van
summary(adatok)

#adatok ismertetése
#2008 januárjától 2023 novemberéig vannak adatok havi rendszerességggel
#-->összesen 191 adat
#az adatok 2005 évi bázison vannak

#ipari termelői árindex
#mezőgazdasági termeéői árindex

#M1: monetáris intézményeken kívüli készpénz + látra szóló betétek.

#árfolyam: ugyanaz az elérés mint a M1-nél
#időszaki átlagos devizaárfolyam

#-------------------------------adatok szemrevételezése----------------------------------
ggplot(adatok, aes(x=ido2, y=M1_bázis))+geom_line(size=1)+
  labs(title="M1 aggregátum idősora 2005-ös bázis évvel", x="Dátum", y="M1 értékei")+
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.ticks = element_line(size = 1.5))+
  scale_x_date(date_breaks = "24 month", date_labels = "%Y-%m")
#exponenciális trend, muétiplikatív modell

ggplot(adatok, aes(x=ido2, y=árfolyam_bázis))+geom_line(size=1)+
  labs(title="EUR/HUF árfolyam 2005-ös bázis évvel", x="Dátum", y="EUR/HUF árfolyam")+
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.ticks = element_line(size = 1.5))+
  scale_x_date(date_breaks = "24 month", date_labels = "%Y-%m")
#növekvő trend, nehezen meghatározható alak

ggplot(adatok, aes(x=ido2, y=árfolyam_érték))+geom_line(size=1)+
  labs(title="EUR/HUF árfolyam", x="Dátum", y="EUR/HUF árfolyam")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")
#ha van bázis év akkor jobban kijön a szezonalitás

ggplot(adatok, aes(x=ido2, y=ipari))+geom_line(size=1)+
  labs(title="Ipari termelői árindex 2005-ös bázis évvel", x="Dátum", y="Árindex")+
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.ticks = element_line(size = 1.5))+
  scale_x_date(date_breaks = "24 month", date_labels = "%Y-%m")
#2019-20 környékén elkezdődik egy exponenciális emelkedés
#előtte enyhén emelkedő trend gyenge szezonalitással

ggplot(adatok, aes(x=ido2, y=mezőgazdasági))+geom_line(size=1)+
  labs(title="Mezőgazdasági termelői árindex 2005-ös bázis évvel", x="Dátum", y="Árindex")+
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), 
        axis.ticks = element_line(size = 1.5))+
  scale_x_date(date_breaks = "24 month", date_labels = "%Y-%m")
#erős ismétlődő szeonális hatások
#2019-20 környékén szintén elkezdődik egy exponenciális trend

#együttes ábrázolás
ggplot(adatok, aes(x=ido2))+
  geom_line(aes(y=ipari, color="Ipari"), size=1)+
  geom_line(aes(y=mezőgazdasági, color="Mezőgazdasági"), size=1)+
  labs(title="Mezőgazdasági és ipari termelői árindex 2005-ös bázisévvel", y="Árindexek", x="Dátum")+
  theme(plot.title = element_text(size=20,hjust = 0.5))+
  scale_x_date(date_breaks = "12 month", date_labels = "%Y-%m")

ggplot(adatok, aes(x=ido2))+
  geom_line(aes(y=ipari, color="Ipari"), size=1)+
  geom_line(aes(y=mezőgazdasági, color="Mezőgazdasági"), size=1)+
  geom_line(aes(y=M1_bázis, color="M1"), size=1)+
  geom_line(aes(y=árfolyam_bázis, color="Árfolyam"), size=1)+
  labs(title="Idősorok 2005-ös bázisévvel", y="Árindexek", x="Dátum")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks = "24 month", date_labels = "%Y-%m")

#-----------------------------------logaritmizálás---------------------------------------------
lnadatok<- adatok
lnadatok$ipari<- log(lnadatok$ipari)
ggplot(lnadatok, aes(x=ido2, y=ipari))+geom_line(size=1)+
  labs(title="Logaritmizált ipari termelői árindex 2005-ös bázis évvel", x="Dátum", y="Árindex")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")

lnadatok$mezőgazdasági<- log(lnadatok$mezőgazdasági)
ggplot(lnadatok, aes(x=ido2, y=mezőgazdasági))+geom_line(size=1)+
  labs(title="Logaritmizált mezőgazdasági termelői árindex 2005-ös bázis évvel", x="Dátum", y="Árindex")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")

lnadatok$M1_bázis<- log(lnadatok$M1_bázis)
ggplot(lnadatok, aes(x=ido2, y=M1_bázis))+geom_line(size=1)+
  labs(title="Logaritmizált M1 2005-ös bázis évvel", x="Dátum", y="M1")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")

lnadatok$árfolyam_bázis<- log(lnadatok$árfolyam_bázis)
ggplot(lnadatok, aes(x=ido2, y=árfolyam_bázis))+geom_line(size=1)+
  labs(title="Logaritmizált euro-forint árfolyam 2005-ös bázis évvel", x="Dátum", y="Árfolyam")+
   theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")

#---------------------------------szezonalitás vizsgálat-----------------------------
#---------------------------------ipari termelői árindex szezonalitás vizsgálat----------------------
theme_set(theme_classic())
ipari_ts<- ts(data=adatok$ipari, start= c(2008,1), frequency=12)
ggseasonplot(ipari_ts) + labs(title="Szezonalitási ábra az ipari árindexre")+geom_line(size=1)
#nem túl kifejező
theme_set(theme_bw())
adatok$honap<- months(adatok$ido2)
adatok$honap <- as.character(adatok$honap)
adatok$honap <- factor(adatok$honap, levels = c("január", "február", "március", "április", "május", "június", "július", "augusztus", "szeptember", "október", "november", "december"))
ggplot(adatok, aes(honap, ipari))+geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Ipari árindex szezonalitásáról doboz ábra és pont diagram formában", 
       x="Hónapok",
       y="Ipari termelői árindex értékek")
#mintha nem lenne szezonalitás az ipari termelői árindexben
lnadatok$honap<- months(lnadatok$ido2)
lnadatok$honap <- as.character(lnadatok$honap)
lnadatok$honap <- factor(lnadatok$honap, levels = c("január", "február", "március", "április", "május", "június", "július", "augusztus", "szeptember", "október", "november", "december"))
lnadatok$trend<- 1:nrow(adatok)
szezon_ipari<- lm(ipari~honap, data=lnadatok)
summary(szezon_ipari) #egyik hónap sem szignifikáns, Korrigált r négyzet negatív, p érték 1
szezon_ipari2<- lm(ipari~trend+honap, data=lnadatok)
summary(szezon_ipari2) #egyik hónap sem szignifikáns
confint(szezon_ipari)
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=ipari ,color="Valós" ), size=1)+
  geom_line(aes(y=szezon_ipari2$fitted.values, color="Becsült"), size=1)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m") #nem tudja jól mérni, hogy van-e szezonalitás
ipari<- lnadatok[,c("ido2", "ipari")]
ipari$szakasz<- "1. szakasz"
ipari$szakasz[ipari$ido2>"2011-11-01"&
              ipari$ido2<="2020-03-01"] <-"2. szakasz"
ipari$szakasz[ipari$ido2>"2020-03-01"]<- "3. szakasz"
unique(ipari$szakasz)
ipari$szakasz<- as.factor(ipari$szakasz)
ipari$trend <- 1:nrow(ipari)
ipari$honap<- months(ipari$ido2)
ipari$honap<- as.factor(ipari$honap)
strukt_szezon_ipari<- lm(ipari~trend+szakasz+trend*szakasz+honap, data=ipari)
summary(strukt_szezon_ipari) #egyik hónap sem szignifikáns
ggplot(ipari, aes(x=ido2))+
  geom_line(aes(y=ipari ,color="Valós" ), size=1)+
  geom_line(aes(y=strukt_szezon_ipari$fitted.values, color="Becsült"), size=1)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m") #pedig az illeszkedés viszonylag okés
#köbös trend illesztése
kobos<- lm(ipari~trend+I(trend^2)+I(trend^3)+ honap, data=ipari)
summary(kobos) #egyik hónap sem szignifikáns
ggplot(ipari, aes(x=ido2))+
  geom_line(aes(y=ipari ,color="Valós" ), size=1)+
  geom_line(aes(y=kobos$fitted.values, color="Becsült"), size=1)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")
#összes modell együttes ábrázolása
ggplot(ipari, aes(x=ido2))+
  geom_line(aes(y=kobos$fitted.values, color="Köbös"), size=0.7)+
  geom_line(aes(y=strukt_szezon_ipari$fitted.values, color="Strukturális"), size=0.7)+
  geom_line(aes(y=szezon_ipari2$fitted.values, color="Determinisztikus trend"), size=0.7)+
  geom_line(aes(y=ipari ,color="Valós" ), size=1.5)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")+
  labs(x="Dátum", y="Ipari termelői árindexek", title="Valós és becsült eredmények az ipari termelő árindexre")+
  theme(plot.title = element_text(hjust = 0.5))

#---------------------------------mezőgazdasági termelői árindex szezonalitás vizsgálat----------------------
mezőgazdasági_ts<- ts(data=adatok$mezőgazdasági, start= c(2008,1), frequency=12)
ggseasonplot(mezőgazdasági_ts) + labs(title="Szezonalitási ábra az mezőgazdasági árindexre")+
  geom_line(size=1) #nem túl kifejező
theme_set(theme_bw())
ggplot(adatok, aes(honap, mezőgazdasági))+geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Mezőgazdasági termelői árindex szezonalitásáról doboz ábra és pont diagram formában", 
       x="Hónapok",
       y="Mezőgazdasági termelői árindex értékek")
#látszódik szezonalitás
szezon_mezőgazdasági<- lm(mezőgazdasági~trend+honap, data=lnadatok)
summary(szezon_mezőgazdasági) #egyik hónap sem szignifikáns
szezon_mezőgazdasági2<- lm(mezőgazdasági~honap, data=lnadatok)
summary(szezon_mezőgazdasági2)
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=mezőgazdasági ,color="Valós" ), size=1)+
  geom_line(aes(y=szezon_mezőgazdasági$fitted.values, color="Becsült"), size=1)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")
#köbös trend
kobos<- lm(mezőgazdasági~trend+I(trend^2)+I(trend^3)+ honap, data=lnadatok)
summary(kobos)
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=mezőgazdasági ,color="Valós" ), size=1)+
  geom_line(aes(y=kobos$fitted.values, color="Becsült"), size=1)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")+
  labs(x="Dátum", y="Mezőgazdasági termelői árindex", title="Valós és becsült eredmények a mezőgazdasági termelői árindexre köbös trenddel")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6))
#április május december 5%on szignifikánsan eltér januártól
#strukturális törésekkel
lnadatok$szakaszok<- "1. szakasz"
lnadatok$szakaszok[lnadatok$ido2>"2010-01-01"&
                   lnadatok$ido2<="2011-03-01"] <-"2. szakasz"
lnadatok$szakaszok[lnadatok$ido2>"2011-03-01"&
                   lnadatok$ido2<="2020-12-01"] <-"3. szakasz"
lnadatok$szakaszok[lnadatok$ido2>"2020-12-01"&
                   lnadatok$ido2<="2022-12-01"] <-"4. szakasz"
lnadatok$szakaszok[lnadatok$ido2>"2022-12-01"]<- "5. szakasz"
lnadatok$szakaszok<- as.factor(lnadatok$szakaszok)
strukt_szezon_mezőgazdasági<- lm(mezőgazdasági~trend+szakaszok+trend*szakaszok+honap, data=lnadatok)
summary(strukt_szezon_mezőgazdasági)
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=mezőgazdasági ,color="Valós" ), size=1)+
  geom_line(aes(y=strukt_szezon_mezőgazdasági$fitted.values, color="Becsült"), size=1)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")+
  labs(x="Dátum", y="Mezőgazdasági termelői árindex", title="Valós és becsült eredmények a mezőgazdasági \n termelői árindexre strukturális törésekkel")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6))
#március, április, május, június, július, augusztus, november, december szignifikáns
#konfidencia intervallumok ábrázolása
a<-strukt_szezon_mezőgazdasági$coefficients
b<-confint(strukt_szezon_mezőgazdasági)
b[1,2]
gfg<-data.frame(x = c("február", "március", "április", "május", "június", "július", 
                      "augusztus", "szeptember", "október", "november", "december"), 
                      y = c(a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17]), 
                      low = c(b[7,1], b[8,1], b[9,1], b[10,1], b[11,1], b[12,1], b[13,1], b[14,1], b[15,1], b[16,1], b[17,1]), 
                      up = c(b[7,2],b[8,2], b[9,2],b[10,2],b[11,2],b[12,2], b[13,2],b[14,2],b[15,2],b[16,2],b[17,2])) 
ggplot(gfg, aes(x, y)) + geom_point() +  
  geom_errorbar(aes(ymin = low, ymax = up)) + geom_hline(yintercept=0, color="red", size=1)+
  labs(x="Hónapok", y="Konfidencia intervallumok", title="Mezőgazdasági árindex strukturális töréseket \n tartalmazó modell szezonalitásának konfidencia intervallumai \n (bináris dummy kódolás)")+
  theme(plot.title = element_text(hjust = 0.5))
#április, augusztus, december, július, május, március, november szignifikáns
#de mindegyik konfidencia intervallum átfed

#átlagtól való szignfikáns eltérés? -->kontraszt kódolás
contrasts(lnadatok$honap)
contr.sum(12) #Milyen kellene a kontraszthoz?
kontrasztmatrix<- contr.sum(12) #feltöltjük a nhóapokkal az előző kontraszt mátrixot
kontrasztmatrix
rownames(kontrasztmatrix)<- rownames(contrasts(lnadatok$honap))
kontrasztmatrix
#elnevezzük az oszlopokat is
colnames(kontrasztmatrix)<- c("január","február","március","április","május","június", "július", "augusztus", "szeptember", "október", "november")
kontrasztmatrix
#a dummy kódolást lecseréljük kontrasztra
contrasts(lnadatok$honap)<- kontrasztmatrix

#újrafuttatjuk a modellt
kontraszt<- lm(mezőgazdasági~trend+szakaszok+trend*szakaszok+honap, data=lnadatok)
summary(kontraszt) #január, április, május, szeptember szignifikáns 5%on
a<- kontraszt$coefficients
a
b<- confint(kontraszt)
b
gfg<-data.frame(x = c("január","február", "március", "április", "május", "június", "július", 
                      "augusztus", "szeptember", "október", "november"), 
                y = c(a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17]), 
                low = c(b[7,1], b[8,1], b[9,1], b[10,1], b[11,1], b[12,1], b[13,1], b[14,1], b[15,1], b[16,1], b[17,1]), 
                up = c(b[7,2],b[8,2], b[9,2],b[10,2],b[11,2],b[12,2], b[13,2],b[14,2],b[15,2],b[16,2],b[17,2])) 
ggplot(gfg, aes(x, y)) + geom_point() +  
  geom_errorbar(aes(ymin = low, ymax = up)) + geom_hline(yintercept=0, color="red", size=1)+
  labs(x="Hónapok", y="Konfidencia intervallumok", title="Strukturális töréseket tartalmazó modell\nszezonalitásának konfidencia intervallumai kontraszt kódolással")+
  theme(plot.title = element_text(hjust = 0.5))
#------ szerintem indokolt a mezőgazdasági index szezonális kiigazítása:
#április május szignifikánsan eltér, de átfednek
#január, szeptember szignifikánsan eltér, de átfednek
ggplot(lnadatok, aes(x=ido2, y=mezőgazdasági))+geom_line(size=1)+
  labs(title="Logaritmizált mezőgazdasági termelői árindex 2005-ös bázis évvel", x="Dátum", y="Árindex")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")+
  geom_vline(xintercept = as.Date("2013-10-01"), color="red", size=1)+
  geom_vline(xintercept = as.Date("2021-01-01"), color="red", size=1)+
  geom_line(aes(y=strukt_szezon_mezőgazdasági$fitted.values, color="Becsült"), size=1)
  
  
#csak 2013-10-01 és 2021-01-01 között van szezonalitás
mezőgazdasági_szezon <- lnadatok[, c("ido2", "mezőgazdasági")]
mezőgazdasági_szezon<- mezőgazdasági_szezon[61:157 ,]
mezőgazdasági_szezon$trend <- 1:nrow(mezőgazdasági_szezon)
mezőgazdasági_szezon$honap <- as.factor(months(mezőgazdasági_szezon$ido2))
model<- lm(mezőgazdasági~trend+I(trend^2)+honap, data=mezőgazdasági_szezon)
summary(model)
ggplot(mezőgazdasági_szezon, aes(x=ido2))+
  geom_line(aes(y=mezőgazdasági ,color="Valós" ), size=1)+
  geom_line(aes(y=model$fitted.values, color="Becsült"), size=1)+
  scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m")+
  labs(x="Dátum", y="Valós és becsült eredmények", title="Szűkített mezőgazdasági idősor szezonalitása")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6))

contrasts(mezőgazdasági_szezon$honap)
contr.sum(12) #Milyen kellene a kontraszthoz?
kontrasztmatrix<- contr.sum(12) #feltöltjük a nhóapokkal az előző kontraszt mátrixot
kontrasztmatrix
rownames(kontrasztmatrix)<- rownames(contrasts(lnadatok$honap))
kontrasztmatrix
#elnevezzük az oszlopokat is
colnames(kontrasztmatrix)<- c("január","február","március","április","május","június", "július", "augusztus", "szeptember", "október", "november")
kontrasztmatrix
#a dummy kódolást lecseréljük kontrasztra
contrasts(mezőgazdasági_szezon$honap)<- kontrasztmatrix

#újrafuttatjuk a modellt
kontraszt_uj<- lm(mezőgazdasági~trend+I(trend^2)+honap, data=mezőgazdasági_szezon)
summary(kontraszt_uj) #január, március, április, május, augusztus, szeptember, október
#ebből új: március, augusztus október
a<- kontraszt_uj$coefficients
a
b<- confint(kontraszt_uj)
b
gfg<-data.frame(x = c("január","február", "március", "április", "május", "június", "július", 
                      "augusztus", "szeptember", "október", "november"), 
                y = c(a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14]), 
                low = c(b[4,1], b[5,1], b[6,1], b[7,1], b[8,1], b[9,1], b[10,1], b[11,1], b[12,1], b[13,1], b[14,1]), 
                up = c(b[4,2],b[5,2], b[6,2],b[7,2],b[8,2],b[9,2], b[10,2],b[11,2],b[12,2],b[13,2],b[14,2])) 
ggplot(gfg, aes(x, y)) + geom_point() +  
  geom_errorbar(aes(ymin = low, ymax = up)) + geom_hline(yintercept=0, color="red", size=1)+
  labs(x="Hónapok", y="Konfidencia intervallumok", title="Strukturális töréseket tartalmazó modell\nszezonalitásának konfidencia intervallumai kontraszt kódolással")+
  theme(plot.title = element_text(hjust = 0.5))

#ennek a szakasznak a szezonális kiigazítása
lnadatok$mozgo_mezőgazdasági <- rollmean(lnadatok$mezőgazdasági, k=12, fill=NA, align="right")
lnadatok$indexek<- lnadatok$mezőgazdasági-lnadatok$mozgo_mezőgazdasági
#szezonindexek kiszámítása
#kiátlagoljuk az indexeket minden hónapra
szezonindexek<- aggregate(indexek~honap, data=lnadatok, FUN= mean)
szezonindexek
#ezek a nyers szezonindexek
#a nyers szezonindexek átlagának 1-et kell adni
mean(szezonindexek$indexek)
#koriigált szezonindexek számítása: nyerseket leosztjuk az átlaggal
szezonindexek$korrindex<- szezonindexek$indexek-mean(szezonindexek$indexek)
mean(szezonindexek$korrindex)
#hozzáadjuk az általunk kiszánolt korrigált szezonindexeket a data framehez
lnadatok<- merge(lnadatok, szezonindexek[,c ("honap", "korrindex")],
                   by="honap", all.x = TRUE)
head(lnadatok) #adatok első 6 sora
lnadatok<- lnadatok[order(lnadatok$ido2),] #megfelelő sorrendbe rendezzük az adatokat
head(lnadatok)
lnadatok$kiigazitott<- lnadatok$mezőgazdasági-lnadatok$korrindex
head(lnadatok)
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=mezőgazdasági ,color="Valós" ), size=1)+
  geom_line(aes(y=kiigazitott, color="Kiigazított"), size=1)+
  scale_x_date(date_breaks = "12 month", date_labels = "%Y-%m")+
  labs(x="Dátum", y="Valós és kiigazított eredmények", title="Mezőgazdasági termelői árindex")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6))
lnadatok$ujmezőgazdasági <- ifelse(lnadatok$ido2 < as.Date("2013-10-01") | lnadatok$ido2 > as.Date("2021-01-01"), lnadatok$mezőgazdasági, lnadatok$kiigazitott)
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=ujmezőgazdasági), size=1)+labs(x="Dátum", y="Mezőgazdasági termelői árindex", title="Szezonálisan kiigazított idősor")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6))+
  scale_x_date(date_breaks = "12 month", date_labels = "%Y-%m")
lnadatok <- lnadatok[,c("honap", "ido2", "ipari", "ujmezőgazdasági", "árfolyam_bázis", "trend", "M1_bázis")]
#--------------------------------M1 pénzaggregátum szezonalitás vizsgálata--------------------------
M1_ts<- ts(data=adatok$M1_bázis, start= c(2008,1), frequency=12)
ggseasonplot(M1_ts) + labs(title="Szezonalitási ábra az M1 aggregátumra")+geom_line(size=1)+
  theme(plot.title = element_text(hjust = 0.5))
ggplot(adatok, aes(honap, M1_bázis))+geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="M1 pénzaggregátum szezonalitásáról doboz ábra és pont diagram formában", 
       x="Hónapok",
       y="M1 pénzaggregátum értékei") #van szezonalitás
szezon_m1<- lm(M1_bázis~trend+honap, data=lnadatok)
summary(szezon_m1) #január, február, március, október, november szignifikánsan eltér az átlagtól
contrasts(lnadatok$honap)
szezon_m1_2<- lm(M1_bázis~honap, data=lnadatok)
summary(szezon_m1_2)
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=M1_bázis ,color="Valós" ), size=1)+
  geom_line(aes(y=szezon_m1$fitted.values, color="Becsült"), size=1)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")
lnadatok$szakasz<- "1. szakasz"
lnadatok$szakasz[lnadatok$ido2>"2013-03-01"&
                  lnadatok$ido2<="2022-06-01"] <-"2. szakasz"
lnadatok$szakasz[lnadatok$ido2>"2022-06-01"]<- "3. szakasz"
lnadatok$szakasz<- as.factor(lnadatok$szakasz)
strukt_szezon_m1<- lm(M1_bázis~trend+szakasz+trend*szakasz+honap, data=lnadatok)
summary(strukt_szezon_m1) #január, február, március, április, szeptember, október, november szignifikánsan eltér az átlagtól
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=M1_bázis ,color="Valós" ), size=1)+
  geom_line(aes(y=strukt_szezon_m1$fitted.values, color="Becsült"), size=1)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")+
  labs(x="Dátum", y="Logaritmizált pénzaggregátum", title="Valós és becsült eredemények az M1 pénzaggregátumra strukturális törésekkel")
#konfidencia intervallumok ábrázolása
confint(strukt_szezon_m1)
a<- strukt_szezon_m1$coefficients
a[15]
b<- confint(strukt_szezon_m1)
b[15,2]
gfg<-data.frame(x = c("január","február", "március", "április", "május", "június", "július", "augusztus",
                      "szeptember", "október", "november"), 
                y = c( a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15]), 
                low = c( b[5,1], b[6,1], b[7,1], b[8,1], b[9,1], b[10,1], b[11,1], b[12,1], b[13,1], b[14,1], b[15,1]), 
                up = c(b[5,2], b[6,2],b[7,2],b[8,2],b[9,2], b[10,2],b[11,2],b[12,2],b[13,2],b[14,2], b[15,2])) 
ggplot(gfg, aes(x, y)) + geom_point() +  
  geom_errorbar(aes(ymin = low, ymax = up)) +
  geom_hline(yintercept=0, color="red", size=1)+
  labs(x="Hónapok", y="konfidencia intervallumok", title="Strukturális töréseket tartalmazó modell szezonalitásainak konfidencia intervallumai")+
  theme(plot.title = element_text(hjust = 0.5))
#átlagtól szignifikánsan eltérnek

lnadatok$mozgom1 <- rollmean(lnadatok$M1_bázis, k=12, fill=NA, align="right")
lnadatok$indexek<- lnadatok$M1_bázis-lnadatok$mozgom1
szezonindexek<- aggregate(indexek~honap, data=lnadatok, FUN= mean)
szezonindexek
mean(szezonindexek$indexek)
#koriigált szezonindexek számítása: nyerseket leosztjuk az átlaggal
szezonindexek$korrindex<- szezonindexek$indexek-mean(szezonindexek$indexek)
mean(szezonindexek$korrindex)
#hozzáadjuk az általunk kiszánolt korrigált szezonindexeket a data framehez
lnadatok<- merge(lnadatok, szezonindexek[,c ("honap", "korrindex")],
                 by="honap", all.x = TRUE)
head(lnadatok) #adatok első 6 sora
lnadatok<- lnadatok[order(lnadatok$ido2),] #megfelelő sorrendbe rendezzük az adatokat
head(lnadatok)
lnadatok$ujm1<- lnadatok$M1_bázis-lnadatok$korrindex
head(lnadatok)
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=M1_bázis ,color="Valós" ), size=1)+
  geom_line(aes(y=ujm1, color="Kiigazított"), size=1)+
  scale_x_date(date_breaks = "12 month", date_labels = "%Y-%m")+
  labs(x="Dátum", y="Valós és kiigazított eredmények", title="M1 pénzaggregátum logaritmizált idősora és kiigazítása")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6))
lnadatok <- lnadatok[,c("honap", "ido2", "ipari", "ujmezőgazdasági", "árfolyam_bázis", "trend", "ujm1")]

#---------------------------------árfolyam szezonlatás vizsgálat----------------------------------
árfolyam_ts<- ts(data=adatok$árfolyam_bázis, start= c(2008,1), frequency=12)
ggseasonplot(árfolyam_ts) + labs(title="Szezonalitási ábra az euro/huf árfolyamra")+geom_line(size=1)
#nem túl kifejező
ggplot(adatok, aes(honap, árfolyam_bázis))+geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Euro/huf árfolyam szezonalitásáról doboz ábra és pont diagram formában", 
       x="Hónapok",
       y="Euro/huf árfolyam")
szezon_árfolyam<- lm(árfolyam_bázis~trend+honap, data=lnadatok)
contrasts(lnadatok$honap)
summary(szezon_árfolyam) #10%on a május szignifikáns
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=árfolyam_bázis ,color="Valós" ), size=1)+
  geom_line(aes(y=szezon_árfolyam$fitted.values, color="Determinisztikus"), size=1)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")+
  labs(x="Dátum", y="Euro/huf árfolyam", title="Logaritmizált idősor és a becsült modell")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6))
  
lnadatok$szakasz<- "1. szakasz"
lnadatok$szakasz[lnadatok$ido2>"2013-01-01"&
                  lnadatok$ido2<="2019-12-01"] <-"2. szakasz"
lnadatok$szakasz[adatok$ido2>"2019-12-01"]<- "3. szakasz"
lnadatok$szakasz<- as.factor(lnadatok$szakasz)
strukt_szezon_árfolyam<- lm(árfolyam_bázis~trend+szakasz+trend*szakasz+honap, data=lnadatok)
summary(strukt_szezon_árfolyam) #febrár, mácius, május 5%on eltér
ggplot(lnadatok, aes(x=ido2))+
  geom_line(aes(y=árfolyam_bázis ,color="Valós" ), size=1)+
  geom_line(aes(y=strukt_szezon_árfolyam$fitted.values, color="Strukturális törésekkel"), size=0.8)+
  scale_x_date(date_breaks = "18 month", date_labels = "%Y-%m")+
  labs(x="Dátum", y="Euro/huf árfolyam", title="Logaritmizált idősor és a becsült modell")+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=65, vjust=0.6))
#nem csinálnék szezonalitást
lnadatok<- lnadatok[,c("honap", "ido2", "ipari", "ujmezőgazdasági", "árfolyam_bázis", "trend", "ujm1")]

#---------------------------------------------konzultáció jegyzet-----------------------------------

#ÁTALAKÍTANI BÁZISSÁ ÉS ÚGY MEGNÉZNI AZ IDŐSOROKAT  -->pipa


#logaritmizálás: általában ezt csinálják a szakirodalomban amikor péznben van valami kifejezve
#megnézni az adatokat 
#első sor az első év, második sor második év, oszlopok a hónaok és egy mátrixot csinálni belőle
#futtani egy regressziót ahol a hónapok benne vannak mint a dummy meg az outlier is
#vannak e szignifikáns értékei
#konfidencia intervallumokat kiíratni, hogy van e átfedés
#van e egyáltalán szükség a kigazításra?
#semmi nincs a regresszióban csak a konstant meg a dummy outlier, annak lesz egy egyóütthatója
#berakom az idősorba a konstanst ami kijött az outlier helyett
#így kiszűröm az outlier hatását majd ezután csinálom a szezonális kiigazítást majd ezután újra belerakom az outliert
# a modellbe is lehet érdemes lesz belerakni az outlier szűrtet
#érdemes lehet először logaritmizáélni és utána szezonálisan kiigazítani


#------------------------------stacioneritás viszgálata órai tesztekkel---------------------------
#egységgyök folyamatokról van szó?
#ipari
aTSA::adf.test(lnadatok$ipari)
#no drift no trend --> magas p elvetjük a nullhipotézisz -->egységgyök
# with drift no trend --> magas p elvetjük a nullhipotézisz -->egységgyök
#with drift and trend --> magas p elvetjük a nullhipotézisz -->egységgyök

#mezőgazdasági
aTSA::adf.test(lnadatok$ujmezőgazdasági)
#no drift no trend --> magas p elvetjük a nullhipotézist -->egységgyök
# with drift no trend --> magas p elvetjük a nullhipotézist -->egységgyök
#with drift and trend --> magas p elvetjük a nullhipotézist -->egységgyök

#M1 kiigazított
aTSA::adf.test(lnadatok$ujm1)
#no drift no trend --> magas p elvetjük a nullhipotézist -->egységgyök
# with drift no trend --> magas p elvetjük a nullhipotézist -->egységgyök
#with drift and trend --> magas p elvetjük a nullhipotézist -->egységgyök

#árfolyam
aTSA::adf.test(lnadatok$árfolyam_bázis)
#no drift no trend --> magas p elvetjük a nullhipotézist -->egységgyök
# with drift no trend --> magas p elvetjük a nullhipotézist -->egységgyök
#with drift and trend --> alacsony p érték --> stacioner

#--------------------------2 idősorra a modellezés órai minta alapján---------------------------------
#kointegráció a termelői árindexekre
kointreg<- lm(ujmezőgazdasági~ipari, data=lnadatok)
summary(kointreg)
#β1 a parciális t-próba alapján szignifikáns minden szokásos szignifikancia-szinten
#hibatag vizsgálata
kointreg_hibatag<- kointreg$residuals
adf.test(kointreg_hibatag) #p alacsony-->hibatag stacioner -->két idősor kointegrált
plot(kointreg_hibatag)
#VECM
#idősorok stacionerré alakítása
lnadatok$d_ipari <- c(NA, diff(lnadatok$ipari))
lnadatok$d_mezőgazdasági <- c(NA, diff(lnadatok$ujmezőgazdasági))
adf.test(lnadatok$d_ipari) #kicsi p -->stacioner
adf.test(lnadatok$d_mezőgazdasági)#kicsi p -->stacioner
#optimális késleltetett meghatározása
VARselect(lnadatok[2:nrow(lnadatok),c("d_ipari", "d_mezőgazdasági")], lag.max = 10) #1-es késleltetés
#VECM építés 1-es laggal
VECM1 <- VECM(lnadatok[,c("ipari", "ujmezőgazdasági")], lag = 1)
summary(VECM1)
#hibakorrekciós tényező 5%on szignifikáns a mezőgazdasági árindexre
#ha változik az ipari árindex hosszú távú pályája, akkor a mezőgazdasági árindex korrigál hozzá
#DE!!! hibakorrekciós tényező előjele pozitív, tehát nem korrigál hozzá, hanem távolodik tőle???
#AR(1) folyamat az ipari árindexél
#az ipari árindex 1 hónapos késleltetettje hatással van  a mezőgazdasági árindexre
vecm1_hibatagok <- as.data.frame(resid(VECM1))
str(vecm1_hibatagok)
lapply(vecm1_hibatagok, function(i) bgtest(i ~ 1, order = 10))
#ipari p érték magas --> fehérzaj
#mezőgazdasági p érték éppen 10% feletti --> fehérzaj
acf(vecm1_hibatagok$ipari)
pacf(vecm1_hibatagok$ipari)
acf(vecm1_hibatagok$ujmezőgazdasági)
pacf(vecm1_hibatagok$ujmezőgazdasági)
#baj, hogy a 12-ik mindenhol kilóg?
#megnézzük, hogy egy 12 hónapos időtávon hogyan cseng le a mezőgazdasági árindexben
plot(irf(VECM1, impulse = "ipari", response = "mezőgazdasági", n.ahead = 12, ortho = TRUE))
#egy szórásnyi pozitív sokk az ipari árindex változóban
#nem cseng le --> egybevág azzal, hogy negatív a hibakorrekciós együttható

#-------------------------4 idősorra az elemzés órai minta alapján------------------------------------
lnadat_ts <- ts(lnadatok[, c("ujmezőgazdasági", "ipari", "ujm1", "árfolyam_bázis")], start=c(2008,01), frequency=12)
jotest<-ca.jo(lnadat_ts, type="eigen", ecdet="const", spec="longrun", K=3)
summary(jotest)
#           test 10pct  5pct  1pct
#r <= 3 |  3.49  7.52  9.24 12.97
#r <= 2 | 11.56 13.75 15.67 20.20 elutasítási tartomány
#r <= 1 | 28.12 19.77 22.00 26.81 elfogadási tartomány
#r = 0  | 42.73 25.56 28.14 33.24 elfogadási tartomány
# EGY KOINTEGRÁLÓ VEKTOR VAN

#VECM
#idősorok stacionerré alakítása
lnadatok$d_ipari <- c(NA, diff(lnadatok$ipari))
lnadatok$d_mezőgazdasági <- c(NA, diff(lnadatok$ujmezőgazdasági))
lnadatok$d_m1 <- c(NA, diff(lnadatok$ujm1))
lnadatok$d_árfolyam <- c(NA, diff(lnadatok$árfolyam_bázis))

adf.test(lnadatok$d_ipari) #kicsi p -->stacioner
adf.test(lnadatok$d_mezőgazdasági)#kicsi p -->stacioner
adf.test(lnadatok$d_m1) #kicsi p -->stacioner
adf.test(lnadatok$d_árfolyam) #kicsi p -->stacioner

#optimális késleltetettje
VARselect(lnadatok[2:nrow(lnadatok),c("d_ipari", "d_mezőgazdasági", "d_m1", "d_árfolyam")], lag.max = 25)
#1 vagy 15

#VECM 1 késleltetés és 1 kointegráló vektor
VECM3 <- VECM(lnadatok[,c("ipari", "ujmezőgazdasági", "ujm1", "árfolyam_bázis")], lag = 1, r=1)
summary(VECM3)
#hibakorrekciós tényező szignifikáns az m1re
#ha változik a hosszú távú pálya, akkor az m1 korrigál hozzá
#egy hónap alatt az m1 a hosszútávú pályától vett eltérés 8,06%-át le tudja dolgozni

#az ipari ári ndexnél van egy AR(1) folyamat
#az ipari árindexet Granger okozza az mezőgazdasági(gyengén) és az árfolyam

#mezőgazdaságinál semmi sem szignifikáns

#mezőgazdasági 1es késleltetéssel gyengén Granger okozza az m1et

#árfolyamot Granger okozza az ipari 1-es késleltetettje

#Rossz gazdaságpolitika magyarázhatja ezt a jelenséget

#VECM 1 késleltetés és 2 kointegráló vektor
VECM4 <- VECM(lnadatok[,c("ipari", "ujmezőgazdasági", "ujm1", "árfolyam_bázis")], lag = 1, r=2)
summary(VECM4)
#első hósszú távú pálya
#pénzmennyiség igazodik hozzá

#impulzus válasz függvény
plot(irf(VECM3, impulse="mezőgazdasági", n.ahead=12, ortho=F))
plot(irf(VECM3, impulse="ipari", n.ahead=12, ortho=F))
plot(irf(VECM3, impulse="m1kiigazitott", n.ahead=12, ortho=F))
plot(irf(VECM3, impulse="árfolyam_bázis", n.ahead=12, ortho=F))

genFEVD(vec2var(jotest, r=2))
#       resids of ujmezőgazdasági resids of ipari resids of ujm1      resids of árfolyam_bázis
#[1,]                0.58118872      0.26937143     0.06049834                0.0889415
#[2,]                0.06704548      0.47358822     0.06259273                0.3967736
#[3,]                0.03418040      0.08103803     0.26720225                0.6175793
#[4,]                0.01702171      0.34216823     0.04624412                0.5945659
#a mazőgazdasági előrejelzését 58%ban magyarázza önmaga, 6%ban az ipari és 3%ban az m1

#-------------------------------kointegráció szűkített idősoron---------------------------------
szukitett<- lnadatok[ ,c("honap", "ido2", "ipari", "ujmezőgazdasági", "árfolyam_bázis", "trend", "ujm1")]
szukitett<- szukitett[szukitett$ido2<"2020-01-01",]
szukitett_ts <- ts(szukitett[, c("ujmezőgazdasági", "ipari", "ujm1", "árfolyam_bázis")], start=c(2008,01), frequency=12)
jotest_szukitett<-ca.jo(szukitett_ts, type="eigen", ecdet="const", spec="longrun", K=3)
summary(jotest_szukitett)
#           test 10pct  5pct  1pct
#r <= 3 |  6.89  7.52  9.24 12.97 elutasítási tartomány
#r <= 2 | 18.55 13.75 15.67 20.20 határeset
#r <= 1 | 35.51 19.77 22.00 26.81 elfogadási tartomány
#r = 0  | 38.71 25.56 28.14 33.24 elfogadási tartomány
#egy vagy kettő kointegráló vektor (1%on 1, de 5%on 2)
#VECM
#idősorok stacionerré alakítása
szukitett$d_ipari <- c(NA, diff(szukitett$ipari))
szukitett$d_mezőgazdasági <- c(NA, diff(szukitett$ujmezőgazdasági))
szukitett$d_m1 <- c(NA, diff(szukitett$ujm1))
szukitett$d_árfolyam <- c(NA, diff(szukitett$árfolyam_bázis))

adf.test(szukitett$d_ipari) #kicsi p -->stacioner
adf.test(szukitett$d_mezőgazdasági)#kicsi p -->stacioner
adf.test(szukitett$d_m1) #kicsi p -->stacioner
adf.test(szukitett$d_árfolyam) #kicsi p -->stacioner

#optimális késleltetettje
VARselect(szukitett[2:nrow(szukitett),c("d_ipari", "d_mezőgazdasági", "d_m1", "d_árfolyam")], lag.max = 12)
#1 vagy 12

#VECM 1 késleltetés és 1 kointegráló vektor
VECM4 <- VECM(szukitett[,c("ipari", "ujmezőgazdasági", "ujm1", "árfolyam_bázis")], lag = 1, r=1)
summary(VECM4)
#hibakorrekciós együttható iparira és árfolyamra szignfikáns, de egyik sem negatív
#iparinál AR(1) folyamat
#mezőgazdaságinál is AR(1) folyamat
#m1nél semmi sem szignifikáns
#árfolyamra gyengén hat az ipari

genFEVD(vec2var(jotest_szukitett, r=1))

#VECM 1 késleltetés ls 2 kointegráló vektor
VECM5 <- VECM(szukitett[,c("ipari", "ujmezőgazdasági", "ujm1", "árfolyam_bázis")], lag = 1, r=2)
summary(VECM5)
VECM5$coefficients
write.csv(VECM5$coefficients, "VECM5_coefficients.csv")
#első hibakorrekciós tagnál a ipari, mezőgazdasági és árfolyam
#is szignifiáns valamilyen szinten, de egyik előjele sem pozitív
#második hibakorrekciós tényező ipari, mezőgazdasági és árfolyam szignifikáns
#mindegyik előjele negatív
#tehát az m1 alakítja a hosszú távú pályát és mindegyik igazpdik hozzá
#az ipari 1 hónap alatt 2%-ot dolgoz le, a mezőgazdasági 9,8%-ot, az árfolyam 6,7%ot
#iparinál AR(1) folyamat
#mezőgazdaságinál is AR(1) folyamat
#m1re gyengén hat az ipari
#árfolyamra gyengén hat az ipari
genFEVD(vec2var(jotest_szukitett, r=2))

#-----------------------------szakirodalom alapján-----------------------------------
#stacioner vagy egységgyök folyamat?
#DF-GLS teszt
#ipari
summary(ur.ers(lnadatok$ipari,
               type=c("DF-GLS"),
               model = "trend", 
               lag.max = 5))
#bal oldali teszt, a teszt statisztika -1.3875 
#Elfogadási tartományba esik, minden használt szignifikancia szinten.
#H0: ipari index nemstacionárius (egységgyök) --> egységgyök folyamat
summary(ur.ers(lnadatok$ipari,
               type=c("DF-GLS"),
               model = "constant", 
               lag.max = 5))
#bal oldali teszt, a teszt statisztika -1.3875 
#Elfogadási tartományba esik, minden használt szignifikancia szinten.
#H0: ipari index nemstacionárius (egységgyök) --> egységgyök folyamat
#a késleltetési számot Akaike alapján ahtározza meg még a modell futtatás előtt?


#mezőgazdasági
summary(ur.ers(log(outlier$mezőgazdasági),
               model = "trend", 
               lag.max = 5))
#bal oldali teszt, a teszt statisztika -1.7159
#nem tudjuk elutasítani a nullhipotézist, minden használt szignifikancia szinten.
#H0: mezőgazdasági index nemstacionárius (egységgyök)

#M1
summary(ur.ers(lnadat$m1kiigazitott,
               model = "trend", 
               lag.max = 5))
#bal oldali teszt, a teszt statisztika -1.1346
#nem tudjuk elutasítani a nullhipotézist, minden használt szignifikancia szinten.
#H0: mezőgazdasági index nemstacionárius (egységgyök)

#árfolyam
summary(ur.ers(lnadat$árfolyam_bázis,
               model = "trend", 
               lag.max = 5))
#bal oldali teszt, a teszt statisztika -4.4038 
#elutasítási tartományba esik, minden használt szignifikancia szinten.
#stacioner folyamat

#------------------------------DF-GLS teszt ipari, trend adatokra--------------------------------
ipari_kes1<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 1))
ipari_kes1_SSE <- sum(ipari_kes1@testreg$residuals^2)
ipari_kes1_k <- nrow(ipari_kes1@testreg$coefficients) #number of parameters
N <- nrow(lnadatok)#number of observations
ipari_kes1_bic<- (ipari_kes1_SSE/N)*N^((ipari_kes1_k+1)/N)
ipari_kes1_bic #0.0001482443

ipari_kes2<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 2))
ipari_kes2_SSE <- sum(ipari_kes2@testreg$residuals^2)
ipari_kes2_k <- nrow(ipari_kes2@testreg$coefficients) #number of parameters
ipari_kes2_bic<- (ipari_kes2_SSE/N)*N^((ipari_kes2_k+1)/N)
ipari_kes2_bic #0.0001482443

ipari_kes3<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 3))
ipari_kes3_SSE <- sum(ipari_kes3@testreg$residuals^2)
ipari_kes3_k <- nrow(ipari_kes3@testreg$coefficients) #number of parameters
ipari_kes3_bic<- (ipari_kes3_SSE/N)*N^((ipari_kes3_k+1)/N)
ipari_kes3_bic

ipari_kes4<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 4))
ipari_kes4_SSE <- sum(ipari_kes4@testreg$residuals^2)
ipari_kes4_k <- nrow(ipari_kes4@testreg$coefficients) #number of parameters
ipari_kes4_bic<- (ipari_kes4_SSE/N)*N^((ipari_kes4_k+1)/N)
ipari_kes4_bic

ipari_kes5<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 5))
ipari_kes5_SSE <- sum(ipari_kes5@testreg$residuals^2)
ipari_kes5_k <- nrow(ipari_kes5@testreg$coefficients) #number of parameters
ipari_kes5_bic<- (ipari_kes5_SSE/N)*N^((ipari_kes5_k+1)/N)
ipari_kes5_bic

ipari_kes6<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 6))
ipari_kes6_SSE <- sum(ipari_kes6@testreg$residuals^2)
ipari_kes6_k <- nrow(ipari_kes6@testreg$coefficients) #number of parameters
ipari_kes6_bic<- (ipari_kes6_SSE/N)*N^((ipari_kes6_k+1)/N)
ipari_kes6_bic

ipari_kes7<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 7))
ipari_kes7_SSE <- sum(ipari_kes7@testreg$residuals^2)
ipari_kes7_k <- nrow(ipari_kes7@testreg$coefficients) #number of parameters
ipari_kes7_bic<- (ipari_kes7_SSE/N)*N^((ipari_kes7_k+1)/N)
ipari_kes7_bic

ipari_kes8<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 8))
ipari_kes8_SSE <- sum(ipari_kes8@testreg$residuals^2)
ipari_kes8_k <- nrow(ipari_kes8@testreg$coefficients) #number of parameters
ipari_kes8_bic<- (ipari_kes8_SSE/N)*N^((ipari_kes8_k+1)/N)
ipari_kes8_bic

ipari_kes9<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 9))
ipari_kes9_SSE <- sum(ipari_kes9@testreg$residuals^2)
ipari_kes9_k <- nrow(ipari_kes9@testreg$coefficients) #number of parameters
ipari_kes9_bic<- (ipari_kes9_SSE/N)*N^((ipari_kes9_k+1)/N)
ipari_kes9_bic

ipari_kes10<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 10))
ipari_kes10_SSE <- sum(ipari_kes10@testreg$residuals^2)
ipari_kes10_k <- nrow(ipari_kes10@testreg$coefficients) #number of parameters
ipari_kes10_bic<- (ipari_kes10_SSE/N)*N^((ipari_kes10_k+1)/N)
ipari_kes10_bic

ipari_kes11<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 11))
ipari_kes11_SSE <- sum(ipari_kes11@testreg$residuals^2)
ipari_kes11_k <- nrow(ipari_kes11@testreg$coefficients) #number of parameters
ipari_kes11_bic<- (ipari_kes11_SSE/N)*N^((ipari_kes11_k+1)/N)
ipari_kes11_bic

ipari_kes12<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 12))
ipari_kes12_SSE <- sum(ipari_kes12@testreg$residuals^2)
ipari_kes12_k <- nrow(ipari_kes12@testreg$coefficients) #number of parameters
ipari_kes12_bic<- (ipari_kes12_SSE/N)*N^((ipari_kes12_k+1)/N)
ipari_kes12_bic

ipari_kes13<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "trend", lag.max = 13))
ipari_kes13_SSE <- sum(ipari_kes13@testreg$residuals^2)
ipari_kes13_k <- nrow(ipari_kes13@testreg$coefficients) #number of parameters
ipari_kes13_bic<- (ipari_kes13_SSE/N)*N^((ipari_kes13_k+1)/N)
ipari_kes13_bic

min(ipari_kes1_bic, ipari_kes2_bic, ipari_kes3_bic, ipari_kes4_bic, ipari_kes5_bic, ipari_kes6_bic, ipari_kes7_bic, ipari_kes8_bic, ipari_kes9_bic, ipari_kes10_bic, ipari_kes11_bic, ipari_kes12_bic, ipari_kes13_bic)
#1 késleltetést javasol
ipari_kes1 #elfodaási tartomány --> egységgyök folyamat

#---------------------------------DF-GLS teszt ipari, constant adatokra----------------------------
ipari_kes1<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 1))
ipari_kes1_SSE <- sum(ipari_kes1@testreg$residuals^2)
ipari_kes1_k <- nrow(ipari_kes1@testreg$coefficients) #number of parameters
N <- nrow(lnadatok)#number of observations
ipari_kes1_bic<- (ipari_kes1_SSE/N)*N^((ipari_kes1_k+1)/N)
ipari_kes1_bic 

ipari_kes2<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 2))
ipari_kes2_SSE <- sum(ipari_kes2@testreg$residuals^2)
ipari_kes2_k <- nrow(ipari_kes2@testreg$coefficients) #number of parameters
ipari_kes2_bic<- (ipari_kes2_SSE/N)*N^((ipari_kes2_k+1)/N)
ipari_kes2_bic 

ipari_kes3<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 3))
ipari_kes3_SSE <- sum(ipari_kes3@testreg$residuals^2)
ipari_kes3_k <- nrow(ipari_kes3@testreg$coefficients) #number of parameters
ipari_kes3_bic<- (ipari_kes3_SSE/N)*N^((ipari_kes3_k+1)/N)
ipari_kes3_bic

ipari_kes4<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 4))
ipari_kes4_SSE <- sum(ipari_kes4@testreg$residuals^2)
ipari_kes4_k <- nrow(ipari_kes4@testreg$coefficients) #number of parameters
ipari_kes4_bic<- (ipari_kes4_SSE/N)*N^((ipari_kes4_k+1)/N)
ipari_kes4_bic

ipari_kes5<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 5))
ipari_kes5_SSE <- sum(ipari_kes5@testreg$residuals^2)
ipari_kes5_k <- nrow(ipari_kes5@testreg$coefficients) #number of parameters
ipari_kes5_bic<- (ipari_kes5_SSE/N)*N^((ipari_kes5_k+1)/N)
ipari_kes5_bic

ipari_kes6<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 6))
ipari_kes6_SSE <- sum(ipari_kes6@testreg$residuals^2)
ipari_kes6_k <- nrow(ipari_kes6@testreg$coefficients) #number of parameters
ipari_kes6_bic<- (ipari_kes6_SSE/N)*N^((ipari_kes6_k+1)/N)
ipari_kes6_bic

ipari_kes7<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 7))
ipari_kes7_SSE <- sum(ipari_kes7@testreg$residuals^2)
ipari_kes7_k <- nrow(ipari_kes7@testreg$coefficients) #number of parameters
ipari_kes7_bic<- (ipari_kes7_SSE/N)*N^((ipari_kes7_k+1)/N)
ipari_kes7_bic

ipari_kes8<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 8))
ipari_kes8_SSE <- sum(ipari_kes8@testreg$residuals^2)
ipari_kes8_k <- nrow(ipari_kes8@testreg$coefficients) #number of parameters
ipari_kes8_bic<- (ipari_kes8_SSE/N)*N^((ipari_kes8_k+1)/N)
ipari_kes8_bic

ipari_kes9<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 9))
ipari_kes9_SSE <- sum(ipari_kes9@testreg$residuals^2)
ipari_kes9_k <- nrow(ipari_kes9@testreg$coefficients) #number of parameters
ipari_kes9_bic<- (ipari_kes9_SSE/N)*N^((ipari_kes9_k+1)/N)
ipari_kes9_bic

ipari_kes10<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 10))
ipari_kes10_SSE <- sum(ipari_kes10@testreg$residuals^2)
ipari_kes10_k <- nrow(ipari_kes10@testreg$coefficients) #number of parameters
ipari_kes10_bic<- (ipari_kes10_SSE/N)*N^((ipari_kes10_k+1)/N)
ipari_kes10_bic

ipari_kes11<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 11))
ipari_kes11_SSE <- sum(ipari_kes11@testreg$residuals^2)
ipari_kes11_k <- nrow(ipari_kes11@testreg$coefficients) #number of parameters
ipari_kes11_bic<- (ipari_kes11_SSE/N)*N^((ipari_kes11_k+1)/N)
ipari_kes11_bic

ipari_kes12<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 12))
ipari_kes12_SSE <- sum(ipari_kes12@testreg$residuals^2)
ipari_kes12_k <- nrow(ipari_kes12@testreg$coefficients) #number of parameters
ipari_kes12_bic<- (ipari_kes12_SSE/N)*N^((ipari_kes12_k+1)/N)
ipari_kes12_bic

ipari_kes13<- summary(ur.ers(lnadatok$ipari, type=c("DF-GLS"), model = "constant", lag.max = 13))
ipari_kes13_SSE <- sum(ipari_kes13@testreg$residuals^2)
ipari_kes13_k <- nrow(ipari_kes13@testreg$coefficients) #number of parameters
ipari_kes13_bic<- (ipari_kes13_SSE/N)*N^((ipari_kes13_k+1)/N)
ipari_kes13_bic

min(ipari_kes1_bic, ipari_kes2_bic, ipari_kes3_bic, ipari_kes4_bic, ipari_kes5_bic, ipari_kes6_bic, ipari_kes7_bic, ipari_kes8_bic, ipari_kes9_bic, ipari_kes10_bic, ipari_kes11_bic, ipari_kes12_bic, ipari_kes13_bic)
#1 késleltetést javasol
ipari_kes1 #elfogadási tartomány, egységgyök folyamat

#-------------------------------DF-GLS teszt m1, trend adatokra---------------------
ujm1_kes1<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 1))
ujm1_kes1_SSE <- sum(ujm1_kes1@testreg$residuals^2)
ujm1_kes1_k <- nrow(ujm1_kes1@testreg$coefficients) #number of parameters
N <- nrow(lnadatok)#number of observations
ujm1_kes1_bic<- (ujm1_kes1_SSE/N)*N^((ujm1_kes1_k+1)/N)
ujm1_kes1_bic 

ujm1_kes2<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 2))
ujm1_kes2_SSE <- sum(ujm1_kes2@testreg$residuals^2)
ujm1_kes2_k <- nrow(ujm1_kes2@testreg$coefficients) #number of parameters
ujm1_kes2_bic<- (ujm1_kes2_SSE/N)*N^((ujm1_kes2_k+1)/N)
ujm1_kes2_bic

ujm1_kes3<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 3))
ujm1_kes3_SSE <- sum(ujm1_kes3@testreg$residuals^2)
ujm1_kes3_k <- nrow(ujm1_kes3@testreg$coefficients) #number of parameters
ujm1_kes3_bic<- (ujm1_kes3_SSE/N)*N^((ujm1_kes3_k+1)/N)
ujm1_kes3_bic

ujm1_kes4<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 4))
ujm1_kes4_SSE <- sum(ujm1_kes4@testreg$residuals^2)
ujm1_kes4_k <- nrow(ujm1_kes4@testreg$coefficients) #number of parameters
ujm1_kes4_bic<- (ujm1_kes4_SSE/N)*N^((ujm1_kes4_k+1)/N)
ujm1_kes4_bic

ujm1_kes5<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 5))
ujm1_kes5_SSE <- sum(ujm1_kes5@testreg$residuals^2)
ujm1_kes5_k <- nrow(ujm1_kes5@testreg$coefficients) #number of parameters
ujm1_kes5_bic<- (ujm1_kes5_SSE/N)*N^((ujm1_kes5_k+1)/N)
ujm1_kes5_bic

ujm1_kes6<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 6))
ujm1_kes6_SSE <- sum(ujm1_kes6@testreg$residuals^2)
ujm1_kes6_k <- nrow(ujm1_kes6@testreg$coefficients) #number of parameters
ujm1_kes6_bic<- (ujm1_kes6_SSE/N)*N^((ujm1_kes6_k+1)/N)
ujm1_kes6_bic

ujm1_kes7<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 7))
ujm1_kes7_SSE <- sum(ujm1_kes7@testreg$residuals^2)
ujm1_kes7_k <- nrow(ujm1_kes7@testreg$coefficients) #number of parameters
ujm1_kes7_bic<- (ujm1_kes7_SSE/N)*N^((ujm1_kes7_k+1)/N)
ujm1_kes7_bic

ujm1_kes8<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 8))
ujm1_kes8_SSE <- sum(ujm1_kes8@testreg$residuals^2)
ujm1_kes8_k <- nrow(ujm1_kes8@testreg$coefficients) #number of parameters
ujm1_kes8_bic<- (ujm1_kes8_SSE/N)*N^((ujm1_kes8_k+1)/N)
ujm1_kes8_bic

ujm1_kes9<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 9))
ujm1_kes9_SSE <- sum(ujm1_kes9@testreg$residuals^2)
ujm1_kes9_k <- nrow(ujm1_kes9@testreg$coefficients) #number of parameters
ujm1_kes9_bic<- (ujm1_kes9_SSE/N)*N^((ujm1_kes9_k+1)/N)
ujm1_kes9_bic

ujm1_kes10<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 10))
ujm1_kes10_SSE <- sum(ujm1_kes10@testreg$residuals^2)
ujm1_kes10_k <- nrow(ujm1_kes10@testreg$coefficients) #number of parameters
ujm1_kes10_bic<- (ujm1_kes10_SSE/N)*N^((ujm1_kes10_k+1)/N)
ujm1_kes10_bic

ujm1_kes11<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 11))
ujm1_kes11_SSE <- sum(ujm1_kes11@testreg$residuals^2)
ujm1_kes11_k <- nrow(ujm1_kes11@testreg$coefficients) #number of parameters
ujm1_kes11_bic<- (ujm1_kes11_SSE/N)*N^((ujm1_kes11_k+1)/N)
ujm1_kes11_bic

ujm1_kes12<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 12))
ujm1_kes12_SSE <- sum(ujm1_kes12@testreg$residuals^2)
ujm1_kes12_k <- nrow(ujm1_kes12@testreg$coefficients) #number of parameters
ujm1_kes12_bic<- (ujm1_kes12_SSE/N)*N^((ujm1_kes12_k+1)/N)
ujm1_kes12_bic

ujm1_kes13<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "trend", lag.max = 13))
ujm1_kes13_SSE <- sum(ujm1_kes13@testreg$residuals^2)
ujm1_kes13_k <- nrow(ujm1_kes13@testreg$coefficients) #number of parameters
ujm1_kes13_bic<- (ujm1_kes13_SSE/N)*N^((ujm1_kes13_k+1)/N)
ujm1_kes13_bic

min(ujm1_kes1_bic, ujm1_kes2_bic, ujm1_kes3_bic, ujm1_kes4_bic, ujm1_kes5_bic, ujm1_kes6_bic, ujm1_kes7_bic, ujm1_kes8_bic, ujm1_kes9_bic, ujm1_kes10_bic, ujm1_kes11_bic, ujm1_kes12_bic, ujm1_kes13_bic)
#7 késleltetést javasol
ujm1_kes7 #elfodaási tartomány --> egységgyök folyamat

#-------------------------------DF-GLS teszt m1, constant adatokra---------------------
ujm1_kes1<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 1))
ujm1_kes1_SSE <- sum(ujm1_kes1@testreg$residuals^2)
ujm1_kes1_k <- nrow(ujm1_kes1@testreg$coefficients) #number of parameters
N <- nrow(lnadatok)#number of observations
ujm1_kes1_bic<- (ujm1_kes1_SSE/N)*N^((ujm1_kes1_k+1)/N)
ujm1_kes1_bic 

ujm1_kes2<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 2))
ujm1_kes2_SSE <- sum(ujm1_kes2@testreg$residuals^2)
ujm1_kes2_k <- nrow(ujm1_kes2@testreg$coefficients) #number of parameters
ujm1_kes2_bic<- (ujm1_kes2_SSE/N)*N^((ujm1_kes2_k+1)/N)
ujm1_kes2_bic

ujm1_kes3<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 3))
ujm1_kes3_SSE <- sum(ujm1_kes3@testreg$residuals^2)
ujm1_kes3_k <- nrow(ujm1_kes3@testreg$coefficients) #number of parameters
ujm1_kes3_bic<- (ujm1_kes3_SSE/N)*N^((ujm1_kes3_k+1)/N)
ujm1_kes3_bic

ujm1_kes4<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 4))
ujm1_kes4_SSE <- sum(ujm1_kes4@testreg$residuals^2)
ujm1_kes4_k <- nrow(ujm1_kes4@testreg$coefficients) #number of parameters
ujm1_kes4_bic<- (ujm1_kes4_SSE/N)*N^((ujm1_kes4_k+1)/N)
ujm1_kes4_bic

ujm1_kes5<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 5))
ujm1_kes5_SSE <- sum(ujm1_kes5@testreg$residuals^2)
ujm1_kes5_k <- nrow(ujm1_kes5@testreg$coefficients) #number of parameters
ujm1_kes5_bic<- (ujm1_kes5_SSE/N)*N^((ujm1_kes5_k+1)/N)
ujm1_kes5_bic

ujm1_kes6<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 6))
ujm1_kes6_SSE <- sum(ujm1_kes6@testreg$residuals^2)
ujm1_kes6_k <- nrow(ujm1_kes6@testreg$coefficients) #number of parameters
ujm1_kes6_bic<- (ujm1_kes6_SSE/N)*N^((ujm1_kes6_k+1)/N)
ujm1_kes6_bic

ujm1_kes7<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 7))
ujm1_kes7_SSE <- sum(ujm1_kes7@testreg$residuals^2)
ujm1_kes7_k <- nrow(ujm1_kes7@testreg$coefficients) #number of parameters
ujm1_kes7_bic<- (ujm1_kes7_SSE/N)*N^((ujm1_kes7_k+1)/N)
ujm1_kes7_bic

ujm1_kes8<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 8))
ujm1_kes8_SSE <- sum(ujm1_kes8@testreg$residuals^2)
ujm1_kes8_k <- nrow(ujm1_kes8@testreg$coefficients) #number of parameters
ujm1_kes8_bic<- (ujm1_kes8_SSE/N)*N^((ujm1_kes8_k+1)/N)
ujm1_kes8_bic

ujm1_kes9<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 9))
ujm1_kes9_SSE <- sum(ujm1_kes9@testreg$residuals^2)
ujm1_kes9_k <- nrow(ujm1_kes9@testreg$coefficients) #number of parameters
ujm1_kes9_bic<- (ujm1_kes9_SSE/N)*N^((ujm1_kes9_k+1)/N)
ujm1_kes9_bic

ujm1_kes10<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 10))
ujm1_kes10_SSE <- sum(ujm1_kes10@testreg$residuals^2)
ujm1_kes10_k <- nrow(ujm1_kes10@testreg$coefficients) #number of parameters
ujm1_kes10_bic<- (ujm1_kes10_SSE/N)*N^((ujm1_kes10_k+1)/N)
ujm1_kes10_bic

ujm1_kes11<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 11))
ujm1_kes11_SSE <- sum(ujm1_kes11@testreg$residuals^2)
ujm1_kes11_k <- nrow(ujm1_kes11@testreg$coefficients) #number of parameters
ujm1_kes11_bic<- (ujm1_kes11_SSE/N)*N^((ujm1_kes11_k+1)/N)
ujm1_kes11_bic

ujm1_kes12<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 12))
ujm1_kes12_SSE <- sum(ujm1_kes12@testreg$residuals^2)
ujm1_kes12_k <- nrow(ujm1_kes12@testreg$coefficients) #number of parameters
ujm1_kes12_bic<- (ujm1_kes12_SSE/N)*N^((ujm1_kes12_k+1)/N)
ujm1_kes12_bic

ujm1_kes13<- summary(ur.ers(lnadatok$ujm1, type=c("DF-GLS"), model = "constant", lag.max = 13))
ujm1_kes13_SSE <- sum(ujm1_kes13@testreg$residuals^2)
ujm1_kes13_k <- nrow(ujm1_kes13@testreg$coefficients) #number of parameters
ujm1_kes13_bic<- (ujm1_kes13_SSE/N)*N^((ujm1_kes13_k+1)/N)
ujm1_kes13_bic

min(ujm1_kes1_bic, ujm1_kes2_bic, ujm1_kes3_bic, ujm1_kes4_bic, ujm1_kes5_bic, ujm1_kes6_bic, ujm1_kes7_bic, ujm1_kes8_bic, ujm1_kes9_bic, ujm1_kes10_bic, ujm1_kes11_bic, ujm1_kes12_bic, ujm1_kes13_bic)
#7 késleltetést javasol
ujm1_kes7 #elfodaási tartomány --> egységgyök folyamat

#-------------------------------DF-GLS teszt mezőgazdasági, trend adatokra---------------------
ujmezőgazdasági_kes1<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 1))
ujmezőgazdasági_kes1_SSE <- sum(ujmezőgazdasági_kes1@testreg$residuals^2)
ujmezőgazdasági_kes1_k <- nrow(ujmezőgazdasági_kes1@testreg$coefficients) #number of parameters
N <- nrow(lnadatok)#number of observations
ujmezőgazdasági_kes1_bic<- (ujmezőgazdasági_kes1_SSE/N)*N^((ujmezőgazdasági_kes1_k+1)/N)
ujmezőgazdasági_kes1_bic 

ujmezőgazdasági_kes2<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 2))
ujmezőgazdasági_kes2_SSE <- sum(ujmezőgazdasági_kes2@testreg$residuals^2)
ujmezőgazdasági_kes2_k <- nrow(ujmezőgazdasági_kes2@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes2_bic<- (ujmezőgazdasági_kes2_SSE/N)*N^((ujmezőgazdasági_kes2_k+1)/N)
ujmezőgazdasági_kes2_bic

ujmezőgazdasági_kes3<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 3))
ujmezőgazdasági_kes3_SSE <- sum(ujmezőgazdasági_kes3@testreg$residuals^2)
ujmezőgazdasági_kes3_k <- nrow(ujmezőgazdasági_kes3@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes3_bic<- (ujmezőgazdasági_kes3_SSE/N)*N^((ujmezőgazdasági_kes3_k+1)/N)
ujmezőgazdasági_kes3_bic

ujmezőgazdasági_kes4<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 4))
ujmezőgazdasági_kes4_SSE <- sum(ujmezőgazdasági_kes4@testreg$residuals^2)
ujmezőgazdasági_kes4_k <- nrow(ujmezőgazdasági_kes4@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes4_bic<- (ujmezőgazdasági_kes4_SSE/N)*N^((ujmezőgazdasági_kes4_k+1)/N)
ujmezőgazdasági_kes4_bic

ujmezőgazdasági_kes5<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 5))
ujmezőgazdasági_kes5_SSE <- sum(ujmezőgazdasági_kes5@testreg$residuals^2)
ujmezőgazdasági_kes5_k <- nrow(ujmezőgazdasági_kes5@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes5_bic<- (ujmezőgazdasági_kes5_SSE/N)*N^((ujmezőgazdasági_kes5_k+1)/N)
ujmezőgazdasági_kes5_bic

ujmezőgazdasági_kes6<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 6))
ujmezőgazdasági_kes6_SSE <- sum(ujmezőgazdasági_kes6@testreg$residuals^2)
ujmezőgazdasági_kes6_k <- nrow(ujmezőgazdasági_kes6@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes6_bic<- (ujmezőgazdasági_kes6_SSE/N)*N^((ujmezőgazdasági_kes6_k+1)/N)
ujmezőgazdasági_kes6_bic

ujmezőgazdasági_kes7<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 7))
ujmezőgazdasági_kes7_SSE <- sum(ujmezőgazdasági_kes7@testreg$residuals^2)
ujmezőgazdasági_kes7_k <- nrow(ujmezőgazdasági_kes7@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes7_bic<- (ujmezőgazdasági_kes7_SSE/N)*N^((ujmezőgazdasági_kes7_k+1)/N)
ujmezőgazdasági_kes7_bic

ujmezőgazdasági_kes8<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 8))
ujmezőgazdasági_kes8_SSE <- sum(ujmezőgazdasági_kes8@testreg$residuals^2)
ujmezőgazdasági_kes8_k <- nrow(ujmezőgazdasági_kes8@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes8_bic<- (ujmezőgazdasági_kes8_SSE/N)*N^((ujmezőgazdasági_kes8_k+1)/N)
ujmezőgazdasági_kes8_bic

ujmezőgazdasági_kes9<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 9))
ujmezőgazdasági_kes9_SSE <- sum(ujmezőgazdasági_kes9@testreg$residuals^2)
ujmezőgazdasági_kes9_k <- nrow(ujmezőgazdasági_kes9@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes9_bic<- (ujmezőgazdasági_kes9_SSE/N)*N^((ujmezőgazdasági_kes9_k+1)/N)
ujmezőgazdasági_kes9_bic

ujmezőgazdasági_kes10<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 10))
ujmezőgazdasági_kes10_SSE <- sum(ujmezőgazdasági_kes10@testreg$residuals^2)
ujmezőgazdasági_kes10_k <- nrow(ujmezőgazdasági_kes10@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes10_bic<- (ujmezőgazdasági_kes10_SSE/N)*N^((ujmezőgazdasági_kes10_k+1)/N)
ujmezőgazdasági_kes10_bic

ujmezőgazdasági_kes11<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 11))
ujmezőgazdasági_kes11_SSE <- sum(ujmezőgazdasági_kes11@testreg$residuals^2)
ujmezőgazdasági_kes11_k <- nrow(ujmezőgazdasági_kes11@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes11_bic<- (ujmezőgazdasági_kes11_SSE/N)*N^((ujmezőgazdasági_kes11_k+1)/N)
ujmezőgazdasági_kes11_bic

ujmezőgazdasági_kes12<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 12))
ujmezőgazdasági_kes12_SSE <- sum(ujmezőgazdasági_kes12@testreg$residuals^2)
ujmezőgazdasági_kes12_k <- nrow(ujmezőgazdasági_kes12@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes12_bic<- (ujmezőgazdasági_kes12_SSE/N)*N^((ujmezőgazdasági_kes12_k+1)/N)
ujmezőgazdasági_kes12_bic

ujmezőgazdasági_kes13<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "trend", lag.max = 13))
ujmezőgazdasági_kes13_SSE <- sum(ujmezőgazdasági_kes13@testreg$residuals^2)
ujmezőgazdasági_kes13_k <- nrow(ujmezőgazdasági_kes13@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes13_bic<- (ujmezőgazdasági_kes13_SSE/N)*N^((ujmezőgazdasági_kes13_k+1)/N)
ujmezőgazdasági_kes13_bic

min(ujmezőgazdasági_kes1_bic, ujmezőgazdasági_kes2_bic, ujmezőgazdasági_kes3_bic, ujmezőgazdasági_kes4_bic, ujmezőgazdasági_kes5_bic, ujmezőgazdasági_kes6_bic, ujmezőgazdasági_kes7_bic, ujmezőgazdasági_kes8_bic, ujmezőgazdasági_kes9_bic, ujmezőgazdasági_kes10_bic, ujmezőgazdasági_kes11_bic, ujmezőgazdasági_kes12_bic, ujmezőgazdasági_kes13_bic)
#12 késleltetést javasol
ujmezőgazdasági_kes12 #határeset, de inkább stacioner

#-------------------------------DF-GLS teszt mezőgazdasági, constant adatokra---------------------
ujmezőgazdasági_kes1<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 1))
ujmezőgazdasági_kes1_SSE <- sum(ujmezőgazdasági_kes1@testreg$residuals^2)
ujmezőgazdasági_kes1_k <- nrow(ujmezőgazdasági_kes1@testreg$coefficients) #number of parameters
N <- nrow(lnadatok)#number of observations
ujmezőgazdasági_kes1_bic<- (ujmezőgazdasági_kes1_SSE/N)*N^((ujmezőgazdasági_kes1_k+1)/N)
ujmezőgazdasági_kes1_bic 

ujmezőgazdasági_kes2<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 2))
ujmezőgazdasági_kes2_SSE <- sum(ujmezőgazdasági_kes2@testreg$residuals^2)
ujmezőgazdasági_kes2_k <- nrow(ujmezőgazdasági_kes2@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes2_bic<- (ujmezőgazdasági_kes2_SSE/N)*N^((ujmezőgazdasági_kes2_k+1)/N)
ujmezőgazdasági_kes2_bic

ujmezőgazdasági_kes3<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 3))
ujmezőgazdasági_kes3_SSE <- sum(ujmezőgazdasági_kes3@testreg$residuals^2)
ujmezőgazdasági_kes3_k <- nrow(ujmezőgazdasági_kes3@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes3_bic<- (ujmezőgazdasági_kes3_SSE/N)*N^((ujmezőgazdasági_kes3_k+1)/N)
ujmezőgazdasági_kes3_bic

ujmezőgazdasági_kes4<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 4))
ujmezőgazdasági_kes4_SSE <- sum(ujmezőgazdasági_kes4@testreg$residuals^2)
ujmezőgazdasági_kes4_k <- nrow(ujmezőgazdasági_kes4@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes4_bic<- (ujmezőgazdasági_kes4_SSE/N)*N^((ujmezőgazdasági_kes4_k+1)/N)
ujmezőgazdasági_kes4_bic

ujmezőgazdasági_kes5<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 5))
ujmezőgazdasági_kes5_SSE <- sum(ujmezőgazdasági_kes5@testreg$residuals^2)
ujmezőgazdasági_kes5_k <- nrow(ujmezőgazdasági_kes5@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes5_bic<- (ujmezőgazdasági_kes5_SSE/N)*N^((ujmezőgazdasági_kes5_k+1)/N)
ujmezőgazdasági_kes5_bic

ujmezőgazdasági_kes6<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 6))
ujmezőgazdasági_kes6_SSE <- sum(ujmezőgazdasági_kes6@testreg$residuals^2)
ujmezőgazdasági_kes6_k <- nrow(ujmezőgazdasági_kes6@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes6_bic<- (ujmezőgazdasági_kes6_SSE/N)*N^((ujmezőgazdasági_kes6_k+1)/N)
ujmezőgazdasági_kes6_bic

ujmezőgazdasági_kes7<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 7))
ujmezőgazdasági_kes7_SSE <- sum(ujmezőgazdasági_kes7@testreg$residuals^2)
ujmezőgazdasági_kes7_k <- nrow(ujmezőgazdasági_kes7@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes7_bic<- (ujmezőgazdasági_kes7_SSE/N)*N^((ujmezőgazdasági_kes7_k+1)/N)
ujmezőgazdasági_kes7_bic

ujmezőgazdasági_kes8<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 8))
ujmezőgazdasági_kes8_SSE <- sum(ujmezőgazdasági_kes8@testreg$residuals^2)
ujmezőgazdasági_kes8_k <- nrow(ujmezőgazdasági_kes8@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes8_bic<- (ujmezőgazdasági_kes8_SSE/N)*N^((ujmezőgazdasági_kes8_k+1)/N)
ujmezőgazdasági_kes8_bic

ujmezőgazdasági_kes9<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 9))
ujmezőgazdasági_kes9_SSE <- sum(ujmezőgazdasági_kes9@testreg$residuals^2)
ujmezőgazdasági_kes9_k <- nrow(ujmezőgazdasági_kes9@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes9_bic<- (ujmezőgazdasági_kes9_SSE/N)*N^((ujmezőgazdasági_kes9_k+1)/N)
ujmezőgazdasági_kes9_bic

ujmezőgazdasági_kes10<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 10))
ujmezőgazdasági_kes10_SSE <- sum(ujmezőgazdasági_kes10@testreg$residuals^2)
ujmezőgazdasági_kes10_k <- nrow(ujmezőgazdasági_kes10@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes10_bic<- (ujmezőgazdasági_kes10_SSE/N)*N^((ujmezőgazdasági_kes10_k+1)/N)
ujmezőgazdasági_kes10_bic

ujmezőgazdasági_kes11<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 11))
ujmezőgazdasági_kes11_SSE <- sum(ujmezőgazdasági_kes11@testreg$residuals^2)
ujmezőgazdasági_kes11_k <- nrow(ujmezőgazdasági_kes11@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes11_bic<- (ujmezőgazdasági_kes11_SSE/N)*N^((ujmezőgazdasági_kes11_k+1)/N)
ujmezőgazdasági_kes11_bic

ujmezőgazdasági_kes12<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 12))
ujmezőgazdasági_kes12_SSE <- sum(ujmezőgazdasági_kes12@testreg$residuals^2)
ujmezőgazdasági_kes12_k <- nrow(ujmezőgazdasági_kes12@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes12_bic<- (ujmezőgazdasági_kes12_SSE/N)*N^((ujmezőgazdasági_kes12_k+1)/N)
ujmezőgazdasági_kes12_bic

ujmezőgazdasági_kes13<- summary(ur.ers(lnadatok$ujmezőgazdasági, type=c("DF-GLS"), model = "constant", lag.max = 13))
ujmezőgazdasági_kes13_SSE <- sum(ujmezőgazdasági_kes13@testreg$residuals^2)
ujmezőgazdasági_kes13_k <- nrow(ujmezőgazdasági_kes13@testreg$coefficients) #number of parameters
ujmezőgazdasági_kes13_bic<- (ujmezőgazdasági_kes13_SSE/N)*N^((ujmezőgazdasági_kes13_k+1)/N)
ujmezőgazdasági_kes13_bic

min(ujmezőgazdasági_kes1_bic, ujmezőgazdasági_kes2_bic, ujmezőgazdasági_kes3_bic, ujmezőgazdasági_kes4_bic, ujmezőgazdasági_kes5_bic, ujmezőgazdasági_kes6_bic, ujmezőgazdasági_kes7_bic, ujmezőgazdasági_kes8_bic, ujmezőgazdasági_kes9_bic, ujmezőgazdasági_kes10_bic, ujmezőgazdasági_kes11_bic, ujmezőgazdasági_kes12_bic, ujmezőgazdasági_kes13_bic)
#12 késleltetést javasol
ujmezőgazdasági_kes12 #stacioner folyamat

#-------------------------------DF-GLS teszt árfolyam, trend adatokra---------------------
árfolyam_bázis_kes1<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 1))
árfolyam_bázis_kes1_SSE <- sum(árfolyam_bázis_kes1@testreg$residuals^2)
árfolyam_bázis_kes1_k <- nrow(árfolyam_bázis_kes1@testreg$coefficients) #number of parameters
N <- nrow(lnadatok)#number of observations
árfolyam_bázis_kes1_bic<- (árfolyam_bázis_kes1_SSE/N)*N^((árfolyam_bázis_kes1_k+1)/N)
árfolyam_bázis_kes1_bic 

árfolyam_bázis_kes2<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 2))
árfolyam_bázis_kes2_SSE <- sum(árfolyam_bázis_kes2@testreg$residuals^2)
árfolyam_bázis_kes2_k <- nrow(árfolyam_bázis_kes2@testreg$coefficients) #number of parameters
árfolyam_bázis_kes2_bic<- (árfolyam_bázis_kes2_SSE/N)*N^((árfolyam_bázis_kes2_k+1)/N)
árfolyam_bázis_kes2_bic

árfolyam_bázis_kes3<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 3))
árfolyam_bázis_kes3_SSE <- sum(árfolyam_bázis_kes3@testreg$residuals^2)
árfolyam_bázis_kes3_k <- nrow(árfolyam_bázis_kes3@testreg$coefficients) #number of parameters
árfolyam_bázis_kes3_bic<- (árfolyam_bázis_kes3_SSE/N)*N^((árfolyam_bázis_kes3_k+1)/N)
árfolyam_bázis_kes3_bic

árfolyam_bázis_kes4<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 4))
árfolyam_bázis_kes4_SSE <- sum(árfolyam_bázis_kes4@testreg$residuals^2)
árfolyam_bázis_kes4_k <- nrow(árfolyam_bázis_kes4@testreg$coefficients) #number of parameters
árfolyam_bázis_kes4_bic<- (árfolyam_bázis_kes4_SSE/N)*N^((árfolyam_bázis_kes4_k+1)/N)
árfolyam_bázis_kes4_bic

árfolyam_bázis_kes5<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 5))
árfolyam_bázis_kes5_SSE <- sum(árfolyam_bázis_kes5@testreg$residuals^2)
árfolyam_bázis_kes5_k <- nrow(árfolyam_bázis_kes5@testreg$coefficients) #number of parameters
árfolyam_bázis_kes5_bic<- (árfolyam_bázis_kes5_SSE/N)*N^((árfolyam_bázis_kes5_k+1)/N)
árfolyam_bázis_kes5_bic

árfolyam_bázis_kes6<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 6))
árfolyam_bázis_kes6_SSE <- sum(árfolyam_bázis_kes6@testreg$residuals^2)
árfolyam_bázis_kes6_k <- nrow(árfolyam_bázis_kes6@testreg$coefficients) #number of parameters
árfolyam_bázis_kes6_bic<- (árfolyam_bázis_kes6_SSE/N)*N^((árfolyam_bázis_kes6_k+1)/N)
árfolyam_bázis_kes6_bic

árfolyam_bázis_kes7<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 7))
árfolyam_bázis_kes7_SSE <- sum(árfolyam_bázis_kes7@testreg$residuals^2)
árfolyam_bázis_kes7_k <- nrow(árfolyam_bázis_kes7@testreg$coefficients) #number of parameters
árfolyam_bázis_kes7_bic<- (árfolyam_bázis_kes7_SSE/N)*N^((árfolyam_bázis_kes7_k+1)/N)
árfolyam_bázis_kes7_bic

árfolyam_bázis_kes8<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 8))
árfolyam_bázis_kes8_SSE <- sum(árfolyam_bázis_kes8@testreg$residuals^2)
árfolyam_bázis_kes8_k <- nrow(árfolyam_bázis_kes8@testreg$coefficients) #number of parameters
árfolyam_bázis_kes8_bic<- (árfolyam_bázis_kes8_SSE/N)*N^((árfolyam_bázis_kes8_k+1)/N)
árfolyam_bázis_kes8_bic

árfolyam_bázis_kes9<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 9))
árfolyam_bázis_kes9_SSE <- sum(árfolyam_bázis_kes9@testreg$residuals^2)
árfolyam_bázis_kes9_k <- nrow(árfolyam_bázis_kes9@testreg$coefficients) #number of parameters
árfolyam_bázis_kes9_bic<- (árfolyam_bázis_kes9_SSE/N)*N^((árfolyam_bázis_kes9_k+1)/N)
árfolyam_bázis_kes9_bic

árfolyam_bázis_kes10<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 10))
árfolyam_bázis_kes10_SSE <- sum(árfolyam_bázis_kes10@testreg$residuals^2)
árfolyam_bázis_kes10_k <- nrow(árfolyam_bázis_kes10@testreg$coefficients) #number of parameters
árfolyam_bázis_kes10_bic<- (árfolyam_bázis_kes10_SSE/N)*N^((árfolyam_bázis_kes10_k+1)/N)
árfolyam_bázis_kes10_bic

árfolyam_bázis_kes11<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 11))
árfolyam_bázis_kes11_SSE <- sum(árfolyam_bázis_kes11@testreg$residuals^2)
árfolyam_bázis_kes11_k <- nrow(árfolyam_bázis_kes11@testreg$coefficients) #number of parameters
árfolyam_bázis_kes11_bic<- (árfolyam_bázis_kes11_SSE/N)*N^((árfolyam_bázis_kes11_k+1)/N)
árfolyam_bázis_kes11_bic

árfolyam_bázis_kes12<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 12))
árfolyam_bázis_kes12_SSE <- sum(árfolyam_bázis_kes12@testreg$residuals^2)
árfolyam_bázis_kes12_k <- nrow(árfolyam_bázis_kes12@testreg$coefficients) #number of parameters
árfolyam_bázis_kes12_bic<- (árfolyam_bázis_kes12_SSE/N)*N^((árfolyam_bázis_kes12_k+1)/N)
árfolyam_bázis_kes12_bic

árfolyam_bázis_kes13<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "trend", lag.max = 13))
árfolyam_bázis_kes13_SSE <- sum(árfolyam_bázis_kes13@testreg$residuals^2)
árfolyam_bázis_kes13_k <- nrow(árfolyam_bázis_kes13@testreg$coefficients) #number of parameters
árfolyam_bázis_kes13_bic<- (árfolyam_bázis_kes13_SSE/N)*N^((árfolyam_bázis_kes13_k+1)/N)
árfolyam_bázis_kes13_bic

min(árfolyam_bázis_kes1_bic, árfolyam_bázis_kes2_bic, árfolyam_bázis_kes3_bic, árfolyam_bázis_kes4_bic, árfolyam_bázis_kes5_bic, árfolyam_bázis_kes6_bic, árfolyam_bázis_kes7_bic, árfolyam_bázis_kes8_bic, árfolyam_bázis_kes9_bic, árfolyam_bázis_kes10_bic, árfolyam_bázis_kes11_bic, árfolyam_bázis_kes12_bic, árfolyam_bázis_kes13_bic)
#13 késleltetést javasol
árfolyam_bázis_kes13 #egységgyök folyamat

#-------------------------------DF-GLS teszt árfolyam, constant adatokra---------------------
árfolyam_bázis_kes1<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 1))
árfolyam_bázis_kes1_SSE <- sum(árfolyam_bázis_kes1@testreg$residuals^2)
árfolyam_bázis_kes1_k <- nrow(árfolyam_bázis_kes1@testreg$coefficients) #number of parameters
N <- nrow(lnadatok)#number of observations
árfolyam_bázis_kes1_bic<- (árfolyam_bázis_kes1_SSE/N)*N^((árfolyam_bázis_kes1_k+1)/N)
árfolyam_bázis_kes1_bic 

árfolyam_bázis_kes2<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 2))
árfolyam_bázis_kes2_SSE <- sum(árfolyam_bázis_kes2@testreg$residuals^2)
árfolyam_bázis_kes2_k <- nrow(árfolyam_bázis_kes2@testreg$coefficients) #number of parameters
árfolyam_bázis_kes2_bic<- (árfolyam_bázis_kes2_SSE/N)*N^((árfolyam_bázis_kes2_k+1)/N)
árfolyam_bázis_kes2_bic

árfolyam_bázis_kes3<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 3))
árfolyam_bázis_kes3_SSE <- sum(árfolyam_bázis_kes3@testreg$residuals^2)
árfolyam_bázis_kes3_k <- nrow(árfolyam_bázis_kes3@testreg$coefficients) #number of parameters
árfolyam_bázis_kes3_bic<- (árfolyam_bázis_kes3_SSE/N)*N^((árfolyam_bázis_kes3_k+1)/N)
árfolyam_bázis_kes3_bic

árfolyam_bázis_kes4<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 4))
árfolyam_bázis_kes4_SSE <- sum(árfolyam_bázis_kes4@testreg$residuals^2)
árfolyam_bázis_kes4_k <- nrow(árfolyam_bázis_kes4@testreg$coefficients) #number of parameters
árfolyam_bázis_kes4_bic<- (árfolyam_bázis_kes4_SSE/N)*N^((árfolyam_bázis_kes4_k+1)/N)
árfolyam_bázis_kes4_bic

árfolyam_bázis_kes5<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 5))
árfolyam_bázis_kes5_SSE <- sum(árfolyam_bázis_kes5@testreg$residuals^2)
árfolyam_bázis_kes5_k <- nrow(árfolyam_bázis_kes5@testreg$coefficients) #number of parameters
árfolyam_bázis_kes5_bic<- (árfolyam_bázis_kes5_SSE/N)*N^((árfolyam_bázis_kes5_k+1)/N)
árfolyam_bázis_kes5_bic

árfolyam_bázis_kes6<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 6))
árfolyam_bázis_kes6_SSE <- sum(árfolyam_bázis_kes6@testreg$residuals^2)
árfolyam_bázis_kes6_k <- nrow(árfolyam_bázis_kes6@testreg$coefficients) #number of parameters
árfolyam_bázis_kes6_bic<- (árfolyam_bázis_kes6_SSE/N)*N^((árfolyam_bázis_kes6_k+1)/N)
árfolyam_bázis_kes6_bic

árfolyam_bázis_kes7<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 7))
árfolyam_bázis_kes7_SSE <- sum(árfolyam_bázis_kes7@testreg$residuals^2)
árfolyam_bázis_kes7_k <- nrow(árfolyam_bázis_kes7@testreg$coefficients) #number of parameters
árfolyam_bázis_kes7_bic<- (árfolyam_bázis_kes7_SSE/N)*N^((árfolyam_bázis_kes7_k+1)/N)
árfolyam_bázis_kes7_bic

árfolyam_bázis_kes8<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 8))
árfolyam_bázis_kes8_SSE <- sum(árfolyam_bázis_kes8@testreg$residuals^2)
árfolyam_bázis_kes8_k <- nrow(árfolyam_bázis_kes8@testreg$coefficients) #number of parameters
árfolyam_bázis_kes8_bic<- (árfolyam_bázis_kes8_SSE/N)*N^((árfolyam_bázis_kes8_k+1)/N)
árfolyam_bázis_kes8_bic

árfolyam_bázis_kes9<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 9))
árfolyam_bázis_kes9_SSE <- sum(árfolyam_bázis_kes9@testreg$residuals^2)
árfolyam_bázis_kes9_k <- nrow(árfolyam_bázis_kes9@testreg$coefficients) #number of parameters
árfolyam_bázis_kes9_bic<- (árfolyam_bázis_kes9_SSE/N)*N^((árfolyam_bázis_kes9_k+1)/N)
árfolyam_bázis_kes9_bic

árfolyam_bázis_kes10<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 10))
árfolyam_bázis_kes10_SSE <- sum(árfolyam_bázis_kes10@testreg$residuals^2)
árfolyam_bázis_kes10_k <- nrow(árfolyam_bázis_kes10@testreg$coefficients) #number of parameters
árfolyam_bázis_kes10_bic<- (árfolyam_bázis_kes10_SSE/N)*N^((árfolyam_bázis_kes10_k+1)/N)
árfolyam_bázis_kes10_bic

árfolyam_bázis_kes11<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 11))
árfolyam_bázis_kes11_SSE <- sum(árfolyam_bázis_kes11@testreg$residuals^2)
árfolyam_bázis_kes11_k <- nrow(árfolyam_bázis_kes11@testreg$coefficients) #number of parameters
árfolyam_bázis_kes11_bic<- (árfolyam_bázis_kes11_SSE/N)*N^((árfolyam_bázis_kes11_k+1)/N)
árfolyam_bázis_kes11_bic

árfolyam_bázis_kes12<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 12))
árfolyam_bázis_kes12_SSE <- sum(árfolyam_bázis_kes12@testreg$residuals^2)
árfolyam_bázis_kes12_k <- nrow(árfolyam_bázis_kes12@testreg$coefficients) #number of parameters
árfolyam_bázis_kes12_bic<- (árfolyam_bázis_kes12_SSE/N)*N^((árfolyam_bázis_kes12_k+1)/N)
árfolyam_bázis_kes12_bic

árfolyam_bázis_kes13<- summary(ur.ers(lnadatok$árfolyam_bázis, type=c("DF-GLS"), model = "constant", lag.max = 13))
árfolyam_bázis_kes13_SSE <- sum(árfolyam_bázis_kes13@testreg$residuals^2)
árfolyam_bázis_kes13_k <- nrow(árfolyam_bázis_kes13@testreg$coefficients) #number of parameters
árfolyam_bázis_kes13_bic<- (árfolyam_bázis_kes13_SSE/N)*N^((árfolyam_bázis_kes13_k+1)/N)
árfolyam_bázis_kes13_bic

min(árfolyam_bázis_kes1_bic, árfolyam_bázis_kes2_bic, árfolyam_bázis_kes3_bic, árfolyam_bázis_kes4_bic, árfolyam_bázis_kes5_bic, árfolyam_bázis_kes6_bic, árfolyam_bázis_kes7_bic, árfolyam_bázis_kes8_bic, árfolyam_bázis_kes9_bic, árfolyam_bázis_kes10_bic, árfolyam_bázis_kes11_bic, árfolyam_bázis_kes12_bic, árfolyam_bázis_kes13_bic)
#13 késleltetést javasol
árfolyam_bázis_kes13 #hegységgyök folyamat










