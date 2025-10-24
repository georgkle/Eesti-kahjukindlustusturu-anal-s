#install.packages("readxl")
#install.packages("haven")
#install.packages("tidyr")
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(lubridate)
library(Polychrome)
library(scales)
library(stringr)
#puhastame andmestiku
andmed_a <- read_excel("C:\\Users\\arvut\\OneDrive - Tartu Ülikool\\ülikool\\R\\projekt\\kindlustusmaksed.xlsx", skip=1)
View(andmed_a)
andmed_a <- andmed_a[c(1:22), ]
colSums(andmed_a == "."| andmed_a == "..")

#salvestame andmed_a uude andmestikku, kus on välja jäätud veerud, kus esineb "." või ".." väärtuseid
andmed <- andmed_a[, colSums(andmed_a == "." | andmed_a == "..", na.rm = TRUE) == 0]
View(andmed)


#arvutame saadud kindlustusmaksed kokku aastate kaupa

#võtame read, kus on aasta olemas
andmed_aasta <- andmed %>%
  filter(!is.na(...1))
View(andmed_aasta)

#paneme veeru nimeks aasta ja lisame veerule õige tüübi
andmed_aasta <- andmed_aasta %>%
  rename(Aasta = ...1)

#paneme väärtuste "Saadud kindlustusmaksed" veeru nimeks SaadudMaksed
andmed_aasta <- andmed_aasta %>%
  rename(SaadudMaksed = ...3)

andmed_aasta$Aasta <- as.numeric(andmed_aasta$Aasta)


#liidame kokku kindlustusmaksed aastate kaupa kõikide seltside peale kokku

summa_aastate_kaupa <- andmed_aasta %>%
  filter(SaadudMaksed == "Saadud kindlustusmaksed") %>%
  group_by(Aasta) %>%
  summarise(kokku = sum(across(where(is.numeric)), na.rm=TRUE))
View(andmed)

#arvutame firmade turuosad saadud kindlustusmaksete järgi aastate kaupa
fondid <- andmed_aasta %>%
  filter(SaadudMaksed == "Saadud kindlustusmaksed")
View(fondid)

#lisame juurde kogusumma
fondid <- fondid %>%
  left_join(summa_aastate_kaupa, by="Aasta")

#arvutame iga fondi osakaalu
fondid_osakaal <- fondid %>%
  mutate(across(where(is.numeric) & !matches("kokku") & !matches("Aasta"),
         ~ .x / kokku * 100,
         .names = "{.col}_osakaal"))
View(fondid_osakaal)

#jätame ebavajaliku info välja
loplik_fondid_osakaal <- fondid_osakaal %>%
  select(Aasta, ends_with("_osakaal"))

View(loplik_fondid_osakaal)

#lõpuks kontrollin, kas arvutused klapivad, ehk kas iga aasta kohta saame osakaalu kokku 100%
kontroll <- loplik_fondid_osakaal %>%
  rowwise() %>%
  mutate(kokku_osakaal = sum(c_across(ends_with("_osakaal"))))
View(kontroll)

#Et luua turuosade graafikut, viime andmestiku pikale kujule
loplik_fondid_osakaal_pikk <- loplik_fondid_osakaal %>%
  pivot_longer(-Aasta, names_to = "Selts", values_to = "Osakaal")

loplik_fondid_osakaal_pikk <- loplik_fondid_osakaal_pikk %>%
  mutate(Selts = str_remove(Selts, "_osakaal"))

 
######################

#loeme sisse andmestiku kindlustusmaksete kohta kuude lõikes aastatest 2014-2024
kuudekaupa <- read_excel("C:\\Users\\arvut\\OneDrive - Tartu Ülikool\\ülikool\\R\\projekt\\kindlustusmaksed_kuudekaupa.xlsx")


#muudame veeru ..1 ja ..2 vastavalt "Aasta" ja "Kuu
kuudekaupa <- kuudekaupa %>%
  rename(Aasta = ...1)

kuudekaupa <- kuudekaupa %>%
  rename(Kuu = ...2)

#eemaldame tühjad read lõpust ja salvestame muudetud andmestiku uude
uuskuudekaupa <- kuudekaupa[1:132, 1:12]

#lisame iga kuu ette vastava aasta (2014-2024)
uuskuudekaupa <- uuskuudekaupa %>%
  fill(Aasta)

#teeme uue veeru "Date", kus on kuu ja aasta kokkupandud
#esmalt faktoriseerime kuud
uuskuudekaupa$Kuu <- factor(uuskuudekaupa$Kuu, levels = c("Jaanuar", "Veebruar", "Märts",
                                                          "Aprill", "Mai", "Juuni",
                                                          "Juuli", "August", "September",
                                                          "Oktoober", "November", "Detsember"))
#lisame kuu numbri
uuskuudekaupa <- uuskuudekaupa %>%
  mutate(kuu_nr=as.integer(Kuu),
         Date = as.Date(paste(Aasta, kuu_nr, 1, sep = "-")))

#võtame veerud kuu numbri, aasta ja kuu kohta ära ja jätame ainult "Date", et andmeliiasust vältida
uuskuudekaupa <- uuskuudekaupa %>%
  mutate(kuu_nr = NULL,
         Aasta = NULL,
         Kuu = NULL)

#tõstame veeru "Date" andmestikus kõige ette, et oleks loetavamal kujul
uuskuudekaupa <- uuskuudekaupa %>%
  select(Date, everything())

#Viime andmestiku pikale kujule, et saaks teha mugavamalt andmestikuga tööd
uuskuudekaupa_pikk <- uuskuudekaupa %>%
  pivot_longer(cols = -Date,
               names_to="Selts",
               values_to="Makse")


#Teeme uue graafiku, mis on sarnane eelmisele, kuid kus kujutatakse kõike 5t aastat, selleks teeme tagasi veeru Aasta
#Et graafik oleks paremini loetav moodustame värvide vektori, kus on 10
#lihtsasti eristatavat värvi
värvid <- c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
)
hooajalisus_viimased5 <- uuskuudekaupa_pikk %>%
  filter(format(Date, "%Y")>="2020") %>%
  mutate(Aasta = format(Date, "%Y"))

aastad20_24_hooajalisus<-ggplot(hooajalisus_viimased5, aes(x=month(Date), y=Makse, colour=Selts)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~Aasta, ncol=5, scales="free_x") + 
  scale_colour_manual(values = värvid) +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6))

#Filtreerime välja 5 suuremat ning 5 väiksemat kindlustusfirmat kindlustusmaksete järgi

#esmalt loome värvid, et suure graafiku 10 värvi ühtiks ka väikeste graafikute värvidega.
värvid2 <- ggplot_build(aastad20_24_hooajalisus)$data[[1]]$colour
names(värvid2) <- unique(hooajalisus_viimased5$Selts)
#5 suuremat:
hooajalisus_top5 <- uuskuudekaupa_pikk %>%
  group_by(Selts) %>%
  summarise(summa=sum(Makse))
hooajalisus_top5 <- arrange(hooajalisus_top5, desc(summa))
hooajalisus_top5 <- hooajalisus_top5[1:5, ]

hooajalisus_top5 <- uuskuudekaupa_pikk %>%
  filter(Selts %in% hooajalisus_top5$Selts[1:5])

hooajalisus_top5 <- hooajalisus_top5 %>%
  mutate(Aasta=year(Date)) %>%
  filter(year(Date)>="2020")

#Teeme sarnase graafiku, kuid kus oleks top5 kindlustusmakse saajate kohta eraldi info
hooajalisus_kindlustusmaksed_5suuremat <- ggplot(hooajalisus_top5, aes(x=month(Date), y=Makse, colour=Selts)) + 
  geom_point() +
  geom_line() +
  scale_colour_manual(values=värvid2) +
  facet_wrap(~Aasta, ncol=5, scales="free_x") + 
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) 

#Nüüd analoogiliselt teeme 5 väiksema kohta:
hooajalisus_top5_2 <- uuskuudekaupa_pikk %>%
  group_by(Selts) %>%
  summarise(summa=sum(Makse))
hooajalisus_top5_2 <- arrange(hooajalisus_top5_2, desc(summa))
hooajalisus_top5_2 <- hooajalisus_top5_2[6:10, ]

hooajalisus_top5_2 <- uuskuudekaupa_pikk %>%
  filter(Selts %in% hooajalisus_top5_2$Selts[1:5])

hooajalisus_top5_2 <- hooajalisus_top5_2 %>%
  mutate(Aasta=year(Date)) %>%
  filter(year(Date)>="2020")

#Graafik 5 väiksema seltsi kohta:
hooajalisus_kindlustusmaksed_5väiksemat <- ggplot(hooajalisus_top5_2, aes(x=month(Date), y=Makse, colour=Selts)) + 
  geom_point() +
  geom_line() +
  scale_colour_manual(values=värvid2) +
  facet_wrap(~Aasta, ncol=5, scales="free_x") + 
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6))

#Kuna need graafikud iseloomustavad kindlustusmaksete toimumise aega, siis ei ütle see õnnetuste toimumiste kohta eriti midagi
#Pigem selle kohta, millised lepingud sõlmitud on (kui kindlustusmaksed on igakuu samad siis saab öelda, et rohkem on kuiseid lepinguid, igal kuul on sõlmitud aastasi lepinguid vm)
#Teeme täpselt samad sammud läbi ka väljamaksete kohta

#Loeme sisse andmestiku kuude lõikes väljamaksete kaupa

väljamaksed_kuud <- read_excel("C:\\Users\\arvut\\OneDrive - Tartu Ülikool\\ülikool\\R\\projekt\\väljamaksed.xlsx")
väljamaksed_kuud <- väljamaksed_kuud %>%
  rename("Aasta"=...1,
         "Kuu"=...2) %>%
  mutate(...3=NULL,
         ...4=NULL)

väljamaksed_kuud <- väljamaksed_kuud %>%
  fill(Aasta)


väljamaksed_kuud$Kuu <- factor(väljamaksed_kuud$Kuu, levels = c("Jaanuar", "Veebruar", "Märts",
                                                          "Aprill", "Mai", "Juuni",
                                                          "Juuli", "August", "September",
                                                          "Oktoober", "November", "Detsember"))

väljamaksed_kuud <- väljamaksed_kuud %>%
  mutate(kuu_nr=as.integer(Kuu),
         Date = as.Date(paste(Aasta, kuu_nr, 1, sep = "-")))


väljamaksed_kuud <- väljamaksed_kuud %>%
  mutate(kuu_nr = NULL,
         Aasta = NULL,
         Kuu = NULL)


väljamaksed_kuud <- väljamaksed_kuud %>%
  select(Date, everything())

väljamaksed_kuud <- väljamaksed_kuud %>%
  pivot_longer(cols = -Date,
               names_to="Selts",
               values_to="Makse")

#Oleks saanud ka kohe excelist importida ühes tabelis nii kindlustusmaksed kui ka väljamaksed, ei tea miks me
#Nii ei teinud ;D



#Filtreerime välja 5 suuremat ja väiksemat nagu ennegi

#5 suuremat
hooajalisusv_top5 <- väljamaksed_kuud %>%
  group_by(Selts) %>%
  summarise(summa=sum(Makse))
hooajalisusv_top5 <- arrange(hooajalisusv_top5, desc(summa))
hooajalisusv_top5 <- hooajalisusv_top5[1:5, ]

hooajalisusv_top5 <- väljamaksed_kuud %>%
  filter(Selts %in% hooajalisusv_top5$Selts[1:5])

hooajalisusv_top5 <- hooajalisusv_top5 %>%
  mutate(Aasta=year(Date)) %>%
  filter(year(Date)>="2020")

#Graafik 5le suuremale, jällegi sarnane eelnevale. Kasutame varem loodud värve
hooajalisus_väljamaksed_5suuremat <- ggplot(hooajalisusv_top5, aes(x=month(Date), y=Makse, colour=Selts)) + 
  geom_point() +
  geom_line() +
  scale_colour_manual(values=värvid2) +
  facet_wrap(~Aasta, ncol=5, scales="free_x") + 
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) 


#5 väiksemat
hooajalisusv_top5_2 <- väljamaksed_kuud %>%
  group_by(Selts) %>%
  summarise(summa=sum(Makse))
hooajalisusv_top5_2 <- arrange(hooajalisusv_top5_2, desc(summa))
hooajalisusv_top5_2 <- hooajalisusv_top5_2[6:10, ]

hooajalisusv_top5_2 <- väljamaksed_kuud %>%
  filter(Selts %in% hooajalisusv_top5_2$Selts[1:5])

hooajalisusv_top5_2 <- hooajalisusv_top5_2 %>%
  mutate(Aasta=year(Date)) %>%
  filter(year(Date)>="2020")



#Graafik
hooajalisus_väljamaksed_5väiksemat <- ggplot(hooajalisusv_top5_2, aes(x=month(Date), y=Makse, colour=Selts)) + 
  geom_point() +
  geom_line() +
  scale_colour_manual(values=värvid2) +
  facet_wrap(~Aasta, ncol=5, scales="free_x") + 
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) 



#Teeme kena osakaalude graafiku (kindlustusseltside kindlustusmaksete põhjal)


turuosad <- ggplot(loplik_fondid_osakaal_pikk,
                   aes(x = Aasta, y = Osakaal/100, fill = Selts)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(Osakaal/100, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 2) +
  scale_fill_manual(values = värvid) +
  scale_x_continuous(breaks = 2014:2024) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10)) +
  ylab("Osakaal")

  
#Uurime ka väljamaksete arvu aastatel 2018-2021 eraldi, et tekiks
#Võrdlusmoment ja oleks näha, kas COVID pandeemia mõjutas makseid.
#Teeme seda jällegi 5 suurema ja 5 väiksema seltsi kohta

#5 suuremat
koroona_top5 <- väljamaksed_kuud %>%
  group_by(Selts) %>%
  summarise(summa=sum(Makse))
koroona_top5 <- arrange(koroona_top5, desc(summa))
koroona_top5 <- koroona_top5[1:5, ]

koroona_top5 <- väljamaksed_kuud %>%
  filter(Selts %in% koroona_top5$Selts[1:5])

koroona_top5 <- koroona_top5 %>%
  mutate(Aasta=year(Date)) %>%
  filter(year(Date)>="2018"&year(Date)<"2022")

#Graafik
hooajalisus_koroona_suuremad <- ggplot(koroona_top5, aes(x=month(Date), y=Makse, colour=Selts)) + 
  geom_point() +
  geom_line() +
  scale_colour_manual(values=värvid2) +
  facet_wrap(~Aasta, ncol=5, scales="free_x") + 
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) 

#5 väiksemat
koroona_top5_2 <- väljamaksed_kuud %>%
  group_by(Selts) %>%
  summarise(summa=sum(Makse))
koroona_top5_2 <- arrange(koroona_top5_2, desc(summa))
koroona_top5_2 <- koroona_top5_2[6:10, ]

koroona_top5_2 <- väljamaksed_kuud %>%
  filter(Selts %in% koroona_top5_2$Selts[1:5])
koroona_top5_2 <- koroona_top5_2 %>%
  mutate(Aasta=year(Date)) %>%
  filter(year(Date)>="2018"&year(Date)<"2022")
#Graafik
hooajalisus_koroona_väiksemad <- ggplot(koroona_top5_2, aes(x=month(Date), y=Makse, colour=Selts)) + 
  geom_point() +
  geom_line() +
  scale_colour_manual(values=värvid2) +
  facet_wrap(~Aasta, ncol=5, scales="free_x") + 
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) 
