#install.packages("readxl")
#install.packages("haven")
#install.packages("tidyr")
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
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

#Teeme graafiku, mis kujutab hooajalisust, võtame aasta, mis sisaldab ka koroonaviirusepandeemia algust (2020)
#Sellejaoks filtreerime valja aasta 2020
hooajalisus2020 <- uuskuudekaupa_pikk %>%
  filter(format(Date, "%Y")=="2020")
hooajalisus1_2020 <- ggplot(hooajalisus2020, aes(Date, Makse, colour = Selts)) +
  geom_point() +                     
  geom_line()

#Teeme uue graafiku, mis on sarnane eelmisele, kuid kus kujutatakse kõike 5t aastat, selleks teeme tagasi veeru Aasta

hooajalisus_viimased5 <- uuskuudekaupa_pikk %>%
  filter(format(Date, "%Y")>="2020") %>%
  mutate(Aasta = format(Date, "%Y"))

ggplot(hooajalisus_viimased5, aes(x=Date, y=Makse, colour=Selts)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~Aasta, ncol=5, scales="free_x") + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
  




  
