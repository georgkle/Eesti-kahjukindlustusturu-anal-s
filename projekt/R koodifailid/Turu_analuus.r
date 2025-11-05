install.packages("readxl")
install.packages("haven")
library(readxl)
library(dplyr) 

#puhastame andmestiku
andmed_a <- read_excel("C:\\R\\kindlustusmaksed.xlsx", skip=1)
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

#teeme graafiku, selle kohta, kuidas ajas on muutunud kindlustusmaksed kokku fondide peale
View(fondid)
kindlustusmaksed_aastates <- fondid[, c("Aasta", "kokku")]
View(kindlustusmaksed_aastates)
plot(kindlustusmaksed_aastates$Aasta,
     kindlustusmaksed_aastates$kokku,
     type = "o",
     pch = 16,
     col = "blue",
     xlab = "Aasta",
     ylab = "Kindlustusmaksed (eur)",
     main = "Kindlustusmaksed aastates")

#võrdlen suurima 4 turuosa:
library(dplyr)
library(tidyr)
names(kontroll) <- gsub("_osakaal$","",names(kontroll))
top4 <- kontroll[, c("Aasta","ERGO Insurance SE","Swedbank P&C Insurance AS","If P&C Insurance AS","AB Lietuvos draudimas Eesti filiaal")]
View(top4)
turuosad_long <- top4 %>%
  pivot_longer(
    cols = -Aasta,                # kõik veerud peale "Aasta"
    names_to = "Ettevõte",        # uus veerg ettevõtte nime jaoks
    values_to = "Turuosa"         # uus veerg turuosa väärtuse jaoks
  )
ggplot(turuosad_long, aes(x=Aasta, y=Turuosa, color=Ettevõte)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  labs(
    title = "TOP 4 kindlustusfirmade turuosad aastatel 2014-2024",
    x = "Aasta",
    y = "Turuosa (%)",
    color = "Ettevõte")

#turukonsentratsioon
library(dplyr)
View(loplik_fondid_osakaal)
turu_konsentratsioon <- loplik_fondid_osakaal %>%
  rowwise() %>%
  mutate(
    CR4 = sum(sort(c_across(-Aasta), decreasing = TRUE)[1:4]),
    HHI = sum((c_across(-c(Aasta,CR4))/100)^2, na.rm=TRUE) *10000
  )


View(turu_konsentratsioon)


#graafik turukonsentratsiooni kohta
install.packages("ggplot2")

library(ggplot2)

ggplot(turu_konsentratsioon, aes(x = Aasta)) +
  geom_line(aes(y = CR4, color = "CR4 (nelja suurima osakaal)"), size = 1.2) +
  geom_line(aes(y = HHI / 100, color = "HHI"), size = 1) +
  scale_y_continuous(
    name = "CR4 (%)",
    sec.axis = sec_axis(~.*100, name = "HHI indeks")
  ) +
  labs(color = "Näitaja") +
  scale_x_continuous(br=2014:2024) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 8))
cor(turu_konsentratsioon$CR4, turu_konsentratsioon$HHI)
#Eesti turg on mõõdukalt konsentreeritud


#3. peatykk kasumlikkus ja riskianalüüs

#--------------------------arvutame kahjusuhte-----------------------------------
library(readxl)
library(reshape2)
andmed_valjamaksed <- read_excel("C:\\R\\RRI07_20250925-203409.xlsx", skip=1)
View(andmed_valjamaksed)
andmed_a <- andmed_valjamaksed[c(1:11),c(1,3,6:25)]
View(andmed_a)

valjamaksed <- andmed_a[, colSums(andmed_a == "." | andmed_a == "..", na.rm=TRUE)==0]
View(valjamaksed)
View(fondid)
sissemaksed <- fondid[, c(1,3,5:15)]
View(sissemaksed)
valjamaksed <- valjamaksed %>%
  rename(Aasta = ...1)

#on kaks andmestikku sissemaksed ja valjamaksed
library(purrr)
valjamaksed$Aasta <- as.numeric(valjamaksed$Aasta)
colnames(sissemaksed)[-1] <- paste0(colnames(sissemaksed)[-1], "_sissemakse")
colnames(valjamaksed)[-1] <- paste0(colnames(valjamaksed)[-1], "_valjamakse")


df <- sissemaksed %>%
  inner_join(valjamaksed, by="Aasta")
View(df)

vajalikud <- df[, !names(df) %in% c("SaadudMaksed_sissemakse","kokku_sissemakse","...3_valjamakse")]
View(vajalikud)
names(vajalikud)

#leiame kõikide firmade nimed
sissemakse_veerud <- grep("_sissemakse$", names(vajalikud), value=TRUE)
firmad <- sub("_sissemakse$", "", sissemakse_veerud)

for (firma in firmad) {
  sissemakse_col <- paste0(firma, "_sissemakse")
  valjamakse_col <- paste0(firma, "_valjamakse")
  suhe_col <- paste0(firma, "_suhe")
  
  vajalikud[[suhe_col]] <- vajalikud[[valjamakse_col]] / vajalikud[[sissemakse_col]]
}
  
#paneme andmestiku kitsasse formaati, paremaks andmete visualisatsiooniks
library(tidyr)
library(dplyr)

vajalikud_pikk <- vajalikud %>%
  pivot_longer(
    cols= -Aasta,
    names_to = c("Firma", ".value"),
    names_sep ="_") %>%
  mutate(suhe = valjamakse / sissemakse)

View(vajalikud_pikk)
###########
keskmised_suhted <- vajalikud_pikk %>%
  group_by(Firma) %>%
  summarise(
    keskmine_suhe = mean(suhe, na.rm = TRUE),
    sd_suhe = sd(suhe, na.rm = TRUE),
    min_suhe = min(suhe, na.rm = TRUE),
    max_suhe = max(suhe, na.rm = TRUE)
  ) %>%
  arrange(desc(keskmine_suhe))

View(keskmised_suhted)

###########
###########
varvid <- c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
)


##Teen graafiku firmade kahjusuhetest
ggplot(vajalikud_pikk, aes(x=Aasta, y=suhe, fill =Firma)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=varvid) +
  scale_x_continuous(breaks = 2014:2024) +
  theme(axis.text.x=element_text(angle=60, hjust=1, size=10))+
  ylab("Kahjusuhe") +
  geom_text(aes(label=round(suhe, 2)),
            position = position_stack(vjust=0.5),
            size=2,
            color="black")

#vahetan sissemakse ja valjamakse omavahel ümber

tulemus <- vajalikud_pikk[, c("Aasta","Firma","valjamakse","sissemakse","suhe")]
View(tulemus)

#--------Kas suurema turuosadega firmadel on madalamad kahjusuhted----------------
#Vaatame 4 suurimat firmat ja võrdleme neid teiste fondide üldkeskmiste suhetena, selleks teeme graafiku ka lisaks
#vajalikud andmestikud tulemus, loplik_fondid_osakaal

View(loplik_fondid_osakaal)
View(tulemus)

#keskmine suhe aastate kaupa
keskmine_suhe <- tulemus %>%
  group_by(Aasta) %>%
  summarise(keskmine = mean(suhe, na.rm=TRUE))

View(keskmine_suhe)

#4 suurimat firmat läbi aastate (suurima turuosaga)

#paneme esmalt pikka formaati
vajalik <- loplik_fondid_osakaal %>%
  pivot_longer(
    cols = -Aasta,
    names_to = "Firma",
    values_to = "turuosa"
  )

#leiame 4 suurimat

suurimadfirmad_4 <- vajalik %>%
  group_by(Aasta) %>%
  arrange(desc(turuosa), .by_group=TRUE) %>%
  slice_head(n=4)

View(suurimadfirmad_4)


#puhastame suurimadfirmade nimed, paneme lihtsalt firma nime ja jatame lopust "osakaal" valja
suurimadfirmad_4 <- suurimadfirmad_4 %>%
  mutate(Firma = sub("_osakaal$","",Firma))
#lisame 4 suurimale ka nende suhted.
suurimate_suhe <- inner_join(tulemus, suurimadfirmad_4,
  by = c("Aasta", "Firma"))
View(suurimate_suhe)

#koostan andmestiku, kus on lisatud ka üldkeskmine suhe kõikide fondide peale kokku

yldkeskmine_suurimad_suhted <- inner_join(suurimate_suhe, keskmine_suhe,
                                          by = c("Aasta"))
View(yldkeskmine_suurimad_suhted)
#Graafik, selle kohta, kas suuremate turuosadega firmadel on madalam kahjusuhe

andmed_filtreeritud <- yldkeskmine_suurimad_suhted %>%
  filter(Firma != "Salva Kindlustuse AS")   # eemalda Salva kindlustus, sest see oli ainult aastal 2014 top 4 seas, ülejäänud aastatel ei ole
#Segaduse vältimiseks.
ggplot(andmed_filtreeritud, aes(x = Aasta)) +
  geom_line(aes(y = suhe, color = Firma), size = 1.2) +
  geom_line(aes(y = keskmine, linetype = "Keskmine"), 
            color = "black", size = 1.2) +
  labs(
    x = "Aasta",
    y = "Väljamakse/Sissemakse suhe",
    color = "Firma",
    linetype = "Näitajad"
  ) +
  scale_x_continuous(breaks = 2014:2024) +
  scale_color_manual(values = varvid) +
  scale_linetype_manual(values = c("Keskmine" = "dashed")) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 8))


#################################################################################################

