
#__________________________________________________
####deklaracja zmiennych####
rm(list = ls()) # usuni?cie zmiennnych kt?re mamy zapisane


start=Sys.time() #zapisanie jaki jest czas by sprawdzi? ile zaje?o obliczanie

options(java.parameters = "- Xmx1024m") #opcje pozwalaj?ce ?adniejsze wy?wietlanie danych
options(scipen=999)                     #opcje pozwalaj?ce ?adniejsze wy?wietlanie danych

MAIN_PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
OUT_PATH <- paste0(MAIN_PATH, "\\CHARTS\\")
setwd(MAIN_PATH) # ustalenie w jakim folderze będziemy pracowa? i s? pliki


#__________________________________________________
#### Wczytalnie bibliotek ####
# install.packages('gifski')
library(dplyr)
library(tidyr)
library(ggplot2)
library(colorspace)
library(lubridate)
library(stringr)

#__________________________________________________
#### Wczytalnie danych ####
Dane <- read.csv2("dane.csv") %>%
  select(Rok, Udział.rowerów, Miasto) %>% 
  filter(!is.na(Rok)) %>%
  filter(Miasto != "Białystok") %>% # odrzucamy, bo jest jedna obserwacja
  mutate(Udział.rowerów = (gsub(" " , "", Udział.rowerów)),
         Udział.rowerów = (gsub("%" , "", Udział.rowerów)),
         Udział.rowerów = (gsub("," , ".", Udział.rowerów)),
         Udział.rowerów = as.numeric(Udział.rowerów),
         Udział.rowerów =  Udział.rowerów / 100) %>%
  mutate(
    Miasto_factor = factor(Miasto, levels = c(
      "Wrocław", "Poznań", "Gdańsk", "Kraków", "Warszawa", "Bydgoszcz", "Katowice")
    )
  )

Dane$ID <- seq.int(nrow(Dane))

Dane_latest <- Dane %>%
  group_by(Miasto) %>%
  filter(Rok == max(Rok)) %>%
  ungroup() %>%
  arrange(desc(Udział.rowerów)) %>%
  mutate(ID = row_number(),
         Udział.rowerów_round = format(Udział.rowerów * 100, nsmall = 1),
         text = paste0(ID, ". ", Miasto, " (", Udział.rowerów_round, "%)"))



#__________________________________________________
#### THEME ####
#752717
#B16227
#F3974A
#FAC26C


C0 <- "#000000"
C1 <- "#993404"
C2 <- "#ad5113"
C3 <- "#c16e22"
C4 <- "#d58b31"
C5 <- "#e9a840"
C6 <- "#fec44f"


FILL_COL <- "#f5f5f2"
FILL_COL <- "#dedfe0"
TEXT_COL <- "#4e4d47"

blankbg <- theme(
  axis.line = element_blank(),
  axis.text.x = element_text(family = "Ubuntu", size = 8,  hjust = 0.5,  color = TEXT_COL),
  axis.text.y = element_text(family = "Ubuntu", size = 8,  hjust = 0.0,  color = TEXT_COL),
  axis.ticks = element_blank(),
  axis.title.x = element_text(family = "Ubuntu", size = 10, hjust = 0.5,  color = TEXT_COL),
  axis.title.y = element_text(family = "Ubuntu", size = 10, hjust = 0.5,  color = TEXT_COL),
  
  
  panel.border = element_blank(),
  # panel.grid.major=element_blank(),
  panel.grid.minor = element_blank(),
  
  #tlo
  plot.background  = element_rect(fill = FILL_COL,  color = NA), 
  panel.background = element_rect(fill = FILL_COL,  color = NA),
  text = element_text(family = "Ubuntu", size = 10, color = TEXT_COL),
  
  # legenda
  legend.position = "bottom",# "none",
  legend.key.width = unit(0.9, "cm"),
  legend.key.height = unit(0.3, "cm"),
  legend.title.align = 0.5,
  legend.title = element_text(family = "Ubuntu", size = 7, hjust = 0, color = TEXT_COL),#element_blank(),
  legend.background = element_rect(fill = FILL_COL, color = NA),
  legend.text       = element_text(family = "Ubuntu", size = 7, hjust = 0, color = TEXT_COL),
  legend.direction = "horizontal",
  
  # tytuy
  plot.title    = element_text(family = "Ubuntu", size = 10, hjust = 0.0,  color = TEXT_COL, face="bold"),
  plot.subtitle = element_text(family = "Ubuntu", size = 7,  hjust = 0.01, face = "italic", color = TEXT_COL),
  plot.caption  = element_text(family = "Ubuntu", size = 7,  hjust = 0.99, color = TEXT_COL),
)  

unique(Dane$Miasto)
#__________________________________________________
#### plot ####

w <- ggplot(Dane, aes(x = Rok, y = Udział.rowerów, color = Miasto_factor)) + 
  geom_line(alpha = 0.5, size = 0.8, linetype = "dotted") +
  geom_point(size = 2.0) +
  scale_color_manual(
    values = c(
      "Wrocław" = C0,
      "Poznań" = C1,
      "Gdańsk" = C2, 
      "Kraków" = C3, 
      "Warszawa" = C4, 
      "Bydgoszcz" = C5,
      "Katowice" = C6
    )
  ) +
  
  geom_text(
    data = Dane_latest, 
    aes(x = 2024.5, y = Udział.rowerów, label = text), 
    hjust = -0.0, size = 3,
    show.legend = FALSE,       # <- no legend entry for this layer
    check_overlap = TRUE
  ) + 
  coord_cartesian(xlim = c(2005, 2027),
                  ylim = c(0,    0.1)) +
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2025)) + 
  labs(
    x = "Rok Kompleksowego Badania Ruchu",
    y = "Udział transportu rowerowego",
    title = "Wrocław planował osiągnąć 15% ruchu rowerowego",
    subtitle = 'do 2020 roku - zgodnie z „Polityką Rowerową Wrocławia” z 2016 roku oraz do 2030 roku - zgodnie z „Planem działań rowerowych do 2030” z 2022 roku',
    #subtitle = "jednak oddalamy się od tego celu",
    caption = paste0(
      "Autor: WroData (Krzysztof Karabon) | Dane: Kompleksowe Badania Ruchu - należy wziąć pod uwagę różnicę metodologiczne badań"
      #"\n",
    ),
    color = NULL
  ) + 
  scale_y_continuous(labels = scales::percent)  +
  blankbg + 
  theme(legend.position = "bottom") +
  
  guides(colour   = guide_legend(byrow = TRUE, nrow = 1),
         fill     = guide_legend(byrow = TRUE, nrow = 1),
         linetype = guide_legend(byrow = TRUE, nrow = 1),
         shape    = guide_legend(byrow = TRUE, nrow = 1))


plot(w)


#__________________________________________________
#### save #### 
png(filename = paste0(OUT_PATH, "", "\\KRB", 
                      Sys.Date(), " .png", sep=""),
    bg="#dedfe0", width = 8, height = 5, units = 'in', res = 500)
plot(w)
dev.off()

