library(readr)
library(magrittr)  # %>%
library(tidyverse)
library(dplyr)
library(forcats)   # order variables
library(gridExtra) # grid.arrange


VideoGames = read_csv("VideoGames.csv")

VideoGames$User_Score = as.numeric(VideoGames$User_Score)
VideoGames$Year = as.character(VideoGames$Year)

VideoGames$NA_Sales[is.na(VideoGames$NA_Sales)] = 0
VideoGames$EU_Sales[is.na(VideoGames$EU_Sales)] = 0
VideoGames$JP_Sales[is.na(VideoGames$JP_Sales)] = 0
VideoGames$Global_Sales[is.na(VideoGames$Global_Sales)] = 0
VideoGames$Publisher[is.na(VideoGames$Publisher)] = "Unknown"


# GENERI PIù POPOLARI
Top_Generi = VideoGames %>%
  mutate(Genre = Genre %>% fct_infreq() %>% fct_rev()) %>%
  group_by(Genre) %>%
  summarise(count_Genres = n()) %>%
  arrange(desc(count_Genres))

Top_Generi %>%
  filter(count_Genres >= 50) %>%
  filter(Genre != "Miscellaneous") %>%
  ggplot(aes(x = Genre, y = count_Genres, fill = Genre)) +
  geom_col() +
  labs(title = "Generi più popolari", x = "Generi", y = "Numero di videogiochi") +
  theme(legend.position = "none") +
  coord_flip()



# PIATTAFORME PIù POPOLARI
Top_Piattaforme = VideoGames %>%
  mutate(Platform = Platform %>% fct_infreq() %>% fct_rev()) %>%
  group_by(Platform) %>%
  summarise(count_Platforms = n()) %>%
  arrange(desc(count_Platforms))


Top_Piattaforme %>%
  filter(count_Platforms >= 100) %>%
  ggplot(aes(x = Platform, y = count_Platforms, fill = Platform)) +
  geom_col() +
  labs(title = "Piattaforme più popolari", x = "Piattaforme", y = "Numero giochi rilasciati") +
  theme(legend.position = "none") + 
  coord_flip()



# TOP 10 EDITORI 
Vendite_Per_Editore = VideoGames %>%
  mutate(Publisher = Publisher %>% fct_infreq() %>% fct_rev()) %>%
  group_by(Publisher) %>%
  summarise(TOT_Sales = sum(Global_Sales)) %>%
  arrange(desc(TOT_Sales))


Top10_Editori = Vendite_Per_Editore[1:10,]


Top10_Editori %>%
  filter(TOT_Sales >= 50) %>%
  ggplot(aes(x = reorder(Publisher,TOT_Sales),y = TOT_Sales, fill = TOT_Sales)) +
  geom_col() +
  labs(title = "Top 10 editori", x = "Editori", y = "videogiochi venduti in milioni di unità") +
  theme(legend.position = "none") +
  coord_flip()



# NUMERO VIDEO GIOCHI RILASCIATI NEGLI ANNI
VideoGames %>%
  ggplot(aes(Year)) +
  geom_bar( aes(fill = Year)) +
  theme(legend.position = "none") +
  geom_text(stat='count', aes(label=..count..), hjust = -0.2, size = 3)+
  coord_flip()

# NUMERO VIDEO GIOCHI RILASCIATI NEGLI ANNI IN ORDINE
VideoGames %>%
  mutate(Year = Year %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(Year)) +
  geom_bar( aes(fill = Year)) +
  theme(legend.position = "none") +
  geom_text(stat='count', aes(label=..count..), hjust = -0.2, size = 3)+
  coord_flip()





# VENDITE PER GENERE IN USA,EU e JP
# USA
NAsales_Genre = VideoGames %>%
  group_by(Genre)  %>%
  filter(Genre != "Miscellaneous") %>%
  summarise(NAsales = sum(NA_Sales))  %>%
  arrange(desc(NAsales))

USA = NAsales_Genre %>%
  filter(NAsales >= 20) %>%
  mutate(Genre = fct_reorder(Genre, NAsales)) %>%
  ggplot( aes(x=Genre, y=NAsales, fill = Genre)) +
  geom_col() +
  labs(title="USA", x= "Genere",
       y= "Vendite totali del genere") +
  theme(legend.position = "none") +
  coord_flip()


#EU
EUsales_Genre = VideoGames %>%
  group_by(Genre)  %>%
  filter(Genre != "Miscellaneous") %>%
  summarise(EUsales = sum(EU_Sales))  %>%
  arrange(desc(EUsales)) 

EU = EUsales_Genre %>%
  filter(EUsales >= 20) %>%
  mutate(Genre = fct_reorder(Genre, EUsales)) %>%
  ggplot( aes(x=Genre, y=EUsales, fill = Genre)) +
  geom_col() +
  labs(title="Europa", x= "Genere",
       y= "Vendite totali del genere") +
  theme(legend.position = "none") +
  coord_flip()


#JP
JPsales_Genre = VideoGames %>%
  group_by(Genre)  %>%
  filter(Genre != "Miscellaneous") %>%
  summarise(JPsales = sum(JP_Sales))  %>%
  arrange(desc(JPsales))


JP = JPsales_Genre %>%
  filter(JPsales >= 10) %>%
  mutate(Genre = fct_reorder(Genre, JPsales)) %>%
  ggplot( aes(x=Genre, y=JPsales, fill = Genre)) +
  geom_col() +
  labs(title="Giappone", x= "Genere",
       y= "Vendite totali del genere") +
  theme(legend.position = "none") +
  coord_flip()

grid.arrange(USA, EU, JP, nrow=3, ncol=1)




# PUNTI PER GENERE METACRITIC
Punti_per_Genere = VideoGames %>%
  group_by(Genre) %>%
  summarise(sum_games = n()) %>%
  filter(sum_games >= 300) %>%
  arrange(desc(sum_games)) %>%
  pull(Genre) 


Punti_Per_Genere_Metacritic = VideoGames %>%
  filter(Genre %in% Punti_per_Genere) %>%
  filter(Genre != "Miscellaneous") %>%
  ggplot(aes(x = Genre, y = Score, fill = Genre, color = Genre)) +
  geom_boxplot(alpha = 0.4) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10)) +
  labs(title = "Distribuzione del punteggio Metacritic", x = "Top generi", y = "punteggio Metacritic") +
  theme(legend.position = "none") +
  coord_flip()




# PUNTI PER GENERE USERS
Punti_Per_Genere_Users = VideoGames %>%
  filter(Genre %in% Punti_per_Genere) %>%
  filter(Genre != "Miscellaneous") %>%
  ggplot(aes(x = Genre, y = User_Score, fill = Genre, color = Genre)) +
  geom_boxplot(alpha = 0.4) +
  scale_y_continuous(limits=c(0,10), breaks = seq(0,10,1)) +
  labs(title = "Distribuzione del punteggio degli utenti", x = "Top Generi", y = "punteggio degli utenti") +
  theme(legend.position = "none") +
  coord_flip()

grid.arrange(Punti_Per_Genere_Metacritic, Punti_Per_Genere_Users, nrow=2, ncol=1)



# PUNTI PER EDITORE METACRITIC
Punti_per_Editore = VideoGames %>%
  group_by(Publisher) %>%
  summarise(sum_games = n()) %>%
  filter(sum_games >= 200) %>%
  arrange(desc(sum_games)) %>%
  pull(Publisher)



Punti_per_Editore_Metacritic = VideoGames %>%
  filter(Publisher %in% Punti_per_Editore) %>%
  filter(Publisher !="Unknown") %>%
  ggplot(aes(x = Publisher, y = Score, fill = Publisher, color = Publisher)) +
  geom_boxplot(alpha = 0.4) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  labs(title = "Distribuzione del punteggio Editori", x = "Top editori", y = "punteggio Metacritic") +
  theme(legend.position = "none") +
  coord_flip()




# PUNTI PER EDITORE UTENTI
Punti_per_Editore_Userscore = VideoGames %>%
  filter(Publisher %in% Punti_per_Editore) %>%
  filter(Publisher !="Unknown") %>%
  ggplot(aes(x = Publisher, y = User_Score, fill = Publisher, color = Publisher)) +
  geom_boxplot(alpha = 0.4) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(title = "Distribuzione del punteggio Editori", x = "Top editori", y = "punteggio degli utenti") +
  theme(legend.position = "none") +
  coord_flip()

grid.arrange(Punti_per_Editore_Metacritic, Punti_per_Editore_Userscore, nrow=2, ncol=1)

















