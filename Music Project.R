#PROJECT 1: Wrangling & Visualizing Musical Data
library(tidyverse)
bb <- read_csv("bb_chords.csv") #downloaded from DataCamp Projects
head(bb)
dim(bb) #19198 rows and 9 columns
bb_count <- bb %>% count(chord, sort = TRUE)
bb_count[1:20,] #displaying top 20 chords
#Let's visualize 
bb_count %>% slice(1:20) %>% 
  mutate(share = n/sum(n), chord = reorder(chord, share)) %>% 
  ggplot(aes(x=chord, y=share, fill = chord))+
  geom_col()+
  xlab("common chords")+
  ylab("% occurance")+
  coord_flip()+
  theme(legend.position = "none")
#Investigating the chord change
bb_bigram_count <- bb %>% 
  mutate(next_chord = lead(chord), next_title = lead(title), 
         bigram = paste(chord, next_chord)) %>% 
  filter(title == next_title) %>% 
  count(bigram, sort = TRUE)
bb_bigram_count[1:20,]
#Now let's visualize
bb_bigram_count %>% slice(1:20) %>% 
  mutate(share = n/sum(n), bigram = reorder(bigram, share)) %>% 
  ggplot(aes(x=bigram, y=share, fill = bigram))+
  geom_col()+
  coord_flip()+
  xlab("chord transition")+
  ylab ("frequency")+
  theme(legend.position = "none")

#finding 30 artists with the most songs in the corpus 
bb_30_artists <- bb %>% 
  select(artist, title) %>% 
  unique()%>% 
  count(artist, sort = TRUE)
bb_30_artists %>% slice(1:30) 

##tagging the corpus
tags <- tibble(
  artist = c('Abba', 'Billy Joel', 'Elton John', 'Stevie Wonder', 'The Rolling Stones', 'The Beatles', 'Eric Clapton'),
  instrument = c('piano', 'piano', 'piano', 'piano', 'guitar', 'guitar', 'guitar'))
bb_tagged <- bb %>% inner_join(tags)
bb_tagged

#comparing chords in guitar-driven vs piano-driven songs
top_20 <- bb_count$chord[1:20]
bb_tagged %>% filter(chord %in% top_20) %>% 
  count(chord, instrument, sort=T) %>% 
  ggplot(aes(chord, n, fill=chord))+
  geom_col()+
  facet_grid(~instrument)+
  coord_flip()+
  xlab("total chords")+
  ylab("Chord")+
  theme(legend.position = "none")

#comparing chord bigrams in piano vs guitar driven songs
# The top 20 most common bigrams
top_20_bigram <- bb_bigram_count$bigram[1:20]

# Creating a faceted plot comparing guitar- and piano-driven songs for bigram frequency
bb_tagged %>%
  mutate(next_chord = lead(chord),
         next_title = lead(title),
         bigram = paste(chord, next_chord)) %>%
  filter(title == next_title) %>%
  count(bigram, instrument, sort = TRUE) %>%
  filter(bigram %in% top_20_bigram) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_col() +
  facet_grid(~instrument) +
  coord_flip() +
  ylab('Total bigrams') +
  xlab('Bigram') +
  theme(legend.position="none")

