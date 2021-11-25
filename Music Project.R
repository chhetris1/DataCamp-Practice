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
  