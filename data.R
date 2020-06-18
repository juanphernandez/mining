library(tidyverse)
library(tidytext)
library(ggplot2)
library(tm)
library(quanteda)
library(stringr)
library(wordcloud)

data<-read_csv("ins_bee.csv")
data<-data %>%  # all documents are different
  distinct(Title, .keep_all = TRUE)
Abs<-data
#functions------
text_clean<-function(text_1){
  data<-str_replace_all(text_1, "\\W", " ")
  data<-str_replace_all(data, "\\d", " ")
  data<-str_to_lower(data)
  data<-removeWords(data, stopwords("english"))
  data<-str_replace_all(data, "\\b[A-z]\\b", " ")
  data<-stripWhitespace(data)
  data<-str_trim(data)
}
text_replacemnt<-function(text_1){
  data<-str_replace_all(text_1, "honey bee", "bee")
  data<-str_replace_all(data,"honeybee", "bee")
  data<-str_replace_all(data,"honey bees", "bee")
  data<-str_replace_all(data,"honeybees", "bee")
  data<-str_replace_all(data,"insects", "insect")
  data<-str_replace_all(data,"behaviour", "behavior")
}

#process----

Abs$Abstract<-text_clean(Abs$Abstract)
Abs$Abstract<-text_replacemnt(Abs$Abstract)

# text mining----

ins_bee_corpus<-corpus(Abs$Abstract, docvars = 
                   data.frame(group=Abs$group, 
                              Bio1=Abs$Bio1, 
                              Title=Abs$Title))

ins_bee_tok<-tokens(ins_bee_corpus)
ins_bee_dfm<-dfm(ins_bee_tok) #we can remove stopswords and
                              #stems
ins_bee_sel<-dfm(ins_bee_dfm, groups = "group", 
                 dictionary = dict)
ins_bee_df<-convert(ins_bee_sel, "data.frame")

Bio1_sel<-dfm(ins_bee_dfm, groups = "Bio1", 
              dictionary = dict)
Bio1_df<-convert(Bio1_sel, "data.frame") %>% 
  group_by(document) 

Bio1_df<-Bio1_df %>% gather("mechanic", 
                   "thermodynamic", 
                   "electrodynamic",
                   "waves", 
                   "optic",
                   key ="phy", value = "cases") %>% 
  group_by(phy)  
  
Bio1_df$phy<-factor(Bio1_df$phy)

Bio1_df %>% 
  ggplot(aes(document, cases))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(~phy)+
  scale_y_log10()+
  theme_classic()





