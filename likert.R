library(tidyverse)

# creating some fake likert data

answersOptions <- c("strongly disagree", 
                    "disagree",
                    "neutral",
                    "agree",
                    "strongly agree")

questions <- c("A1","A2","B1","B2")
peopleAsked <- 70

creatorList <- vector("list", length(questions))

for (i in seq_along(questions)) {
  creatorList[[i]] <- sample(answersOptions, peopleAsked, replace = T)
}

names(creatorList) <- questions
likertData <- as_tibble(creatorList)

likertData %>% 
  pivot_longer(everything(), names_to = "Question", values_to = "Answer") %>%
  count(Question, Answer) %>%
  mutate(Group = map_chr(Question, function(x) str_extract(x, "\\D+"))) %>%
  select(Group, everything()) %>%
  group_by(Question) %>%
  mutate(perc = n / sum(n),
         perc = perc * 100) %>%
  ungroup %>%
  mutate(Question = factor(Question, Question %>% unique %>% rev, ordered = T))-> cleanLikert

cleanLikert %>% 
  mutate(highs = if_else(Answer %in% c("agree", "strongly agree"), perc, 0), 
         highs = if_else(Answer != "neutral", highs, perc / 2)) %>%
  mutate(Answer = factor(Answer, rev(answersOptions), ordered = T)) %>%
  filter(highs > 0) -> highs

cleanLikert %>% 
  mutate(lows = if_else(Answer %in% c("disagree", "strongly disagree"), perc, 0),
         lows = if_else(Answer != "neutral", lows, perc / 2)) %>%
  mutate(Answer = factor(Answer, answersOptions, ordered = T)) %>%
  filter(lows > 0) -> lows
  
ggplot() +
  geom_col(data = highs, aes(Question, highs, fill = Answer)) +
  geom_col(data = lows, aes(Question, -lows, fill = Answer)) +
  geom_hline(yintercept = 0, color =c("white")) +
  coord_flip(ylim = c(-100,100))

# let's make it prettier ####
library(ggthemes)

ggplot() +
  geom_col(data = lows, aes(Question, -lows, fill = Answer)) +
  geom_col(data = highs, aes(Question, highs, fill = Answer)) + 
  geom_hline(yintercept = 0, color =c("white")) +
  coord_flip(ylim = c(-100,100)) +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank()) +
  scale_fill_colorblind(breaks=answersOptions) +
  labs(title = "Answers in my Survey")

# Or we could use the likert package (which uses data.frames and not tibbles though) ####

library(likert)
  
likertData2 <-  likertData %>% mutate_all(function(x) factor(x, answersOptions, ordered = T))

likertData3 <- likert(likertData2 %>% data.frame)
plot(likertData3,
     type="bar")

plot(likertData3, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

plot(likertData3,
     type="density",
     facet = TRUE, 
     bw = 0.5)

