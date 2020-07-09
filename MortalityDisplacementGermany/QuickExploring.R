library(tidyverse)
library(readxl)

# quick look into RKI data

RKI_COVID19 <- read_csv("RKI_COVID19.csv")
shouldBeIncluded <- c(1,0)

# Produce RefKW
RKI_COVID19 <- RKI_COVID19 %>% mutate(RefKW = map_chr(Refdatum, function(x) strftime(x, format = "%V")),
                                      RefKW = RefKW %>% as.integer)

RKI_COVID19 %>% 
  filter(NeuerTodesfall %in% shouldBeIncluded) %>%
  ggplot() +
  geom_bar(aes(x = RefKW, fill = Altersgruppe))

RKI_COVID19 %>% 
  filter(NeuerTodesfall %in% shouldBeIncluded) %>%
  ggplot() +
  geom_bar(aes(x = RefKW, fill = Altersgruppe)) +
  facet_wrap(~Bundesland, nrow = 4)

# determine mortality displacement

mortalityGermany <- read_excel("sonderauswertung-sterbefaelle.xlsx", 
                                            sheet = "BL_2016_2020_Wochen_AG", skip = 8)

columnNames <- c("Bundesland", "Jahr", "Alter", paste0("KW_", 1:52))
colnames(mortalityGermany) <- columnNames

onlySummariesNo2020 <- mortalityGermany %>% 
  filter(Bundesland != "Deutschland", Jahr != 2020, Alter == "Insgesamt") %>%
  select(-Alter) %>% 
  mutate(KW_1 = KW_1 %>% as.double,
         KW_16 = KW_16 %>% as.double,
         KW_31 = KW_31 %>% as.double,
         KW_46 = KW_46 %>% as.double) %>%
  pivot_longer(
    cols = starts_with("KW_"),
    names_to = "KW",
    names_prefix = "KW_",
    values_to = "Todesfaelle",
    values_drop_na = FALSE
    ) %>%
  select(-Jahr) %>%
  group_by(Bundesland, KW)

meanDeathKW <- onlySummariesNo2020 %>%
  summarize(mean = mean(Todesfaelle, na.rm = T)) %>%
  ungroup %>%
  mutate(KW = KW %>% as.integer) %>%
  arrange(Bundesland, KW)

only2020 <- mortalityGermany %>% 
  filter(Bundesland != "Deutschland", Jahr == 2020, Alter == "Insgesamt") %>%
  select(-Alter) %>% 
  mutate(KW_1 = KW_1 %>% as.double,
         KW_16 = KW_16 %>% as.double,
         KW_31 = KW_31 %>% as.double,
         KW_46 = KW_46 %>% as.double) %>%
  pivot_longer(
    cols = starts_with("KW_"),
    names_to = "KW",
    names_prefix = "KW_",
    values_to = "Todesfaelle",
    values_drop_na = FALSE
  ) %>%
  select(-Jahr) %>%
  mutate(KW = KW %>% as.integer) %>%
  arrange(Bundesland, KW)

mortalityDisplacement <- meanDeathKW %>%
  mutate(deaths2020 = only2020$Todesfaelle,
         mortDispl = deaths2020 - mean) %>%
  select(Bundesland, KW, mortDispl)

mortalityDisplacement %>% 
  ggplot() +
  geom_line(aes(x = KW, y = mortDispl), col = "red") +
  facet_wrap(~Bundesland, nrow = 4)

# bringing it together ####

RKI_COVID19 %>% 
  filter(NeuerTodesfall %in% shouldBeIncluded) %>%
  ggplot() +
  geom_bar(aes(x = RefKW, fill = Altersgruppe)) +
  geom_line(data = mortalityDisplacement, aes(x = KW, y = mortDispl), col = "red") +
  facet_wrap(~Bundesland, nrow = 4)

RKI_COVID19 %>% 
  filter(NeuerTodesfall %in% shouldBeIncluded) %>%
  filter(Bundesland == "Bayern") %>%
  ggplot() +
  geom_bar(aes(x = RefKW, fill = Altersgruppe)) +
  geom_line(data = mortalityDisplacement %>% filter(Bundesland == "Bayern"), aes(x = KW, y = mortDispl, col = "2020 - mean 2016-19")) +
  xlim(0,30) +
  labs(title = "Excess Mortality and COVID-19", 
       subtitle = "COVID-19 Deaths in Bavaria",
       x = "Calendar Week",
       y = "No. of Deaths",
       fill = "COVID-19 Deaths by Age\n(Source: RKI)",
       color = "Excess Mortality\n(Source: Destatis)")
  

# Another way ####

findMortDispl <- function(x,y) {
  result <- mortalityDisplacement %>% filter(Bundesland == x, KW == y) %>% pull(mortDispl)
  if (length(result >= 0)) {
    return(result) 
  }
  return(NA)
}

RKI_COVID19 %>% 
  filter(NeuerTodesfall %in% shouldBeIncluded) %>%
  filter(Bundesland == "Bayern") %>%
  select(Bundesland, RefKW) %>%
  arrange(Bundesland, RefKW) %>%
  group_by(Bundesland, RefKW) %>%
  mutate(CovidDeath = n()) %>%
  ungroup %>%
  distinct %>%
  mutate(mortDispl = map2_dbl(Bundesland, RefKW, findMortDispl)) %>%
  ggplot() +
  geom_col(aes(RefKW,CovidDeath), fill = "red", alpha = .2) +
  geom_col(aes(RefKW,mortDispl), fill = "blue", alpha = .2)



