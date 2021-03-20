diamonds %>% 
  mutate(caratgroup = cut(carat, breaks = c(0,1,2,3,4,5))) %>% 
  ggplot(aes(caratgroup, fill = cut)) + 
  geom_bar(position="fill")

ggplot(data = mpg) +
  geom_boxploth(mapping = aes(y = reorder(class, hwy, FUN = median), x = hwy))

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram() +
  facet_wrap(~cut, ncol = 1, scales = "free_y")

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin() +
  coord_flip()

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_quasirandom(size = .1, alpha = .3) +
  coord_flip()

ggplot(data = mpg) +
  geom_beeswarm(mapping = aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy
  ))

diamonds %>% ggplot(aes(cut, price, col = clarity)) + geom_jitter(size=.1,alpha = 1/3)


