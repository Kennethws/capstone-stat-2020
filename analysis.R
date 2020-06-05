#####################################################################
###------------------------------------------------------------------
### Topic : Capstone 2020
### Date  : 2020-06-04
### Author: Chao Wang
###------------------------------------------------------------------
#####################################################################

library(tidyverse)
library(broom)
library(dslabs)
library(ggrepel)
ds_theme_set()
options(digit = 3)

# load data
load('rdas/avocado.rda')

## Q1

# quantity vs. time
dat %>% 
  ggplot(aes(x = year, y = quantity)) +
  geom_line(col = 'blue') +
  geom_point() +
  ggtitle('quantity change over time') +
  geom_text_repel(data = subset(dat, year %in% c(min(year), max(year))),
            aes(label = quantity), col = 'red')
ggsave('figs/pic1.png')

# price vs. time
dat %>% 
  ggplot(aes(x = year, y = price)) +
  geom_line(col = 'blue') +
  geom_point() +
  ggtitle('price change over time') +
  ylab('avg price / cents per pound') +
  geom_text_repel(data = subset(dat, year %in% c(min(year), max(year))),
                  aes(label = c('290.9', '171.8')), col = 'red')
ggsave('figs/pic2.png')

# price vs. quantity
# Pearson correlation
cor1 <- cor(dat$quantity, dat$price)
# Spearman correlation
cor2 <- cor(dat$quantity, dat$price, method = c('spearman'))

dat %>% 
  ggplot(aes(quantity, price)) +
  geom_point(col = 'red') +
  geom_smooth(method = 'lm') +
  ylab('avg price / cents per pound') +
  xlab('quantity / million pound') +
  ggtitle('price vs. quantity') +
  annotate(geom = 'text', x = 450, y = 250, label = paste0('Pearson: ', round(cor1, 2), '\n',
                                                           'Spearman: ', round(cor2, 2)))
ggsave('figs/pic3.png')

