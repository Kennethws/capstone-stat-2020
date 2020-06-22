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
library(stargazer)
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


## Q2

## a
# mathematical notation (in Rmd)
lin.fit <- lm(price ~ quantity, data = dat)
lin.fit %>% tidy()
stargazer(lin.fit, type = 'text', align = T)

## b confidence interval for beta
n <- nrow(dat)
x.bar <- mean(dat$quantity)
y.bar <- mean(dat$price)
Cxy <- cov(dat$quantity, dat$price) * (n - 1)
Cxx <- crossprod(dat$quantity - x.bar)[1]
beta.hat <- Cxy / Cxx
alpha.hat <- y.bar - beta.hat*x.bar
RSS <- crossprod(dat$price - alpha.hat - beta.hat*dat$quantity)[1]
sigma.hat <- sqrt(RSS / (n-2))
# 95% CI for beta
t <- qt(p = .975, df = n-2)
beta.hat + c(-1, 1) * t * sigma.hat / sqrt(Cxx)


## Q3

## a
c1 <- (112.1 - 80)/29.53
p.L.A <- round(pt(q = c1, df = 46, lower.tail = F), 3)

## b
c2 <- (135.8 - 80) / 29.48
pL <- round(.5 * pt(q = c1, df = 46, lower.tail = F) + .5 * pt(q = c2, df = 46, lower.tail = F), 3)

## c
p.L.A <- pt(q = c1, df = 46, lower.tail = F)
p.L.B <- pt(q = c2, df = 46, lower.tail = F)

p.A.L <- .5 * p.L.A / pL
p.B.L <- .5 * p.L.B / pL