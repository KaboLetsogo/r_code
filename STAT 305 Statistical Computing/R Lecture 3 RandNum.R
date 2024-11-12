library(ggplot2)
library(dplyr)
data("diamonds")
set.seed(202)

n1 <- rnorm(1,5)
n2 <- rnorm(mean = 4,sd = 2,n=15)
hist(n2)

n <- rnorm(mean = 4,sd = 2,n=15)
n

n3 <- rpois(mean(2.5),n=1780,)
hist(n3)
n3
str(diamonds)

#STRATIFIED SAMPLING

set.seed(123)
diam_srs <- diamonds%>%slice_sample(n=100)
diam_srs
diam_srs%>%summarise(mean = mean(carat))


set.seed(123)
diam_strat <- diamonds%>%
  group_by(cut)%>%
  slice_sample(n=100)%>%
  ungroup()
diam_strat%>%summarise(mean = mean(carat))


set.seed(123)
diam_strat <- diamonds%>%
  group_by(cut)%>%
  slice_sample(prop=0.1)%>%
  ungroup()

diam_strat%>%summarise(mean = mean(carat))

diamonds%>%distinct(cut)


#CLUSTER SAMPLING
top_cut <- c('Ideal','Premium','Very Good') 
diam_clus <- diamonds%>%filter(cut%>%in%>%top_cut)%>%
  group_by(cut)%>%slice_sample(prop=0.1)%>%
  ungroup()


#create a cluster sample with cuts that have 2 lowest populations

set.seed(123)
cut_variety <- unique(diamonds$cut)
cut_diam_samp <- sample(cut_variety,size=3)
diam_clust <- diamonds %>% filter(cut %in% cut_diam_samp)%>%
  group_by(cut)%>%slice_sample(n=15)%>%
  ungroup()

diam_clust%>%summarise(mean = mean(carat))


mean_carat_point <- replicate(
  n=1000,
  expr = diamonds %>%
    slice_sample(n=7)%>%
    summarise(mean_carat_size = mean(carat))%>%
    pull(mean_carat_size)
)
mean(mean_carat_point)
