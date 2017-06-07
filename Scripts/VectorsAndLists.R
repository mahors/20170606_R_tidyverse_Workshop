x <- 5*6
x

is.vector(x)
length(x)

x[2] <- 31
x

x[5]<-44
x

x[11]

x[0]

x <- 1:4
x


y <- x^2
y

x <- 1:5
y <- 3:7

x
y

x+y

z <- y[-5]
z

# Recycling
x + z

z <- 1:10
x+z

z^x

x
y <- x[-5]
y[5] <- NA
y
x+y

str(c("Hello","workshop", "participants!"))

str(c(9:11,200,x))

str(c("something",pi,2:4,pi>3))


w <- rnorm(10)
seq_along(w)
w
which(w<0)
w[which(w<0)]

w[w<0]

w
w[-c(2,5)]




str(list("Something",pi,2:4,pi>3))

x <- list(vegetable="cabbage",
     number=pi,
     series=2:4,
     telling=pi>3)

str(x)
str(x$vegetable)
str(x[1])

str(x[[3]])


x <- list(vegetable=list("cabbage","carrot","spinach"),
          number=list(c(pi,0,2.14,NA)),
          series=list(list(2:4,3:5)),
          telling=pi>3)
str(x)
str(x$vegetable)






mod <- lm(lifeExp ~ gdpPercap, data=gapminder_plus)
str(mod)

str(mod$df.residual)
mod$df.residual %>% str()

str(mod$qr$qr)
mod$qr$qr[[1]]



gapminder_plus %>% 
  group_by(continent) %>% 
  summarize(mean_le=mean(lifeExp),
            min_le=min(lifeExp),
            max_le=max(lifeExp))



gapminder_plus %>% 
  ggplot() +
  geom_line(mapping=aes(x=year,y=lifeExp,color=continent,group=country))

gapminder_plus %>% 
  ggplot() +
  geom_line(mapping=aes(x=year,y=lifeExp,color=continent,group=country)) +
  facet_wrap(~continent)


gapminder_plus %>% 
  ggplot() +
  geom_line(mapping=aes(x=year,y=lifeExp,color=continent,group=country)) +
  geom_smooth(mapping=aes(x=year,y=lifeExp),method="lm") +
  facet_wrap(~continent)
  


by_country <- gapminder_plus %>% 
  group_by(continent,country) %>% 
  nest()

by_country$data %>%  str()

by_country$data[[1]]

#map(list,function)

purrr::map(1:3,sqrt)

library(purrr)

detach("package:maps")

by_country %>% 
  mutate(model=map(data,~lm(lifeExp~year,data=.x)))



model_by_country <- by_country %>% 
  mutate(model=purrr::map(data,~lm(lifeExp~year,data=.x))) %>% 
  mutate(summr=purrr::map(model,broom::glance)) %>% 
  unnest(summr) %>% 
  arrange(r.squared)
  
model_by_country %>% 
  ggplot()+
  geom_jitter(mapping=aes(x=continent,y=r.squared))




by_country %>% 
  mutate(model=purrr::map(data,~lm(lifeExp~year,data=.x))) %>% 
  mutate(summr=purrr::map(model,broom::glance)) %>% 
  unnest(summr) %>% 
  arrange(r.squared) %>% 
  filter(r.squared<0.3) %>% 
  select(country) %>% 
  left_join(gapminder_plus) %>% 
  ggplot()+
  geom_line(mapping=aes(x=year,y=lifeExp,color=country,group=country))




# lifeExp vs gdp


gapminder_plus %>% 
  ggplot() + 
  geom_point(mapping=aes(x = gdpPercap, y = lifeExp))


gapminder_plus %>% 
  ggplot() + 
  geom_line(mapping=aes(x = log(gdpPercap), y = lifeExp,color=continent,group=country))


# lifeExp ~ gdpPercap
by_country %>% 
  mutate(model=purrr::map(data,~lm(lifeExp~log(gdpPercap),data=.x))) %>% 
  mutate(summr=purrr::map(model,broom::glance)) %>% 
  unnest(summr) %>% 
  arrange(r.squared) %>% 
  filter(r.squared<0.1) %>% 
  select(country) %>% 
  left_join(gapminder_plus) %>% 
  ggplot()+
  geom_point(mapping=aes(x=log(gdpPercap),y=lifeExp,color=country))

saveRDS(by_country,"Data/by_country_tibble.rds")

my_fresh_by_country <- readRDS("Data/by_country_tibble.rds")

write_csv(gapminder_plus,"Data/gapminder_plus_for_professor.csv")

