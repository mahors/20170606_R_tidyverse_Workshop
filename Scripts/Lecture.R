# We are loading the tidyverse package

library("tidyverse")

gapminder <- read_csv(file = "Data/gapminder-FiveYearData.csv")

gapminder

ggplot(data=gapminder) + 
  geom_point(mapping=aes(x = gdpPercap, y = lifeExp))

ggplot(data=gapminder) + 
  geom_jitter(mapping=aes(x = gdpPercap, y = lifeExp,
                          color=continent))

ggplot(data=gapminder) + 
  geom_point(mapping=aes(x = log(gdpPercap), y = lifeExp,
                          color=continent, size=pop))

ggplot(data=gapminder) + 
  geom_point(mapping=aes(x = log(gdpPercap), y = lifeExp),
             alpha=0.1, size=2, color="blue")

ggplot(data=gapminder) + 
  geom_line(mapping=aes(x = year, y = lifeExp, group=country, color=continent))

ggplot(data=gapminder) + 
  geom_boxplot(mapping=aes(x = continent, y = lifeExp))

ggplot(data=gapminder) + 
  geom_boxplot(mapping=aes(x = continent, y = lifeExp, color=continent)) +
  geom_jitter(mapping=aes(x = continent, y = lifeExp, color=continent))

ggplot(data=gapminder) + 
  geom_jitter(mapping=aes(x = continent, y = lifeExp, color=continent)) +
  geom_boxplot(mapping=aes(x = continent, y = lifeExp, color=continent))

ggplot(data=gapminder,mapping=aes(x = continent, y = lifeExp, color=continent)) + 
  geom_jitter() +
  geom_boxplot()



ggplot(data=gapminder,mapping=aes(x = log(gdpPercap), y = lifeExp, color=continent)) + 
  geom_jitter(alpha=0.1) +
  geom_smooth(method="lm")


ggplot(data=gapminder,mapping=aes(x = log(gdpPercap), y = lifeExp)) + 
  geom_jitter(mapping=aes(color=continent),alpha=0.2) +
  geom_smooth(method="lm")


#Challenge 6
ggplot(data=gapminder,mapping=aes(x = as.factor(year), y = lifeExp)) + 
  geom_boxplot()


ggplot(data=gapminder,mapping=aes(x = as.factor(year), y = log(gdpPercap))) + 
  geom_boxplot()


ggplot(data=gapminder) + 
  geom_density2d(mapping=aes(x = lifeExp, y = log(gdpPercap)))


ggplot(data=gapminder,mapping=aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  geom_smooth() + 
  scale_x_log10() +
  facet_wrap(~ continent)


# Challenge 7
ggplot(data=gapminder,mapping=aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  geom_smooth(method="lm") + 
  scale_x_log10() +
  facet_wrap(~ year)


ggplot(data=gapminder,mapping=aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  geom_smooth(method="lm") + 
  scale_x_log10() +
  facet_wrap(~ continent)

# filtering
ggplot(data=filter(gapminder, year==2007)) +
  geom_bar(mapping=aes(x=continent), stat="count")




ggplot(data=filter(gapminder, year==2007, continent=="Oceania")) +
  geom_bar(mapping=aes(x=country, y=pop), stat="identity")

ggplot(data=filter(gapminder, year==2007, continent=="Oceania")) +
  geom_col(mapping=aes(x=country, y=pop))

ggplot(data=filter(gapminder, year==2007, continent=="Asia")) +
  geom_col(mapping=aes(x=country, y=pop))

ggplot(data=filter(gapminder, year==2007, continent=="Asia")) +
  geom_col(mapping=aes(x=country, y=pop)) +
  coord_flip()

ggplot(data=gapminder,mapping=aes(x = gdpPercap, y = lifeExp,color=continent)) + 
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ year)


ggplot(data=gapminder,mapping=aes(x = gdpPercap, y = lifeExp,
                                  color=continent, size=pop/10^6)) + 
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ year) +
  labs(title="Life Expectancy vs GDP per caita over time",
       subtitle="In the last 50 years the life expectance has improved in most countries of the world",
       caption="Source: Gapminder foundation, gapminder.com",
       x="GDP per capita, in 1000 USD",
       y="Life Expectancy in years",
       color="Continent",
       size="Population, in millions")

ggsave("Plots/my_fancy_plot.png")


