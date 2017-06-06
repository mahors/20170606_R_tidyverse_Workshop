download.file(url = c("http://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0TAlJeCEzcGQ&output=xlsx", 
                      "http://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0NpF2PTov2Cw&output=xlsx"), 
              destfile = c("Data/indicator undata total_fertility.xlsx", 
                           "Data/indicator gapminder infant_mortality.xlsx"))

library("readxl")

raw_fert <- read_excel(path="Data/indicator undata total_fertility.xlsx", sheet="Data")
raw_infantMort <- read_excel(path="Data/indicator gapminder infant_mortality.xlsx")

gapminder
# gapminder is tidy format

raw_fert
# wide format

fert <- raw_fert %>% 
  rename(country=`Total fertility rate`) %>% 
  gather(key=year,value=fert,-country) %>% 
  mutate(year=as.integer(year))
fert
