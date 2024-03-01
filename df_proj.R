library(tidyquant)
library(tidyverse)
library(lubridate)


### Create Dataframes for the portfolio's of 5(or so) best stocks for the different years


wts3 = rep(1/3, 3)
wts4 = rep(.25, 4)
wts5 = rep(.2, 5)
## 2009 Portfolio
stocks_2009_v <- c("XL", "THC", "AMD", "F", "GNW")
df_stocks_2009 <- stocks_2009_v %>% 
  tq_get(get = "stock.prices", from = "2010-01-01", to = "2014-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts4,
               col_rename = "Return")
## 2010 Portfolio
stocks_2010_v <- c("CMI", "AKAM", "CRM", "BKNG", "CTBB")
df_stocks_2010 <- stocks_2010_v %>% 
  tq_get(get = "stock.prices", from = "2011-01-01", to = "2015-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts4,
               col_rename = "Return")
## 2011 Portfolio
stocks_2011_v <- c("CTRA", "EP-C", "MA", "HUM", "OKE")
df_stocks_2011 <- stocks_2011_v %>% 
  tq_get(get = "stock.prices", from = "2012-01-01", to = "2016-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts4,
               col_rename = "Return")
## 2012 Portfolio
stocks_2012_v <- c("SHLD", "BAC", "NFLX", "WHR", "FII")
df_stocks_2012 <- stocks_2012_v %>% 
  tq_get(get = "stock.prices", from = "2013-01-01", to = "2017-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts3,
               col_rename = "Return")
## 2013 Portfolio
stocks_2013_v <- c("NFLX", "BBY", "MU", "DAL", "PBI")
df_stocks_2013 <- stocks_2013_v %>% 
  tq_get(get = "stock.prices", from = "2014-01-01", to = "2018-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts5,
               col_rename = "Return")
## 2014 Portfolio
stocks_2014_v <- c("LUV", "EA", "EW", "AVGO", "AGN")
df_stocks_2014 <- stocks_2014_v %>% 
  tq_get(get = "stock.prices", from = "2015-01-01", to = "2019-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts4,
               col_rename = "Return")
## 2015 Portfolio
stocks_2015_v <- c("NFLX", "AMZN", "ATVI", "NVDA", "HRL")
df_stocks_2015 <- stocks_2015_v %>% 
  tq_get(get = "stock.prices", from = "2016-01-01", to = "2020-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts4,
               col_rename = "Return")
## 2016 Portfolio
stocks_2016_v <- c("NVDA", "OKE", "FCX", "NEM", "SEP")
df_stocks_2016 <- stocks_2016_v %>% 
  tq_get(get = "stock.prices", from = "2017-01-01", to = "2021-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts5,
               col_rename = "Return")  %>% 
  filter(date != "2018-12-17")
## 2017 Portfolio
stocks_2017_v <- c("ALGN", "NRG", "FSLR", "VRTX", "MU")
df_stocks_2017 <- stocks_2017_v %>% 
  tq_get(get = "stock.prices", from = "2018-01-01", to = "2022-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts5,
               col_rename = "Return")
## 2018 Portfolio
stocks_2018_v <- c("AMD", "ABMD", "FTNT", "TRIP", "RHT")
df_stocks_2018 <- stocks_2018_v %>% 
  tq_get(get = "stock.prices", from = "2019-01-01", to = "2023-12-31") %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return") %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Return,
               weights = wts4,
               col_rename = "Return") %>% 
  filter(date != "2022-12-23")


### Create the S&P 500 dataframes for each year

##SP500 2009
df_SP500_2009 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2010-01-01",
                        to = "2014-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")
##SP500 2010
df_SP500_2010 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2011-01-01",
                        to = "2015-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")
##SP500 2011
df_SP500_2011 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2012-01-01",
                        to = "2016-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")
##SP500 2012
df_SP500_2012 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2013-01-01",
                        to = "2017-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")
##SP500 2013
df_SP500_2013 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2014-01-01",
                        to = "2018-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")
##SP500 2014
df_SP500_2014 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2015-01-01",
                        to = "2019-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")
##SP500 2015
df_SP500_2015 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2016-01-01",
                        to = "2020-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")
##SP500 2016
df_SP500_2016 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2017-01-01",
                        to = "2021-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")
##SP500 2017
df_SP500_2017 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2018-01-01",
                        to = "2022-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")
##SP500 2018
df_SP500_2018 <- tq_get(x = "SPY",
                        get = "stock.prices",
                        from = "2019-01-01",
                        to = "2023-12-31") %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               col_rename = "Return")


### Join each year into its own distinct dataframe with a S&P 500 column and a portfolio column


##2009
df_2009_final <- df_SP500_2009 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2009$Return,
         date = ymd(date),
         year_inception = '2009') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
##2010
df_2010_final <- df_SP500_2010 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2010$Return,
         date = ymd(date),
         year_inception = '2010') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
##2011
df_2011_final <- df_SP500_2011 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2011$Return,
         date = ymd(date),
         year_inception = '2011') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
##2012
df_2012_final <- df_SP500_2012 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2012$Return,
         date = ymd(date),
         year_inception = '2012') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
##2013
df_2013_final <- df_SP500_2013 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2013$Return,
         date = ymd(date),
         year_inception = '2013') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
##2014
df_2014_final <- df_SP500_2014 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2014$Return,
         date = ymd(date),
         year_inception = '2014') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
##2015
df_2015_final <- df_SP500_2015 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2015$Return,
         date = ymd(date),
         year_inception = '2015') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
##2016
df_2016_final <- df_SP500_2016 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2016$Return,
         date = ymd(date),
         year_inception = '2016') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
##2017
df_2017_final <- df_SP500_2017 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2017$Return,
         date = ymd(date),
         year_inception = '2017') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
##2018
df_2018_final <- df_SP500_2018 %>% 
  rename("SP500_Return" = "Return") %>% 
  mutate(portfolio = df_stocks_2018$Return,
         date = ymd(date),
         year_inception = '2018') %>% 
  group_by(year_inception) %>% 
  mutate(SP500_Cum = cumprod(1 + SP500_Return),
         RAND_Cum = cumprod(1 + portfolio))
