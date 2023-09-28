library(readxl)
library(tidyverse)
library(zoo)
library(mFilter)

df1 <- read_excel("data/grid1_cjcnsm3z.xlsx", skip = 1)
df2 <- read_excel("data/grid1_upkoqq33.xlsx")
df3 <- read_excel("data/dados_bbg.xlsx")
df4 <- read_excel("data/china_targets.xlsx")
rate <- read_excel("data/china_rate.xlsx", skip = 15)

##### Data manipulation #####

df1 <- df1 %>%
  filter(!is.na(Date)) %>% 
  select(date = Date, gdp_constant_price = CNGDGDP, gdp_current_price = `CNNGPQ$`,
         cpi_yoy = CNCPIYOY, m2_usd_b = ECORCNN, foreign_usd_m = WIRACHIN,
         m2_cny_t = CNMSM2, gdp_current_price_cumulative = `CNNGPC$`) %>% 
  mutate(across(!date, as.numeric),
         date = as.Date(date))

df2 <- df2 %>% 
  filter(!is.na(Date)) %>% 
  select(date = Date, m2_yoy = CNMS2YOY, foreign_usd_b = CNGFOREX, 
         monetary_conditions_index = CHBGMCI, trade_balance_usd_b = `CNFRBAL$`,
         real_estate_climate_index = CHRCCLIM, unemployment_rate = CNUERATE,
         indutrial_cpi_yoy = CHEFTYOY, cpi_yoy = CNCPIYOY, real_gdp_yoy = CNGDPYOY) %>% 
  mutate(across(!date, as.numeric),
         date = as.Date(date))

df3 <- df3 %>% 
  filter(!is.na(Date)) %>%
  select(date = Date, minimum_wage_beijing = CNWMBEIJ...2, commodities_price_yoy = CHOMCMDY...4,
         exports_b = ECOYECNN, imports_b = ECOYMCNN, total_trade = CNFRTTNO,
         real_exchange_rate = BISBCNR) %>% 
  mutate(across(!date, as.numeric),
         date = as.Date(date))

df4 <- df4 %>% 
  mutate(growth_target = as.numeric(growth_target),
         date = as.Date(date),
         year = year(date))

rate <- rate %>% 
  select(date = Period, rate = aCNLPR1YRR) %>% 
  filter(month(date) %% 3 == 0) %>% 
  mutate(year = year(date),
         quarter = month(date) / 3) %>% 
  select(-date)

df <- df1 %>% 
  full_join(df2 %>% select(-cpi_yoy), 
            by = c("date")) %>%
  full_join(df3, by = c("date")) %>% 
  mutate(year = year(date),
         quarter = ceiling(month(date) / 3)) %>% 
  left_join(df4 %>% select(-date), by = c("year")) %>% 
  mutate(across(c(m2_usd_b, minimum_wage_beijing, commodities_price_yoy), na.approx))

df_tri <- df %>% 
  select(trade_balance_usd_b, total_trade, exports_b, imports_b, year, quarter) %>% 
  group_by(year, quarter) %>% 
  summarise(trade_balance_usd_b = sum(trade_balance_usd_b),
            total_trade = sum(total_trade),
            exports_b = sum(exports_b),
            imports_b = sum(imports_b))

df <- df %>% 
  filter(!is.na(gdp_constant_price)) %>% 
  mutate(delta_m2 = (m2_cny_t / lead(m2_cny_t) - 1) * 100,
         delta_gdp = (gdp_constant_price / lead(gdp_constant_price) - 1) * 100,
         delta_v = rollapply(delta_m2, 4, mean, align = "left", fill = NA),
         growth_target = growth_target ^ (1/4)) %>% 
  select(-c(trade_balance_usd_b, total_trade, exports_b, imports_b)) %>% 
  left_join(df_tri, by = c("year", "quarter")) %>% 
  left_join(rate, by = c("year", "quarter")) %>% 
  mutate(bp1 = exports_b - imports_b,
         gdp_natural = hpfilter(gdp_constant_price, freq = 1600)$trend,
         gdp_cycle = hpfilter(gdp_constant_price, freq = 1600)$cycle) %>% 
  select(year, quarter, delta_m2, growth_target, delta_v, delta_gdp, gdp_natural, gdp_cycle, 
         unemployment_rate, unemployment_target, inflation_rate = cpi_yoy, inflation_target, 
         bp1, bp2 = trade_balance_usd_b, interest_rate = rate, wage = minimum_wage_beijing, 
         real_exchange_rate, commoditie_index = commodities_price_yoy, gdp_constant_price) %>% 
  filter(!is.na(delta_v)) %>% 
  mutate(covid = ifelse(is.na(growth_target), 1, 0)) %>% 
  arrange(year, quarter)

##### Estimation #####

fit_y <- df %>% 
  mutate(rate = lag(interest_rate)) %>% 
  filter(!is.na(rate)) %>% 
  lm(log(gdp_constant_price) ~ rate + year + quarter, data = .)
summary(fit_y)

fit_u <- df %>% 
  mutate(cycle = lag(gdp_cycle),
         wage = lag(wage)) %>% 
  filter(!is.na(cycle), !is.na(wage)) %>% 
  lm(unemployment_rate ~ cycle + log(wage), data = .)
summary(fit_u)

fit_pi <- df %>% 
  mutate(cycle = lag(gdp_cycle),
         rate = lag(interest_rate)) %>% 
  filter(!is.na(cycle), !is.na(rate)) %>% 
  lm(inflation_rate ~ cycle + rate, data = .)
summary(fit_pi)

fit_bp <- df %>% 
  mutate(commodity = lag(commoditie_index)) %>% 
  filter(!is.na(commodity)) %>% 
  lm(bp1 ~ real_exchange_rate + commodity, data = .)
summary(fit_bp)

df %>% 
  slice(-1) %>% 
  mutate(growth_target = ifelse(is.na(growth_target), 0, growth_target),
         growth_deviation = fit_y$fitted.values - growth_target,
         unemployment_deviation = fit_u$fitted.values - unemployment_target,
         inflation_deviation = fit_pi$fitted.values - inflation_target,
         bp_deviation = fit_bp$fitted.values - 0,
         assimetria_gdp = ifelse(growth_deviation < 0, 1, 0)) %>% 
  lm(delta_m2 ~ growth_target + delta_m2 + inflation_deviation + 
       covid * (growth_deviation + unemployment_deviation + bp_deviation), 
     data = .) %>% 
  summary()



