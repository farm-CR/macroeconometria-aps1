library(readxl)
library(tidyverse)

df1 <- read_excel("data/grid1_cjcnsm3z.xlsx", skip = 1)
df2 <- read_excel("data/grid1_upkoqq33.xlsx")

df1 <- df1 %>%
  filter(!is.na(Date)) %>% 
  select(date = Date, gdp_constant_price = CNGDGDP, gdp_current_price = `CNNGPQ$`,
         cpi_yoy = CNCPIYOY, m2_usd_b = ECORCNN, foreign_usd_m = WIRACHIN,
         m2_cny_t = CNMSM2, gdp_current_price_cumulative = `CNNGPC$`) %>% 
  mutate(across(!date, as.numeric))

df2 <- df2 %>% 
  filter(!is.na(Date)) %>% 
  select(date = Date, m2_yoy = CNMS2YOY, foreign_usd_b = CNGFOREX, 
         monetary_conditions_index = CHBGMCI, trade_balance_usd_b = `CNFRBAL$`,
         real_estate_climate_index = CHRCCLIM, unemployment_rate = CNUERATE,
         indutrial_cpi_yoy = CHEFTYOY, cpi_yoy = CNCPIYOY, real_gdp_yoy = CNGDPYOY) %>% 
  mutate(across(!date, as.numeric))

df <- df1 %>% 
  full_join(df2, by = c("date", "cpi_yoy"))

df



