if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  fixest,        # for convenient implementation of fixed effects, IV and clustering
  data.table     # for more efficient data arrangement functions
)

# Products dataset
setwd("C:/Users/user/Dropbox Alon/Dropbox/Thesis/Flexible Logit/Notes and Drafts/Drafts/Empirical application")
products <- fread("NL Estimation Set.csv") %>% 
  as_tibble() %>% 
  filter(uniquequarter == 2) %>% 
  mutate(
    # Product cost per gallon variable
    prod_costpergallon = case_when(
      !is.na(aptcode3) ~ (costpergallon1*distance1 + costpergallon2*distance2) / (distance1 + distance2),
      is.na(aptcode3) ~ costpergallon1
    ),
    # Ticket carrier with smaller LCCs lumped together
    ticket_carrier2 = ifelse(LCC == TRUE & ticket_carrier != 'WN', 'LCC', ticket_carrier),
    # Plugging in a value for avg_dist_rivals when monopoly == 1
    avg_dist_rivals = case_when(monopoly == 0 ~ avg_dist_rivals,
                                monopoly == 1 ~ 0)
  )

lin_iv <- feols(
  log(sj) - log(s0) ~ nonstop + presence_origin + presence_dest + nonstop_miles +
    extra_miles + popestimate1 + popestimate2 + i(ticket_carrier2, ref = 'UA') 
  | price + log(sjg) ~ LCC_presence + avg_dist_rivals + rival_num + monopoly,
  products
)

lin_iv_fuel <- feols(
  log(sj) - log(s0) ~ nonstop + presence_origin + presence_dest + nonstop_miles +
    extra_miles + popestimate1 + popestimate2 + i(ticket_carrier2, ref = 'UA') 
  | price + log(sjg) ~ LCC_presence + avg_dist_rivals + rival_num + monopoly + prod_costpergallon,
  products
)

first_stage <- etable(lin_iv$iv_first_stage, lin_iv_fuel$iv_first_stage, sdBelow = T)
write.csv(first_stage, "First stage.csv")

second_stage <- etable(lin_iv, lin_iv_fuel, sdBelow = T)
write.csv(first_stage, "First stage.csv")

################################################################################

cor_set <- fread("Second step correlations set.csv") %>% 
  as_tibble()

cor_reg <- etable(feols(grf_ests ~ presence_origin + presence_dest + nonstop_miles
                        + popestimate1 + popestimate2 + DL + LCC + UA + US + WN, cor_set),
                  sdBelow = T)
write.csv(cor_reg, "Correlations regression.csv")
