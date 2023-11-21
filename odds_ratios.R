library(tidyverse)

# first, what's the coefficient size

log(35)

# what does the log-odds to probability difference look like

dat <- 
  tibble(
    log_odds = seq(-4, 4, 0.01)
  ) |> 
  mutate(prob = plogis(log_odds))

dat |> 
  ggplot(aes(x = log_odds, y = prob)) +
  geom_line()



tibble(
  group1 = seq(-4, 4, 0.01)
) |> 
  mutate(group2 = group1 + log(35),
         prob1 = plogis(group1),
         prob2 = plogis(group2)) |> 
  ggplot(aes(x = group1, y = prob1)) +
  geom_line() +
  geom_line(aes(y = prob2), colour = "red")


# to calculate population FPN rate
# is basically 4.5 million ish adults in Scotland and 20k? FPNs?
# but then isn't this controlled by the matching?
# maybe this is the prevalence of hospital contacts?

contact_rate <- 0.08

contact_3_rate_case <- 0.08 * 0.234

contact_3_rate_control <- 0.08 * 0.106

rate <- 2e4 / 4.5e6

pop_odds <- 2e4 / (4.5e6 - 2e4)


(4.5e6 - 2e4) / 2e4
# if it's about the matched control rate then the prevalence is... 0.25?

dat2 <- 
tibble(
  group1 = seq(-10, 10, 0.01)
) |> 
  mutate(group2 = group1 + log(35),
         prob1 = plogis(group1),
         prob2 = plogis(group2))

or_plot <- 
dat2 |> 
  ggplot(aes(x = prob1, y = prob2)) +
  geom_line() +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  labs(title = "What does an odds ratio of 35x look like on the probabilty scale?",
       caption = "Dashed line is overall FPN prevalence (25%)",
       x = "Reference group",
       y = "Comparison group")

or_plot <- 
dat2 |> 
  ggplot(aes(x = prob2, y = prob1)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  labs(title = "What does an odds ratio of 35x look like on the probabilty scale?",
       caption = "Dashed line is overall FPN prevalence (25%)",
       y = "Reference group probability",
       x = "Comparison group probability")

ggsave(
  here::here("03_figures", "or_plot.png"),
  or_plot,
  height = 5, width = 7,
  type = "cairo-png"
)

tibble(
  group1 = seq(-10, 10, 0.01)
) |> 
  mutate(group2 = group1 + log(35),
         prob1 = plogis(group1),
         prob2 = plogis(group2)) |> 
  filter(prob1 > 0.008 & prob1 < 0.009)

contact_3_rate_case * 2e4
contact_3_rate_control * 2e4


fpn_yes <- 30
fpn_no <- 70
ctrl_yes <- 5
ctrl_no <- 395

fpn_odds <- fpn_yes / fpn_no
ctrl_odds <- ctrl_yes / ctrl_no

or <- fpn_odds / ctrl_odds


calc_or <- function(fpn_prob, ctrl_prob, total_n){

  fpn_yes <- round(fpn_prob * (total_n * 0.25))
  fpn_no <- round((total_n * 0.25) - fpn_yes)
  
  ctrl_yes <- round(ctrl_prob * (total_n * 0.75))
  ctrl_no <- round((total_n * 0.75) - ctrl_yes)
  
fpn_odds <- fpn_yes / fpn_no
ctrl_odds <- ctrl_yes / ctrl_no

or <- fpn_odds / ctrl_odds

tibble(
  fpn_yes = fpn_yes,
  fpn_no = fpn_no,
  ctrl_yes = ctrl_yes,
  ctrl_no = ctrl_no,
  or = or
)

}

or_dat <- 
crossing(
  fpn_prob = seq(0.001, 0.991, 0.001),
  ctrl_prob = seq(0.001, 0.991, 0.001)
) |> 
  mutate(or = map2(fpn_prob, ctrl_prob, ~ calc_or(.x, .y, total_n = 70000),
                   .progress = TRUE))

or_dat |> 
  unnest(or) |> 
  filter(or > 35 & or < 36) |> View()


oc

fpn_yes <- 90
fpn_no <- 230
ctrl_yes <- 5
ctrl_no <- 440

fpn_odds <- fpn_yes / fpn_no
ctrl_odds <- ctrl_yes / ctrl_no

or <- fpn_odds / ctrl_odds




# functionizing -----------------------------------------------------------


make_logs <- function(ref, log_or){
  
  tidyr::crossing(
    ref,
    log_or
  ) |> 
    mutate(or = ref + log(log_or),
           p_ref = plogis(ref),
           p_or = plogis(or))
  
}


dat3 <- 
tibble(
  reference = seq(-10, 10, 0.01)
) |> 
  mutate(logs = map(reference, make_logs, seq(1, 100, 1),
                    .progress = TRUE)) |> 
  select(-reference) |> 
  unnest(logs)

dat3 |> 
  ggplot(aes(x = p_ref, y = p_or, colour = log_or, group = log_or)) +
  geom_line(alpha = 0.3)


dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 35) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or)) +
  geom_point()


or_dat |> 
  unnest(or) |> 
  filter(or > 35 & or < 36) |> 
  View()



or_dat_7k <- 
  crossing(
    fpn_prob = seq(0.001, 0.991, 0.01),
    ctrl_prob = seq(0.001, 0.991, 0.01)
  ) |> 
  mutate(or = map2(fpn_prob, ctrl_prob, ~ calc_or(.x, .y, total_n = 7000),
                   .progress = TRUE))

or_dat_7k |> 
  unnest(or) |> 
  filter(or > 35 & or < 36) |> 
  View()


# how is it possible to estimate this OR from data?

or_subset <- 
or_dat |> 
  unnest(or) |> 
  filter(or > 35 & or < 36) |> 
  group_by(fpn_prob, ctrl_prob) |> 
  nest()


reformat_dat <- function(dat){
dat |> 
  select(-or) |> 
  pivot_longer(cols= everything()) |> 
  separate(col = name,
           into = c("group", "health"),
           sep = "_") |> 
  pivot_wider(names_from = c(health),
              values_from = value)
}

tmp <- or_subset$data[[1]] |> reformat_dat()

tmp |> 
  mutate(ratio = yes / no) |> 
  summarise(or = ratio[group == "fpn"] / ratio[group == "ctrl"])

get_results <- function(d){

glm(cbind(yes, no) ~ group, data = d,
      family = "binomial") |> 
    broom::tidy() |> 
    mutate(exp_est = exp(estimate),
           conf_low = exp(estimate - 1.96 * std.error),
           conf_upp = exp(estimate + 1.96 * std.error))
}

tidy_get_results <- function(da){
  
  reformat_dat(da) |> get_results()
  
}

or_subset |> 
  mutate(res = map(data, tidy_get_results)) |> 
  unnest(res) |> View()

  
# so there's something funky here in my expectations between
# ORs and odds? and log odds? and probabilities?
  
# so i guess you can calculate these massive ORs because it relates
# to the _odds_ in the reference group not the probability?
  
# no i was just calculating the wrong thing...


# so we want to hack together the probability of a FPN in the general public?

population_prev <- 2e4 / 4.5e6

# around half a percent

dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 35) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or)) +
  geom_vline(xintercept = 2e4 / 4.5e6) +
  geom_point()

# but in the sample this is

dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 35) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or)) +
  geom_vline(xintercept = 0.25) +
  geom_point()


dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or >= 35 & log_or <= 36) |> 
  filter(p_ref >= population_prev - 0.001 & p_ref <= population_prev + 0.001) |> 
  ggplot(aes(x = p_ref, y = p_diff)) +
  geom_point()


# around 14% more likely

or_dat |> 
  unnest(or) |> 
  filter(or > 35 & or < 36)

# this is lower even than the smallest probability i looked at! 



dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 35 |
         log_or == 9|
         log_or == 90|
         log_or == 3) |> 
  ggplot(aes(x = p_ref, y = p_diff, colour = log_or, group = log_or)) +
  geom_line() +
  facet_wrap(~log_or)



# so if ~ 25% of the 3+

fpn_prop_any <- 3150 / (13220 + 3150)
ctrl_prop_any <- 1834 / (43974 + 1834)

# of which 3 +

(43974 + 1834) * (ctrl_prop_any * 0.1) * lockdown_n / total_n

((13220 + 3150) * (fpn_prop_any * 0.234)) * lockdown_n / total_n

# ~ 183 ctrls with 3+ contacts
# 737 fpns with 3+ contacts

1279* 0.234

# lockdown n

lockdown_n <- 866 + 10822

total_n <- 2537 + 59641 

lockdown_n / total_n


1279 / 417


(1279 / 417) / (1850 / 8142)


(1279 * 0.234) / (417 * 0.1) / (1850 / 8142)

300 / 41

(1850 + 1279 - 300) / (8142 + 417 - 41)

(300 / 41) / ((1850 + 1279 - 300) / (8142 + 417 - 41))


# and here the proportions are:

300 / (1850 + 1279) * 100

# 9.5% of FPN recipients

41 / (8142 + 417 - 41) * 100

# half a percent of ctrls

dat3 |> 
  filter(p_ref > 0.004 & p_ref < 0.05) |> 
  filter(or > 22)



or_dat |> 
  unnest(or) |> 
  filter(or > 22 & or < 23) |> 
  group_by(fpn_prob, ctrl_prob) |> 
  nest()


rate * 100



dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 22,
         round(p_ref, 3) == round(rate / 10, 3)) |> 
  ggplot(aes(x = p_ref, y = p_diff, colour = log_or, group = log_or)) +
  geom_line() +
  geom_vline(xintercept = rate / 10) +
  facet_wrap(~log_or)


# so it's basically an increase of 60ish percent, from around
# 28% to 88%ish

dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 22,
         round(p_ref, 2) == 0.28) |> 
  ggplot(aes(x = p_ref, y = p_diff, colour = log_or, group = log_or)) +
  geom_line() +
  geom_vline(xintercept = 0.28) +
  facet_wrap(~log_or)


dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 22) |> 
  ggplot(aes(x = p_ref, y = p_diff, colour = log_or, group = log_or)) +
  geom_line() +
  geom_vline(xintercept = 0.28) +
  facet_wrap(~log_or)


# but in the population the proportion is closer to 0.04%
# so it would go up to like 0.85%

dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 22,
         round(p_ref, 4) == 0.0004) |> 
  ggplot(aes(x = p_ref, y = p_diff, colour = log_or, group = log_or)) +
  geom_line() +
   geom_vline(xintercept = 0.0004) +
  facet_wrap(~log_or)



# but it's actually closer to 9%

1850 / 9992

30/105

300/340


