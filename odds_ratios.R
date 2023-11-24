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
  mutate(logs = map(reference, make_logs, seq(1, 200, 1),
                    .progress = TRUE)) |> 
  select(-reference) |> 
  unnest(logs)

# odds-ratios all the way to 200

or_selection <- c(1, 1.25, 3, 5, 10, 25, 50, 100, 200)

merlo_plot <- 
dat3 |> 
  ggplot(aes(x = p_ref, y = p_or, colour = log_or, group = log_or)) +
  geom_line(alpha = 0.3) +
  geomtextpath::geom_textline(data = dat3 |> 
                                filter(log_or %in% or_selection),
                              aes(label = log_or)) +
  theme_minimal() +
  labs(x = "Probability in reference group",
       y = "Probability in comparison group",
       colour = "Odds\nratio") +
  coord_equal()

ggsave(
  here::here("03_figures", "merlo_plot.png"),
  merlo_plot,
  height = 5, width = 7,
  type = "cairo-png"
)



# difference plot

diff_plot <- 
dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or, colour = log_or)) +
  geom_line(alpha = 0.3) +
  geomtextpath::geom_textline(data = dat3 |> 
                                mutate(p_diff = p_or - p_ref) |> 
                                filter(log_or %in% or_selection),
                              aes(label = log_or)) +
  theme_minimal() +
  labs(x = "Probability in reference group",
       y = "Difference in probability in comparison group",
       colour = "Odds\nratio") +
  coord_equal()

ggsave(
  here::here("03_figures", "diff_plot.png"),
  diff_plot,
  height = 5, width = 7,
  type = "cairo-png"
)

# but in the sample this is

sample_plot <- 
dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or, colour = log_or)) +
  geom_line(alpha = 0.1) +
  geomtextpath::geom_textline(data = dat3 |> 
                                mutate(p_diff = p_or - p_ref) |> 
                                filter(log_or == 35),
                              aes(label = log_or)) +
  theme_minimal() +
  labs(x = "Probability in reference group",
       y = "Difference in probability in comparison group",
       colour = "Odds\nratio") +
  coord_equal()


ggsave(
  here::here("03_figures", "sample_plot.png"),
  sample_plot,
  height = 5, width = 7,
  type = "cairo-png"
)

# so what is a marginal effect?

# for a given odds ratio it has to live on this line


# MEM

mem_dat <- 
dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 35 & ref == -2)
  
mem_plot <- 
dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or, colour = log_or)) +
  geom_line(alpha = 0.1) +
  geomtextpath::geom_textline(data = dat3 |> 
                                mutate(p_diff = p_or - p_ref) |> 
                                filter(log_or == 35),
                              aes(label = log_or)) +
  geom_point(data = mem_dat,
             size = 2) +
  geom_segment(data = mem_dat,
               aes(xend = p_ref, yend = 0),
               linetype = "dashed") +
  theme_minimal() +
  labs(x = "Probability in reference group",
       y = "Difference in probability in comparison group",
       colour = "Odds\nratio") +
  coord_equal()

ggsave(
  here::here("03_figures", "mem_plot.png"),
  mem_plot,
  height = 5, width = 7,
  type = "cairo-png"
)


# MER plot

mer_dat <- 
  dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 35 & ref == -2 |
           log_or == 35 & ref == -2.5)

mer_plot <- 
dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or, colour = log_or)) +
  geom_line(alpha = 0.1) +
  geomtextpath::geom_textline(data = dat3 |> 
                                mutate(p_diff = p_or - p_ref) |> 
                                filter(log_or == 35),
                              aes(label = log_or)) +
  geom_point(data = mer_dat,
             size = 2) +
  geom_segment(data = mer_dat,
               aes(xend = p_ref, yend = 0),
               linetype = "dashed") +
  theme_minimal() +
  labs(x = "Probability in reference group",
       y = "Difference in probability in comparison group",
       colour = "Odds\nratio") +
  coord_equal()

ggsave(
  here::here("03_figures", "mer_plot.png"),
  mer_plot,
  height = 5, width = 7,
  type = "cairo-png"
)

# AMEs

ame_dat <- 
  dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(log_or == 35) |> 
  filter(ref %in% round(rnorm(100, -2, 1), 2))

ame_plot <- 
dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or, colour = log_or)) +
  geom_line(alpha = 0.1) +
  geomtextpath::geom_textline(data = dat3 |> 
                                mutate(p_diff = p_or - p_ref) |> 
                                filter(log_or == 35),
                              aes(label = log_or)) +
  geom_point(data = ame_dat,
             size = 2,
             alpha = 0.5) +
  geom_point(data = ame_dat |> 
               summarise(p_ref = mean(p_ref),
                         p_diff = mean(p_diff),
                         log_or = mean(log_or)),
             size = 3, colour = "red") +
  theme_minimal() +
  labs(x = "Probability in reference group",
       y = "Difference in probability in comparison group",
       colour = "Odds\nratio") +
  coord_equal()

ggsave(
  here::here("03_figures", "ame_plot.png"),
  ame_plot,
  height = 5, width = 7,
  type = "cairo-png"
)

# back to our data --------------------------------------------------------


our_dat_plot <- 
dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or, colour = log_or)) +
  geom_line(alpha = 0.1) +
  geomtextpath::geom_textline(data = dat3 |> 
                                mutate(p_diff = p_or - p_ref) |> 
                                filter(log_or == 35),
                              aes(label = log_or)) +
  geom_vline(aes(xintercept = 0.25)) +
  theme_minimal() +
  labs(x = "Probability in reference group",
       y = "Difference in probability in comparison group",
       colour = "Odds\nratio") +
  coord_equal()


ggsave(
  here::here("03_figures", "our_dat_plot.png"),
  our_dat_plot,
  height = 5, width = 7,
  type = "cairo-png"
)

dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(round(p_ref, 3) == 0.250,
         log_or == 35)



# for our study approximately 92% of people with 3 or more health conditions
# had an fpn? yeah i think so!

# increase in prob from prob of having an fpn with ! 3 or more health conditions
# so n_fpn !3+ health / n_fpn 3+ health + n_ctrl 3+ health
# compared to n_fpn 3+ health / n_fpn 3+ health + n_ctrl 3+health




# but we know that in the population the probability in the reference was
# way way way lower


our_dat_plot2 <- 
dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  ggplot(aes(x = p_ref, y = p_diff, group = log_or, colour = log_or)) +
  geom_line(alpha = 0.1) +
  geomtextpath::geom_textline(data = dat3 |> 
                                mutate(p_diff = p_or - p_ref) |> 
                                filter(log_or == 35),
                              aes(label = log_or)) +
  geom_vline(aes(xintercept = 0.00044)) +
  theme_minimal() +
  labs(x = "Probability in reference group",
       y = "Difference in probability in comparison group",
       colour = "Odds\nratio") +
  coord_equal()

ggsave(
  here::here("03_figures", "our_dat_plot2.png"),
  our_dat_plot2,
  height = 5, width = 7,
  type = "cairo-png"
)

dat3 |> 
  mutate(p_diff = p_or - p_ref) |> 
  filter(round(p_ref, 5) == 0.00044,
         log_or == 35)



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

300 / (300 + 41)



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



