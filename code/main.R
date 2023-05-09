# --------------------------------------------------------------------------------------------------
# 2022 Ebola Epidemic, Uganda -- MOH Report
#
# Author : Catherine Eisenhauer
# Date : Feb 2023
# --------------------------------------------------------------------------------------------------


library(tidytable)
library(ggplot2)
library(epiplots)

colors_exit <- c(colors$blue_light_3,
                 colors$blue,
                 colors$blue_darker)

ggplot2::theme_set(theme_clean(legend_position = 'top'))



# IMPORT -------------------------------------------------------------------------------------------
df <- rio::import(here::here('data', 'linelist_clean.rds')) %>%
  select.(-age) %>%
  rename.(class = classification,
          age = age_class,
          week_onset = week_of_onset,
          week_conf = week_of_confirmation,
          date_onset = date_of_onset,
          date_conf = date_of_confirmation,
          date_iso = date_of_isolation,
          date_death = date_of_death,
          date_discharge = date_of_discharge) %>%
  mutate.(exit = case_when(class == 'Probable' ~ 'Probable Death',
                           status == 'Alive' ~ 'Confirmed Recovery',
                           TRUE ~ 'Confirmed Death'),
          exit = factor(exit,
                        levels = c('Confirmed Recovery', 'Probable Death', 
                                   'Confirmed Death'))) %>%
  select.(district, age, sex, hcw, exit, status, date_onset, week_onset, date_conf, 
          week_conf, date_iso, date_death, date_discharge)



# PLOTS : DESCRIPTIVE ------------------------------------------------------------------------------

# FIG : EPICURVE -----
tmp_cases <- df %>%
  summarize.(cases = n(),
             .by = c(exit, week_onset)) 

tmp_survivors <- df %>%
  #filter.(exit == 'Confirmed Recovery') %>%
  summarize.(cases = n(),
             survivors = sum(exit == 'Confirmed Recovery'),
             .by = c(week_onset)) %>%
  mutate.(risk_survive = survivors / cases)

scale_factor <- max(tmp_survivors$cases) / max(tmp_survivors$risk_survive)

tmp_cases %>%
  ggplot(aes(x = week_onset)) +
             #y = cases,
             #fill = exit)) +
  geom_col(aes(y = cases,
               fill = exit)) +
  geom_line(data = tmp_survivors,
            aes(y = risk_survive * scale_factor),
            color = colors$grey_light,
            size = 0.5,
            linetype = 'dashed') +
  scale_fill_manual(name = '',
                    values = c(colors$blue_light_3,
                               colors$blue,
                               colors$blue_darker)) +
  second_axis(scale_factor = scale_factor,
              breaks = c(0, 0.5, 1),
              percent = TRUE,
              color = colors$grey_light,
              title = 'Survival Risk') +
  date_axis(date_breaks = '2 weeks') +
  ylab('Cases') +
  theme(legend.position = 'top')

ggsave(here::here('out', 'plots', '01_epicurve_by_exit_wide.svg'))
       #height = 4.69,
       #width = 5.66)


# FIG : SEX PYRAMID -----
tmp <- df %>%
  summarize.(cases = n(),
             .by = c(exit, sex, age)) %>%
  tidyr::pivot_wider(names_from = sex,
                     values_from = cases) 

tmp %>%
  mutate.(age = factor(age,
                       levels = c(
                                  '70+',
                                  '60-69',
                                  '50-59',
                                  '40-49',
                                  '30-39',
                                  '20-29',
                                  '10-19',
                                  '0-9')),
          age = paste(age, 'Years')) %>%
  plot_pyramid(id = 'age',
               fill = 'exit',
               val_left = 'Female',
               fills_left = colors_exit,
               color_text_left = colors$blue,
               val_right = 'Male',
               fills_right = c(colors$grey_light_3,
                                colors$grey,
                                colors$grey_dark),
               color_text_right = colors$grey)

ggsave(here::here('out', 'plots', 'sex_pyramid.svg'),
       height = 4.17,
       width = 8.39)



# FIG : HCW -----
tmp_cases <- df %>%
  summarize.(cases = n(),
             .by = c(hcw, week_onset)) 

tmp_hcw <- df %>%
  #filter.(exit == 'Confirmed Recovery') %>%
  summarize.(cases = n(),
             hcw = sum(hcw),
             .by = c(week_onset)) %>%
  mutate.(risk_hcw = hcw / cases)

scale_factor <- max(tmp_hcw$cases) / max(tmp_hcw$risk_hcw)

p_series <- tmp_cases %>%
  mutate.(hcw = ifelse(hcw, 'HCW', 'Others'),
          hcw = factor(hcw, levels = c('Others', 'HCW'))) %>%
  ggplot(aes(x = week_onset)) +
             #y = cases,
             #fill = exit)) +
  geom_col(aes(y = cases,
               fill = hcw)) +
  geom_line(data = tmp_hcw,
            aes(y = risk_hcw * scale_factor),
            color = colors$grey,
            size = 0.5,
            linetype = 'dashed') +
  scale_fill_manual(name = '',
                    values = c(colors$blue_lighter,
                               colors$blue_dark)) +
  second_axis(scale_factor = scale_factor,
              breaks = c(0, 0.25, 0.5),
              percent = TRUE,
              color = colors$grey,
              title = 'HCWs') +
  date_axis(date_breaks = '2 weeks') +
  ylab('Cases') +
  theme(legend.position = 'top')

ggsave(here::here('out', 'plots', 'hcw_curve.svg'))
       #height = 4.69,
       #width = 5.66)

tmp <- df %>%
  summarize.(cases = n(),
             .by = c(district, hcw)) %>%
  mutate.(hcw = ifelse(hcw, 'HCW', 'Others'),
          hcw = factor(hcw, levels = c('Others', 'HCW')))

p_dist <- tmp %>%
  ggplot(aes(x = district,
             y = cases,
             fill = hcw)) +
  geom_col() +
  scale_fill_manual(name = '',
                    values = c(colors$blue_lighter,
                               colors$blue_dark)) +
  theme(axis.title.y = element_blank()) +
  coord_flip() +
  ylab('Cases') 

cowplot::plot_grid(p_dist, p_series, nrow = 1, rel_widths = c(1, 1.5))

ggsave(here::here('out', 'plots', 'hcw_subfigs.svg'))


# TIME TO ISOLATION ----
tmp <- df %>%
  mutate.(delay = as.numeric(date_iso - date_onset)) %>%
  filter.(!is.na(delay),
          delay >= 0) %>%
  select.(status, delay)

tmp %>%
  ggplot(aes(x = delay,
             #color = status,
             fill = status)) +
  geom_density(alpha = 0.5,
               outline.type = 'upper') +
  scale_fill_manual(name = '',
                    values = c(colors$gold,
                               colors$blue)) +
  labs(x = 'Delay to Isolation (Days)',
       y = 'Density')

ggsave(here::here('out', 'plots', 't_to_isolation.svg'))


# MAPS ---------------------------------------------------------------------------------------------
uga <- rio::import(here::here('data', 'UGA_adm1.rds')) %>%
  mutate.(adm1_name = ifelse(adm1_name == 'Kassnda', 'Kassanda', adm1_name),
          district = adm1_name) %>%
  sf::st_as_sf()

pal_cases <- list(
  '#819CBB',
  '#567099',
  '#2E4473',
  '#11244E',
  '#020A1B')


pal_deaths <- list(
  '#CEB992',
  '#B39F6E',
  '#94854d',
  '#625523',
  '#211C08')

df %>%
  summarize.(cases = n(),
             deaths = sum(status == 'Dead'),
             .by = district) %>%
  plot_map(x = 'cases',
           map = uga,
           color_palette = pal_cases,
           legend_title = 'Cases',
           na_color = 'white')

ggsave(here::here('out', 'map_cases.svg'),
       width = 5.59,
       height = 7.16)

df %>%
  summarize.(cases = n(),
             deaths = sum(status == 'Dead'),
             .by = district) %>%
  plot_map(x = 'deaths',
           map = uga,
           color_palette = pal_deaths,
           legend_title = 'Deaths',
           na_color = 'white')

ggsave(here::here('out', 'map_deaths.svg'),
       width = 5.59,
       height = 7.16)

