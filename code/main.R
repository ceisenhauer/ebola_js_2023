# --------------------------------------------------------------------------------------------------
# 2022 Ebola Epidemic, Uganda -- MOH Report
#
# Author : Catherine Eisenhauer
# Date : May 2023
# --------------------------------------------------------------------------------------------------

library(tidytable)
library(ggplot2)
library(epiplots)

ggplot2::theme_set(theme_clean(legend_position = 'top'))



# SET UP MAPS --------------------------------------------------------------------------------------
# background tiles : historic outbreaks
uga_box <- c(top = 1.1,
             bottom = -0.45,
             left = 30,
             right = 33.5)

uga_map <- ggmap::get_map(location = uga_box,
                          zoom = 10,
                          source = 'stamen',
                          maptype = 'terrain-background')

# background tiles : 2022 outbreak
hist_box <- c(top = 5,
              bottom = -4,
              left = 26,
              right = 40)

hist_map <- ggmap::get_map(location = hist_box,
                           zoom = 8,
                           source = 'stamen',
                           maptype = 'terrain-background')


# admin boundaries -----
uga <- epiplaces::load_map('uganda',
                           level = 'zone')

africa <- epiplaces::load_map('africa')



# IMPORT DATA --------------------------------------------------------------------------------------
locations <- rio::import(here::here('data', 'map_locations.csv'))

etus <- rio::import(here::here('data', 'locations_etus.xlsx')) %>%
  mutate.(week = lubridate::floor_date(as.Date(date_operational),
                                       unit = 'weeks',
                                       week_start = 1)) %>%
  select.(lat, long, type, week)

hist <- rio::import(here::here('data', 'global_history.xlsx')) %>%
  select.(strain, lat, long, cases, deaths) %>%
  rename.(Cases = cases,
         Deaths = deaths) %>%
  tidyr::pivot_longer(c(Cases, Deaths),
                      names_to = 'type',
                      values_to = 'n')


ll <- rio::import(here::here('data', 'linelist_clean.rds'))

df <- rio::import(here::here('data', 'linelist_clean.rds')) %>%
  select.(-age) %>%
  rename.(class = classification,
          age = age_class,
          week_onset = week_of_onset) %>%
  mutate.(exit = case_when(class == 'Probable' ~ 'Probable Death',
                           status == 'Alive' ~ 'Confirmed Recovery',
                           TRUE ~ 'Confirmed Death'),
          exit = factor(exit,
                        levels = c('Confirmed Recovery', 'Probable Death', 
                                   'Confirmed Death'))) %>%
  select.(district, age, sex, exit, status, week_onset) 



# MAP OF HISTORIC UGANDAN OUTBREAKS ----------------------------------------------------------------
geom_choro <- purrr::partial(
  geom_sf,
  inherit.aes = FALSE,
  color = 'black'
)

districts_sudan <- c('gulu', 'luwero', 'kibaale', 'mubende')

ggmap::ggmap(hist_map) +
  geom_choro(data = africa,
             fill = 'transparent',
             linewidth = 0.75) +
  geom_choro(data = uga,
             fill = 'transparent',
             linetype = 'dotted',
             linewidth = 0.6) +
  geom_choro(data = uga %>% filter.(pcode %in% districts_sudan) %>% sf::st_as_sf(),
             fill = colors$blue,
             linewidth = 0.6) +
  geom_choro(data = uga %>% filter.(pcode == 'kasese') %>% sf::st_as_sf(),
             linewidth = 0.6,
             fill = colors$gold) +
  geom_choro(data = uga %>% filter.(pcode == 'bundibugyo') %>% sf::st_as_sf(),
             linewidth = 0.6,
             fill = colors$grey) +
  theme_void()

ggsave(here::here('out', '02_history.svg'),
       width = 19.6,
       height = 11.9)



# MAP AND EPICURVE OF OUTBREAK PROGRESSION ---------------------------------------------
# epicurve -----
tmp_cases <- df %>%
  summarize.(cases = n(),
             .by = c(exit, week_onset)) 

tmp_survivors <- df %>%
  summarize.(cases = n(),
             survivors = sum(exit == 'Confirmed Recovery'),
             .by = c(week_onset)) %>%
  mutate.(risk_survive = survivors / cases)

scale_factor <- max(tmp_survivors$cases) / max(tmp_survivors$risk_survive)

tmp_cases %>%
  ggplot(aes(x = week_onset)) +
  geom_col(aes(y = cases,
               fill = exit)) +
  geom_line(data = tmp_survivors,
            aes(y = risk_survive * scale_factor),
            color = colors$grey_dark_2,
            linewidth = 1.5,
            linetype = 'dotted') +
  scale_fill_manual(name = '',
                    values = c(colors$grey_light_3,
                               colors$blue_light,
                               colors$blue_darker)) +
  second_axis(scale_factor = scale_factor,
              breaks = c(0, 0.5, 1),
              percent = TRUE,
              color = colors$grey_dark_2,
              title = 'Survival Risk') +
  date_axis(date_breaks = '2 weeks') +
  ylab('Cases') +
  theme(legend.position = 'top')

ggsave(here::here('out', '04_epicurve_by_exit_wide.svg'),
       height = 5.09,
       width = 5.77)


# maps
timepoints <- c('2022-09-12',
                '2022-09-26',
                '2022-09-19',
                '2022-10-03',
                '2022-10-10',
                '2022-10-17',
                '2022-10-24',
                '2022-10-31',
                '2022-11-14')

for (t in timepoints) {
  tmp_etus <- etus %>%
    filter.(week <= t)

  tmp_cases <- df %>%
    filter.(week_onset <= t) %>%
    summarize.(cases = n(),
               .by = c('district', 'status')) %>%
    tidyr::pivot_wider(names_from = status,
                       values_from = cases) %>%
    mutate.(across.(-district, ~ tidyr::replace_na(., 0)),
            Alive = Alive + Dead) %>%
    tidyr::pivot_longer(-district,
                        names_to = 'type',
                        values_to = 'n') %>%
    mutate.(type = dplyr::recode(type,
                                 Alive = 'Cases',
                                 Dead = 'Deaths')) %>%
    left_join.(locations)

  districts <- tmp_cases %>%
    mutate.(district = case_when(district == 'Jinja' ~ 'Jinja City',
                                 TRUE ~ district)) %>%
    pull.(district) %>%
    unique()

  ggmap::ggmap(uga_map) +
    geom_choro(data = africa,
               fill = 'transparent',
               linewidth = 0.75) +
    geom_choro(data = uga,
               fill = 'transparent',
               linetype = 'dotted',
               linewidth = 0.6) +
    geom_choro(data = uga %>% filter.(adm1_name %in% districts) %>% sf::st_as_sf(),
               fill = 'transparent',
               linewidth = 0.6) +
    geom_point(data = data.frame(n = 83,
                                 lat = 0.567,
                                 long = 31.9),
               aes(x = long,
                   y = lat,
                   size = n),
               color = 'transparent') +
    geom_point(data = tmp_cases,
               aes(x = long,
                   y = lat,
                   size = n),
               color = colors$blue,
               alpha = 0.4) +
    scale_size_continuous(name = 'Patients',
                          breaks = c(20, 40, 60, 80),
                          range = c(5, 45)) +
    geom_point(data = tmp_etus,
               aes(x = long,
                   y = lat,
                   shape = type),
               color = 'white',
               size = 4) +
    geom_point(data = tmp_etus,
               aes(x = long,
                   y = lat,
                   shape = type),
               color = 'black',
               size = 3) +
    scale_shape_manual(name = '',
                       values = c(5, 0)) + 
    theme_void()

  ggsave(here::here('out', paste0('04_snapshot_', t, '.svg')),
         width = 15.7,
         height = 8.16)
}



# CHAINS OF TRANSMISSION ---------------------------------------------------------------------------



# DEMOGRAPHY ---------------------------------------------------------------------------------------
# sex/age pyramid -----
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
               text_size = 7,
               limits_left = c(0, 30),
               val_left = 'Female',
               fills_left = c(colors$blue_light_4,
                              colors$blue_light,
                              colors$blue_dark),
               color_text_left = colors$blue,
               val_right = 'Male',
               fills_right = c(colors$grey_light_4,
                                colors$grey_light,
                                colors$grey_dark),
               color_text_right = colors$grey)

ggsave(here::here('out', '13_sex_pyramid.svg'),
       width = 10.1,
       height = 6.25)


# cfr by sex -----
tmp <- df %>%
  summarize.(cases = n(),
             deaths = sum(status == 'Dead'),
             .by = c(sex)) %>%
  mutate.(cfr = deaths / cases,
          lower = binom::binom.confint(deaths, cases, methods = 'exact')$lower,
          upper = binom::binom.confint(deaths, cases, methods = 'exact')$upper)

tmp %>%
  ggplot(aes(x = sex,
             y = cfr,
             color = sex)) +
  geom_point(size = 10) +
  geom_point(aes(x = sex,
                 y = upper),
             size = 2) +
  geom_point(aes(x = sex,
                 y = lower),
             size = 2) +
  geom_segment(aes(x = sex,
                   xend = sex,
                   y = lower,
                   yend = upper),
               size = 3) +
  scale_color_manual(values = c(colors$blue_light_1,
                                colors$grey_light_1)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0.2, 0.9)) +
  theme(legend.position = 'none') +
  ylab('Case Fatality') +
  xlab('Sex')

ggsave(here::here('out', '13_fatality_by_sex.png'),
       width = 3.73,
       height = 4.07)

# cfr by age ------
tmp <- df %>%
  mutate.(age = case_when(age %in% c('60-69', '70+', '50-59') ~ '50+',
                          TRUE ~ age),
          age = factor(age,
                       levels = c(
                                  '0-9',
                                  '10-19',
                                  '20-29',
                                  '30-39',
                                  '40-49',
                                  '50+'
                                  ))) %>%
  summarize.(cases = n(),
             deaths = sum(status == 'Dead'),
             .by = c(age)) %>%
  mutate.(cfr = deaths / cases,
          lower = binom::binom.confint(deaths, cases, methods = 'exact')$lower,
          upper = binom::binom.confint(deaths, cases, methods = 'exact')$upper)

tmp %>%
  ggplot(aes(x = age,
             y = cfr,
             color = age)) +
  geom_point(size = 10) +
  geom_point(aes(x = age,
                 y = upper),
             size = 2) +
  geom_point(aes(x = age,
                 y = lower),
             size = 2) +
  geom_segment(aes(x = age,
                   xend = age,
                   y = lower,
                   yend = upper),
               size = 3) +
  scale_color_manual(values = c(colors$gold_dark_4,
                                colors$gold_dark_3,
                                colors$gold_dark_1,
                                colors$gold,
                                colors$gold_light_1,
                                colors$gold_light_2)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0.2, 0.9)) +
  theme(legend.position = 'none') +
  ylab('Case Fatality') +
  xlab('Age')

ggsave(here::here('out', '13_fatality_by_age.png'),
       width = 7.2,
       height = 4.07)


# KIDS ---------------------------------------------------------------------------------------------
# % < 10 in time
tmp_cases <- df %>%
  summarize.(kids = sum(age == '0-9'),
             cases = n(),
             .by = c(week_onset)) %>%
  filter.(!is.na(week_onset)) %>%
  mutate.(prop_kids = kids / cases)

scale_factor <- max(tmp_cases$cases) / max(tmp_cases$prop_kids)

tmp_cases %>%
  ggplot(aes(x = week_onset)) +
  geom_col(aes(y = cases / scale_factor),
           fill = colors$grey_light_4) +
  geom_line(aes(y = prop_kids),
            color = colors$blue,
            linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent) +
  date_axis(date_breaks = '2 weeks') +
  ylab('Children < 10yo') +
  theme(legend.position = 'top')

ggsave(here::here('out', '14_kids.png'),
       width = 8.94,
       height = 4.12)


