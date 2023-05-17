# --------------------------------------------------------------------------------------------------
# 2022 Ebola Epidemic, Uganda -- MOH Report
#
# Author : Catherine Eisenhauer
# Date : May 2023
# --------------------------------------------------------------------------------------------------


library(tidytable)
library(ggplot2)
library(epiplots)

colors_exit <- c(colors$blue_light_3,
                 colors$blue,
                 colors$blue_darker)

ggplot2::theme_set(theme_clean(legend_position = 'top'))

# thanks @andyfeucher: https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# IMPORT -------------------------------------------------------------------------------------------
uga_box <- c(top = 1.1,
             bottom = -0.45,
             left = 30,
             right = 33.5)

uga_map <- ggmap::get_map(location = uga_box, #osmdata::getbb('mubende'),
                          zoom = 10,
                          source = 'stamen',
                          maptype = 'terrain')

hist_box <- c(top = 3.2,
              bottom = 0.1,
              left = 29.5,
              right = 32.8)

hist_box <- c(top = 5,
              bottom = -4,
              left = 26,
              right = 40)

hist_map <- ggmap::get_map(location = hist_box,
                           zoom = 8,
                           source = 'stamen',
                           maptype = 'terrain-background')

#uga <- epiplaces::load_map('uganda',
                           #level = 'zone')

#uga_subset <- uga %>%
  #filter.(adm1_name %in% c('Mubende', 'Kyegegwa', 'Kassanda', 'Kabarole', 'Kibaale')) %>%
  #sf::st_as_sf()

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


# WRANGLE ------------------------------------------------------------------------------------------

# TIME VARIABLE INDICATORS -----
## survival rate
#df_cases <- df %>%
  #summarize.(cases = n(),
             #.by = c(exit, week_onset)) 

#df_survivors <- df %>%
  #summarize.(cases = n(),
             #survivors = sum(exit == 'Confirmed Recovery'),
             #.by = c(week_onset)) %>%
  #mutate.(risk_survive = survivors / cases)

#scale_factor <- max(tmp_survivors$cases) / max(tmp_survivors$risk_survive)



# % under 10

# PLOTS : DESCRIPTIVE ------------------------------------------------------------------------------

# SLIDE 2 : MAP OF HISTORIC UGANDAN OUTBREAKS -----
ggmap::ggmap(hist_map) +
  #geom_sf(data = africa,
          #fill = 'transparent',
          #color = colors$grey)
  #geom_point(data = hist,
             #aes(x = long,
                 #y = lat,
                 #color = strain),
                 ##size = n),
             #size = 10,
             #alpha = 1) +
  #scale_size_continuous(name = 'Patients',
                        ##trans = 'log',
                        #range = c(5, 65)) +
  #scale_color_manual(name = 'Strain',
                     #values = c(colors$grey_dark,
                                #colors$blue,
                                #colors$gold)) +
  theme_void()

districts_sudan <- c('gulu', 'luwero', 'kibaale', 'mubende')

#ggplot(africa) +
  #geom_sf(fill = 'white',
          #color = colors$grey_light_3) +
#ggplot() +
#ggmap::ggmap(hist_map) +
map_bg <- ggmap_bbox(hist_map)

ggmap::ggmap(hist_map) +

ggplot() +
  geom_sf(data = uga,
          #linetype = 'dashed',
          fill = 'transparent',
          color = 'black') +
  geom_sf(data = uga %>% filter.(pcode %in% districts_sudan) %>% sf::st_as_sf(),
          #linetype = 'dashed',
          fill = colors$blue,
          color = 'white') +
  geom_sf(data = uga %>% filter.(pcode == 'kasese') %>% sf::st_as_sf(),
          #linetype = 'dashed',
          fill = colors$gold,
          color = 'white') +
  geom_sf(data = uga %>% filter.(pcode == 'bundibugyo') %>% sf::st_as_sf(),
          #linetype = 'dashed',
          fill = colors$grey,
          color = 'white') +
  theme_void()

ggsave(here::here('out', '02_history_choro_forground.svg'),
       width = 12.9,
       height = 9.61)

ggsave(here::here('out', '02_history_background.svg'),
       width = 33.9,
       height = 21.8)
       #width = 19.6,
       #height = 11.9)

,
       width = 13.6,
       height = 21.9)



# SLIDE 4-9 : MAP AND EPICURVE OF OUTBREAK PROGRESSION -----

# epicurve
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
                '2022-10-03',
                '2022-10-17',
                '2022-10-24',
                '2022-11-24')

for (t in timepoints) {

  tmp_etus <- etus %>%
    filter.(week <= t)

  tmp_cases <- df %>%
    filter.(week_onset <= t) %>%
    #summarize.(cases = n(),
               #.by = 'district')
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

  #ggplot(uga) +
    #geom_sf(fill = 'white',
            #color = colors$grey_light_3) +
    #geom_sf(data = uga_subset,
            #fill = 'white',
            #color = colors$grey) +
    #geom_point(data = tmp_cases,
               #aes(x = long,
                   #y = lat,
                   #size = n),
               #color = colors$blue,
               #alpha = 0.5) +
    #scale_size_continuous(name = 'Patients',
                          ##trans = 'log',
                          #range = c(1, 10)) +
    #theme_void()


    ggmap::ggmap(uga_map) +
      geom_point(data = tmp_cases,
                 aes(x = long,
                     y = lat,
                     size = n),
                 color = colors$blue,
                 alpha = 0.4) +
      scale_size_continuous(name = 'Patients',
                            #trans = 'log',
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

# SLIDE 11 : CHAINS OF TRANSMISSION -----



# SLIDE 13 : DEMOGRAPHY -----
# sex/age pyramid
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
               #limits_left = c(0, 30),
               limits_right = c(0, 30),
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
       #height = 4.17,
       #width = 8.39)

# cfr by sex 
tmp <- df %>%
  summarize.(cases = n(),
             deaths = sum(status == 'Dead'),
             .by = c(sex)) %>%
  mutate.(cfr = deaths / cases,
          lower = binom::binom.confint(deaths, cases, methods = 'exact')$lower,
          upper = binom::binom.confint(deaths, cases, methods = 'exact')$upper)

tmp %>%
  ggplot(aes(x = sex,
             y = cfr)) +
  geom_point(size = 8,
             color = colors$grey_light_1) +
  geom_segment(aes(x = sex,
                   xend = sex,
                   y = lower,
                   yend = upper),
               color = colors$grey_light_1) + 
  scale_y_continuous(labels = scales::percent,
                     limits = c(0.2, 0.9)) +
  ylab('Case Fatality') +
  xlab('Sex')

ggsave(here::here('out', '13_fatality_by_sex.png'),
       width = 3.73,
       height = 4.07)

# cfr by age 
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
                                  #'50-59',
                                  #'60-69',
                                  #'70+'
                                  ))) %>%
  summarize.(cases = n(),
             deaths = sum(status == 'Dead'),
             .by = c(age)) %>%
  mutate.(cfr = deaths / cases,
          lower = binom::binom.confint(deaths, cases, methods = 'exact')$lower,
          upper = binom::binom.confint(deaths, cases, methods = 'exact')$upper)

tmp %>%
  ggplot(aes(x = age,
             y = cfr)) +
  geom_point(size = 8,
             color = colors$blue) +
  geom_segment(aes(x = age,
                   xend = age,
                   y = lower,
                   yend = upper),
               color = colors$blue) + 
  scale_y_continuous(labels = scales::percent,
                     limits = c(0.2, 0.9)) +
  ylab('Case Fatality') +
  xlab('Age')

ggsave(here::here('out', '13_fatality_by_age.png'),
       width = 7.2,
       height = 4.07)


# cfr by age + sex
tmp <- df %>%
  mutate.(age = case_when(age %in% c('60-69', '70+', '50-59') ~ '50+'),
          age = factor(age,
                       levels = c(
                                  '0-9',
                                  '10-19',
                                  '20-29',
                                  '30-39',
                                  '40-49',
                                  '50+'
                                  #'50-59',
                                  #'60-69',
                                  #'70+'
                                  ))) %>%
  summarize.(cases = n(),
             deaths = sum(status == 'Dead'),
             .by = c(sex, age)) %>%
  mutate.(cfr = deaths / cases,
          lower = binom::binom.confint(deaths, cases, methods = 'exact')$lower,
          upper = binom::binom.confint(deaths, cases, methods = 'exact')$upper)


tmp %>%
  ggplot(aes(x = age,
             y = cfr,
             color = sex)) +
  geom_point(size = 8) +
  geom_segment(aes(x = age,
                   xend = age,
                   y = lower,
                   yend = upper),
               color = colors$blue) + 
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '',
                     values = c(colors$blue_light,
                                colors$grey_dark)) +
  ylab('Case Fatality') +
  xlab('Age')

ggsave(here::here('out', '13_fatality_by_sex_age.png'),
       width = 10.1,
       height = 6.25)


# SLIDE 14 : KIDS -----
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
  #second_axis(scale_factor = scale_factor,
              #breaks = c(0, 0.5, 1),
              #percent = TRUE,
              #color = colors$grey_dark_2,
              #title = 'Survival Risk') +
  scale_y_continuous(labels = scales::percent) +
  date_axis(date_breaks = '2 weeks') +
  ylab('Children < 10yo') +
  theme(legend.position = 'top')

ggsave(here::here('out', '14_kids.png'),
       width = 8.94,
       height = 4.12)


