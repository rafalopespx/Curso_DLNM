## ----setup, include=FALSE-------------------------------------------------
knitr::opts_chunk$set(dpi=300, fig.width = 10)


## ----echo=FALSE,warning=FALSE,message=FALSE-------------------------------
library(knitr)

tb <- data.frame(paquete=c("ggplot2", "purrr", "tibble", "dplyr", "tidyr", "stringr", "readr", "forcats"),
                 descripcion=c("Gramática para la creación de gráficos",
                               "Programación funcional de R",
                               "Sistema moderno y efectivo de tablas",
                               "Gramatica para la  manipulación de datos",
                               "Conjunto de funciones para ordenar datos",
                               "Conjunto de funciones para trabajar con caracteres",
                               "Una forma fácil y rápida para importar datos",
                               "Herramientas y funciones para trabajar fácilmente con factores"))

kable(tb,booktabs = TRUE,col.names=c("Paquete","Descripción"))



## ----echo=FALSE,warning=FALSE,message=FALSE-------------------------------
# cargar el paquete
library(tidyverse)

tidyverse_packages()


## -------------------------------------------------------------------------
1:5 %>% mean()


## ----echo=FALSE-----------------------------------------------------------
library(knitr)
df <- data.frame(funcion=c("read_csv() o read_csv2()",
                           "read_delim()", "read_table()"),
                 descripcion=c("coma o punto-coma (CSV)",
                               "separador general", "espacio blanco"))


kable(df,booktabs = TRUE,col.names=c("Función lectura","Descripción"))



## -------------------------------------------------------------------------
# cargar el paquete
library(tidyverse)

google_mobility <- read_csv("./datos/Global_Mobility_Report.csv")
google_mobility



## ----echo=FALSE-----------------------------------------------------------
library(knitr)
df <- data.frame(funcion=c("str_replace()", "str_c()",
                           "str_detect()", "str_extract()",
                           "str_sub()", "str_length()"),
                 descripcion=c("reemplazar patrones",
                               "combinar characteres",
                               "detectar patrones", 
                               "extraer patrones",
                               "extraer por posición",
                               "longitud de la cadena de caracteres"))


kable(df,booktabs = TRUE,col.names=c("Función","Descripción"))



## -------------------------------------------------------------------------
# reemplazamos 'er' al final por vacío

str_replace(month.name, "er$", "")

str_replace(month.name, "^Ma", "")

# combinar caracteres

a <- str_c(month.name, 1:12, sep = "_")
a

# colapsar combinación

str_c(month.name, collapse = ", ")

# dedectamos patrones

str_detect(a, "_[1-5]{1}")

# extraemos patrones

str_extract(a, "_[1-9]{1,2}")

# extraermos los caracteres en las posiciones entre 1 y 2

str_sub(month.name, 1, 2)

# longitud de cada mes

str_length(month.name)

# con pipe, el '.' representa al objeto que pasa el operador %>%
str_length(month.name) %>% 
   str_c(month.name, ., sep = ".")



## -------------------------------------------------------------------------
name <- c("Juan", "Michael")
age <- c(50, 80) 
date_today <- Sys.Date()

str_glue(
  "My name is {name}, ",
  "I'am {age}, ",
  "and my birth year is {format(date_today-age*365, '%Y')}."
)


## -------------------------------------------------------------------------
# paquete
library(lubridate)

# vector de fechas
dat <- c("1999/12/31", "2000/01/07", "2005/05/20","2010/03/25")

# vector de fechas y horas
dat_time <- c("1988-08-01 05:00", "2000-02-01 22:00")

# convertir a clase date
dat <- ymd(dat) 
dat

# otras formatos
dmy("05-02-2000")
ymd("20000506")

# convertir a POSIXct
dat_time <- ymd_hm(dat_time)
dat_time

# diferentes formatos en un vector 
dat_mix <- c("1999/12/05", "05-09-2008", "2000/08/09", "25-10-2019")

# indicar formato con la convención conocida en ?strptime
parse_date_time(dat_mix, order = c("%Y/%m/%d", "%d-%m-%Y"))




## -------------------------------------------------------------------------
# extraer el año
year(dat)

# el mes
month(dat)
month(dat, label = TRUE) # como etiqueta

# el día de la semana
wday(dat)
wday(dat, label = TRUE) # como etiqueta

# la hora
hour(dat_time)

# sumar 10 días
dat + days(10)

# sumar 1 mes
dat + months(1)



## -------------------------------------------------------------------------
# crear fecha a partir de sus elementos, aquí con año y mes
make_date(2000, 5)

# crear fecha con hora 
make_datetime(2005, 5, 23, 5)


## ----echo=FALSE-----------------------------------------------------------
library(knitr)
df <- data.frame(funcion = c("mutate()","select()",
                           "filter()","summarise()","arrange()",
                           "group_by()", "rename()"),
                 descripcion = c("añadir nuevas variables o modificar existentes",
                               "seleccionar variables",
                               "filtrar","resumir/reducir","ordenar", "agrupar", "renombrar columnas"))


kable(df,booktabs = TRUE, col.names=c("Función","Descripción"))


## -------------------------------------------------------------------------
google_mobility <- read_csv("./datos/Global_Mobility_Report.csv")


## -------------------------------------------------------------------------
residential_mobility <- select(google_mobility, 
                               country_region_code:sub_region_1, 
                               date, 
                               residential_percent_change_from_baseline) %>% 
                        rename(resi = 5)


## -------------------------------------------------------------------------
filter(residential_mobility, 
       country_region_code == "US")

filter(residential_mobility, 
       country_region_code == "US", 
       sub_region_1 == "New York")

filter(residential_mobility, 
       resi > 50) %>% 
          arrange(-resi)


## ---- message=FALSE, error=FALSE, warning=FALSE---------------------------

resi_variability <- residential_mobility %>% 
                        filter(date == ymd("2020-04-01"),
                               !is.na(sub_region_1)) %>% 
                          group_by(country_region) %>% 
                             summarise(mx = max(resi, na.rm = TRUE), 
                                       min = min(resi, na.rm = TRUE),
                                       range = abs(mx)-abs(min))

arrange(resi_variability, -range)



## -------------------------------------------------------------------------
library(rnaturalearth) # paquete de datos vectoriales

# datos de países
wld <- ne_countries(returnclass = "sf")

# filtramos los países con código y seleccionamos las dos columnas de interés
wld <- filter(wld, !is.na(iso_a2)) %>% select(iso_a2, subregion)

# plot
plot(wld)


## -------------------------------------------------------------------------
subset_europe <- filter(residential_mobility, 
                        is.na(sub_region_1),
                        !is.na(resi)) %>%
                 left_join(wld, by = c("country_region_code"="iso_a2")) %>% 
                 filter(subregion %in% c("Northern Europe",
                                         "Southern Europe",
                                          "Western Europe",
                                          "Eastern Europe")) %>%
                 mutate(resi_real = resi + 100,
                        region = fct_reorder(country_region, 
                                             resi, 
                                            .fun = "max", 
                                            .desc = FALSE)) %>% 
                select(-geometry, -sub_region_1)

str(subset_europe)


## -------------------------------------------------------------------------
# subconjunto 
mobility_selection <- select(subset_europe, country_region_code, date:resi)
mobility_selection


# tabla ancha
mobi_wide <- pivot_wider(mobility_selection, 
                         names_from = country_region_code,
                         values_from = resi)
mobi_wide

# tabla larga
pivot_longer(mobi_wide,
             2:36,
             names_to = "country_code",
             values_to = "resi")


## -------------------------------------------------------------------------
# paquete
library(tsibble)

# cargar datos
load("./datos/datos_ts.RData")

# estructura
str(recetas_all) 



## -------------------------------------------------------------------------
# load package
recetas_all <- tsibble(recetas_all, key = c("ciudad", "atc2_codigo"), index = "fecha_pres") 

# clase
class(recetas_all)



## -------------------------------------------------------------------------
# filas dublicadas
is_duplicated(recetas_all, key = c("ciudad", "atc2_codigo")) # are_dublicated()

# lagunas?
has_gaps(recetas_all)
count_gaps(recetas_all)

# llenar lagunas
recetas_all <- recetas_all %>% 
                  fill_gaps(n = 0) 



## -------------------------------------------------------------------------
recetas_all %>% 
 group_by_key() %>% 
  index_by(year_month = ~ yearmonth(.)) %>%
   summarise(n = sum(n))


## -------------------------------------------------------------------------

recetas_all <- recetas_all %>% 
                arrange(ciudad, atc2_codigo, fecha_pres) %>% 
                  group_by(ciudad, atc2_codigo) %>% 
                    mutate(trend = row_number(),
                           dow = wday(fecha_pres, label = TRUE),
                           yy = year(fecha_pres),
                           mm = month(fecha_pres),
                           dd = day(fecha_pres),
                           ciudad = str_to_lower(ciudad)) 


str(recetas_all)



## ---- dpi = 300-----------------------------------------------------------
# creamos el subconjunto
it <- filter(google_mobility, 
             country_region == "Italy", 
             is.na(sub_region_1)) %>% 
      mutate(resi = residential_percent_change_from_baseline/100,   
             parks = parks_percent_change_from_baseline/100)


# gráfico de línea 
ggplot(it, 
       aes(date, resi)) + 
  geom_line()


# gráfico de dispersión con línea de correlación
ggplot(it, 
       aes(parks, resi)) + 
  geom_point() +
  geom_smooth(method = "lm")


## ---- dpi = 300-----------------------------------------------------------
# time serie plot
ggplot(it, 
       aes(date, resi)) + 
  geom_line(colour = "#560A86", size = 0.8) +
  scale_x_date(date_breaks = "10 days", 
               date_labels = "%d %b") +
  scale_y_continuous(breaks = seq(-0.1, 1, 0.1), 
                     labels = scales::percent) +
  labs(x = "", 
       y = "Residential mobility",
       title = "Mobility during COVID-19") +
  theme_light()

# scatter plot
ggplot(it, 
       aes(parks, resi)) + 
  geom_point(alpha = .4, size = 2) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(-1, 1.4, 0.2), 
                     labels = scales::percent) +
  scale_y_continuous(breaks = seq(-1, 1, 0.1), 
                     labels = scales::percent) +
  labs(x = "Park mobility", 
       y = "Residential mobility",
       title = "Mobility during COVID-19") +
  theme_light()


## ---- dpi = 300-----------------------------------------------------------
# subconjunto
subset_europe_reg <- filter(residential_mobility, 
                           !is.na(sub_region_1),
                           !is.na(resi)) %>%
                     left_join(wld, by = c("country_region_code"="iso_a2")) %>% 
                     filter(subregion %in% c("Northern Europe",
                                         "Southern Europe",
                                          "Western Europe",
                                          "Eastern Europe")) %>% 
                     mutate(resi = resi/100, 
                            country_region = fct_reorder(country_region, resi))

# boxplot
ggplot(subset_europe_reg, 
       aes(country_region, resi, fill = subregion)) + 
  geom_boxplot() +
  scale_y_continuous(breaks = seq(-0.1, 1, 0.1), labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
   labs(x = "", 
       y = "Residential mobility",
       title = "Mobility during COVID-19", 
       fill = "") +
  theme_minimal()



## -------------------------------------------------------------------------
# secuencia de fechas
df <- data.frame(d = seq(ymd("2020-02-15"), ymd("2020-06-07"), "day"))

# filtramos los domingos creando el día de la semana
sundays <- df %>% 
            mutate(wd = wday(d, week_start = 1)) %>% 
             filter(wd == 7) %>% 
              pull(d)


## -------------------------------------------------------------------------
Sys.setlocale("LC_TIME", "English")


## ---- dpi = 300, fig.width = 12.25, fig.height = 8------------------------
# headmap
ggplot(subset_europe, 
       aes(date, region, fill = resi_real)) +
  geom_tile() +
  scale_x_date(breaks = sundays,
               date_labels = "%d %b") +
  scale_fill_viridis_c(option = "A", 
                       breaks = c(91, 146),
                       labels = c("Less", "More"), 
                       direction = -1) +
  theme_minimal() +
  theme(legend.position = "top", 
        title = element_text(size = 14),
        panel.grid.major.x = element_line(colour = "white", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.ontop = TRUE,
        plot.margin = margin(r = 1, unit = "cm")) +
  labs(y = "", 
       x = "", 
       fill = "", 
       title = "Mobility trends for places of residence",
       caption = "Data: google.com/covid19/mobility/") +
  guides(fill = guide_colorbar(barwidth = 10, 
                               barheight = .5,
                               label.position = "top", 
                               ticks = FALSE)) +
  coord_cartesian(expand = FALSE)


## -------------------------------------------------------------------------
# paquetes

library(foreign)
library(ggthemes)
library(scales)

# datos
pee <- read.dta("./datos/EEpooled.dta")

str(pee) #estructura

pee <- mutate(pee, cause = str_sub(outcome, 1, 2) %>% 
                             factor(c("Re", "Cv"), c("Respiratory", "Cardiovascular")),
                   outcome = str_sub(outcome, 3, 3) %>% 
                             factor(c("D","A"), c("Deaths", "Hospital admissions"))
                    )

pee_ta <- read.dta("./datos/EEpooledCurves.dta")

str(pee_ta) # estructura

pee_ta <- mutate(pee_ta, cause = str_sub(outcome, 1, 2) %>% 
                                     factor(c("Cv", "Re"), c("Cardiovascular", "Respiratory")),
                         outcome = str_sub(outcome, 3, 3) %>% 
                                   factor(c("D","A"), c("Deaths", "Hospital admissions"))
                   )


## -------------------------------------------------------------------------
## pooled overall 
pee <- mutate(pee, lab = str_c(number(RR, big.mark = "", accuracy = .01), " (", 
                               number(lowRR, big.mark = "", accuracy = .01), "; ",
                               number(highRR, big.mark = "", accuracy = .01),")"),
              ylab = ifelse(exp == "Heat", 1.9, 1.6))



ggplot(pee, 
       aes(cause, RR, 
           ymin = lowRR, 
           ymax = highRR, 
           colour = outcome, 
           group = outcome)) +
  geom_hline(yintercept = 1) +
  geom_linerange(position = position_dodge(width = .5)) +
  geom_point(position = position_dodge(width = .5),
             fill = "white", 
             shape = 21,
             size = 1.5) +
  geom_text(aes(cause, ylab, label = lab), 
            colour = "black", size = 3, 
            hjust = 0, fontface = "bold",
            position = position_dodge(width = .5)) +
scale_color_brewer(palette = "Set1") +
scale_y_continuous(breaks = seq(0, 2, .2), limit = c(0.8, 2)) +
labs(colour = "", y = "RR", x = "") +
facet_wrap(~ exp) +
coord_flip(clip = "off") +
theme_hc() +
theme(plot.margin = margin(r = 50))


## -------------------------------------------------------------------------
## pooled by ta
ggplot(pee_ta) +
  geom_hline(yintercept = 1) +
  geom_vline(aes(xintercept = mmp, 
                 colour = outcome), 
             linetype = "dashed", 
             show.legend = FALSE) +
  geom_ribbon(aes(TempPer, 
                  ymin = lowRR, 
                  ymax = highRR, 
                  group = outcome), 
              fill = "grey70", 
              alpha = 0.3) +
  geom_line(aes(TempPer, RR, 
                colour = outcome, 
                group = outcome), 
            size = 1) +
  facet_grid(~ cause) +
  coord_cartesian(ylim=c(0.9, 2)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), 
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 2, 0.1), 
                     limit = c(0.9, 2)) +
  scale_color_brewer(palette = "Set1") +
  theme_classic() +
  theme(legend.position = "bottom", 
        panel.border = element_rect(fill = "transparent"),
        strip.background = element_rect(colour = NA , fill = "grey85")) +
  labs(x = "Temperature percentiles", colour = "") 



## -------------------------------------------------------------------------
# lista con dos vectores
vec_list <- list(x = 1:10, y = 50:70)

# calculamos el promedio para cada uno
map(vec_list, mean)

# podemos cambiar tipo de salida map_* (dbl, chr, lgl, etc.)
map_dbl(vec_list, mean)



## -------------------------------------------------------------------------
library(broom) # tidy outputs

# función adaptada 
cor_test <- function(x, formula) { 
  
df <- cor.test(as.formula(formula), data = x) %>% tidy()

return(df)
  
}

# preparamos los datos
europe_reg <- filter(google_mobility, 
                           !is.na(sub_region_1),
                           !is.na(residential_percent_change_from_baseline)) %>%
                     left_join(wld, by = c("country_region_code"="iso_a2")) %>% 
                     filter(subregion %in% c("Northern Europe",
                                         "Southern Europe",
                                          "Western Europe",
                                          "Eastern Europe"))
# aplicamos la función a cada país creando una lista
europe_reg %>%
  split(.$country_region_code) %>% 
  map(cor_test, formula = "~ residential_percent_change_from_baseline + parks_percent_change_from_baseline")  



## -------------------------------------------------------------------------
cor_mobility <- europe_reg %>%
                  split(.$country_region_code) %>% 
                     map_df(cor_test, 
                            formula = "~ residential_percent_change_from_baseline + parks_percent_change_from_baseline", 
                            .id = "country_code")

arrange(cor_mobility, estimate)

