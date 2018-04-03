# 1. Cargamos las librerias de tidyverse ----

#install.packages("tidyverse")
library(tidyverse)

# 2. Leemos los datos ----

data_sales <- readr::read_csv("data/data.csv")

head(data_sales)

# 3. Limpiamos los datos ----

table_sales <- data_sales %>% 
  tidyr::gather(gastos, ventas, key = "transaccion", value = "total") %>% 
  tidyr::separate_rows(total) %>% 
  dplyr::mutate(
    dia_del_mes = lubridate::dmy(dia_del_mes), 
    total = as.double(total)
  )
  
# 4. Balance de cada dia ----

table_sales %>% 
  dplyr::mutate(total = dplyr::case_when(transaccion == "gastos" ~ -1 * total,
                                         TRUE ~ total)
                ) %>% 
  dplyr::group_by(dia_del_mes) %>% 
  dplyr::summarise(balance = sum(total))

# 5. Dia de la semana en el que se realizan mas ventas ----

table_sales %>% 
  dplyr::filter(transaccion == "ventas") %>% 
  dplyr::mutate(dia_semana = lubridate::wday(dia_del_mes, label = TRUE)) %>% 
  group_by(dia_semana) %>% 
  dplyr::summarise(total_ganado = mean(total)) %>% 
  ggplot(aes(x = dia_semana, y = total_ganado)) +
  geom_col()
