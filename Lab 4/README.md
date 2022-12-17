# Основы обработки данных с помощью R

## Цель работы
1. Зекрепить практические навыки использования языка программирования R для обработки данных
2. Закрепить знания основных функций обработки данных экосистемы tidyverse языка R
3. Развить пркатические навыки использования функций обработки данных пакета dplyr – функции select(), filter(), mutate(), arrange(), group_by()


```{r}
library(nycflights13)
library(dplyr)
```



# 1. Сколько встроенных в пакет nycflihgts13 датафремов?
```{r}
nycflights13::airlines
nycflights13::airports
nycflights13::flights
nycflights13::planes
nycflights13::weather
```

# 2. Сколько строк в каждом датафрейме?
```{r}
print('количество строк в airlines:')
nrow(airlines)

print('количество строк в airports:')
nrow(airports)

print('количество строк в flights:')
nrow(flights)

print('количество строк в planes:')
nrow(planes)

print('количество строк в weather:')
nrow(weather)
```

# 3. Сколько столбцов каждом датафрейме?
```{r}
print('количество столбцов в airlines:')
ncol(airlines)

print('количество столбцов в airports:')
ncol(airports)

print('количество столбцов в flights:')
ncol(flights)

print('количество столбцов в planes:')
ncol(planes)

print('количество столбцов в weather:')
ncol(weather)
```


# 4. Как посмотреть примерный вид датафрейма?
```{r}
glimpse(flights)
```


# 5. Сколько компаний-перевозчиков (carrier) учитывают эти наборы данных ?
```{r}
airlines %>% nrow()
```

# 6. Сколько рейсов принял аэропорт John F Kennedy Intl в мае?
```{r}
z <- flights %>% filter(origin == 'JFK', month == 5)
count(z)

```


# 7. Какой самый северный аэропорт?
```{r}
sever <- max(airports$lat)
airports %>% filter(lat == sever)
```



# 8. Какой аэропорт самый высокогорный?
```{r}
visokiy <- max(airports$alt)
airports %>% filter(alt == visokiy)
```


# 9. Какие бортовые номера у самых старых самолётов?
```{r}
planes %>% 
  filter(year == min(year,na.rm = TRUE)) %>% select (tailnum)

```

# 10. Какая средняя температура воздуха была в сентябре в аэропорту John F Kennedy Intl (в градусах Цельсия).
```{r}
weather %>% filter(origin == "JFK",month == 9) %>% summarise(avg_temp = mean(5/9*(temp - 32), na.rm=TRUE))
```


# 11. Самолеты какой авиакомпании совершили больше всего вылетов в июне?
```{r}
carr <- flights %>% filter(month == 6) %>%
  group_by(carrier) %>% 
  summarise(n_flights=n()) %>% 
  arrange(desc(n_flights)) %>%
  head(1) %>%
  select(carrier) %>% paste(sep='')
airlines %>% filter(carrier == carr)
```

# 12.Самолеты какой авиакомпании задерживались чаще других в 2013 году?
```{r}
flights.2013 <- flights %>% 
  filter(year == 2013) %>%
  filter(dep_delay>0) %>%
  group_by(carrier) %>% 
  summarise(N = n())
airlines %>% filter(carrier == carr)
```