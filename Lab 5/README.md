# Исследование метаданных DNS трафика
## Практическое задание №5
## Цель работы
 1. Закрепить практические навыки использования языка программирования R для обработки данных
 2. Закрепить знания основных функций обработки данных экосистемы tidyverse языка R
 3. Закрепить навыки исследования метаданных DNS трафика


Общая ситуация
Вы исследуете подозрительную сетевую активность во внутренней сети Доброй Организации. Вам в руки попали метаданные о DNS трафике в исследуемой сети. Исследуйте файлы, восстановите данные, подготовьте их к анализу и дайте обоснованные ответы на поставленные вопросы исследования.

# 1. Импортируйте данные DNS
```{r}
library(dplyr)

log_data = read.csv("dns.csv", header = FALSE,sep = "\t",encoding = "UTF-8")
```


# 2. Добавьте пропущенные данные о структуре данных (назначении столбцов)
```{r}
head=read.csv("header.csv", header = TRUE)
field<-head[,1]
log_data = read.csv("dns.csv", header = FALSE,sep = "\t",encoding = "UTF-8")
names(log_data)<-field
log_data%>%glimpse()
```



# 4. Сколько участников информационного обмена в сети Доброй Организации?

```{r }
orig_ip<-unique(log_data$`orig_ip `)
resp_ip<-unique(log_data$`resp_ip `)
all_ip<-merge(orig_ip,resp_ip)
NROW(unique(all_ip$`x`))
```

# 5. Какое соотношение участников обмена внутри сети и участников обращений к внешним ресурсам?
```{r}
point51<-unique(log_data$`orig_ip `,sort=TRUE)
point52<-unique(log_data$`resp_ip `,sort=TRUE)
collect <- c(point51, point52)
collect<-collect[!duplicated(collect)]
lall=length(collect)
toMatch <- c("(192.168.)([0-9]{1,3}[.])[0-9]{1,3}","(10.0.)([0-9]{1,3}[.])[0-9]{1,3}","(100.64.)([0-9]{1,3}[.])[0-9]{1,3}","(172.16.)([0-9]{1,3}[.])[0-9]{1,3}")
l1=length(unique(grep(paste(toMatch,collapse="|"), 
                        collect, value=TRUE)))
l2=lall-l1
res=l1/l2
res
```

# 6. Найдите топ-10 участников сети, проявляющих наибольшую сетевую активность.
```{r}
point1<-log_data%>%
count(log_data$`orig_ip `,sort=TRUE)
colnames(point1) <- c("Person", "count")
point2<-log_data%>%
count(log_data$`resp_ip `,sort=TRUE)
colnames(point2) <- c("Person", "count")
point_inf<-rbind(point1,point2)
point_inf<-point_inf %>%
  group_by(Person) %>%
  summarise(count)


point_inf<-point_inf[order(point_inf$Person, decreasing = TRUE), ]   
knitr::kable(point_inf, "pipe")
```


# 7. Найдите топ-10 доменов, к которым обращаются пользователи сети и соответственное количество обращений

```{r}
point_7<-log_data%>%
count(log_data$`query `,sort=TRUE)
point_7_head<-point_7%>%
head(10)
point_7_head
```

# 8. Опеределите базовые статистические характеристики (функция summary()) интервала времени между последовательным обращениями к топ-10 доменам.
```{r}
point_7_head%>%summary()
```

# 9. Часто вредоносное программное обеспечение использует DNS канал в качестве канала управления, периодически отправляя запросы на подконтрольный злоумышленникам DNS сервер. По периодическим запросам на один и тот же домен можно выявить скрытый DNS канал. Есть ли такие IP адреса в исследуемом датасете?
```{r}
point_9s<-log_data[,c("orig_ip ","query ")]
point_8<-point_9s
point_8%>%group_by(point_8$`query `)
```

```{r}
point_8<-point_8%>%group_by(point_8$orig_ip ) %>%count(point_8$`query `,sort=TRUE)
point_8<-point_8%>%count(point_8$`point_8$orig_ip`,)
point_8<-point_8[which(point_8$n==1),]
knitr::kable(point_8[,c('point_8$orig_ip')], "pipe")
```