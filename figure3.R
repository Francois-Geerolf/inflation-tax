library(tidyverse)

load("data.rds")

data |>
  transmute(date = as.Date(paste0(year, "-01-01")),
            `Public balance (% of GDP)` = deficit_PIB/100,
            `Public balance adjusted for the "inflation tax" (% of GDP)` = deficit_PIB/100+taxe_inflationniste/PIB) |>
  gather(variable, value, -date) |>
  ggplot() + geom_line(aes(x = date, y = value, color = variable)) +
  theme_minimal() + xlab("") + ylab("") +
  theme(legend.title = element_blank(),
        legend.position = c(0.3, 0.15)) +
  scale_x_date(breaks = as.Date(paste0(seq(1978, 2100, 5), "-01-01")),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-100, 100, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(data = . %>% 
               filter(date == max(date)),
             aes(x = date, y = value,  color = variable, label = scales::percent(value, acc = 0.1))) +
  labs(caption = "Source: Insee, author's calculations")

ggsave("figure3.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure3.pdf", width = 1.25*6, height = 1.25*3.375)


data |>
  transmute(date = as.Date(paste0(year, "-01-01")),
            `Public deficit (% of GDP)` = -deficit_PIB/100,
            `Public deficit adjusted for the "inflation tax" (% of GDP)` = -deficit_PIB/100-taxe_inflationniste/PIB) |>
  gather(variable, value, -date) |>
  ggplot() + geom_line(aes(x = date, y = value, color = variable)) +
  theme_minimal() + xlab("") + ylab("") +
  theme(legend.title = element_blank(),
        legend.position = c(0.3, 0.85)) +
  scale_x_date(breaks = as.Date(paste0(seq(1978, 2100, 5), "-01-01")),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-100, 100, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(data = . %>% 
               filter(date == max(date)),
             aes(x = date, y = value,  color = variable, label = scales::percent(value, acc = 0.1))) +
  labs(caption = "Source: Insee, author's calculations")

ggsave("figure3b.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure3b.pdf", width = 1.25*6, height = 1.25*3.375)

