library(tidyverse)
library(readxl)

load("data.rds")

data |>
  transmute(date = as.Date(paste0(year, "-01-01")),
            `Interest burden (% of GDP)` = charge_interets/PIB,
            `Real interest burden (% of GDP)` = charge_interets/PIB-taxe_inflationniste/PIB) |>
  gather(variable, value, -date) |>
  ggplot() + geom_line(aes(x = date, y = value, color = variable)) +
  theme_minimal() + xlab("") + ylab("") +
  theme(legend.title = element_blank(),
        legend.position = c(0.4, 0.2)) +
  scale_x_date(breaks = as.Date(paste0(seq(1978, 2100, 5), "-01-01")),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-100, 100, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(data = . %>% 
               filter(date == max(date)),
             aes(x = date, y = value,  color = variable, label = scales::percent(value, acc = 0.1))) +
  labs(caption = "Source: Insee, author's calculations")

ggsave("figure2.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure2.pdf", width = 1.25*6, height = 1.25*3.375)


