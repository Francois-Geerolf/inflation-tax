library(tidyverse)
load("data.rds")

donnees <- data %>%
  filter(year >= 2022) %>%
  select(-year) %>%
  t() 

colnames(donnees) <- c("2022", "2023")

donnees

table1 <- tibble(`% or €bn` = c("% of GDP", 
                                "€bn"),
                 `Debt 2022` = c(donnees["dette_PIB", "2022"]/100,
                                 donnees["PIB", "2022"]*donnees["dette_PIB", "2022"]/100),
                 `Debt 2023` = c(donnees["dette_PIB", "2023"]/100,
                                 donnees["PIB", "2023"]*donnees["dette_PIB", "2023"]/100),
                 `Deficit 2023` = c(-donnees["deficit_PIB", "2023"]/100,
                                    -donnees["PIB", "2023"]*donnees["deficit_PIB", "2023"]/100),
                 `Inflation tax` = c(-donnees["taxe_inflationniste_PIB", "2023"]/100,
                                     -donnees["taxe_inflationniste", "2023"]),
                 `Growth effect` = c(-donnees["croissance_reelle", "2023"]*donnees["dette_PIB", "2022"]/10000,
                                     -donnees["croissance_reelle", "2023"]*donnees["dette_PIB", "2022"]/10000*donnees["PIB", "2023"]),
                 `Interest burden` = c(donnees["charge_interets", "2023"]/donnees["PIB", "2023"],
                                       donnees["charge_interets", "2023"]),
                 `Real interest burden` = c(donnees["charge_interets", "2023"]/donnees["PIB", "2023"]-donnees["taxe_inflationniste_PIB", "2023"]/100,
                                            donnees["charge_interets", "2023"] - donnees["taxe_inflationniste", "2023"])) |>
  gt::gt() |>
  gt::fmt_percent(
    rows = 1,
    decimals = 1
  ) |>
  gt::fmt_number(
    rows = 2,
    decimals = 0
  ) |>
  gt::cols_align(
    align = "center",
    columns = everything()
  ) |>
  gt::cols_label(
    `% or €bn` = gt::html(""),
    `Debt 2022` = gt::html("Debt<br>2022"),
    `Debt 2023` = gt::html("Debt<br>2023"),
    `Deficit 2023` = gt::html("Deficit<br>2023"),
    `Inflation tax` = gt::html("Inflation<br>tax"),
    `Growth effect` = gt::html("Growth<br>effect")
  ) |>
  gt::tab_footnote("Source: Insee, author’s calculations")


table1

table1  |>
  gt::gtsave(filename = "table1.png")

table1  |>
  gt::gtsave(filename = "table1.pdf")

system("pdfcrop table1.pdf table1.pdf")
