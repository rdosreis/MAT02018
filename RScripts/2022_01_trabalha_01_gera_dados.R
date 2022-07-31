library(ggplot2)
library(UsingR)

# Diamonds
data("diamonds")
writexl::write_xlsx(x = diamonds, path = here::here("data", "diamonds.xlsx"))

# mpg
data("mpg")
writexl::write_xlsx(x = mpg, path = here::here("data", "mpg.xlsx"))

# msleep
data("msleep")
writexl::write_xlsx(x = msleep, path = here::here("data", "msleep.xlsx"))

# babies
data("babies")
writexl::write_xlsx(x = babies, path = here::here("data", "babies.xlsx"))

# House Sales in Tyne and Wear
housing <- readxl::read_excel(path = here::here("data", "Rogerson Chapter 2 Key.xlsx"),
                              sheet = "Housing Data")
writexl::write_xlsx(x = housing, path = here::here("data", "housing.xlsx"))
