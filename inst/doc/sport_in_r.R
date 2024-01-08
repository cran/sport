## ----eval=FALSE, warning=FALSE , message=FALSE--------------------------------
#  install.packages("sport")
#  devtools::install_github("gogonzo/sport")

## ----echo=TRUE, warning=FALSE , message=FALSE---------------------------------
library(sport)
data <- gpheats[1:1002, ]
str(data)

## ----echo=FALSE, warning=FALSE , message=FALSE--------------------------------
data[1:8, c("id", "rider", "rank")]

## ----warning=FALSE , message=FALSE--------------------------------------------
glicko <- glicko_run(formula = rank | id ~ player(rider), data = data)
glicko2 <- glicko2_run(formula = rank | id ~ player(rider), data = data)
bbt <- bbt_run(formula = rank | id ~ player(rider), data = data)
dbl <- dbl_run(formula = rank | id ~ player(rider), data = data)

print(glicko)

## ----warning=FALSE , message=FALSE--------------------------------------------
summary(dbl)

## ----message=FALSE, fig.show='hold', out.width = "50%", warnings=FALSE--------
plot(glicko, n = 15)
plot(glicko, players = c("Greg Hancock", "Tomasz Gollob", "Tony Rickardsson"))

## ----warning=FALSE , message=FALSE--------------------------------------------
names(glicko)

## ----warning=FALSE , message=FALSE--------------------------------------------
tail(glicko$r)
tail(glicko$pairs)

## -----------------------------------------------------------------------------
glicko2 <- glicko2_run(
  formula = rank_player | id ~ player(player),
  data = data.frame(
    id = c(1, 1, 1, 1),
    player = c("a", "b", "c", "d"),
    rank_player = c(3, 4, 1, 2)
  )
)

## -----------------------------------------------------------------------------
glicko2 <- glicko2_run(
  formula = rank_team | id ~ player(player | team),
  data = data.frame(
    id = c(1, 1, 1, 1),
    team = c("A", "A", "B", "B"),
    player = c("a", "b", "c", "d"),
    rank_team = c(1, 1, 2, 2)
  )
)

## -----------------------------------------------------------------------------
dbl <- dbl_run(
formula = rank | id ~ player(name) + gate * factor1,
data = data.frame(
  id = c(1, 1, 1, 1),
  name = c("A", "B", "C", "D"),
  rank = c(3, 4, 1, 2),
  gate = c(1, 2, 3, 4),
  factor1 = c("a", "a", "b", "b"),
  factor2 = c("a", "b", "a", "b")
)
)

## ----warning=FALSE , message=FALSE--------------------------------------------
model <- glicko_run(
  formula = rank | id ~ player(rider),
  data = gpheats[1:16, ]
)

## ----warning=FALSE , message=FALSE--------------------------------------------
glicko_run(
  formula = rank | id ~ player(rider),
  data = gpheats[17:20, ],
  r = model$final_r,
  rd = model$final_rd
)$final_r

## ----warning=FALSE , message=FALSE, echo=FALSE--------------------------------
library(dplyr)
data <- mutate(data,
  weight = ifelse(heat >= (max(heat) - 3), 2, 1)
)

glicko <- glicko_run(
  formula = rank | id ~ player(rider),
  data = data,
  weight = "weight"
)

## ----warning=FALSE , message=FALSE, echo=FALSE--------------------------------
bbt1 <- bbt_run(
  formula = rank | id ~ player(rider),
  data = data,
  kappa = 0.99
) # RD decreases at most 1%

## ----warning=FALSE , message=FALSE--------------------------------------------
# bbt example
data <- data %>%
  group_by(rider) %>%
  mutate(idle_30d = if_else(as.integer(date - lag(date)) > 30, 1.0, 2.0)) %>%
  filter(!is.na(idle_30d))

bbt <- bbt_run(
  formula = rank | id ~ player(rider),
  data = data,
  lambda = "idle_30d"
)

## ----warning=FALSE , message=FALSE--------------------------------------------
glicko2 <- glicko2_run(
  data = data.frame(
    id = c(1, 1, 1, 1),
    team = c("A", "A", "B", "B"),
    player = c("a", "b", "c", "d"),
    rank_team = c(1, 1, 2, 2),
    share = c(0.4, 0.6, 0.5, 0.5)
  ),
  formula = rank_team | id ~ player(player | team),
  share = "share"
)

glicko2$final_r

## ----warning=FALSE , message=FALSE--------------------------------------------
glicko2$pairs
glicko2$r

