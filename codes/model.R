library(tidyverse)
library(bayesplot)
library(patchwork)
library(gridExtra)


X_mat1 = matrix(nrow = 200, ncol = 5)
colnames(X_mat1) = colnames(data)[12:16]
X_mat1 = data[, 12:16]

X_mat2 = matrix(nrow = 200, ncol = 4)
colnames(X_mat2) = colnames(data)[17:20]
X_mat2 = data[, 17:20]

X_mat3 = matrix(nrow = 200, ncol = 2)
colnames(X_mat3) = colnames(data)[21:22]
X_mat3 = data[, 21:22]

Y = data$n
n <- nrow(X_mat1)
p1 <- ncol(X_mat1)
p2 <- ncol(X_mat2)
p3 <- ncol(X_mat3)

N_data = data %>% group_by(route) %>% summarise(N = sum(n), m = n())
log_N0 = log(rep(N_data$N, N_data$m))
data = data %>% mutate(log_N = log_N0)

# poisson model with offset

pois_model =
  rstan::stan_model(file = "poisson_aux_offset.stan")

pois_args =
  list(
    n = n,
    p1 = p1,
    p2 = p2,
    p3 = p3,
    y = Y,
    X1 = X_mat1,
    X2 = X_mat2,
    X3 = X_mat3,
    log_N = log_N0
  )
pois_fit =
  rstan::sampling(
    object = pois_model,
    data = pois_args4,
    iter = 20000,
    init = 0
  )


# negative binomial model with offset
nb_model <-
  rstan::stan_model(file = "negbinom_offset.stan")

nb_args <- list(
  n = n,
  p1 = p1,
  p2 = p2,
  p3 = p3,
  y = Y,
  X1 = X_mat1,
  X2 = X_mat2,
  X3 = X_mat3,
  log_N = log_N0
)

nb_fit <-
  rstan::sampling(
    object = nb_model,
    data = nb_args,
    iter = 20000,
    init = 0
  )

# analysis on poisson model

pois_post_pred_y <-
  rstan::summary(pois_fit, probs = c(0.005, 0.025, 0.25, 0.50, 0.75, 0.975, 0.995))$"summary"[233:432,]


pois_post_pred_y = as.data.frame(pois_post_pred_y) %>% mutate(
  off_percent = (Y - mean) ^ 2 / Y,
  off_count = mean - Y,
  y = Y
)
# calculate RRMSE
RRMSE_pois = pois_post_pred_y %>% summarise(rrmse = sqrt(sum(off_count ^
                                                               2) / (200 * sum(mean ^ 2))))


# posterior predictive graph of lambda
poi_lambda = rstan::summary(pois_fit, probs = c(0.005, 0.025, 0.25, 0.50, 0.75, 0.975, 0.995))$"summary"[433:632,] %>%
  as.data.frame() %>% mutate(
    num = 1:200,
    lambda = data$n / exp(log_N0),
    in95 = ifelse(lambda < `2.5%`, F,
                  ifelse(lambda <= `97.5%`, T, F)),
    in99 = ifelse(lambda < `0.5%`, F,
                  ifelse(lambda <= `99.5%`, T, F))
  )
poi_lambda %>% ggplot(aes(x = num)) +
  geom_point(aes(y = lambda, color = in95), size = 0.2) +
  geom_segment(aes(
    x = num,
    xend = num,
    y = `2.5%`,
    yend = `97.5%`
  ), alpha = 0.6) +
  xlab("index")



# convergence graph of beta1

poi_par = rstan::summary(pois_fit, probs = c(0.005, 0.025, 0.25, 0.50, 0.75, 0.975, 0.995))$"summary"#[1:21, ]



beta1_poi = rstan::extract(
  pois_fit,
  pars = c("beta1"),
  permuted = F,
  inc_warmup = T
)

beta1c_poi = data.frame(
  iter = rep(1:20000, 5),
  beta1 = c(beta1_poi[1:20000], beta1_poi[80001:100000], beta1_poi[160001:180000],
            beta1_poi[240001:260000], beta1_poi[320001:340000]),
  beta = rep(1:5, each = 20000),
  mu = rep(c(
    mean(beta1_poi[1:20000]),
    mean(beta1_poi[80001:100000]),
    mean(beta1_poi[160001:180000]),
    mean(beta1_poi[240001:260000]),
    mean(beta1_poi[320001:340000])
  ), each = 20000)
)

beta_names <- c(
  "1" = "DRY",
  "2" = "ICE",
  "3" = "MUD/DIRT/SAND",
  "4" = "SLUSH/SNOW/WATER",
  "5" = "WET"
)

beta1c_poi %>% ggplot(aes(x = iter, y = beta1)) + geom_line(aes(y = beta1)) +
  geom_line(aes(y = mu), color = "red") + facet_grid(beta ~ .,
                                                     labeller = as_labeller(beta_names))


# graph of distribution of posterior predictive y and observed y
poi = as.data.frame(poi_par[233:432, ])

poi$y = data$n

colors <- c("Predict_Y" = "blue",
            "Y" = "red",
            "O" = "orange")

p1 = poi %>%
  ggplot() + geom_density(aes(x = y, color = "Y")) +
  geom_density(aes(x = floor(mean), color = "Predict_Y")) +
  xlim(c(1, 500)) +
  ggtitle("Poisson Density") + labs(x = "Count",
                                    y = "Density",
                                    color = "Legend") +
  scale_color_manual(values = colors) + theme_bw() + ylim(c(0, 0.0125))




# analysis on negbinom model


nb_post_pred_y <-
  rstan::summary(nb_fit, probs = c(0.005, 0.025, 0.25, 0.50, 0.75, 0.975, 0.995))$"summary"[235:434,]



nb_post_pred_y = as.data.frame(nb_post_pred_y) %>% mutate(
  off_percent = (Y - mean) ^ 2 / Y,
  off_count = mean - Y,
  y = Y
)
# calculate RRMSE
RRMSE_nb = nb_post_pred_y %>% summarise(rrmse = sqrt(sum(off_count ^ 2) /
                                                       (200 * sum(mean ^ 2))))


# posterior predictive graph of lambda
nb_lambda = rstan::summary(nb_fit, probs = c(0.005, 0.025, 0.25, 0.50, 0.75, 0.975, 0.995))$"summary"[435:634,] %>%
  as.data.frame() %>% mutate(
    num = 1:200,
    lambda = data$n / exp(log_N0),
    in95 = ifelse(lambda < `2.5%`, F,
                  ifelse(lambda <= `97.5%`, T, F)),
    in99 = ifelse(lambda < `0.5%`, F,
                  ifelse(lambda <= `99.5%`, T, F))
  )
nb_lambda %>% ggplot(aes(x = num)) +
  geom_point(aes(y = lambda, color = in95), size = 0.2) +
  geom_segment(aes(
    x = num,
    xend = num,
    y = `2.5%`,
    yend = `97.5%`
  ), alpha = 0.6) +
  xlab("index")


# convergence graph of beta1

nb_par = rstan::summary(nb_fit, probs = c(0.005, 0.025, 0.25, 0.50, 0.75, 0.975, 0.995))$"summary"

beta1_nb = rstan::extract(
  nb_fit,
  pars = c("beta1"),
  permuted = F,
  inc_warmup = T
)

beta1c_nb = data.frame(
  iter = rep(1:20000, 5),
  beta1 = c(beta1_nb[1:20000], beta1_nb[80001:100000], beta1_nb[160001:180000],
            beta1_nb[240001:260000], beta1_nb[320001:340000]),
  beta = rep(1:5, each = 20000),
  mu = rep(c(
    mean(beta1_nb[1:20000]),
    mean(beta1_nb[80001:100000]),
    mean(beta1_nb[160001:180000]),
    mean(beta1_nb[240001:260000]),
    mean(beta1_nb[320001:340000])
  ), each = 20000)
)



beta1c_nb %>% ggplot(aes(x = iter, y = beta1)) + geom_line(aes(y = beta1)) +
  geom_line(aes(y = mu), color = "red") + facet_grid(beta ~ .,
                                                     labeller = as_labeller(beta_names))




# graph of distribution of posterior predictive y and observed y

nb = as.data.frame(nb_par[235:434, ])
nb$y = data$n
p2 = nb %>%
  ggplot() + geom_density(aes(x = y, color = "Y")) +
  geom_density(aes(x = floor(mean), color = "Predict_Y")) +
  xlim(c(1, 500)) +
  ggtitle("NegativeBinomial Density") + labs(x = "Count",
                                             y = "Density",
                                             color = "Legend") +
  scale_color_manual(values = colors) + theme_bw() + ylim(c(0, 0.0125))

grid.arrange(p1, p2, ncol=2)
