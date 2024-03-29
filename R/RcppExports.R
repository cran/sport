# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

glicko <- function(
    unique_id, id, rank, team, player, r, rd, sigma, share, lambda, weight, init_r, init_rd, init_sigma, kappa, tau) {
    .Call(
        "_sport_glicko",
        PACKAGE = "sport",
        unique_id, id, rank, team, player, r, rd, sigma, share, lambda, weight,
        init_r, init_rd, init_sigma, kappa, tau
    )
}

glicko2 <- function(
    unique_id, id, rank, team, player, r, rd, sigma, share, lambda, weight, init_r, init_rd, init_sigma, kappa, tau) {
    .Call(
        "_sport_glicko2",
        PACKAGE = "sport",
        unique_id, id, rank, team, player, r, rd, sigma, share, lambda, weight,
        init_r, init_rd, init_sigma, kappa, tau
    )
}

bbt <- function(
    unique_id, id, rank, team, player, r, rd, sigma, share, lambda, weight, init_r, init_rd, init_sigma, kappa, tau) {
    .Call(
        "_sport_bbt",
        PACKAGE = "sport",
        unique_id, id, rank, team, player, r, rd, sigma, share, lambda,
        weight, init_r, init_rd, init_sigma, kappa, tau
    )
}

dbl <- function(
    unique_id, id_vec, rank_vec, team_vec, MAP, X, cls, R, RD, lambda_vec, weight_vec, kappa) {
    .Call(
        "_sport_dbl",
        PACKAGE = "sport",
        unique_id, id_vec, rank_vec, team_vec, MAP, X, cls, R, RD, lambda_vec, weight_vec, kappa
    )
}
