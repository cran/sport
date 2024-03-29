context("utils")

testthat::test_that("check single arguments", {
  testthat::expect_error(
    check_single_argument(c(1, 2), "test"),
    "test should be a single value"
  )
  testthat::expect_error(
    check_single_argument(c("A", "b"), "test"),
    "test should be a single value"
  )
  testthat::expect_error(
    check_single_argument(1, "test", min = 0, max = .99),
    "test should be lower than 0.99"
  )

  testthat::expect_error(
    check_single_argument(1, "test", min = 1.1),
    "test should be greater than 1.1"
  )

  testthat::expect_silent(check_single_argument("a", "test"))
  testthat::expect_silent(check_single_argument(1, "test"))
  testthat::expect_silent(check_single_argument(1, "test", min = 0.99))
  testthat::expect_silent(check_single_argument(1, "test", max = 1.1))
})

testthat::test_that("check vector arguments", {
  testthat::expect_error(
    check_numeric_argument("a", "test"),
    "test should be of type numeric"
  )

  testthat::expect_error(
    check_numeric_argument(c(1, NA_real_), "test"),
    "test contains non-finite values"
  )

  testthat::expect_error(
    check_numeric_argument(c(-1, 1), "test", min = 0),
    "test should be in range \\[0, Inf\\]"
  )

  testthat::expect_error(
    check_numeric_argument(c(1, 2, 3), "test", max = 2),
    "values in variable test should be in range \\[-Inf, 2\\]"
  )

  testthat::expect_error(
    check_numeric_argument(c(1, 2, 3), "test", min = 2),
    "values in variable test should be in range \\[2, Inf\\]"
  )

  testthat::expect_silent(check_numeric_argument(c(1, 2, 3), "test", min = 1, max = 3))



  testthat::expect_error(
    check_integer_argument("a", "test"),
    "test should be of type integer"
  )

  testthat::expect_error(
    check_integer_argument(c(1L, NA_integer_), "test"),
    "test contains non-finite values"
  )

  testthat::expect_error(
    check_integer_argument(c(-1L, 1L), "test", min = 0),
    "test should be in range \\[0, Inf\\]"
  )

  testthat::expect_error(
    check_integer_argument(c(1L, 2L, 3L), "test", max = 2),
    "values in variable test should be in range \\[-Inf, 2\\]"
  )

  testthat::expect_error(
    check_integer_argument(c(1L, 2L, 3L), "test", min = 2),
    "values in variable test should be in range \\[2, Inf\\]"
  )

  testthat::expect_silent(check_integer_argument(c(1L, 2L, 3L), "test", min = 1, max = 3))


  testthat::expect_error(
    check_string_argument(1, "test"),
    "test should be of type character"
  )

  testthat::expect_error(
    check_string_argument(c("A", NA_character_), "test"),
    "test contains non-finite values"
  )
})

testthat::test_that("check data", {
  testthat::expect_error(
    is_data_provided(),
    "Data is not provided"
  )

  testthat::expect_error(
    are_variables_in_dataset(vars = c("rider2", "team"), data = gpheats),
    "rider2, team specified in formula not present in data"
  )
})

testthat::test_that("check initial r", {
  testthat::expect_identical(
    setNames(1500, "A"),
    init_check_r(
      r = setNames(1500, "A"),
      player = "player_Var",
      init_r = 1500,
      unique_names = "A"
    )
  )

  testthat::expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_r(
      r = setNames(c(1500, 1500), c("A", "B")),
      player = "player_Var",
      init_r = 1500,
      unique_names = c("A", "B")
    )
  )

  testthat::expect_identical(
    setNames(c(1500, -1), c("A", "B")),
    init_check_r(
      r = setNames(c(1500, -1), c("A", "B")),
      player = "player_var",
      init_r = 1500,
      unique_names = c("A", "B")
    )
  )



  testthat::expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_r(
      r = setNames(c(1500, 1500), c("A", "B")),
      player = "player_var",
      init_r = -1500,
      unique_names = c("A", "B")
    )
  )

  testthat::expect_identical(
    setNames(c(99, 99), c("A", "B")),
    init_check_r(
      r = NULL,
      player = "player_var",
      init_r = 99,
      unique_names = c("A", "B")
    )
  )

  testthat::expect_warning(
    new_r <- init_check_r(
      r = c(A = 1500),
      player = "player_var",
      init_r = 1501,
      unique_names = c("A", "B")
    ),
    "Missing parameters will be added"
  )
  testthat::expect_identical(new_r, c(A = 1500, B = 1501))

  testthat::expect_error(
    init_check_r(
      r = setNames(c(1500, 1500, 1500), c("A", "B", "A")),
      player = "player_var",
      init_r = 1500,
      unique_names = c("A", "B")
    ),
    "All names in r should be unique. Duplicated names not allowed"
  )

  testthat::expect_error(
    init_check_r(
      r = setNames(c(1500, NA_real_), c("A", "B")),
      player = "player_var",
      init_r = 1500,
      unique_names = c("A", "B")
    ),
    "All values in r should be a finite number. NA's not allowed"
  )

  testthat::expect_error(
    init_check_r(
      r = NULL,
      player = "player_var",
      init_r = NA_real_,
      unique_names = c("A", "B")
    ),
    "init_r should be a finite number"
  )

  r <- setNames(c(1500, 1500), c("A", "B"))
  new_r <- init_check_r(
    r = r,
    player = "player_var",
    init_r = 1500,
    unique_names = c("A", "B")
  )

  testthat::expect_identical(
    lobstr::obj_addr(r),
    lobstr::obj_addr(new_r)
  )
})

testthat::test_that("check initial rd", {
  testthat::expect_identical(
    setNames(1500, "A"),
    init_check_rd(
      rd = setNames(1500, "A"),
      player = "player_Var",
      init_rd = 1500,
      unique_names = "A"
    )
  )

  testthat::expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_rd(
      rd = setNames(c(1500, 1500), c("A", "B")),
      player = "player_Var",
      init_rd = 1500,
      unique_names = c("A", "B")
    )
  )

  testthat::expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_rd(
      rd = setNames(c(1500, 1500), c("A", "B")),
      player = "player_var",
      init_rd = -1500,
      unique_names = c("A", "B")
    )
  )

  testthat::expect_identical(
    setNames(c(9999, 9999), c("A", "B")),
    init_check_rd(
      rd = NULL,
      player = "player_var",
      init_rd = 9999,
      unique_names = c("A", "B")
    )
  )

  testthat::expect_warning(
    new_rd <- init_check_rd(
      rd = c(A = 1500),
      player = "player_var",
      init_rd = 1501,
      unique_names = c("A", "B")
    ),
    "Missing parameters will be added"
  )
  testthat::expect_identical(new_rd, c(A = 1500, B = 1501))

  testthat::expect_error(
    init_check_rd(
      rd = setNames(c(1500, 1500, 1500), c("A", "B", "A")),
      player = "player_var",
      init_rd = 1500,
      unique_names = c("A", "B")
    ),
    "All names in rd should be unique. Duplicated names not allowed"
  )

  testthat::expect_error(
    init_check_rd(
      rd = setNames(c(1500, NA_real_), c("A", "B")),
      player = "player_var",
      init_rd = 1500,
      unique_names = c("A", "B")
    ),
    "All values in rd should be a finite number. NA's not allowed"
  )

  testthat::expect_error(
    init_check_rd(
      rd = NULL,
      player = "player_var",
      init_rd = NA_real_,
      unique_names = c("A", "B")
    ),
    "init_rd value should be positive"
  )

  testthat::expect_error(
    init_check_rd(
      rd = NULL,
      player = "player_var",
      init_rd = 0,
      unique_names = c("A", "B")
    ),
    "init_rd value should be positive"
  )


  testthat::expect_error(
    init_check_rd(
      rd = setNames(c(1500, -1), c("A", "B")),
      player = "player_var",
      init_rd = 1500,
      unique_names = c("A", "B")
    ),
    "All values in rd should be positive"
  )


  rd <- setNames(c(1500, 1500), c("A", "B"))
  new_rd <- init_check_rd(
    rd = rd,
    player = "player_var",
    init_rd = 1500,
    unique_names = c("A", "B")
  )

  testthat::expect_identical(
    lobstr::obj_addr(rd),
    lobstr::obj_addr(new_rd)
  )
})

testthat::test_that("check initial sigma", {
  testthat::expect_identical(
    numeric(0),
    init_check_sigma(
      sigma = setNames(1500, "A"),
      player = "player_Var",
      init_sigma = 1500,
      unique_names = "A",
      method = "glicko"
    )
  )

  testthat::expect_identical(
    setNames(1500, "A"),
    init_check_sigma(
      sigma = setNames(1500, "A"),
      player = "player_Var",
      init_sigma = 1500,
      unique_names = "A",
      method = "glicko2"
    )
  )

  testthat::expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_sigma(
      sigma = setNames(c(1500, 1500), c("A", "B")),
      player = "player_Var",
      init_sigma = 1500,
      unique_names = c("A", "B"),
      method = "glicko2"
    )
  )

  testthat::expect_identical(
    setNames(c(1500, 1500), c("A", "B")),
    init_check_sigma(
      sigma = setNames(c(1500, 1500), c("A", "B")),
      player = "player_var",
      init_sigma = -1500,
      unique_names = c("A", "B"),
      method = "glicko2"
    )
  )

  testthat::expect_identical(
    setNames(c(9999, 9999), c("A", "B")),
    init_check_sigma(
      sigma = NULL,
      player = "player_var",
      init_sigma = 9999,
      unique_names = c("A", "B"),
      method = "glicko2"
    )
  )
  testthat::expect_warning(
    new_sigma <- init_check_sigma(
      method = "glicko2",
      sigma = c(A = 1500),
      player = "player_var",
      init_sigma = 1501,
      unique_names = c("A", "B")
    ),
    "Missing parameters will be added"
  )
  testthat::expect_identical(new_sigma, c(A = 1500, B = 1501))

  testthat::expect_error(
    init_check_sigma(
      sigma = setNames(c(1500, 1500, 1500), c("A", "B", "A")),
      player = "player_var",
      init_sigma = 1500,
      unique_names = c("A", "B"),
      method = "glicko2"
    ),
    "All names in sigma should be unique. Duplicated names not allowed"
  )

  testthat::expect_error(
    init_check_sigma(
      sigma = setNames(c(1500, NA_real_), c("A", "B")),
      player = "player_var",
      init_sigma = 1500,
      unique_names = c("A", "B"),
      method = "glicko2"
    ),
    "All values in sigma should be a finite number. NA's not allowed"
  )

  testthat::expect_error(
    init_check_sigma(
      sigma = NULL,
      player = "player_var",
      init_sigma = NA_real_,
      unique_names = c("A", "B"),
      method = "glicko2"
    ),
    "init_sigma value should be positive"
  )

  testthat::expect_error(
    init_check_sigma(
      sigma = NULL,
      player = "player_var",
      init_sigma = 0,
      unique_names = c("A", "B"),
      method = "glicko2"
    ),
    "init_sigma value should be positive"
  )



  testthat::expect_error(
    init_check_sigma(
      sigma = setNames(c(1500, -1), c("A", "B")),
      player = "player_var",
      init_sigma = 1500,
      unique_names = c("A", "B"),
      method = "glicko2"
    ),
    "All values in sigma should be positive"
  )


  sigma <- setNames(c(1500, 1500), c("A", "B"))
  new_sigma <- init_check_sigma(
    sigma = sigma,
    player = "player_var",
    init_sigma = 1500,
    unique_names = c("A", "B"),
    method = "glicko2"
  )

  testthat::expect_identical(
    lobstr::obj_addr(sigma),
    lobstr::obj_addr(new_sigma)
  )
})

testthat::test_that("initialize vector", {
  gpheats$weight <- 1

  testthat::expect_identical(
    lobstr::obj_addr(initialize_vec("weight", gpheats, "weight", min = 0)),
    lobstr::obj_addr(gpheats$weight)
  )

  testthat::expect_identical(
    initialize_vec(2, gpheats, "weight", min = 0),
    rep(2, times = nrow(gpheats))
  )

  testthat::expect_error(
    initialize_vec("weight", gpheats, "weight", min = 0, max = .99),
    "range"
  )

  testthat::expect_error(
    initialize_vec(2, gpheats, "weight", min = 0, max = .99),
    "range"
  )

  testthat::expect_error(
    initialize_vec("wrong", gpheats, "weight", min = 0, max = .99),
    "is not present in data"
  )
})
