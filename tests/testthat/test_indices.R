context("Testing index formulations")
library(testthat)
library(reportcards)

test_that("Binary index formulation", {
  expect_equal(RC_index(x=c(30,40,50,60,70), GL=50, DOF='H', type='Binary'), c(1,1,1,0,0))
  expect_equal(RC_index(x=c(30,40,50,60,70), GL=50, DOF='L', type='Binary'), c(0,0,1,1,1))
  expect_equal(RC_index(x=c(30,40,50,60,70), Lower=40, Upper=60, DOF='B', type='Binary'), c(0,1,1,1,0))
})

test_that("WCS index formulation", {
  expect_equal(RC_index(x=c(30,40,50,60,70), GL=50, uBound=65, DOF='H', type='WCS'), c(100,100,100,100/3,0))
  expect_equal(RC_index(x=c(30,40,50,60,70), GL=50, lBound=35, DOF='L', type='WCS'), c(0,100/3,100,100,100))
  expect_equal(RC_index(x=c(30,40,50,60,70), Lower=45, Upper=55, lBound=35, uBound=65, DOF='B', type='WCS'), c(0,50,100,50,0))
})

test_that("MAMP index formulation", {
    expect_equal(round(RC_index(x=c(25,40,50,60,100), GL=50, fold=2, DOF='H', type='MAMP'),7), c(1.0000000,0.3219281,0.0000000,-0.2630344,-1.0000000))
    expect_equal(round(RC_index(x=c(25,40,50,60,100), GL=50, fold=2, DOF='L', type='MAMP'),7), c(-1.0000000,-0.3219281,0.0000000,0.2630344,1.0000000))
    expect_equal(round(RC_index(x=c(20,40,50,60,100), Lower=40, Upper=50, fold=2, DOF='B', type='MAMP'),7), c(0.0000000,1.0000000,1.0000000,0.7369656,0.0000000))
})

test_that("HSAMP index formulation", {
    expect_equal(round(RC_index(x=c(25,40,50,60,100), GL=50, lBound=30, uBound=70, T=2, DOF='H', type='HSAMP'),7), c(1.0000000, 0.6620136, 0.5000000, 0.3379864, 0.0000000))
    expect_equal(round(RC_index(x=c(25,40,50,60,100), GL=50, lBound=30, uBound=70, T=2, DOF='L', type='HSAMP'),7), c(0.0000000, 0.3379864, 0.5000000, 0.6620136, 1.0000000))
    expect_equal(round(RC_index(x=c(25,40,50,60,100), Lower=45, Upper=65, lBound=30, uBound=70, T=2, DOF='B', type='HSAMP'),7), c(0.0000000, 0.8022647, 1.0000000, 1.0000000, 0.0000000))
})

test_that("LAMP index formulation", {
    expect_equal(round(RC_index(x=c(25,40,50,60,100), GL=50, T=2, DOF='H', type='LAMP'),7), c(0.7310586, 0.5986877, 0.5000000, 0.4174298, 0.2689414))
    expect_equal(round(RC_index(x=c(25,40,50,60,100), GL=50, T=2, DOF='L', type='LAMP'),7), c(0.2689414, 0.4013123, 0.5000000, 0.5825702, 0.7310586))
    expect_equal(round(RC_index(x=c(25,40,50,60,100), Lower=45, Upper=55, T=2, DOF='B', type='LAMP'),7), c(0.4111123, 0.8007374, 1.0000000, 0.8337529, 0.1946867))
})

