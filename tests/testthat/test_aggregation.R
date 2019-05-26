context("Testing index aggregations")
library(testthat)
library(reportcards)

test_that("Score to Grade conversions", {
    expect_equal(RC_generateGrades(x=seq(0,1,length=6), type='Uniform'), c('E','D','C','B','A','A'))
    expect_equal(RC_generateGrades(x=seq(0,1,length=6), type='MMP'), c('E','E','D','C','B','A'))
    expect_equal(RC_generateGrades(x=seq(0,1,length=6), type='GHHP'), c('E','E','D','C','B','A'))
    expect_equal(RC_generateGrades(x=seq(0,1,length=6), type='MCWC'), c('E','E','D','C','B','A'))
})
