# Test for cachematrix.R
# uses testthat framework
# running tests:
#   load R
#   > require("test_that")
#   > test_file("test_cachematrix.R")

source("cachematrix.R");

context("makeCacheMatrixTest");

test_matrix <- matrix(c(1, 2, 3, 4), 2, 2)
test_matrix_inverse <- solve(test_matrix)

test_that("Can be initialized with matrix", {
    cm <- makeCacheMatrix(test_matrix)
    expect_that(cm$get(), equals(test_matrix));
});

test_that("Inverse of the cached matrix without matrix is null", {
    cm <- makeCacheMatrix()
    expect_that(cm$getInverse(), equals(NULL));
});

test_that("Setting matrix should empty inverse", {
    cm <- makeCacheMatrix()
    cm$set(test_matrix)
    cm$setInverse(test_matrix_inverse)
    cm$set(test_matrix)
    expect_that(cm$getInverse(), equals(NULL));
});

test_that("Getting inverse should return cached inverse", {
    cm <- makeCacheMatrix()
    cm$set(test_matrix)
    cm$setInverse(test_matrix_inverse)
    expect_that(cm$getInverse(), equals(test_matrix_inverse));
});


context("cacheSolveTest");

test_that("Returns inverse from cached matrix", {
    cm <- makeCacheMatrix(mtx=test_matrix)
    cm$setInverse(test_matrix_inverse)
    expect_that(cacheSolve(cm), equals(test_matrix_inverse))
});

test_that("Computes and caches inverse", {
    cm <- makeCacheMatrix(mtx=test_matrix)
    expect_that(cm$getInverse(), equals(NULL))
    expect_that(cacheSolve(cm), equals(test_matrix_inverse))
});
