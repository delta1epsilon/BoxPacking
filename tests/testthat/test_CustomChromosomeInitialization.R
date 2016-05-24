context('CustomChromosomeInitialization')

b1 <- Box(length = 0.5, height = 0.5, width = 0.5)
b2 <- Box(length = 1, height = 0.5, width = 0.5)
b3 <- Box(length = 2, height = 0.5, width = 0.5)
b4 <- Box(length = 1, height = 1, width = 1)

boxes <- list(b1, b2, b3, b4)

test_that('Test by length',
          {
            result <- CustomChromosomeInitialization(boxes, 2)
            expected <- c(3, 2, 4, 1)
            expect_equal(result[[1]]$BPS, expected)
          }
         )

test_that('Test by height',
          {
            result <- CustomChromosomeInitialization(boxes, 2)
            expected <- c(4, 1, 2, 3)
            expect_equal(result[[2]]$BPS, expected)
          }
         )

test_that('Test by width',
          {
            result <- CustomChromosomeInitialization(boxes, 2)
            expected <- c(4, 1, 2, 3)
            expect_equal(result[[3]]$BPS, expected)
          }
         )

test_that('Test by volume',
          {
            result <- CustomChromosomeInitialization(boxes, 2)
            expected <- c(4, 3, 2, 1)
            expect_equal(result[[4]]$BPS, expected)
          }
         )