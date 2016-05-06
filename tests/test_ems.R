context('EMS')

source('../R/EMS.R')
source('../R/objects_definition.R')

test_that('Test EMS 1',
          {
            container <- Container(length = 2, height = 2, width = 2)
            box <- Box(origin = c(0, 0, 0), length = 0.5, height = 0.5, width = 0.5)
            
            expected <- list(NULL,
                             NULL,
                             EMS(origin = c(0.5, 0, 0), length = 1.5, height = 2, width = 2),
                             EMS(origin = c(0, 0, 0.5), length = 2, height = 2, width = 1.5),
                             EMS(origin = c(0, 0.5, 0), length = 2, height = 1.5, width = 2)
                             )
            
            result <- CreateEMS(container, box)
            expect_equal(result,
                         expected
                         )
          }
          )

test_that('Test EMS 2',
          {
            container <- Container(length = 1, height = 1, width = 1)
            box <- Box(origin = c(0, 2, 0), length = 0.5, height = 0.5, width = 0.5)
            
            expected <- list(NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL
                             )
            
            result <- CreateEMS(container, box)
            expect_equal(result,
                         expected
                         )
          }
          )

test_that('Test EMS 3',
          {
            container <- Container(length = 3, height = 3, width = 3)
            box <- Box(origin = c(1, 0, 1), length = 1, height = 1, width = 1)
            
            expected <- list(EMS(origin = c(0, 0, 0), length = 1, height = 3, width = 3),
                             EMS(origin = c(0, 0, 0), length = 3, height = 3, width = 1),
                             EMS(origin = c(2, 0, 0), length = 1, height = 3, width = 3),
                             EMS(origin = c(0, 0, 2), length = 3, height = 3, width = 1),
                             EMS(origin = c(0, 1, 0), length = 3, height = 2, width = 3)
                             )

            result <- CreateEMS(container, box)
            expect_equal(result,
                         expected
                         )
          }
          )

test_that('Test EMS 4',
          {
            container <- Container(length = 2, height = 2, width = 2)
            box <- Box(origin = c(0, 0, 0), length = 2, height = 1, width = 1)
            
            expected <- list(NULL,
                             NULL,
                             NULL,
                             EMS(origin = c(0, 0, 1), length = 2, height = 2, width = 1),
                             EMS(origin = c(0, 1, 0), length = 2, height = 1, width = 2)
                             )

            result <- CreateEMS(container, box)
            expect_equal(result,
                         expected
                         )
          }
          )
