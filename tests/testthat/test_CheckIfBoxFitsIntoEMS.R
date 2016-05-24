context('CheckIfBoxFitsIntoEMS')

test_that('Test 1',
          {
            box <- Box(origin = c(0, 0, 0), length = 2, height = 1, width = 1)
            ems <- EMS(origin = c(0, 0, 0), length = 1.5, height = 3, width = 2)

            result <- CheckIfBoxFitsIntoEMS(box, ems)
            expect_equal(result,
                         TRUE
                         )

          }
         )

test_that('Test 2',
          {
            box <- Box(origin = c(0, 0, 0), length = 2, height = 1, width = 1)
            ems <- EMS(origin = c(0, 0, 0), length = 1, height = 2, width = 1)

            result <- CheckIfBoxFitsIntoEMS(box, ems)
            expect_equal(result,
                         TRUE
                         )

          }
         )

test_that('Test 3',
          {
            box <- Box(origin = c(0, 0, 0), length = 2, height = 1, width = 1)
            ems <- EMS(origin = c(0, 0, 0), length = 1.5, height = 1.5, width = 1)

            result <- CheckIfBoxFitsIntoEMS(box, ems)
            expect_equal(result,
                         FALSE
                         )

          }
         )
