context('CheckIfEMSisInsideOtherEMS')

test_that('Test 1',
          {
            ems1 <- EMS(origin = c(1.5, 0.5, 0), length = 0.5, height = 1.5, width = 2)
            ems2 <- EMS(origin = c(1.5, 0, 0), length = 0.5, height = 2, width = 2)

            result <- CheckIfEMSisInsideOtherEMS(ems1, ems2)
            expect_equal(result,
                         TRUE
                         )

          }
         )

test_that('Test 2',
          {
            ems1 <- EMS(origin = c(0.5, 0.5, 0.5), length = 0.5, height = 1.5, width = 2)
            ems2 <- EMS(origin = c(0, 0, 0), length = 1, height = 1, width = 1)

            result <- CheckIfEMSisInsideOtherEMS(ems1, ems2)
            expect_equal(result,
                         FALSE
                         )

          }
         )