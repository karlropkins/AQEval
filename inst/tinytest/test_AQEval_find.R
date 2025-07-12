# AQEval data

if(at_home()){
  test <- dim(AQEval::aq.data)
  expect_equivalent(test, c(26280, 6))
  test <- names(AQEval::aq.data)
  expect_equivalent(test, c("date", "no2", "bg.no2", "ws", "wd", "air_temp"))
  test <- AQEval::aq.data$no2[1:10]
  expect_equal(test, c(5.768200, 6.500367, 10.950667, 7.377375, 5.772975,
                       5.772975, 9.812625, 12.510500, 10.653025, 15.657225))
}
