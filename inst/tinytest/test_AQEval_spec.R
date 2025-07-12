# AQEval spec

if(at_home()){
  test <- spectralFrequency(AQEval::aq.data, "no2")
  expect_equal(names(test), c("plot", "data", "call"))
  expect_equal(dim(test$data), c(13500, 4))
}
