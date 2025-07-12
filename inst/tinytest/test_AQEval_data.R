# AQEval find

if(at_home()){
  temp <- openair::timeAverage(AQEval::aq.data, "2 day")
  test <- findBreakPoints(temp, "no2", h=0.3)
  expect_equivalent(unlist(test), c(213, 317, 402))
  test <- testBreakPoints(temp, "no2", test)$signif
  expect_equivalent(unlist(test), c(TRUE, FALSE))
}
