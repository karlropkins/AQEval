# AQEval isolate

if(at_home()){
  test <- suppressMessages(isolateContribution(AQEval::aq.data[1:3600,], "no2"))
  expect_equal(test[1:4], c(41.42068, 39.68593, 41.58314, 43.53074), tolerance=0.0001)
}
