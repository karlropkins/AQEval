# AQEval quant (main functions)

if(at_home()){
  temp <- openair::timeAverage(AQEval::aq.data, "3 day")
  suppressMessages(ans <- quantBreakPoints(temp, "no2", h=0.15, show="nothing"))
  expect_equal(names(ans), c("data", "breaks", "data2", "plot", "report", "model"))
  expect_equal(ans$breaks$bpt[1], 211)
  expect_equal(class(ans$plot), c("gg", "ggplot"))

  suppressMessages(ans <- quantBreakSegments(temp, "no2", h=0.15, show="nothing"))
  expect_equal(names(ans), c("data", "segments", "data2", "plot", "report", "model"))
  expect_equal(ans$segments$seg.str[2], 209)
  expect_equal(class(ans$plot), c("gg", "ggplot"))

}
