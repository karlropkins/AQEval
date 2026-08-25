# AQEval misc

# aqe_quickText should always return balanced <sub>/<sup> markup;
# an unclosed tag superscripts the remainder of the label.

qt <- AQEval:::aqe_quickText

opened <- function(x, tag){
  lengths(regmatches(x, gregexpr(paste0("<", tag, ">"), x)))
}
closed <- function(x, tag){
  lengths(regmatches(x, gregexpr(paste0("</", tag, ">"), x)))
}

labs <- c("speed km/hr and more text", "speed km/h and more text",
          "speed km/hour and more text", "accel km/hr/s here",
          "accel km/hour/s here", "accel km/h/s here",
          "model r2 value", "model R2 value",
          "no2 ug/m3", "pm2.5 level", "wind spd m/s", "o3 dgrC",
          "pmcoarse level", "pmfine level", "PMcoarse level", "PMfine level")

for(lab in labs){
  ans <- qt(lab)
  expect_equal(opened(ans, "sup"), closed(ans, "sup"), info = lab)
  expect_equal(opened(ans, "sub"), closed(ans, "sub"), info = lab)
}

# longest match first; km/hour must not be part-matched by km/h
expect_equal(qt("speed km/hour"), "speed km.hour<sup>-1</sup>")
expect_equal(qt("speed km/hr"), "speed km.hour<sup>-1</sup>")
expect_equal(qt("speed km/h"), "speed km.hour<sup>-1</sup>")
expect_equal(qt("model r2"), "model R<sup>2</sup>")
expect_equal(qt("pmcoarse"), "PM<sub>coarse</sub>")
expect_equal(qt("PMcoarse"), "PM<sub>coarse</sub>")
expect_equal(qt("pmfine"), "PM<sub>fine</sub>")
expect_equal(qt("PMfine"), "PM<sub>fine</sub>")
expect_equal(qt("model R2"), "model R<sup>2</sup>")

# auto.text = FALSE passes text through untouched
expect_equal(qt("speed km/hour", auto.text = FALSE), "speed km/hour")
