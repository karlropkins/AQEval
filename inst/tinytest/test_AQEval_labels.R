# AQEval labels

# ggtext is optional; when it is missing aqe_label() renders the same
# markup using unicode sub/superscripts.

md2u <- AQEval:::aqe_mdToUnicode

# sub/superscripts
expect_equal(md2u("NO<sub>2</sub>"), "NO\u2082")
expect_equal(md2u("PM<sub>10</sub>"), "PM\u2081\u2080")
expect_equal(md2u("NO<sub>x</sub>"), "NO\u2093")
expect_equal(md2u("R<sup>2</sup>"), "R\u00b2")
expect_equal(md2u("m.s<sup>-1</sup>"), "m.s\u207b\u00b9")
expect_equal(md2u("<sup>o</sup>C"), "\u00b0C")

# entities and line breaks
expect_equal(md2u("&mu;g.m<sup>-3</sup>"), "\u00b5g.m\u207b\u00b3")
expect_equal(md2u("a<br>b"), "a\nb")

# content with no unicode form drops to inline text rather than vanishing
expect_equal(md2u("PM<sub>coarse</sub>"), "PMcoarse")

# unhandled markup is stripped, not left in the label
expect_equal(md2u("<i>x</i>"), "x")

# plain text and empty strings are untouched
expect_equal(md2u("date"), "date")
expect_equal(md2u(""), "")

# vectorised
expect_equal(md2u(c("NO<sub>2</sub>", "O<sub>3</sub>")), c("NO\u2082", "O\u2083"))

# helpers behave
expect_true(is.logical(AQEval:::aqe_useMarkdown()))
expect_true(inherits(AQEval:::aqe_titleElement(), "element"))

# aqe_label agrees with aqe_quickText when ggtext is present
if(AQEval:::aqe_useMarkdown()){
  expect_equal(AQEval:::aqe_label("no2"), AQEval:::aqe_quickText("no2"))
}

# auto.text = FALSE passes text straight through either way
expect_equal(AQEval:::aqe_label("no2 ug/m3", auto.text = FALSE), "no2 ug/m3")
