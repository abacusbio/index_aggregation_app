app <- ShinyDriver$new("../../", loadTimeout = 1e+04)
app$snapshotInit("FilterEBV")

app$setInputs(demo = "click")
app$setInputs(upload = "tab.step2")
app$snapshot()
