app <- ShinyDriver$new("../../", loadTimeout = 1e+04)
app$snapshotInit("preprocessUploadMod-DemoData")

app$setInputs(demo = "click")
app$snapshot()
