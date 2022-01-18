app <- ShinyDriver$new("../../")
app$snapshotInit("preprocessUploadMod-DemoData")

app$setInputs(demo = "click")
app$snapshot()
