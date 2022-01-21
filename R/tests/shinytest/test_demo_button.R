app <- ShinyDriver$new("../../", loadTimeout = 1e+04)
app$snapshotInit("test_demo_button")

app$snapshot(list(output = "step1-sanity_message"))
app$setInputs(demo = "click", timeout_ = 1e+04)
app$snapshot(list(output = "step1-demo_message"))
