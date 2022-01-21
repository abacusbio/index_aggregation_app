app <- ShinyDriver$new("../../", loadTimeout = 1e+04)
app$snapshotInit("test-file-uploads")

app$uploadFile(`step1-desc_ebv-file` = "../../data/description_bv.xlsx") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(`step1-dat_ebv-file` = "../../data/bv.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(`step1-desc_ev-file` = "../../data/description_ev.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(`step1-dat_ev-file` = "../../data/ev.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$snapshot(list(output = "step1-demo_message"))
app$snapshot(list(output = "step1-sanity_message"))
