box::use(
  shiny[testServer],
  testthat[...],
)
box::use(
  app/main[...],
)

test_that("selected year, month and time range is equal to respective inputs", {
  testServer(server, {
    expect_equal(selected_year(), input$selected_year)
    expect_equal(selected_month(), input$selected_month)
    expect_equal(previous_time_range(), input$previous_time_range)
  })
})
