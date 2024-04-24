
sparkline <- plot_ly(mdf) %>%
  add_lines(
    x = ~date, y = ~cases,
    color = I("white"), span = I(1),
    fill = 'tozeroy', alpha = 0.2
  ) %>%
  layout(
    xaxis = list(visible = F, showgrid = F, title = ""),
    yaxis = list(visible = F, showgrid = F, title = ""),
    hovermode = "x",
    margin = list(t = 0, r = 0, l = 0, b = 0),
    font = list(color = "white"),
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent"
  ) %>%
  config(displayModeBar = F) %>%
  htmlwidgets::onRender(
    "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
  )

bar_counts <- plot_ly(
  data = mdf,
  x = ~date,
  y = ~cases,
  type = 'bar'
)

vbs <- list(
  value_box(
    title = 'Total Cases',
    value = format_bignum(totalCases),
    theme = 'success',
    #showcase = sparkline,
    #showcase = bs_icon('bar-chart-fill'),
    full_screen = TRUE
    
  ),
  value_box(
    title = 'Absolute Count',
    value = totalCases,
    theme = 'blue',
    #showcase = bs_icon('bar-chart-fill')
  )
)