responseModelsUI <- function(id) {
  fluidPage(
    HTML(
      '
        <h3 style="color:#2f5496">Biostimulatory Response Models</h3>
        <p> Response models can help managers identify stress levels above which poor biological conditions are likely to occur. We calibrated a type of general additive model (shape-constrained additive models with monotonic negative relationships) between biointegrity index scores and biostimulatory indicator concentrations. Managers can use these models to identify candidate biostimulatory threshold for a wide range of biointegrity goals for the CSCI, ASCI_D, or ASCI_H. </p>
        <p>After entering the biointegrity goal and selecting the desired biointegrity and biostimulatory indicators, the dashboard will provide a table of the resulting biostimulatory thresholds, along with a plot showing the model response.</p>
      '
    )
  )
}