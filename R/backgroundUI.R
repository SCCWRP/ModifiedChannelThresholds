#' UI for Background tab
#'
#' @param id module id
#'
#' @return html for page
#'
#' @import bslib
backgroundUI <- function(id) {
  last_updated_date <- list.files(
    path = c("data", "data-raw", "R", "app.R"), recursive = TRUE, full.names = TRUE
  ) |>
    file.info() |>
    dplyr::pull(mtime) |>
    max()
  page_fixed(
    tags$head(tags$style('
      .hangingIndent {padding-left: 22px; text-indent: -22px}
      .center {text-align: center;}
    ')),
    h2('[Draft] Biointegrity and Eutrophication Thresholds for Modified Channels and Other Classes of Streams'),
    p('Last updated',  format(last_updated_date, "%Y-%m-%d")),
    h3("Introduction"),
    p('Watershed managers in California face a number of challenges when it comes to applying bioassessment tools (such as biointegrity indices or thresholds for eutrophication substances) on certain types of streams. These challenges include suspicions about the applicability of tools in regions or stream types that differ from reference data sets used to calibrate tools (e.g., Central Valley Floor streams, intermittent streams), as well as concerns about the ability to achieve reference-based thresholds in channels that have been modified for flood protection or water conveyance. These issues are particularly relevant in the Central Valley, although they pertain to watershed managers in all parts of California.'),
    p('This dashboard is intended to help managers explore thresholds for biointegrity and eutrophication indicators for classes of streams where standard approaches may not be appropriate, either due to uncertainty about their validity or doubts about the feasibility of achieving these thresholds. Details on how these thresholds were identified can be found in SCCWRP Technical Report #1367. The use of the thresholds in this dashboard and in Technical Report #1367 is provided for informational purposes, and it is not intended to endorse the use of thresholds or waterbody classifications in policy or regulatory programs.'),
    p('This dashboard (Version 1.2) was created by Raphael Mazor, Nicholas Lombardo, and Adriana Le Compte-Santiago. For more information, please contact Raphael Mazor (raphaelm@sccwrp.org).'),
    h3('Biointegrity and Eutrophication Indicators'),
    h4('Biointegrity Indicators'),
    strong('California Stream Condition Index (CSCI)'),
    br(),
    p('The CSCI is the standard index to assess the ecological condition of wadeable streams in California using benthic macroinvertebrates (Mazor et al. 2016). It is a predictive index, meaning that it assesses condition by comparing observed benthic macroinvertebrate assemblage composition to the composition expected to occur at environmentally similar reference sites. CSCI scores close to 1 are considered to be in reference condition, whereas lower scores indicate degradation of ecological health. Although most of the reference sites used to calibrate the CSCI are perennial, many in southern California have subsequently been determined to be intermittent. Benthic macroinvertebrate samples should be collected by SWAMP-comparable protocols (e.g., Ode et al. 2016) for calculating CSCI scores.'),
    imageOutput(NS(id, "csci"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    strong('Algal Stream Condition Indices (ASCIs)'),
    br(),
    p('California has two benthic algal indices for assessing wadeable streams: an index based on benthic diatoms (ASCI_D), and an index based on a hybrid of diatoms and soft-bodied algal taxa (ASCI_H; Theroux et al. 2020). Like the CSCI, the ASCIs are predictive indices. In general, the ASCIs are more sensitive to water quality degradation, whereas the CSCI is more sensitive to habitat degradation; however, all indices reflect the overall condition of streams and their response to both water and habitat quality. ASCI scores close to 1 are considered to be in reference condition, whereas lower scores indicate degradation of ecological health. Benthic algal samples should be collected by SWAMP-comparable protocols (e.g., Ode et al. 2016) for calculating ASCI scores.'),
    imageOutput(NS(id, "asci"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    h4('Eutrophication Indicators'),
    strong('Total Nitrogen (TN) and Total Phosphorus (TP)'),
    br(),
    p('TN and TP are the most important nutrients driving growth rates in algae and higher plants. Consequently, TN and TP concentrations have very strong relationships with ASCI scores, as well as CSCI scores (Mazor et al. 2022). Concentrations of TN and TP should be measured in baseflow conditions and expressed in units of mg/l for use in this dashboard.'),
    strong('Benthic Chlorophyll-a (Chl-a)'),
    br(),
    p('Chl-a is one of the primary pigments used by algal taxa to conduct photosynthesis and convert sunlight into energy. Chl-a should be measured using SWAMP-comparable protocols (e.g., Ode et al. 2016) and expressed in units of mg/m2 for use in this dashboard.'),
    strong('Benthic Ash-Free Dry Mass (AFDM)'),
    br(),
    p('AFDM is a measure of organic material found on the streambed, including organic material associated with algal, microbial, and fungal biomass. AFDM should be measured using SWAMP-comparable protocols (e.g., Ode et al. 2016) and expressed in units of g/m2 for use in this dashboard.'),
    strong('Percent Macroalgal Cover (% cover)'),
    br(),
    p('% cover is measure of excess algal growth. Although its relationship with biointegrity indicators is weaker than other eutrophication indicators (Mazor et al. 2022), it is particularly useful for assessing aesthetic impacts of eutrophication on wadeable streams (Biggs 2000, Suplee et al. 2009).'),
    br(),
    imageOutput(NS(id, "perc_cover"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    h3('Classes of Streams'),
    p('See SCCWRP Technical Report #1367 for more details about stream classes.'),
    strong('Wadeable Streams'),
    br(),
    p('Wadeable Streams are those where standard bioassessment protocols may be implemented (e.g., Ode et al. 2016). As defined in Ode et al. (2016), assessment reaches are typically between 150 and 250 m (but may be as short as 100 m), have continuous surface water throughout the reach, and have visible flow for at least short sections of the reach. At least half the reach should be less than 1 m deep, and half should be at least 1 m wide. This is the “standard” class of streams for which most bioassessment tools are intended.'),
    br(),
    imageOutput(NS(id, "ws"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Regularly Flowing Intermittent (RFI) streams'),
    br(),
    p('RFI streams typically flow for at least one month in most years, but unlike perennial streams, they typically dry for at least one week in most years. For this dashboard, RFI streams are sub-categorized as those occurring in Southern California (specifically within the boundaries of the Water Quality Control Boards for regions 4, 7, 8, and 9), and those within xeric portions of Northern California (primarily, the coastal and interior Chaparral). RFI streams in other parts of Northern California remain poorly studied and are not included in this dashboard.'),
    br(),
    imageOutput(NS(id, "rfi"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Seldomly Flowing Intermittent (SFI) streams'),
    br(),
    p('SFI streams typically flow less than one month per year, and may not flow at all in dry years. However, they may have substantial flow in high precipitation years, sufficient for implementation of bioassessment protocols (e.g., Ode et al. 2016). SFI streams are poorly studied and are not included in this dashboard.'),
    imageOutput(NS(id, "sfi"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Central Valley Floor (CVF)'),
    br(),
    p('The Central Valley of California has been so extensively altered that essentially no reference sites may be found in this region. Thus, the applicability of the CSCI and ASCIs is difficult to assess.'),
    br(),
    imageOutput(NS(id, "cvf"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Hard-bottom engineered channels (HB)'),
    br(),
    p('HB channels are hardened by concrete, stone or other erosion-resistant structures. Banks of HB streams are often armored as well, although not always.'),
    br(),
    imageOutput(NS(id, "hb"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Soft-bottom engineered channels with 2 hardened sides (SB2)'),
    br(),
    p('SB2 streams have a soft or natural streambed, but banks are armored with riprap, concrete, or similar material.'),
    br(),
    imageOutput(NS(id, "sb2"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Soft-bottom engineered channels with 1 hardened side (SB1)'),
    br(),
    p('SB1 streams have armoring on one bank, but the other bank is in a relatively natural state. These streams are often found along roadsides in undeveloped areas.'),
    br(),
    imageOutput(NS(id, "sb1"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Soft-bottom engineered channels with no hardened side (SB0)'),
    br(),
    p('SB0 streams are unhardened, but have been straightened or demonstrate an otherwise non-natural channel form, with greatly reduced complexity of in-stream habitat. In some circumstances, it can be difficult to determine if a stream should be classified as SB0 or natural (e.g., streams with close-set levees on the floodplain).'),
    imageOutput(NS(id, "sb0"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Constructed channels (CC)'),
    br(),
    p('Unlike the aforementioned categories of channels, CCs are formerly upland habitat and have an entirely anthropogenic origin. As a rule, CCs have ambiguous watersheds that defy traditional delineation methods (in contrast to streams running through realigned channels, which are properly classified as SB0 or another class of engineered channel). Although CCs likely occur in all parts of California, they are rarely included in monitoring programs.'),
    br(),
    imageOutput(NS(id, "cc"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    h3('Approaches to Identifying Thresholds'),
    p('Technical Report #1367 describes three approaches to identifying thresholds for biointegrity indicators. A fourth approach for identifying thresholds for eutrophication indicators is also described.'),
    strong('Threshold Stringency'),
    br(),
    p('This dashboard presents high, intermediate, and low-stringency numbers for all types of thresholds. For biointegrity indicators (e.g., CSCI), high stringency thresholds have higher numeric values than low stringency thresholds. For eutrophication indicators (e.g., TN), high stringency thresholds have lower numeric values than low stringency thresholds.'),
    br(),
    strong('Reference Thresholds'),
    br(),
    p('Undisturbed streams exhibit a range of values for biointegrity and eutrophication indicators. Thresholds based on these statistical distributions are useful for distinguishing natural variability from impacts related to human activity. Numbers representing the low end of the distribution of biointegrity scores (such as the 1st, 10th, or 30th percentiles) or high end of eutrophication indicator concentrations (such as the 70th, 90th, or 99th percentiles) are usually used to identify reference-based thresholds (U.S. Environmental Protection Agency 2000, Hawkins et al. 2010). Selecting less stringent reference-based thresholds (e.g., the 1st or 99th percentiles) reflects a high confidence in the quality of the reference sites from which data were collected, or a low tolerance for misidentifying a site as having poor conditions. Conversely, selecting more stringent thresholds (e.g., 30th or 70th percentiles) is appropriate if many reference sites are influenced by human activity, or if there is a low tolerance for misidentifying a site as having good conditions.'),
    br(),
    imageOutput(NS(id, "ref_thresholds"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Best observed thresholds'),
    br(),
    p('The highest biointegrity scores (or lowest eutrophication indicator concentrations) observed in classes of streams represent conditions that are, at least in certain circumstances, attainable (U.S. Environmental Protection Agency 2000, Dodds and Welch 2000, Heiskary and Bouchard 2015). These may be considered a form of reference-based thresholds when reference conditions are themselves unknowable due to pervasive disturbances characteristic of the Anthropocene, and are most appropriate for novel, human dominated aquatic ecosystems (Kopf et al. 2015).'),
    p('To calculate best observed thresholds, statistical distribution points are derived similar manner as with reference conditions. Selecting more stringent thresholds (e.g., the 1st or 99th percentiles) is appropriate when there is low risk tolerance for misidentifying a site as having poor conditions, or when there is low confidence that the best observed conditions are close to reference conditions. Conversely, selecting less stringent thresholds (e.g., 30th or 70th percentiles) is appropriate when there is low risk for misidentifying a site as having good conditions, or when there is high confidence that the best observed conditions are close to reference conditions.'),
    strong('Best attainable thresholds'),
    br(),
    p('Thresholds based on best attainable conditions represent the theoretical best state a reach can achieve, given appropriate management within long-term constraints that are unlikely to change in the near future. Such constraints may include watershed development, channel modification, upstream dams or diversions, or discharge of effluent achieving the maximum technically feasible treatment. Although determining the expected improvements at a site given proper management is a technical task, determining what constitutes a long-term constraint is a political decision that should reflect community values and agency priorities. Best attainable thresholds are inherently site-specific and are not addressed by this study. Because best attainable thresholds are site-specific, they are usually a better choice than other types of thresholds.'),
    br(),
    imageOutput(NS(id, "best_thresholds"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    br(),
    strong('Response models'),
    br(),
    p('A fourth approach to identifying thresholds, response models, is appropriate for eutrophication indicators, which can drive changes in biointegrity (U.S. Environmental Protection Agency 2000, Heiskary and Bouchard 2015). These models can take the form of logistic models, which predict the likelihood of attaining biointegrity goals as stress increases, or the form of continuous models, which directly predict the likely biointegrity index score as stress increases. Thresholds derived from response models have already been identified for California’s wadeable streams (Mazor et al. 2022). '),
    br(),
    imageOutput(NS(id, "res_thresholds"), height = "auto") |>
      tagAppendAttributes(class = "center"),
    h3('Cited Literature'),
    p(class = 'hangingIndent', 'Biggs, B. J. F. 2000. Eutrophication of streams and rivers: Dissolved nutrient-chlorophyll relationships for benthic algae. Journal of the North American Benthological Society 19:17–31.'),
    p(class = 'hangingIndent', ' Dodds, W. K. K., and E. B. Welch. 2000. Establishing nutrient criteria in streams. Journal of the North American Benthological Society 19:186–196.'),
    p(class = 'hangingIndent', 'Hawkins, C. P., J. R. Olson, and R. A. Hill. 2010. The reference condition: Predicting benchmarks for ecological and water-quality assessments. Journal of the North American Benthological Society 29:312–343.'),
    p(class = 'hangingIndent', 'Heiskary, S. A., and R. W. Bouchard. 2015. Development of eutrophication criteria for Minnesota streams and rivers using multiple lines of evidence. Freshwater Science 34:574–592.'),
    p(class = 'hangingIndent', 'Kopf, R. K., C. M. Finlayson, P. Humphries, N. C. Sims, and S. Hladyz. 2015. Anthropocene Baselines: Assessing Change and Managing Biodiversity in Human-Dominated Aquatic Ecosystems. BioScience 65:798–811.'),
    p(class = 'hangingIndent', 'Mazor, R. D., A. C. Rehn, P. R. Ode, M. Engeln, K. C. Schiff, E. D. Stein, D. J. Gillett, D. B. Herbst, and C. P. Hawkins. 2016. Bioassessment in complex environments: Designing an index for consistent meaning in different settings. Freshwater Science 35:249–271.'),
    p(class = 'hangingIndent', 'Mazor, R. D., M. Sutula, S. Theroux, M. Beck, and P. R. Ode. 2022. Eutrophication thresholds associated with protection of biological integrity in California wadeable streams. Ecological Indicators 142:109180.'),
    p(class = 'hangingIndent', 'Ode, P. R., A. E. Fetscher, and L. B. Busse. 2016. Standard Operating Procedures (SOP) for the Collection of Field Data for Bioassessment of California Wadeable Streams: Benthic Macroinvertebrates, Algae, and Physical Habitat. SWAMP SOP, Surface Water Ambient Monitoring Program, Sacramento, CA. (Available from: https://drive.google.com/file/d/0B40pxPC5g-D0MS1zMjNacnJZOEk/view?resourcekey=0-QM9z67su33KFwKroPrU0sw)'),
    p(class = 'hangingIndent', 'Suplee, M. W., V. Watson, M. Teply, and H. McKee. 2009. How Green is Too Green? Public Opinion of What Constitutes Undesirable Algae Levels in Streams. JAWRA Journal of the American Water Resources Association 45:123–140.'),
    p(class = 'hangingIndent', 'Theroux, S., R. D. Mazor, M. W. Beck, P. R. Ode, E. D. Stein, and M. Sutula. 2020. Predictive biological indices for algae populations in diverse stream environments. Ecological Indicators 119:106421.'),
    p(class = 'hangingIndent', 'U.S. Environmental Protection Agency. 2000. Nutrient Criteria Technical Guidance Manual: Rivers and Streams. Page 253. EPA-822-B-00-002, U.S. Environmental Protection Agency, Office of Science and Technology, Washington, D.C.')
  )
}
