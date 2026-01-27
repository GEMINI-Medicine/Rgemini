test_that("CCI sections and subsections are correct", {
  cci_codes <- data.table(
    intervention_code = c(
      "1AA.13H.AC2",
      "1M-A52H-A",
      "2EA20ZZ ",
      "2 PB70DA",
      "3SC10KM",
      "_3ZA10VA",
      "5LB$08ZZ",
      "6KA02ME)",
      "#7SJ08ZZ",
      "8AA70BABA",
      "4AAAAAA"
    ),
    section_descr_expected = c(
      "Physical and physiological therapeutic interventions",
      "Physical and physiological therapeutic interventions",
      "Diagnostic interventions",
      "Diagnostic interventions",
      "Diagnostic imaging interventions",
      "Diagnostic imaging interventions",
      "Obstetrical and fetal interventions",
      "Cognitive, psychosocial and sensory therapeutic interventions",
      "Other healthcare interventions",
      "Therapeutic interventions strengthening the immune system and/or genetic composition",
      NA
    ),
    subsection_descr_expected = c(
      "Therapeutic Interventions on the Nervous System",
      "Therapeutic Interventions on the Lymphatic System",
      "Diagnostic Interventions on the Orocraniofacial Region",
      "Diagnostic Interventions on the Genitourinary System",
      "Diagnostic Imaging Interventions on the Musculoskeletal System",
      "Diagnostic Imaging Interventions on the Body NEC",
      "Interventions During Labour and Delivery",
      "Therapeutic Interventions for Cognition and Learning",
      "Support Activity Healthcare Interventions",
      "Therapeutic Interventions Strengthening the Immune System and/or Genetic Composition",
      NA
    )
  )

  res <- cci_group(cci_codes)

  expect_equal(res$section_descr, res$section_descr_expected)
  expect_equal(res$subsection_descr, res$subsection_descr_expected)
})
