# Review notes — `paper/report.qmd`

Copyedit and internal-consistency review only. Reported numbers, figures, and
conclusions are treated as final and correct; findings below concern whether the
prose accurately and consistently *explains* what was done, not whether it was right.

Line numbers refer to `paper/report.qmd`. Severity: **Critical** (breaks understanding
or misstates a result), **Moderate** (inconsistency/gap worth fixing), **Minor**
(typo/style). Where a finding is a genuine inconsistency, I describe both sides rather
than picking the "correct" one — that is the authors' call.

Cross-checked against: `paper/output/regression/*.tex`,
`analysis/hedonic_xsec_zhvi_window_full.Rmd`, `paper/R/theme.R`,
`paper/R/06_hedonic_plots_rev.R`, `paper/R/eda_zhvi.R`, and the cached CSV/attrition
outputs.

---

## 1. Internal consistency

**1a. (Moderate) Significance stars mismatch — pre-2023 log(EAL).**
Line 254: "the pre-2023 regime check: -.035\*\*\*, -.022\*\*\*, and -0.016\*\* respectively".
`table_robustness.tex` (HVRI / pre-2023 column) reports Log expected annual loss =
**-0.016\*\*\*** (p<0.01), not \*\* (p<0.05). The prose understates the significance level.
(The -.035 / -.022 values for the 2020 and 2021 single-vintage cross-sections are not in
any table included in the report — they are hardcoded from notebook output and could not
be checked against a rendered table.)

**1b. (Moderate) ZHVI coverage end date conflict — "Feb 2026" vs March 2026.**
Line 272: "complete post-storm window within ZHVI coverage (through Feb 2026)". The cached
`zhvi_coverage_summary.csv` has `date_max = 2026-03-31`, so the Data section renders
"…to March 2026" via the inline `zhvi_date_max`, and the fig-tier-trajectory caption
(line 630) says "2020–2026". "Feb 2026" contradicts the March 2026 coverage end used
elsewhere. Reconcile (is Feb 2026 a filter cutoff, or a stale date?).

**1c. (Moderate) CRF ratio stated backwards.**
Line 235: "The proportionate ratio of **resiliency to social vulnerability** in CRF
obscures the relationship…". CRF is defined at line 150 and in the equation (line 152) as
Social Vulnerability ÷ Community Resilience — i.e., the ratio of vulnerability to
resilience. Line 235 inverts that order.

**1d. (Moderate) "±6-month window" vs "full ZHVI window" for the same cross-section.**
Line 247 describes the vintage-collapsed cross-section as "a mean ZHVI +/-6 month window
from NRI release." The appendix figure caption (line 621) and the source notebook
(`hedonic_xsec_zhvi_window_full.Rmd`, and its "full ZHVI window" heading) call the same
object (xs_B) the "full ZHVI window" cross-section. The underlying `hedonic_xsec.csv`
uses 12-month (±6) per-vintage windows, so "±6 month" matches the data; "full ZHVI window"
appears to mean "using the full ZHVI history." As written the two labels read as
contradictory descriptions of xs_B; clarify.

**1e. (Moderate) Two distinct NRI methodology changes may be conflated.**
Line 156 describes an NRI change "between the 2021 and 2023 releases, shifting from a
relative index to an absolute dollar-based measure… discontinuity for events near the 2022
transition." Separately, Spec C and the robustness checks (lines 220, 245, 550, 613) test a
**SoVI** methodology break in **March 2023** (HVRI → CDC). These are two different
discontinuities in two different components, discussed in different places without noting
they are distinct. A reader can conflate "the 2022 transition" (EAL) with "the March 2023
SoVI break." Also, line 156's "2022 transition" has no 2022 vintage — it means the gap
between the 2021 and 2023 releases; say so.

**1f. (Moderate/Discussion vs Limitations) "causal" framing.**
Line 545 (Conclusions): "Both the descriptive and **causal** examinations yield null
results." Elsewhere the second design is framed as a "natural experiment" (line 186) and
Limitations states "This regression measures **correlation** conditional on observables"
(line 549). "Causal examination" is stronger than the paper's hedging elsewhere; reconcile
the framing.

**1g. (Moderate) Orphaned appendix figures — placed in the Appendix but never referenced.**
No `@fig-`/`@tbl-` cross-reference in the prose points to these, so a reader is never
directed to them:
- `fig-hedonic-income` (line 601)
- `fig-events-type` (line 626)
- `fig-tier-trajectory` (line 630)
- `fig-hedonic-urban-rural` (line 634)
- `fig-balance` (line 638)
- `fig-attrition` (line 642)
- `fig-es-bottom` (line 646)
- `fig-es-top` (line 650)

(Referenced and fine: `@tbl-hedonic` L231, `@tbl-robustness` L245, `@fig-hedonic-mde` L249,
`@fig-hedonic-panel-vs-xsec` L551, `@tbl-baseline` L457, `@tbl-tier` L461.)

**1h. (Moderate) Covariate-balance table is built but suppressed.**
The `tbl-balance-summary` chunk (line 321) has `#| output: false` and `#| results: hide`,
so the balance table (caption at line 340) never renders. Its companion `fig-balance` is
orphaned (1g). The "Attrition and Covariate Balance" heading (line 282) therefore sets up a
balance analysis whose numeric table never appears and whose figure is never referenced.

**1i. (Moderate) Two figure chunks have no caption.**
`fig-hedonic-income` (line 601) and `fig-hedonic-mde` (line 617) have no `fig.cap`.
`fig-hedonic-mde` is referenced at line 249 but will render without a descriptive caption;
`fig-hedonic-income` has neither a caption nor a reference.

**1j. (Minor) "marginally significant" vs "does not achieve significance" (bottom-tier damage).**
Line 461: "Log damage is marginally significant on the bottom tier only (-0.26\*)."
Line 465: the same coefficient "approaches but does not achieve significance when measured
as cumulative impulse response." Both refer to the bottom-tier CIR damage coefficient
(-0.2648\*, significant at 10% not 5%); the two phrasings read as mildly contradictory.

**1k. (Minor) "Null on NRI risk value robust to baseline choices" vs R1 result.**
Line 457: "The null results on NRI risk value and storm characteristics are robust to
baseline choices." `table_baseline_sensitivity.tex` shows Log NRI Risk Value = **1.091\***
(significant at 10%) for R1. The adjacent claim "No other variable reaches significance at
the 5% level" is consistent, but the "robust null on NRI risk value" statement glosses the
R1 10% result.

**1l. (Minor) "larger magnitude … SoVI in the pre-2023 regime."**
Line 245 says log(EAL), resilience, and SoVI all have "larger magnitude and significance
level … in the pre-2023 regime." In `table_robustness.tex`, pre-2023 (HVRI) SoVI = -0.022
vs Main -0.024 — marginally *smaller* in magnitude, not larger (EAL and resilience are
larger, as claimed).

**1m. (Minor) Event counts (prose) vs regression N (tables).**
Lines 351-355 report R1 / R2 / Regional as 1,240 / 1,648 / 2,341 events (from the attrition
table). The regression tables show N = 1,239 / 1,646 / 2,341 (`table_baseline_sensitivity`,
`table_buildup`). The 1-2 observation differences (presumably dropped for missing
covariates) aren't explained; a reader comparing prose to table may notice.

**1n. (Minor) In-body floats without cross-references.**
These render in place but are never `@`-referenced: `fig-tier-dist` (134),
`fig-storm-heatmap` (144), `fig-hedonic-coeff` (227), `fig-es-mid` (387), `tbl-buildup`
(445), `fig-lp-baseline` (491), `fig-lp-tier` (499). Not broken, but an explicit pointer is
best practice.

**1o. (Minor) `tbl-hedonic` columns labeled by name, not spec letter.**
Caption (line 607) says "specifications A, B, C, D, E, F," but the table headers read
"NRI Basic / NRI Split / NRI Regime / No SOVI / No Income / Urban-Rural FE." The A–F ↔ name
mapping lives only in §"Hedonic Model Specifications"; consider labeling columns with the
spec letters or repeating the mapping near the table.

---

## 2. Methodology fully explained

**2a. (Moderate) "Month fixed effects" is actually year-month (`month_id`).**
Lines 184 and 216 say "state and month fixed effects" / "state + month fixed effects." The
code uses `fe <- "state + month_id"` (a year-month time index), and the hedonic table
labels it "Month." "Month" is ambiguous — a reader may take it as a 12-level seasonal
calendar-month effect, but it is a year-month time FE. State explicitly that it is a
year-month (time) fixed effect.

**2b. (Moderate) Spec formulas omit the always-included controls.**
The per-spec formulas (lines 218-224) list only NRI terms, e.g. "Spec A … ZHVI ~ log(EAL) +
log(CRF)." But every column of `hedonic_regression.tex` also includes log median income,
log population density, and coastal. A reader would not expect income in Spec A from the
formula. State that income + population density + coastal enter all specs (or write them
into the formulas).

**2c. (Moderate) Population density never introduced as a control.**
Log population density appears in every hedonic spec, is discussed in Results (lines 233,
541), and appears in the CIR tables, but the "County-level data" controls list (lines
200-212) names only income and coastal, and the Data section never mentions population
density or its source. Introduce it (and credit its source — see 3c).

**2d. (Moderate) CIR-regression specification not described in prose.**
The Event Study "Regression" section (lines 395-461) never states the estimating equation,
that the CIR regressions use month-year fixed effects, or that SEs are two-way clustered
(stcofips & month_year). All of that is visible only in the tables. Likewise the
local-projection section (lines 463-501) gives the concept but not the horizon-by-horizon
specification (regressors, fixed effects, standard errors). A reader without the code/tables
cannot reconstruct either model.

**2e. (Moderate) MDE explanation is ambiguous about the three thresholds.**
Line 249: "log(EAL) coefficients … are below the detectable effect threshold of 0.0149 …
Resilience and SoVI coefficients … exceed the MDE threshold at 80% power of 0.024 and 0.01."
The three thresholds (0.0149, 0.024, 0.01) differ because each coefficient has a different
SE (MDE = 2.8 × SE, computed per coefficient from xs_B), but the text doesn't say so — it
reads as one threshold followed by two loose numbers. Also clarify whether the coefficients
being compared are the panel or the cross-section estimates. (The "MDE ≈ 2.8 × SE, α = 0.05
two-sided, 80% power" description itself matches `06_hedonic_plots_rev.R` and the notebook.)

**2f. (Minor) Cross-section fixed effects not stated.**
Line 247 describes the vintage-collapsed cross-section but not its FE. In the notebook,
xs_B uses `state + nri_vintage`; the single-vintage cuts (xs_2020…xs_2025) use `state` only.
A one-line note would tell the reader what is absorbed.

---

## 3. Data and sources fully explained

**3a. (Moderate) USDA Rural-Urban Continuum Codes (RUCC) not described or credited.**
RUCC is used in Spec F (lines 223, 239) and `fig-hedonic-urban-rural` (line 634), and the
code uses `rucc_2023`, but there is no Data-section entry describing what RUCC is, its
vintage, granularity, or source, and USDA/RUCC has no entry in References. (Also misspelled
"RUUC" at line 239 — see 4a.)

**3b. (Moderate) Coastal-county status: source not described.**
"Coastal County Status" is a control in every hedonic spec and a central event-study result
(lines 203, 233, 457, 461, 531), but the Data section never says where the coastal
designation comes from or how "coastal" is defined.

**3c. (Moderate) Population density: source not described.**
See 2c. Used throughout, never attributed to a data source (e.g., ACS/Census) in the Data
section.

**3d. (Moderate) Inline citations missing from References.**
Cited in the Introduction but with no entry in §References:
- Gourevitch et al. (2023, *Nature Climate Change*) — line 111.
- Congressional Budget Office flood-damage estimate — line 109.
- FHFA portfolio analysis (Sept 2024 loans) — line 109.

**3e. (Minor) References not cited in text (orphaned entries).**
Contat (2024), Jordà (2005), and Zivin, Liao & Panassié (2023) appear in References but are
never cited inline.

**3f. (Minor) Ambiguous "Coren 2024" citation.**
Line 107 cites "(Coren 2024, Parshley 2023)", but References has two 2024 Coren entries
(Nov 5 solo; Nov 20 Coren/Ahmed/Crowe). Disambiguate (e.g., 2024a / 2024b).

**3g. (Moderate — verify) ZHVI described as a "repeat-sales index."**
Line 132: "the Zillow Home Value Index (ZHVI), a repeat-sales index of monthly county-level
median home values." ZHVI is Zillow's smoothed, seasonally-adjusted measure of *typical*
home value derived from its valuation models — not a repeat-sales index (repeat-sales is the
Case-Shiller / FHFA HPI methodology), and "typical value" rather than a strict "median."
Verify the data-source description. (Flag, not a statistics check.)

**3h. (Minor — verify) "18 climatological hazards."**
Abstract, line 96: NRI's 18 hazard types include non-climatological ones (earthquake,
volcanic activity, tsunami). Consider "18 natural hazards."

**3i. (Minor) "SoVI" never expanded/defined on first use.**
"SoVI" is used from line 219 onward but never spelled out (Social Vulnerability Index) or
explicitly tied to the NRI "Social Vulnerability" component. The report also uses "Social
Vulnerability," "CDC SVI," and "HVRI" for related quantities; a one-line definition on first
use would help.

**3j. (Minor) "six full years 2020-2025" vs ZHVI through 2026.**
Line 128 says the data span 2020–2025, but ZHVI coverage runs to March 2026 (needed for
post-storm windows and stated as such elsewhere). A clause noting ZHVI extends into 2026 for
the post-event windows would remove the tension.

---

## 4. Grammar, typos, copyediting

**4a. (Minor) Line 239: "RUUC" → "RUCC"** (Rural-Urban Continuum Codes; spelled correctly at
line 634).

**4b. (Minor) Line 254: missing space** — "≈ −0.017.The estimated" → "≈ −0.017. The estimated".

**4c. (Minor) Line 551: "Resillience" → "Resilience".**

**4d. (Minor) Line 555: "specifci" → "specific"; add article/comma** — "ZHVI is an index,
not specifci market transaction so we cannot…" → "…not a specific market transaction, so we
cannot…" ("transaction" likely should be "transactions").

**4e. (Minor) SoVI capitalization inconsistent** — line 221 "Sovi" and line 243 "SoVi" vs
"SoVI" everywhere else. Standardize to "SoVI." (Line 221 also: "Spec B - Sovi".)

**4f. (Minor) "resilience" vs "resiliency" used interchangeably** — "resiliency" at lines 97,
170, 235; "resilience" ~39× elsewhere. Pick one.

**4g. (Minor) Line 107: missing serial "and" / garden-path list** — "including storms,
floods, fires, tornadoes are becoming increasingly frequent" → e.g., "Extreme weather events
— storms, floods, fires, and tornadoes — are becoming increasingly frequent."

**4h. (Minor) Line 128: "the geographic isolation differentiates those areas" → "their
geographic isolation differentiates those areas".**

**4i. (Minor) Line 140: number agreement** — "Storm events are organized into episodes, a
single storm system that may produce multiple events" → "…into episodes; an episode is a
single storm system that may produce multiple events…".

**4j. (Minor) Line 393: subject-verb** — "Pre-storm deviation of `r pre_t1_dev` at T-1 are
significant" → "…is significant".

**4k. (Minor) Line 563: "NRI methodology changed within study" → "within the study".**

**4l. (Minor) "2 part" (line 96) and "2 phase" (line 182) → "two-part" / "two-phase".**

**4m. (Minor) Line 223: stray backslash in Spec F bullet** — "**Spec F (…):** \ see if…";
remove the "\ ".

**4n. (Minor) Line 235: "would correspond with actually reducing property prices" → "would
correspond to actually reducing…"** (awkward phrasing).

**4o. (Minor) Market-tier capitalization inconsistent** — "bottom/mid/top" (lines 132, 176)
vs "Bottom/Mid/Top" (line 461 and tables). Standardize.

**4p. (Minor) Stray double spaces** — e.g., line 545 "yield null results.  This result" and
trailing double spaces on abstract lines 97-98. Cosmetic.

---

## Ambiguities flagged for the authors (meaning unclear, not just a typo)

- **Line 249 (MDE):** "Resilience and SoVI coefficients … exceed the MDE threshold at 80%
  power of 0.024 and 0.01." Unclear whether 0.024 / 0.01 are the two coefficients' MDE
  *thresholds* (my reading, given per-coefficient SEs) or the coefficient magnitudes. See 2e.
- **Line 254:** "the only models where EAL increased and was significant were the 2020 and
  2021 … snapshots, and the pre-2023 regime check" — but Spec E (No Income) also has a
  significant log(EAL) (+0.023\*\*\*, positive). "EAL increased" appears to mean "the negative
  discount grew," which conflicts with the plain reading and with the positive Spec E result.
  Clarify the intended sense.
- **Line 254:** "In this example, the corresponding log(EAL) coefficient would be above our
  minimum detectable effect threshold." "In this example" is unclear — it seems to mean the
  −0.017 capitalization benchmark exceeds the 0.0149 MDE threshold; state that directly.
