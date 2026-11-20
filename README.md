# Tarik Skubal: What Would Eight Years Be Worth to Toronto?

An independent, public-data exploration of a hypothetical eight-year Tarik Skubal contract with the Toronto Blue Jays, covering the 2027 through 2034 seasons.

[Read the interactive HTML report](https://YOUR-USERNAME.github.io/YOUR-REPOSITORY/)

This project asks two related questions: What might Skubal produce over eight seasons, and what could that production be worth to Toronto? It is an analytical exercise, not a prediction that Toronto will sign him or a recommendation based on internal team information.

## Why I built this

I wanted to see how far I could take a real baseball decision using information available to someone outside an MLB front office. I started by examining Skubal's pitches and potential fit in Toronto, then explored how comparable pitchers aged, how starting pitchers have been paid in free agency, and how those pieces fit into a contract valuation.

Just as important as the final numbers is the path to them. I tested several approaches, revisited assumptions, and kept the uncertainty visible.

## Start here

- [Interactive report](index.html): The main story, with visualizations, explanations, results, and limitations.
- [Annotated R analysis](skubal_analysis_public.R): The exploratory code, organized into teaching sections with notes on what each step does and why.

The report is designed for readers without a statistics background. The R file is there for anyone who wants to follow the reasoning in more detail.

## The analysis, in brief

1. Examine Skubal's public Statcast pitch data, including velocity, movement, pitch mix, and outcomes.
2. Explore roof conditions and comparable pitch characteristics at Rogers Centre.
3. Review changes in his 2026 pitching profile.
4. Build a historical aging cohort and project eight seasons of FanGraphs WAR.
5. Study publicly reported starting-pitcher free-agent contracts.
6. Compare one-year and three-year performance as salary predictors, using leave-one-out validation.
7. Test hypothetical eight-year contract prices against projected production value.

## Main findings

| Question | Illustrative result |
| --- | ---: |
| Modeled WAR, 2027 to 2034 | 24.6 |
| 95% prediction interval for eight-year WAR | 2.7 to 46.6 |
| Salary model using previous-season WAR | $451 million over eight years |
| Salary model using three-year WAR | $595 million over eight years |
| Present value of modeled production at an assumed $10M per WAR | $210 million |
| Present value of a $320M contract with equal annual payments | $259 million |
| Present-value surplus at $320M under those assumptions | -$48.5 million |

These figures are conditional model outputs, not precise forecasts or verified market valuations. The two salary models produce substantially different results, and their prediction intervals are very wide. Skubal's three-year WAR also exceeds the observed range in the salary-model training data.

The $10 million per WAR figure is an illustrative assumption, not an independently estimated future market price.


## Bayesian extension: carrying uncertainty into the valuation

I also added a Bayesian Student-t regression using the same historical aging cohort and recent three-year WAR predictor. The goal is to draw possible eight-year outcomes for an individual pitcher, then carry those posterior predictive draws through the contract valuation under different starting values per WAR and annual growth assumptions.

This extension is documented in section 28 of the R script and discussed in the HTML report. I have not published numerical Bayesian probabilities because the model has not yet been run and validated. The next steps are to inspect sampling diagnostics, check prior sensitivity, and evaluate predictive performance. The model uses the same limited cohort, and its probabilities remain conditional on the chosen priors, likelihood, annual WAR allocation, and contract assumptions. It is not a hierarchical annual-performance model.

## Data sources

The analysis uses publicly accessible information from:

- [Baseball Savant / Statcast](https://baseballsavant.mlb.com/): Pitch tracking, movement, velocity, and outcomes.
- [MLB Stats API](https://statsapi.mlb.com/): Public game and weather information used in roof-condition exploration.
- [FanGraphs](https://www.fangraphs.com/): Historical pitcher WAR and publicly reported free-agent contract information.

The code uses the `baseballr` package and other R packages to collect and analyze the data. Source availability, field definitions, and published contract terms can change over time.

No proprietary team data, internal medical records, private scouting evaluations, or confidential contract information were used.

## Reproducing the work

The annotated R file preserves the exploratory analysis, including checks, revisions, and sensitivity tests. It is intended as a teaching and transparency resource, not yet a fully automated, one-click pipeline.

To work through it:

1. Install R and RStudio, then install the packages referenced in the script.
2. Open `skubal_analysis_public.R`.
3. Set `project_dir` to a directory on your own computer. The published script intentionally leaves local paths blank.
4. Run the sections in order, reviewing the diagnostic outputs.
5. Confirm public data downloads and manual roof-condition classifications before interpreting the results.

Some sections depend on cached data created earlier in the script. The original exploratory script also references `rogers_model_data` without a complete standalone definition. That section needs reconciliation before the analysis can be described as independently reproducible from a fresh R session.

## Limitations

I approached this project using publicly available Statcast, FanGraphs, and MLB data. That gave me plenty to work with, but I still had to make choices about what to compare, how to build the models, and how much confidence to put in the results. Here are the main limitations I kept in mind.

### The historical comparisons are not perfect

I started by identifying pitchers who were roughly the same age as Skubal and had similar recent performance. I wanted to see what happened to them over the following eight seasons. No two pitchers are exactly alike, though, and the final group included just 31 pitchers. That gives me a useful historical reference, not a guarantee that Skubal will follow the same path.

### The 2020 season complicated things

Every pitcher in the historical comparison group had 2020 in the eight-year follow-up period. Since that season was shortened, I tested an adjustment using the surrounding seasons. That helped me see how sensitive the results were to 2020, but the adjusted numbers are estimates, not performance that actually happened.

### The aging model has a lot of uncertainty

My model projected about 24.6 WAR over eight seasons, but the 95% prediction interval ran from roughly 2.7 to 46.6 WAR. That is a pretty substantial range. Recent performance tells us something about future production, but clearly not everything. I tried additional variables and different models, and they did not consistently improve the predictions.

### The salary models had limited information

I paired publicly reported free-agent contracts with pitchers' recent performance. The two salary models performed similarly when tested against historical contracts, yet estimated very different eight-year prices for Skubal: about $451 million and $595 million. His three-year WAR was also higher than anything in that model's training data, so I was asking it to predict beyond what it had observed. That makes both figures worth treating cautiously.

### The Rogers Centre analysis was exploratory

I wanted to see whether pitching conditions at Rogers Centre might matter, especially with the roof open or closed. Separating those conditions from differences between pitchers, seasons, and games is difficult. I used comparisons and statistical controls to explore the question, but the results show associations, not proof that the roof caused a change.

### The financial results depend on my assumptions

To connect projected WAR with contract value, I used an illustrative $10 million per WAR and a 5% discount rate. Those were assumptions I chose for the scenarios, not values established by the data. Change those assumptions and the financial results change too. The comparisons help show how the numbers respond, rather than identifying one exact contract value.

### What I take away from all of this

I did not start this project expecting one perfect answer, and I do not think the results provide one. I wanted to use the public information I could access, test different approaches, see what held up, and understand where the uncertainty came from. Some approaches worked better than others. Some results raised more questions than they answered. That is part of the process. For me, the value is knowing what the data supports, what it does not, and what I would want to investigate next.

## Where I would take this next

With access to appropriately authorized internal data, I would explore:

- More individualized performance and injury-risk projections using medical, biomechanical, scouting, and player-development information.
- A Toronto-specific fit model that considers the defense, catching, opponent mix, and alternatives already on the roster.
- More complete contract economics, including payment schedules, tax effects, payroll flexibility, and opportunity costs.
- A simulation tool that shows how the decision changes across plausible performance, health, and contract scenarios.

The aim would not be to find one perfect contract number. It would be to understand which assumptions matter most and make the uncertainty useful for decision-making.

## Publishing on GitHub Pages

This repository includes `index.html`, so GitHub Pages can serve the report directly.

In the repository, go to Settings > Pages. Under Build and deployment, choose Deploy from a branch, select your main branch and the root folder, then save. Replace the placeholder URL at the top of this README with the Pages URL GitHub provides.

## About this project

Independent research and educational analysis using publicly available baseball data.

The code and commentary document an exploratory workflow. They should not be interpreted as professional financial advice, an official MLB projection, or an endorsement by any team or data provider.
