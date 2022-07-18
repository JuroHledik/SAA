# Strategic Asset Allocation (SAA)
## _by Juraj Hledik_

SAA is a comprehensive solution for investors looking to diversify their portfolio according to their preferences. For a detailed description of the math, see the folder /report. The framework consists of three modular parts:

- Estimation of future asset returns from historical data
- Portfolio optimization accorting to investor preferences on expcted return, variance and conditional value at risk (aka expected shortfall).
- ShinyApp GUI for easy user access.

## Future return estimation

- Pairwise vine copula methodology
- Possibility to choose different copula families and historical return frequencies
- Visualization of pairwise return correlations
- Visualization of estimated copula structures

## Portfolio optimization

- Preferences focused on portfolio expected return, variance, conditional value at risk (expected shortfall) and difference from the current portfolio.
- Constraints include minimum allowed expected return, maximum allowed expected shortfall as well as individual asset weight- and amount- constraints.
- Ability to calculate the optimal portfolio using different models for future returns
- Comprehensive tracking of the recommended portfolios, easy comparison with respect to the preferences chosen.
- Output showing the optimal portfolio weights, future return density as well as hypothetical in-sample evaluation of said portfolio's performance on historical data.

## License

National Bank of Slovakia