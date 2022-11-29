This repo contains a few simple scripts for understanding economic data and portfolio modeling. First, 
 a simple set of scripts on top of [fredr](https://github.com/sboysel/fredr) with plotting 
tools by [ggplot2](https://github.com/tidyverse/ggplot2). Secondly, a set of portfolio modeling functions.

It's aim is to give a sense of the macroeconomy for macro-driven momentum trading strategies.

## Usage: Macroeconomic data

The primary run-book is in the file `economy_momentum.R`. Simply run the script. Users should first 
request a new FRED API key ([instructons](https://fred.stlouisfed.org/docs/api/api_key.html)) and 
then edit their copy of `01_pull_mung_data.R` (line 9) to utilize their own API key.

## Usage: Portfolio Modeling

This section of the code is **much less** developed, but the primary run-book is the file 
`portfolio_modeling.R`.

## To-Do

- Add German economic indicators. See https://www.destatis.de/EN/Home/_node.html
- Complete portfolio modeling scripts for mininum variance portfolio + maximum Sharpe portfolio. Both 
require reasonable expected return vectors $\mu$. 

## References

1. http://www.philosophicaleconomics.com/2016/01/gtt/
2. Ledoit, Olivier, and Michael Wolf. "Honey, I shrunk the sample covariance matrix." The Journal of Portfolio Management 30.4 (2004): 110-119.
