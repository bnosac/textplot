## CHANGES IN textplot VERSION 0.2.2

- textplot_dependencyparser.default now gains an argument layout, allowing to change the layout to something else than 'linear'

## CHANGES IN textplot VERSION 0.2.1

- Fix bug in textplot_bitermclusters.default (and plot.BTM as it uses textplot_bitermclusters.default) which 
  unintentionally left the option open that if a biterm which was assigned to several topics it could be shown in one of the 2 topics by chance depending on the order of the biterms data. This could only occur if the most emitted words by each of these 2 topics were the same. See issue #7.

## CHANGES IN textplot VERSION 0.2.0

- Added textplot_embedding_2d

## CHANGES IN textplot VERSION 0.1.4

- Make example conditionally on availability of udpipe

## CHANGES IN textplot VERSION 0.1.3

- Changes regarding running examples only if packages are available (in casu ggforce which depends on concaveman which depends on V8, which triggered these changes)

## CHANGES IN textplot VERSION 0.1.2

- Move igraph package from Imports to Suggests
- Added extra examples in the vignette
- Made functions textplot_bar, textplot_bitermclusters, textplot_cooccurrence, textplot_correlation_lines, textplot_correlation_glasso, textplot_dependencyparser generic
- First argument of function textplot_bitermclusters is now called x instead of terminology

## CHANGES IN textplot VERSION 0.1.1

- Fix NOTE on CRAN indicating the use of UTF-8 encoded text in the data
- Added vignette

## CHANGES IN textplot VERSION 0.1.0

- Initial package
- Added textplot_bar
- Added textplot_bitermclusters
- Added textplot_cooccurrence
- Added textplot_correlation_lines
- Added textplot_correlation_glasso
- Added textplot_dependencyparser
