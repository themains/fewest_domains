## Cheap Precision: What's the Fewest Items You Can Select

Passively observed browsing data is a powerful tool for social science. Working with it means learning how to infer some characteristics of the URLs that people visit. Measurement of some of those characteristics is expensive and it is useful to think about the following question: what is the least number of domains you need to get inference on so that you can measure some person level metric, e.g., proportion of time spent on domains with adult content, can be answered with a standard error equal or less than some specified number. The setup has the following characteristics: large skew in visitation, large overlap across people in sites that they commonly visit, and a large idiosyncratic tail per person with little overlap. 

More generally, we want to solve the following optimization problem:

Let $i$ iterate over respondents 1 to $n$, let $j$ iterate over domains that range from 1 to m, let $c_{ij}$ denote number of visits by respondent $i$ to domain $j$, $t_j$ as trait of domain $j$ with $t$ being a boolean, and let $d$ denote the number of selected domains. For each respondent, we select $d_i$ domains. Formally,

minimize $d$ s.t. $ \sigma_i <= s$ for each $i$.

where $\sigma_i = \sqrt \frac{(p_i (1 - p_i)}{d_i}$

where $p_i = \frac{\Sigma_j c_{ij}*(t_i == 1)}{\Sigma_j c_{ij}}$

d is the set of all $d_i$
