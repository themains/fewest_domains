## Cheap Precision: What's the Fewest Items You Can Select

Passively observed browsing data is a powerful tool for social science. Working with it means learning how to infer some characteristics of the URLs that people visit. Measurement of some of those characteristics is expensive and it is useful to think about the following question: what is the fewest number of domains (and identify which domains) you need to get inference on so that you can measure some person level metric, e.g., proportion of time spent on domains with adult content, can be answered with a standard error equal or less than some specified number. The setup has the following characteristics: large skew in visitation, large overlap across people in sites that they commonly visit, and a large idiosyncratic tail per person with little overlap.

More generally, we want to solve the following optimization problem:

Let $i$ iterate over respondents 1 to $n$, let $j$ iterate over domains that range from 1 to m, let $c_{ij}$ denote number of visits by respondent $i$ to domain $j$, let $t_j$ denote an unmeasured trait of domain $j$, with $t$ being a boolean, let $d_i$ denote the total number of selected domains for respondent $i$, and $D_i$ as the list of domains selected, and $D$ as the set of all domains selected across respondents.

minimize $d$ s.t. $ \sigma_i <= s$ for each $i$ where $s$ is some constant.

$\sigma_i = \sqrt \frac{(p_i (1 - p_i))}{d_i}$

where $p_i = \frac{\Sigma_j c_{ij}*(t_{ij})}{\Sigma_j c_{ij}}$

Since the latent trait, $t_j$ of each domain is unknown, we have to solve for the worst case scenario: $p_i = .5$. This leaves the two unknowns as $d_i$ and $D_i$, which jointly tell us about the quantities of interest $d$ and $D$. Technically, $d$ is an attribute of $D$ and hence needn't be seen as a separate unknown. We assume that the only way to get to $d$ is to find $D$.

### Solution

The computationally expensive strategy to identify the smallest $D$ is as follows: do a weighted random sample per person that is large enough to get a s.e. less than or equal to $s$ and create a set $D$. Re-randomize till you find the smallest $d$ or till the number of iterations reaches a particular number. 

One problem with weighted sampling when there is a sharp skew in the data is that the s.e. can be large. The intuition is as follows: much will depend on selecting a few very heavily frequently visited domains. One way to solve for that is to deterministically sample the heavily visited domains and then take a smaller sample from the longer tail.
