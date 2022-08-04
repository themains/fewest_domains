## Cheap Precision: What's the Fewest Items You Can Select

Passively observed browsing data is a powerful tool for social science. Working with it means learning how to infer some characteristics of the URLs that people visit. Measurement of some of those characteristics is expensive and it is useful to think about the following question: what is the least number of domains you need to get inference on so that you can measure some person level metric, e.g., proportion of time spent on domains with adult content, can be answered with a standard error equal or less than some specified number. The setup has the following characteristics: large skew in visitation, large overlap across people in sites that they commonly visit, and a large idiosyncratic tail per person with little overlap. 

More generally, we want to solve the following optimization problem:

Let $i$ iterate over respondents 1 to $n$, let $j$ iterate over domains that range from 1 to m, let $c_{ij}$ denote number of visits by respondent $i$ to domain $j$, $t_j$ as trait of domain $j$ with $t$ being a boolean, and let $d$ denote the number of selected domains. For each respondent, we select $d_i$ domains. Formally,

minimize $d$ s.t. $ \sigma_i <= s$ for each $i$.

where $\sigma_i = \sqrt \frac{(p_i (1 - p_i))}{d_i}$

where $p_i = \frac{\Sigma_j c_{ij}*(t_{ij})}{\Sigma_j c_{ij}}$

$d$ is the set of all the domains we select for each respondent.

Since the state of each domain is unknown, we have to solve for the worst case scenario: $p_i = .5$. 

### Heuristics

1. Strategy that *always* works from a s.e. perspective
	a. weighted random sample per person and then do a set operation on top
	b. deterministic sample of the fat portion per person and then small random sample of the tail

2. Dumb strategies
	* sample again and again and find where d is smallest

3. Change the problem
	* what is the smallest d for the average s.e. <= X
		- 

1. One person
	- weighted random sampling with w proportional to counts
	- skew in the distribution --- our estimated s.e. will be strongly dependent on what is sampled -> skew/variance
		+ one q. is solve
			* deterministically selecting the fat portion and then randomly selecting a small sample from the long tail
			* 1 website is 80% of the visits, 20% -> 100 -> 10% of 20% -> 2%
				- 101 websites -> 2% (1%---3%)
				- estimate s.e. of this estimate is super low as i am deterministically choosing the big fat tail

3. 1000 people
	- overlap
		+ proportion of people who visit a domain
		+ c_j: skew in the count on the i (person) dimension
	
	- filter out domains where c_j <= something

	- min. of domains that we need to measure the latent construct of --- measurement of t is hard