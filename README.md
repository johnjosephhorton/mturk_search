# Notes

This is the associated code for ``Task Search in a Human Computation Market.'' 
The paper is available at:

1. My website: [http://john-joseph-horton.com/papers/task_search_in_a_human_computation_market.pdf](http://www.john-joseph-horton.com/papers/sharing.pdf)

## Replication

The repository is set up to make it transparent how the final PDF is constructed from the raw data. 
To replicate, you will need a Linux or Mac OX machine that has the following installed:

1. `R`
1. `pdflatex`
1. `make`
1. `gpg`
1. `curl`
1. `gs` (GhostScript)

To replicate the data analysis, you will need `ggplot2` and `lme4` installed. 
Note that this repository does not contain the actual experimental data, but the `Makefile` will fetch the data for you. 

####Download the repository from github
```
 git clone git@github.com:johnjosephhorton/mturk_search.git 
```
#### Get the data you need
From `/mturk_search`, run: 
```
cd etl
make data
```
### Build the pdf
From `/mtuk_search`, run
```
cd writeup
make search.pdf
```

If you run into any trouble replicating, please contact me at ``john.joseph.horton@gmail.com``. 
