# N-N-BEATS
This repository provides code for the paper "Probabilistic forecasting with modified N-BEATS networks" (IEEE TNNLS, 2024)

## Installation
The ```requirements.txt``` provides the necessary packages.
All code was written for ```python 3.10```.
Weights and Biases ([W&B](https://wandb.com)) is required to log the experiments. 

## Usage
To get the results for DeepAR, run ```DeepAR.ipynb``` for the different seeds as indicated in the notebooks. 

To get the results for ETS, ETS-C1, and ETS-C2, run ``ETS_probabilistic.ipynb```.
To get the results for ETS-C3, run ``ETS_C3.ipynb```.

To get the results for N-N-BEATS-C run the ```N-N-BEATS-C.ipynb``` notebook for the different seeds as indicated in the notebook. 

To get the results for the different setups of N-N-BEATS-S, change the hyperparameters in the ```N-N-BEATS-S.ipynb``` notebook. Note that $\lambda$ in the paper has to be re-calculated to $\lambda$ in the code as follows: $\lambda_{code}=\lambda_{paper}/(\lambda_{paper}+1)$

The results are saved as ```.csv``` files. Save these files for all different setups in folders as indicated in the ```Evaluation.R``` file. Run this file to aggregate the results and generate the plots.


