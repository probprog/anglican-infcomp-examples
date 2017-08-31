# Polynomial Regression (with variable x)

- a model is used where y is distributed by a location-scale student-t with mean w0 + w1*x + w2*x^2, nu = 4 and sigma = 1

- `data_{dataset_num}.csv` and `data_{dataset_num}_test.csv` contain data in the form

```
<w0>,<w1>,<w2>
<x_0>,<x_1>,...,<x_{num_data_points}>
<y_0>,<y_1>,...,<y_{num_data_points}>
```
The weights are the same in the test and training data sets for each dataset_num

- `{algorithm}_{dataset_num}_{num_particles}.csv` contains inference result in the form of `{num_particles}` lines where `{algorithm}` is one of `is, smc, csis`:

```
<unnormalized_log_weight>,<w0>,<w1>,<w2>
...
<unnormalized_log_weight>,<w0>,<w1>,<w2>
```

- `inference_{dataset_num}_{num_particles_1}_{num_particles_2}_{num_particles_3}.pdf` shows plots from weights proposed by each of CSIS, SMC and importance sampling for dataset {dataset_num} for each of the 3 numbers of particles - the opacity of each particle is weighted using the given log-weight

- `test_error_{dataset_num}.pdf` compares the performance of the algorithms by taking the empirical mean of their estimates of the weights and comparing predictions from these with data from the data_{dataset_num}_test.csv datasets