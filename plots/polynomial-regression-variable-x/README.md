# Polynomial Regression (with variable x)

- a model is used where y is distributed by a location-scale student-t with mean w0 + w1*x + w2*x^2, nu = 4 and sigma = 1

- `data_{dataset_num}.csv` contains data in the form

```
<w0>,<w1>,<w2>
<x_0>,<x_1>,...,<x_{num_data_points}>
<y_0>,<y_1>,...,<y_{num_data_points}>
```

- `{algorithm}_{dataset_num}_{num_particles}.csv` contains inference result in the form of `{num_particles}` lines where `{algorithm}` is one of `is, smc, csis`:

```
<unnormalized_log_weight>,<latent_1>,...,<latent_{num_timesteps}>
...
<unnormalized_log_weight>,<latent_1>,...,<latent_{num_timesteps}>
```

- `inference_{algorithm}_{dataset_num}_{num_particles}.pdf` shows outputs from weights proposed by {algorithm} for dataset {dataset_num} with {num_particles} particles