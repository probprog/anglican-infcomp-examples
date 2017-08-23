# State space model

- `model.csv` contains the model specification in the form of

```
<initial_mean>,<initial_variance>,<transition_multiplier>,<transition_offset>,<transition_variance>,<emission_multiplier>,<emission_offset>,<emission_variance>
```

- `data_{dataset_num}.csv` contains an observation sequence in the form

```
<observation_1>,<observation_2>,...,<observation_{num_timesteps}>
```

- `{algorithm}_{dataset_num}_{num_particles}.csv` contains inference result in the form of `{num_particles}` lines where `{algorithm}` is one of `is, smc, csis`:

```
<unnormalized_log_weight>,<latent_1>,...,<latent_{num_timesteps}>
...
<unnormalized_log_weight>,<latent_1>,...,<latent_{num_timesteps}>
```

- `inference_{dataset_num}_{num_particles}.pdf` contains the plots of posterior state space for different inference algorithms

- `kl_{dataset_num}.pdf` contains the plots of KL versus number of particles
