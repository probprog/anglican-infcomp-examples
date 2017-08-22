# Hidden markov model

- `model.csv` contains the model specification in the form of

```
<initial_probability_1>,...,<initial_probability_{num_states}>
<transition_probability_1_1>,...,<transition_probability_1_{num_states}>
...
<transition_probability_{num_states}_1>,...,<transition_probability_{num_states}_{num_states}>
<emission_mean_1>,...,<emission_mean_{num_states}>
<emission_variance_1>,...,<emission_variance_{num_states}>
```
where `<transition_probability_j_k>` is the probability of transitioning from state `j` to state `k`.

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

- `l2_{dataset_num}.pdf` contains the plots of L2 error
