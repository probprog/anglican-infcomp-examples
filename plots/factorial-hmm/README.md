# [Factorial hidden Markov Model](http://mlg.eng.cam.ac.uk/pub/pdf/GhaJor97a.pdf)

Two state factorial HMM with one-dimensional observations.

- `model.csv` contains the model specification in the form of

```
<initial_probability_1>,...,<initial_probability_{num_features}>
<transition_probability_1_0_0>,<transition_probability_1_0_1>,<transition_probability_1_1_0>,<transition_probability_1_1_1>
...
<transition_probability_{num_features}_0_0>,<transition_probability_{num_features}_0_1>,<transition_probability_{num_features}_1_0>,<transition_probability_{num_features}_1_1>
<emission_1>,...,<emission_{num_features}>
<emission_var>
```

where states can be either 0 or 1 and
- `<initial_probability_i>` is the probability of the initial state being 1,
- `<transition_probability_j_k_l>` is the probability of feature `j` transitioning from state `k` to state `l`,
- the emission distribution is normal distribution with  
  - mean `<emission_1> * state_of_feature_1 + ... + <emission_{num_features}> * state_of_feature_{num_features}` and
  - variance `<emission_var`.

- `data_{dataset_num}.csv` contains an observation sequence in the form

```
<observation_1>,<observation_2>,...,<observation_{num_timesteps}>
```

- `{algorithm}_{dataset_num}_{num_particles}.csv` contains inference result in the form of `{num_particles}` lines where `{algorithm}` is one of `is, smc, csis`:

```
<unnormalized_log_weight>,<state_1_1>,...,<state_1_{num_features}>,<state_2_1>,...,<state_2_{num_features}>,......,<state_{num_timesteps}_1>,...,<state_{num_timesteps}_{num_features}>
...
<unnormalized_log_weight>,<state_1_1>,...,<state_1_{num_features}>,<state_2_1>,...,<state_2_{num_features}>,......,<state_{num_timesteps}_1>,...,<state_{num_timesteps}_{num_features}>
```

- `inference_{dataset_num}_{num_particles}.pdf` contains the plots of posterior state space for different inference algorithms
