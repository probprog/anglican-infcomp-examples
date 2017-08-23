import matplotlib.pyplot as plt
import numpy as np
import pykalman

np.random.seed(1)


def main():
    model = np.genfromtxt('model.csv', delimiter=',')
    initial_mean, initial_variance, transition_multiplier, transition_offset, \
        transition_variance, emission_multiplier, emission_offset, \
        emission_variance = model

    kf = pykalman.KalmanFilter(
        initial_state_mean=[initial_mean],
        initial_state_covariance=[[initial_variance]],
        transition_matrices=[[transition_multiplier]],
        transition_offsets=[transition_offset],
        transition_covariance=[[transition_variance]],
        observation_matrices=[[emission_multiplier]],
        observation_offsets=[emission_offset],
        observation_covariance=[[emission_variance]]
    )

    num_samples = 100
    num_timesteps = 100
    alpha = 0.2

    latents_list = []
    observations_list = []
    for _ in range(num_samples):
        latents, observations = kf.sample(num_timesteps)
        latents_list.append(np.reshape(latents, (-1)))
        observations_list.append(np.reshape(observations, (-1)))

    fig, axs = plt.subplots(2, 1)

    line1 = axs[0].plot(latents_list[0], label='latents', alpha=alpha)
    line2 = axs[0].plot(observations_list[0], label='observations', alpha=alpha)
    for n in range(1, num_samples):
        axs[0].plot(
            latents_list[n], color=line1[0].get_color(), alpha=alpha
        )
        axs[0].plot(
            observations_list[n], color=line2[0].get_color(), alpha=alpha
        )

    axs[1].plot(latents_list[0], label='latents')
    axs[1].plot(observations_list[0], label='observations')
    axs[1].legend()

    fig.savefig('prior.pdf', bbox_inches='tight')


if __name__ == '__main__':
    main()
