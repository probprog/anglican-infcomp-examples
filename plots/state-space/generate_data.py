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

    num_timesteps_list = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

    amplitude = 4
    period = 20
    noise_variance = 0.1

    fig, axs = plt.subplots(10, 2)
    fig.set_size_inches(8, 20)
    for i, num_timesteps in enumerate(num_timesteps_list):
        _, observations = kf.sample(num_timesteps)
        observations = np.reshape(observations, (-1))
        observations_reshaped = np.reshape(observations, (1, -1))
        np.savetxt(
            'data_{}.csv'.format(i + 1), observations_reshaped, delimiter=','
        )

        axs[i][0].set_title('Data {}'.format(i + 1))
        axs[i][0].plot(observations)

        # sin
        observations = amplitude * \
            np.sin(np.arange(num_timesteps) * 2 * np.pi / period) + \
            np.random.randn(num_timesteps) * np.sqrt(noise_variance)
        observations_reshaped = np.reshape(observations, (1, -1))
        np.savetxt(
            'data_{}.csv'.format(i + 11), observations_reshaped, delimiter=','
        )

        axs[i][1].set_title('Data {}'.format(i + 11))
        axs[i][1].plot(observations)

    fig.tight_layout()
    fig.savefig('data.pdf', bbox_inches='tight')


if __name__ == '__main__':
    main()
