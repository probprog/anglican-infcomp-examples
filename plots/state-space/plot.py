import argparse
import matplotlib.pyplot as plt
import numpy as np
import pykalman
import scipy.misc
import scipy.stats


def empirical_expectation(particle_values, normalized_weights, f):
    """Returns empirical expectation.

    input:
        particle_values: np.ndarray [num_particles, dim_1, ..., dim_N]
        normalized_weights: np.ndarray [num_particles]
        f: function that takes np.ndarray [dim_1, ..., dim_N] and returns
            np.ndarray [out_dim_1, ..., out_dim_M]

    output: np.ndarray [out_dim_1, ..., out_dim_M]
    """

    result = 0
    for p in range(len(normalized_weights)):
        f_temp = f(particle_values[p])
        result += normalized_weights[p] * f_temp

    return result


def empirical_mean(particle_values, normalized_weights):
    """Returns empirical mean.

    input:
        particle_values: np.ndarray [num_particles, dim_1, ..., dim_N]
        normalized_weights: np.ndarray [num_particles]

    output: np.ndarray [dim_1, ..., dim_N]
    """

    return empirical_expectation(
        particle_values, normalized_weights, lambda x: x
    )


def empirical_variance(particle_values, normalized_weights):
    """Returns empirical variance.

    input:
        particle_values: np.ndarray [num_particles, dim_1, ..., dim_N]
        normalized_weights: np.ndarray [num_particles]

    output: np.ndarray [dim_1, ..., dim_N]
    """

    return empirical_expectation(
        particle_values, normalized_weights, lambda x: x**2
    ) - empirical_mean(particle_values, normalized_weights)**2


def get_sum_kl1(means_1, variances_1, means_2, variances_2, epsilon=1e-10):
    return np.sum(
        0.5 * (np.log(variances_2 + epsilon) - np.log(variances_1 + epsilon)) +
        (variances_1 + (means_1 - means_2)**2) / (2 * variances_2 + epsilon) -
        0.5
    )


def get_sum_kl2(
    particle_values, normalized_weights, means, variances, epsilon=1e-10
):
    num_timesteps = len(means)
    result = 0
    for time in range(num_timesteps):
        result += np.sum(normalized_weights * (
            np.log(normalized_weights + epsilon) -
            scipy.stats.norm.logpdf(
                particle_values[:, time],
                loc=means[time],
                scale=np.sqrt(variances[time])
            )
        ))

    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--dataset-num-list', nargs='+', type=int,
                        help='space separated list of dataset numbers')
    parser.add_argument('--num-particles-list', nargs='+', type=int,
                        help='space separated list of number of particles')
    parser.add_argument('--algorithms', nargs='+', type=str,
                        help='space separated list of algorithms')
    args = parser.parse_args()

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

    true_state_means = {}
    true_state_variances = {}
    particle_values = {}
    normalized_weights = {}
    posterior_means = {}
    posterior_variances = {}
    data = {}
    for dataset_num in args.dataset_num_list:
        data[dataset_num] = np.genfromtxt(
            'data_{}.csv'.format(dataset_num), delimiter=','
        )

        true_state_means[dataset_num], true_state_variances[dataset_num] = \
            kf.smooth(data[dataset_num])
        true_state_means[dataset_num] = \
            np.reshape(true_state_means[dataset_num], (-1))
        true_state_variances[dataset_num] = \
            np.reshape(true_state_variances[dataset_num], (-1))

        particle_values[dataset_num] = {}
        normalized_weights[dataset_num] = {}
        posterior_means[dataset_num] = {}
        posterior_variances[dataset_num] = {}

        for num_particles in args.num_particles_list:
            particle_values[dataset_num][num_particles] = {}
            normalized_weights[dataset_num][num_particles] = {}
            posterior_means[dataset_num][num_particles] = {}
            posterior_variances[dataset_num][num_particles] = {}

            for algorithm in args.algorithms:
                inference_result = np.genfromtxt('{}_{}_{}.csv'.format(
                    algorithm, dataset_num, num_particles
                ), delimiter=',')
                if len(np.shape(inference_result)) == 1:
                    inference_result = np.reshape(inference_result, (1, -1))

                unnormalized_log_weights = inference_result[:, 0]
                normalized_log_weights = unnormalized_log_weights - \
                    scipy.misc.logsumexp(unnormalized_log_weights)
                normalized_weights[dataset_num][num_particles][algorithm] = \
                    np.exp(normalized_log_weights)

                particle_values[dataset_num][num_particles][algorithm] = \
                    inference_result[:, 1:]

                posterior_means[dataset_num][num_particles][algorithm] = \
                    empirical_mean(
                        particle_values[dataset_num][num_particles][algorithm],
                        normalized_weights[dataset_num][num_particles][
                            algorithm]
                )
                posterior_variances[dataset_num][num_particles][algorithm] = \
                    empirical_variance(
                        particle_values[dataset_num][num_particles][algorithm],
                        normalized_weights[dataset_num][num_particles][
                            algorithm]
                )

    # Plot inference_{dataset_num}_{num_particles}.pdf
    for dataset_num in args.dataset_num_list:
        for num_particles in args.num_particles_list:
            fig, ax = plt.subplots(1, 1)
            fig.suptitle('{} particle{}\nDataset {}'.format(
                num_particles,
                '' if num_particles == 1 else 's',
                dataset_num
            ), fontsize=14)

            ax.plot(data[dataset_num], label='observations', color='black')

            temp = ax.plot(
                true_state_means[dataset_num],
                linewidth=1,
                label='ground truth'
            )
            ax.fill_between(
                range(len(data[dataset_num])),
                true_state_means[dataset_num] -
                np.sqrt(true_state_variances[dataset_num]),
                true_state_means[dataset_num] +
                np.sqrt(true_state_variances[dataset_num]),
                alpha=0.15,
                color=temp[0].get_color()
            )
            for algorithm in args.algorithms:
                temp = ax.plot(
                    posterior_means[dataset_num][num_particles][algorithm],
                    linewidth=1,
                    label=algorithm
                )
                ax.fill_between(
                    range(len(data[dataset_num])),
                    posterior_means[dataset_num][num_particles][algorithm] -
                    np.sqrt(posterior_variances
                            [dataset_num][num_particles][algorithm]),
                    posterior_means[dataset_num][num_particles][algorithm] +
                    np.sqrt(posterior_variances
                            [dataset_num][num_particles][algorithm]),
                    alpha=0.15,
                    color=temp[0].get_color()
                )

            ax.legend()
            ax.set_xlabel('Time')
            ax.set_ylabel('Posterior means $\pm$ posterior standard deviation')
            ax.set_xlim(0, len(data[dataset_num]) - 1)

            fig.tight_layout(rect=[0, 0, 1, 0.93])

            filename = 'inference_{0}_{1}.pdf'.format(
                dataset_num, num_particles
            )
            fig.savefig(filename, bbox_inches='tight')
            plt.close(fig)
            print('\nPlot saved to {}'.format(filename))

    # Plot meanvarl2_{dataset_num}_{num_particles}.pdf
    for dataset_num in args.dataset_num_list:
        fig, axs = plt.subplots(2, 2)
        fig.suptitle('L2 between state means and variances\nDataset {}'.format(
            dataset_num
        ), fontsize=14)

        for algorithm in args.algorithms:
            mean_l2 = []
            variance_l2 = []
            for num_particles in args.num_particles_list:
                mean_l2.append(np.sqrt(np.sum((
                    true_state_means[dataset_num] -
                    posterior_means[dataset_num][num_particles][algorithm]
                )**2)))
                variance_l2.append(np.sqrt(np.sum((
                    true_state_variances[dataset_num] -
                    posterior_variances[dataset_num][num_particles][algorithm]
                )**2)))

            axs[0][0].plot(args.num_particles_list, mean_l2, label=algorithm)
            axs[0][1].loglog(args.num_particles_list, mean_l2, label=algorithm)
            axs[1][0].plot(
                args.num_particles_list, variance_l2, label=algorithm
            )
            axs[1][1].loglog(
                args.num_particles_list, variance_l2, label=algorithm
            )

        axs[0][0].set_xticks([])
        axs[0][1].set_xticks([])
        axs[0][0].set_ylabel('$L_2(\mu_{1:T}, \hat \mu_{1:T})$')
        axs[1][1].legend()
        axs[1][0].set_xlabel('Number of particles')
        axs[1][1].set_xlabel('Number of particles')
        axs[1][0].set_ylabel('$L_2(\sigma_{1:T}^2, \hat \sigma_{1:T}^2)$')

        filename = 'meanvarl2_{}.pdf'.format(dataset_num)

        fig.tight_layout(rect=[0, 0, 1, 0.92])
        fig.savefig(filename, bbox_inches='tight')
        plt.close(fig)
        print('\nPlot saved to {}'.format(filename))

    # # Plot kl1_{dataset_num}_{num_particles}.pdf
    # for dataset_num in args.dataset_num_list:
    #     fig, ax = plt.subplots(1, 1)
    #     fig.suptitle('Sum of KL divergences (1)\nDataset {}'.format(
    #         dataset_num
    #     ), fontsize=14)
    #     for algorithm in args.algorithms:
    #         kl = []
    #         for num_particles in args.num_particles_list:
    #             kl.append(get_sum_kl1(
    #                 true_state_means[dataset_num],
    #                 true_state_variances[dataset_num],
    #                 posterior_means[dataset_num][num_particles][algorithm],
    #                 posterior_variances[dataset_num][num_particles][algorithm]
    #             ))
    #
    #         ax.plot(args.num_particles_list, kl, label=algorithm)
    #
    #     ax.legend()
    #     ax.set_xlabel('Number of particles')
    #     ax.set_ylabel('$\sum_{t = 1}^T KL\\left(p(x_t | y_{1:T})\,||\,\mathrm{Normal}(\hat \mu_t, \hat \sigma_t^2)\\right)$')
    #     filename = 'kl1_{}.pdf'.format(dataset_num)
    #     fig.savefig(filename, bbox_inches='tight')
    #     print('\nPlot saved to {}'.format(filename))

    # Plot kl2_{dataset_num}_{num_particles}.pdf and
    # kl2log_{dataset_num}_{num_particles}.pdf
    for dataset_num in args.dataset_num_list:
        fig, ax = plt.subplots(1, 1)
        fig.suptitle('Sum of KL divergences (2)\nDataset {}'.format(
            dataset_num
        ), fontsize=14)

        for algorithm in args.algorithms:
            kl = []
            for num_particles in args.num_particles_list:
                kl.append(get_sum_kl2(
                    particle_values[dataset_num][num_particles][algorithm],
                    normalized_weights[dataset_num][num_particles][algorithm],
                    true_state_means[dataset_num],
                    true_state_variances[dataset_num]
                ))

            ax.plot(args.num_particles_list, kl, label=algorithm)

        ax.legend()
        ax.set_xlabel('Number of particles')
        ax.set_xlabel('Number of particles')
        ax.set_ylabel('$\sum_{t = 1}^T KL\\left(\sum_{k = 1}^K w^k \delta_{x_t^k}(x_t) \,||\, p(x_t | y_{1:T})\\right)$')

        filename = 'kl2_{}.pdf'.format(dataset_num)
        fig.tight_layout(rect=[0, 0, 1, 0.92])
        fig.savefig(filename, bbox_inches='tight')
        plt.close(fig)
        print('\nPlot saved to {}'.format(filename))


if __name__ == '__main__':
    main()
