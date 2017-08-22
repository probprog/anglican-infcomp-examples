import argparse
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import hmmlearn.hmm as hmm
import scipy as sp
import seaborn as sns

plt.style.use('seaborn-whitegrid')
plt.style.use('seaborn-colorblind')


def get_posterior(inference_result, num_states):
    unnormalized_log_weights = inference_result[:, 0]
    normalized_log_weights = unnormalized_log_weights - \
        sp.misc.logsumexp(unnormalized_log_weights)
    normalized_weights = np.exp(normalized_log_weights)

    particle_values = inference_result[:, 1:]

    num_timesteps = np.shape(particle_values)[1]

    normalized_weights_duplicated = np.tile(
        np.reshape(normalized_weights, (-1, 1)), reps=(1, num_timesteps)
    )

    posterior = np.zeros((num_states, num_timesteps))
    for state in range(num_states):
        posterior[state] = np.sum(
            normalized_weights_duplicated * (particle_values == state), axis=0
        )

    return posterior


def get_sum_kl(posterior_1, posterior_2, epsilon=1e-10):
    return np.sum(posterior_1 * (np.log(posterior_1 + epsilon) - np.log(posterior_2 + epsilon)))


def get_sum_l2(posterior_1, posterior_2):
    return np.sum(np.sqrt(np.sum((posterior_1 - posterior_2)**2, axis=0)))


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
    num_states = np.shape(model)[1]
    my_hmm = hmm.GaussianHMM(n_components=num_states, covariance_type='full')
    my_hmm.startprob_ = model[0]
    my_hmm.transmat_ = model[1:(num_states + 1)]
    my_hmm.means_ = np.reshape(model[num_states + 1], (-1, 1))
    my_hmm.covars_ = np.reshape(model[num_states + 2], (-1, 1, 1))

    true_posteriors = {}
    posteriors = {}
    data = {}
    for dataset_num in args.dataset_num_list:
        data[dataset_num] = np.genfromtxt(
            'data_{}.csv'.format(dataset_num), delimiter=','
        )
        true_posteriors[dataset_num] = np.transpose(
            my_hmm.predict_proba(np.reshape(data[dataset_num], (-1, 1)))
        )
        posteriors[dataset_num] = {}

        for num_particles in args.num_particles_list:
            posteriors[dataset_num][num_particles] = {}

            for algorithm in args.algorithms:
                inference_result = np.genfromtxt('{}_{}_{}.csv'.format(
                    algorithm, dataset_num, num_particles
                ), delimiter=',')
                if len(np.shape(inference_result)) == 1:
                    inference_result = np.reshape(inference_result, (1, -1))

                posteriors[dataset_num][num_particles][algorithm] = \
                    get_posterior(inference_result, num_states)

    # Plot inference_{dataset_num}_{num_particles}.pdf
    for dataset_num in args.dataset_num_list:
        for num_particles in args.num_particles_list:
            fig, axs = plt.subplots(len(args.algorithms) + 1, 1)
            fig.set_size_inches(8, 10)
            fig.suptitle('{} particle{}'.format(
                num_particles, '' if num_particles == 1 else 's'
            ), fontsize=14)
            temp = axs[0].imshow(
                true_posteriors[dataset_num],
                clim=[0, 1], cmap=matplotlib.cm.Blues
            )
            axs[0].grid(False)
            axs[0].set_ylabel('State')
            axs[0].set_title('Ground Truth', fontsize=12)
            axs[0].set_xticks([])

            for ax, algorithm in zip(axs[1:], args.algorithms):
                temp = ax.imshow(
                    posteriors[dataset_num][num_particles][algorithm],
                    clim=[0, 1], cmap=matplotlib.cm.Blues
                )
                ax.grid(False)
                ax.set_ylabel('State')
                ax.set_title(algorithm.upper(), fontsize=12)
                ax.set_xticks([])

            axs[-1].set_xticks(np.arange(len(data[dataset_num])))
            axs[-1].set_xlabel('Time')
            cbar = fig.colorbar(temp, ax=axs[-1], orientation='horizontal')

            filename = 'inference_{0}_{1}.pdf'.format(
                dataset_num, num_particles
            )
            fig.savefig(filename, bbox_inches='tight')
            print('\nPlot saved to {}'.format(filename))

    # Plot kl_{dataset_num}.pdf
    for dataset_num in args.dataset_num_list:
        fig, ax = plt.subplots(1, 1)
        fig.suptitle('Sum of KL divergences\n{} particle{}'.format(
            num_particles, '' if num_particles == 1 else 's'
        ), fontsize=14)
        kls = {}
        for algorithm in args.algorithms:
            kls[algorithm] = []
            for num_particles in args.num_particles_list:
                kls[algorithm].append(get_sum_kl(
                    true_posteriors[dataset_num],
                    posteriors[dataset_num][num_particles][algorithm]
                ))

            ax.plot(args.num_particles_list, kls[algorithm], label=algorithm)

        ax.legend()
        ax.set_xlabel('Number of particles')
        ax.set_ylabel(
            '$\sum_{t = 1}^T KL(p(x_t | y_{1:T}) || \hat p(x_t | y_{1:T}))$'
        )
        # ax.set_xticks(args.num_particles_list)
        ax.set_xlim(
            np.min(args.num_particles_list), np.max(args.num_particles_list)
        )

        filename = 'kl_{}.pdf'.format(dataset_num)
        fig.savefig(filename, bbox_inches='tight')
        print('\nPlot saved to {}'.format(filename))

    # Plot l2_{dataset_num}.pdf
    for dataset_num in args.dataset_num_list:
        fig, ax = plt.subplots(1, 1)
        fig.suptitle('Sum of L2 norms\n{} particle{}'.format(
            num_particles, '' if num_particles == 1 else 's'
        ), fontsize=14)
        l2s = {}
        for algorithm in args.algorithms:
            l2s[algorithm] = []
            for num_particles in args.num_particles_list:
                l2s[algorithm].append(get_sum_l2(
                    true_posteriors[dataset_num],
                    posteriors[dataset_num][num_particles][algorithm]
                ))

            ax.plot(args.num_particles_list, l2s[algorithm], label=algorithm)

        ax.legend()
        ax.set_xlabel('Number of particles')
        ax.set_ylabel(
            '$\sum_{t = 1}^T L_2(p(x_t | y_{1:T}), \hat p(x_t | y_{1:T}))$'
        )
        # ax.set_xticks(args.num_particles_list)
        ax.set_xlim(
            np.min(args.num_particles_list), np.max(args.num_particles_list)
        )

        filename = 'l2_{}.pdf'.format(dataset_num)
        fig.savefig(filename, bbox_inches='tight')
        print('\nPlot saved to {}'.format(filename))


if __name__ == '__main__':
    main()
