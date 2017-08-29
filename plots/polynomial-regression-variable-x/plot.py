import argparse
import matplotlib.pyplot as plt
import numpy as np
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

def plot(algorithm, num_dataset, num_particles):
    """ Plots the contents of the specified file, as described in the README """
    filename = "inference_" + algorithm + "_" + str(num_dataset) + "_" + str(num_particles) + ".pdf"
    with open("data_" + str(num_dataset) + ".csv") as file:
        true_weights = map(float, file.readline().rstrip().split(","))
        X = [float(i) for i in file.readline().rstrip().split(",")]
        Y = [float(i) for i in file.readline().rstrip().split(",")]
        
        
    fig, ax = plt.subplots(1, 1)
    fig.suptitle(str(num_particles))
    fig.savefig(filename, bbox_inches='tight')
        
    with open(algorithm + '_' + str(num_dataset) + '_' + str(num_particles) + ".csv") as file:
        pass

if __name__ == "__main__":
    plot("csis", 1, 10)