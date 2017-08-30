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

def normalize_weights(log_weights):
    weights = np.exp(log_weights)
    norm = sum(weights)
    return [i/norm for i in weights]

def quadratic(w, x):
    """"Returns mapping of a single x-value to a polynomial with w = [w0,w1,w2]"""
    return w[0]+w[1]*x+w[2]*x**2

def plotQuadratic(w, xrange = [-10, 10]):
    X = np.arange(xrange[0], xrange[1], 0.1)
    Y = [quadratic(w,x) for x in X]
    return (X,Y)

def plot(num_dataset, particles_range):
    """ Plots the contents of the specified file, as described in the README """
    with open("data_" + str(num_dataset) + ".csv") as file:
        true_weights = [float(i) for i in file.readline().rstrip().split(",")]
        X = [float(i) for i in file.readline().rstrip().split(",")]
        Y = [float(i) for i in file.readline().rstrip().split(",")]
    
    true_x, true_y = plotQuadratic(true_weights, [X[0]-1, X[-1]+1])
    fig, ax = plt.subplots(len(particles_range), 3, sharex = True, sharey = True)
    
    #fig.suptitle(algorithm + " on dataset " + str(num_dataset))
    
    for column in range(3):
        for subplot_no in range(len(particles_range)):
            num_particles = particles_range[subplot_no]
            current_ax = ax[subplot_no,column]
            current_ax.plot(true_x, true_y, 'k')
            current_ax.plot(X, Y, 'k*')
            
            log_weights, w0, w1, w2 = [], [], [], []
            with open(["csis", "smc", "is"][column] + "_" + str(num_dataset) + "_" + str(num_particles) + ".csv") as predictions:
                for line in predictions:
                    line = [float(i) for i in line.split(",")]
                    log_weights.append(line[0])
                    w0.append(line[1])
                    w1.append(line[2])
                    w2.append(line[3])
            weights = normalize_weights(log_weights)
            mean_weights = [empirical_mean(i, weights) for i in [w0, w1, w2]]
            mean_x, mean_y = plotQuadratic(mean_weights, [X[0]-1, X[-1]+1])
            current_ax.set_xlim(X[0]-1, X[-1]+1)
            current_ax.set_ylim(min(Y)*1.1-max(Y)*0.1, max(Y)*1.1-min(Y)*0.1)
            #current_ax.plot(mean_x, mean_y, 'b--')
            for particle in range(len(w0)):
                particle_x, particle_y = plotQuadratic([w0[particle],w1[particle],w2[particle]], [X[0]-1, X[-1]+1])
                current_ax.plot(particle_x, particle_y, 'b', alpha=min(1, 5*weights[particle]))
    
    for i in range(len(particles_range)):
        ax[i, 0].set_ylabel(str(particles_range[i]) + " particles")
    for i in range(3):
        ax[0,i].set_title(["CSIS", "SMC", "Importance"][i])
    
    fig.tight_layout()
    filename = "inference_" + str(num_dataset) + "_" + "_".join([str(i) for i in particles_range]) + ".pdf"
    fig.savefig(filename, bbox_inches='tight')
    
def plotError(num_dataset, particles_range):
    with open("data_" + str(num_dataset) + ".csv") as file:
        true_weights = [float(i) for i in file.readline().rstrip().split(",")]
    errors = {}
    for algorithm in ["csis", "smc", "is"]:
        errors[algorithm] = []
        for num_particles in particles_range:
            log_weights, w0, w1, w2 = [], [], [], []
            with open(algorithm + "_" + str(num_dataset) + "_" + str(num_particles) + ".csv") as predictions:
                for line in predictions:
                    line = [float(i) for i in line.split(",")]
                    log_weights.append(line[0])
                    w0.append(line[1])
                    w1.append(line[2])
                    w2.append(line[3])
            weights = normalize_weights(log_weights)
            predicted_weights = np.array([empirical_mean(i, weights) for i in [w0, w1, w2]])
            errors[algorithm].append(np.linalg.norm(predicted_weights - true_weights))
    for algorithm in errors:
        plt.semilogx(particles_range, errors[algorithm], label = algorithm)
        plt.xlabel("Number of Particles")
        plt.ylabel("Error of Empirical Mean of Weights")
    plt.legend()
    plt.savefig("l2_" + str(num_dataset) + ".pdf")

if __name__ == "__main__":
    plotError(3, [10,20,40,80,160,320,640,1280,2560])
    #for dataset in [1,2,3]:
    #    for particles_range in [[10,20,40],[80,160,320],[640,1280,2560]]:
    #        plot(dataset, particles_range)