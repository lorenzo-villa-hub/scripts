import matplotlib.pyplot as plt
import numpy as np
import numbers
import sys
import gzip
from collections import defaultdict


class lammpsLog:
    def __init__(self, filename=None,):
        self.n = None
        self.mWA = None
        if filename is not None:
            self.filename = filename
        else:
            raise ValueError('Please provide a filename')
        self.__converged = True
        self.__read()
        self.__create_vectors()

    def __keyError(self, key):
        raise IndexError(
            '"{}" is not a valid key! Valid keys are: {}'.format(
                key, ', '.join(self.parameters)))

    def getVec(self, key, StoT_factor=None, normalize=False):
        if normalize is True:
            div = self.N
        elif normalize is False:
            div = 1
        else:
            raise ValueError(
                '"normalize" can only be "True" or "False".')
        if key in self.parameters:
            return self.dict[key]/div
        elif key.casefold() == 'time'.casefold():
            self.__stepsToTime(StoT_factor)
            return self.dict['Time']
        else:
            self.__keyError(key)

    def getMWA(self, key, n, normalize=False):
        if normalize is True:
            div = self.N
        elif normalize is False:
            div = 1
        else:
            raise ValueError(
                '"normalize" can only be "True" or "False".')
        if (not self.mWA) or (self.n != n):
            self.__movingWindowAverage(n)
        if key in self.parameters:
            return self.mWA[key]/div
        else:
            self.__keyError(key)

    def getN(self):
        return self.N

    def getKeys(self):
        print(', '.join(self.parameters))
        return self.parameters

    def isConverged(self):
        return self.__converged

    def __stepsToTime(self, StoT_factor):
        try:
            self.dict['Time'] = self.dict['Step']*StoT_factor
            self.parameters.append('Time')
        except TypeError:
            raise ValueError('Conversion from "Steps" to "Time" requires '
                             'a valid (numerical) factor. You provided "{}" instead.'
                             .format(StoT_factor))

    def __movingWindowAverage(self, n):
        self.mWA = {}
        for key in self.parameters:
            self.mWA[key] = np.convolve(self.dict[key],
                                        np.ones((n,))/n, mode='valid')

    def __read(self):
        c = []
        N = []
        minimize = False
        with open(self.filename, 'r') as f:
            for i, line in enumerate(f):
                if line[:12] == "Minimization":
                    minimize = True
                    self.__converged = True
                if minimize and (line[2:20] == 'Stopping criterion'):
                    if 'max' in line.split():
                        self.__converged = False
                if (line[0:12] == 'Memory usage') \
                        or (line[0:12] == 'Per MPI rank'):
                    c0 = i+1
                if line[0:4] == 'Loop':
                    c.append([c0, i])
                    N.append(int(line.split()[-2]))
        y = []
        for i, j in enumerate(c):
            y.append(np.genfromtxt(
                self.filename,
                skip_header=j[0],
                names=True,
                max_rows=(j[1]-j[0]-1),
                loose=False,
                unpack=True))
        if len(y) == 1:
            self.N = N[0]
            self.y = y[0]
        else:
            self.N = N
            self.y = y

    def __create_vectors(self):
        # Sometimes minimization in lammps only writes a single line.
        # This line cannot be iterated. This causes the type error
        # Which is basically ignored right now.
        # This should not be a problem, since these lines to not
        # contribute to the overall energy minimization.
        self.dict = defaultdict(list)
        if isinstance(self.y, list):
            self.parameters = list(self.y[0].dtype.names)
            for i in self.y:
                try:
                    for j in i.dtype.names:
                        self.dict[j] += i[j].tolist()
                except TypeError as e:
                    pass
        else:
            self.parameters = list(self.y.dtype.names)
            for j in self.y.dtype.names:
                try:
                    self.dict[j] += self.y[j].tolist()
                except TypeError as e:
                    pass
        for i in self.parameters:
            self.dict[i] = np.asarray(self.dict[i])

        if isinstance(self.N, list):
            if self.N.count(self.N[0]) == len(self.N):
                self.N = self.N[0]
            else:
                raise Warning('Atoms were lost during the simulation!')
