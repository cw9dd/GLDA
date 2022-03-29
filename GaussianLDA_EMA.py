#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun  4 14:09:12 2021

@author: Congyu Peter Wu June 2021
"""

"""
GaussianLDA package intro:
    
The package provides two classes for training Gaussian LDA:
Cholesky only, gaussianlda.GaussianLDATrainer: Simple Gibbs sampler with optional Cholesky decomposition trick.
Cholesky+aliasing, gaussianlda.GaussianLDAAliasTrainer: Cholesky decomposition (not optional) and the Vose aliasing trick.

The trainer is prepared by instantiating the training class:
1. corpus: List of documents, where each document is a list of int IDs of words. These are IDs into the vocabulary and the embeddings matrix.
2. vocab_embeddings: (V, D) Numpy array, where V is the number of words in the vocabulary and D is the dimensionality of the embeddings.
3. vocab: Vocabulary, given as a list of words, whose position corresponds to the indices using in the data. This is not strictly needed for training, but is used to output topics.
4. num_tables: Number of topics to learn.
5. alpha, kappa: Hyperparameters to the doc-topic Dirichlet and the inverse Wishart prior
6. save_path: Path to write the model out to after each iteration.
7. mh_steps (aliasing only): Number of Montecarlo-Hastings steps for each topic sample.

Then you set the sampler running for a specified number of iterations over the training data by calling trainer.sample(num_iters).

"""

import os
import pandas as pd
import numpy as np
import matplotlib
from gaussianlda import GaussianLDAAliasTrainer

os.chdir('/Users/cw29265-admin/Documents/LDA_EMA')
# os.chdir('/Users/cw29265-admin/Documents/UT1000')

data = pd.read_csv('data.csv')
# data = pd.read_csv('data_centered.csv')

vocab = data.word.unique().tolist()

word_to_id = dict(zip(set(vocab), range(len(vocab)))) # https://stackoverflow.com/questions/33919672/convert-list-of-strings-to-list-of-integers

corpus = [list(map(lambda x: word_to_id[x], data[data['pid'] == pid].word.tolist())) for pid in data.pid.unique()] # smart way to create a list of lists without using for loop

embeddings = data.drop(['Unnamed: 0', 'pid', 'time'], axis = 1).drop_duplicates('word').drop('word', axis = 1).to_numpy() # 4996 words across 10 dimensions
# embeddings = data.drop(['Unnamed: 0', 'date', 'datetime', 'pid'], axis = 1).drop_duplicates('word').drop('word', axis = 1).to_numpy() # 4996 words across 10 dimensions

trainer_raw_4 = GaussianLDAAliasTrainer(corpus, embeddings, vocab, 4, 0.1, 0.1)
trainer_raw_5 = GaussianLDAAliasTrainer(corpus, embeddings, vocab, 5, 0.1, 0.1)
trainer_4 = GaussianLDAAliasTrainer(corpus, embeddings, vocab, 4, 0.1, 0.1)
trainer_5 = GaussianLDAAliasTrainer(corpus, embeddings, vocab, 5, 0.1, 0.1)
trainer_6 = GaussianLDAAliasTrainer(corpus, embeddings, vocab, 6, 0.1, 0.1)

trainer_raw_4.sample(1000)
trainer_raw_5.sample(1000)
trainer_5.sample(1000)
trainer_6.sample(1000)

# data['assignment'] = [item for sublist in trainer_5.table_assignments for item in sublist]
data['assignment'] = [item for sublist in trainer_raw_5.table_assignments for item in sublist]

# data.to_csv('data_assignments.csv', index = False)
# data.to_csv('data_centered_assignments_4_new.csv', index = False)
# data.to_csv('data_centered_assignments_5_new.csv', index = False)
data.to_csv('data_raw_assignments_5.csv', index = False)

# pd.DataFrame(trainer.table_means.np, columns = ['irritable', 'angry', 'afraid', 'worried', 'ruminating', 'down', 'hopeless', 'anhedonic', 'avoidact', 'avoidpeople']).to_csv("state_means.csv", index = True)
# pd.DataFrame(trainer_4.table_means.np, columns = ['irritable', 'angry', 'afraid', 'worried', 'ruminating', 'down', 'hopeless', 'anhedonic', 'avoidact', 'avoidpeople']).to_csv("state_centered_means_4_new.csv", index = True)
# pd.DataFrame(trainer_5.table_means.np, columns = ['irritable', 'angry', 'afraid', 'worried', 'ruminating', 'down', 'hopeless', 'anhedonic', 'avoidact', 'avoidpeople']).to_csv("state_centered_means_5_new.csv", index = True)
pd.DataFrame(trainer_raw_5.table_means.np, columns = ['irritable', 'angry', 'afraid', 'worried', 'ruminating', 'down', 'hopeless', 'anhedonic', 'avoidact', 'avoidpeople']).to_csv("state_raw_means_5.csv", index = True)
