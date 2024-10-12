import numpy as np
from gensim.models import Word2Vec
from features import get_features_w2v
from classifier import search_C


def search_hyperparams(Xt_train, y_train, Xt_val, y_val):
    """Search the best values of hyper-parameters for Word2Vec as well as the
    regularisation parameter C for logistic regression, using the validation set.

    Args:
        Xt_train, Xt_val (list(list(list(str)))): Lists of (tokenised) documents (each
            represented as a list of tokenised sentences where a sentence is a
            list of tokens) for training and validation, respectively.
        y_train, y_val: Dense vectors (np.ndarray) of class labels for training
            and validation, respectively. Each element of the vector is either
            0 or 1.

    Returns:
        dict(str : union(int, float)): The best values of hyper-parameters.
    """
    # TODO: tune at least two of the many hyper-parameters of Word2Vec 
    #       (e.g. vector_size, window, negative, alpha, epochs, etc.) as well as
    #       the regularisation parameter C for logistic regression
    #       using the validation set.
    best_params = dict()
    params_space=[]
    scores=[]
    vector_space=[5,20,100,200]
    window_space=[1,5,10,50]
    for v in vector_space:
        for w in window_space:
            word_vectors=train_w2v(Xt_train=Xt_train,vector_size=v,window=w)
            X_train=get_features_w2v(Xt_train,word_vectors)
            X_val=get_features_w2v(Xt_val,word_vectors)
            C,sc=search_C(X_train,y_train,X_val,y_val,return_best_acc=True)
            params_space.append([v,w,C])
            scores.append(sc)
    index=scores.index(max(scores))
    best_params['vect_size']=params_space[index][0]
    best_params['window']=params_space[index][1]
    best_params['C']=params_space[index][2]
    assert 'C' in best_params
    print(f'best:{best_params}')
    return best_params


def train_w2v(Xt_train, vector_size=200, window=5, min_count=5, negative=10, epochs=5, seed=101, workers=1,
              compute_loss=False, **kwargs):
    """Train a Word2Vec model.

    Args:
        Xt_train (list(list(list(str)))): A list of (tokenised) documents (each
            represented as a list of tokenised sentences where a sentence is a
            list of tokens).
        See https://radimrehurek.com/gensim/models/word2vec.html#gensim.models.word2vec.Word2Vec
        for descriptions of the other arguments.

    Returns:
        gensim.models.keyedvectors.KeyedVectors: A mapping from words (string) to their embeddings
            (np.ndarray)
    """
    sentences_train = [sent for doc in Xt_train for sent in doc]

    # TODO: train the Word2Vec model
    print(f'Train word2vec using {len(sentences_train):,d} sentences ...')
    w2v_model = Word2Vec(sentences=sentences_train,vector_size=vector_size,window=window,min_count=min_count,negative=negative,epochs=epochs,seed=seed,workers=workers,compute_loss=compute_loss)
    return w2v_model.wv

