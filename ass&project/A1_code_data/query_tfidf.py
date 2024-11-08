import math
from collections import defaultdict
from string_processing import (
    process_tokens,
    tokenize_text,
)
from query import (
    get_query_tokens,
    count_query_tokens,
    query_main,
)



def get_doc_to_norm(index, doc_freq, num_docs):
    """Pre-compute the norms for each document vector in the corpus using tfidf.

    Args:
        index (dict(str : list(tuple(int, int)))): The index aka dictionary of posting lists
        doc_freq (dict(str : int)): document frequency for each term
        num_docs (int): number of documents in the corpus

    Returns:
        dict(int: float): a dictionary mapping doc_ids to document norms
    """

    # TODO: Implement this function using tfidf
    # Hint: This function is similar to the get_doc_to_norm function in query.py
    #       but should use tfidf instead of term frequency
    doc_norm=defaultdict(float)
    N=num_docs
    for term in index.keys():
        for (docid,tf) in index[term]:
            doc_norm[docid]+=(tf*math.log(N/(doc_freq[term]+1)))**2
    
    for docid in doc_norm.keys():
        doc_norm[docid] = math.sqrt(doc_norm[docid])

    return doc_norm


def run_query(query_string, index, doc_freq, doc_norm, num_docs):
    """ Run a query on the index and return a sorted list of documents. 
    Sorted by most similar to least similar.
    Documents not returned in the sorted list are assumed to have 0 similarity.

    Args:
        query_string (str): the query string
        index (dict(str : list(tuple(int, int)))): The index aka dictionary of posting lists
        doc_freq (dict(str : int)): document frequency for each term
        doc_norm (dict(int : float)): a map from doc_ids to pre-computed document norms
        num_docs (int): number of documents in the corpus

    Returns:
        list(tuple(int, float)): a list of document ids and the similarity scores with the query
        sorted so that the most similar documents to the query are at the top.
    """

    # TODO: Implement this function using tfidf
    # Hint: This function is similar to the run_query function in query.py
    #       but should use tfidf instead of term frequency
    N=num_docs
    qt=get_query_tokens(query_string)
    query_token_counts=count_query_tokens(qt)
    
    #caculate query-norm & cos-similarity socres
    query_norm=0
    dxq_inner_pdt=defaultdict(float)
    doc_tfidf_scores=[]
    for (term,qtf) in query_token_counts:
        if term not in index: 
            continue
        query_norm+=(qtf*math.log(N/(doc_freq[term]+1)))**2
        for (docid,tf) in index[term]:
            dxq_inner_pdt[docid]+=qtf*tf*(math.log(N/(doc_freq[term]+1))**2)
    query_norm=math.sqrt(query_norm)
    for docid in dxq_inner_pdt.keys():
        doc_tfidf_scores.append((docid,dxq_inner_pdt[docid]/(query_norm*doc_norm[docid])))
    sorted_docs=sorted(doc_tfidf_scores,key=lambda x :x[1],reverse=True)

    return sorted_docs


if __name__ == '__main__':
    queries = [
        'Is nuclear power plant eco-friendly?',
        'How to stay safe during severe weather?',
    ]
    query_main(queries=queries, query_func=run_query, doc_norm_func=get_doc_to_norm)
    
