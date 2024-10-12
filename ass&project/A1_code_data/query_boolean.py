import pickle
from string_processing import (
    process_tokens,
    tokenize_text,
)


def intersect_query(doc_list1, doc_list2):
    # TODO: you might like to use a function like this in your run_boolean_query implementation
    # for full marks this should be the O(n + m) intersection algorithm for sorted lists
    # using data structures such as sets or dictionaries in this function will not score full marks
    res=[]
    p1=0
    p2=0
    while p1!= len(doc_list1) and p2!=len(doc_list2):
        if doc_list1[p1][0]==doc_list2[p2][0]:
            res.append(doc_list1[p1])
            p1+=1
            p2+=1
        elif doc_list1[p1][0]<doc_list2[p2][0]:
            p1+=1
        else:
            p2+=1

    return res


def union_query(doc_list1, doc_list2):
    # TODO: you might like to use a function like this in your run_boolean_query implementation
    # for full marks this should be the O(n + m) union algorithm for sorted lists
    # using data structures such as sets or dictionaries in this function will not score full marks
    res=[]
    p1=0
    p2=0
    print(len(doc_list2))
    while p1!= len(doc_list1) and p2!=len(doc_list2):
        if doc_list1[p1][0]==doc_list2[p2][0]:
            res.append(doc_list1[p1])
            p1+=1
            p2+=1
        elif doc_list1[p1][0]<doc_list2[p2][0]:
            res.append(doc_list1[p1])
            p1+=1
        else:
            res.append(doc_list2[p2])
            p2+=1
    while p1< len(doc_list1):
        res.append(doc_list1[p1])
        p1+=1
    while p2<len(doc_list2):
        res.append(doc_list2[p2])
        p2+=1
    return res


def run_boolean_query(query_string, index):
    """Runs a boolean query using the index.

    Args:
        query_string (str): boolean query string
        index (dict(str : list(tuple(int, int)))): The index aka dictionary of posting lists

    Returns:
        list(int): a list of doc_ids which are relevant to the query
    """

    # TODO: implement this function
    query_list=tokenize_text(query_string)
    p_query_list=process_tokens(query_list)
    print(p_query_list)
    relevant_index=index[p_query_list[0]]
    p=1
    q=1
    while p<len(query_list):
        if query_list[p]=="AND":
            relevant_index=intersect_query(relevant_index,index[p_query_list[q]])
            p+=2
            q+=1
        elif query_list[p]=="OR":
            relevant_index=union_query(relevant_index,index[p_query_list[q]])
            p+=2
            q+=1
        else:
            print('wrong input')
            break
    relevant_docs=[id[0] for id in relevant_index]

    return relevant_docs


if __name__ == '__main__':
    # load the stored index
    (index, doc_freq, doc_ids, num_docs) = pickle.load(open("stored_index.pkl", "rb"))

    print("Index length:", len(index))
    if len(index) != 808777:
        print("Warning: the length of the index looks wrong.")
        print("Make sure you are using `process_tokens_original` when you build the index.")
        raise Exception()

    # the list of queries asked for in the assignment text
    queries = [
        "Workbooks",
        "Australasia OR Airbase",
        "Warm AND WELCOMING",
        "Global AND SPACE AND economies",
        "SCIENCE OR technology AND advancement AND PLATFORM",
        "Wireless OR Communication AND channels OR SENSORY AND INTELLIGENCE",
    ]

    # run each of the queries and print the result
    ids_to_doc = {docid: path for (path, docid) in doc_ids.items()}
    i=0
    for query_string in queries:
        print(f'Query{i}:',query_string)
        i+=1
        doc_list = run_boolean_query(query_string, index)
        #print(doc_list)
        res = sorted([ids_to_doc[docid] for docid in doc_list])
        for path in res:
            print(path)

