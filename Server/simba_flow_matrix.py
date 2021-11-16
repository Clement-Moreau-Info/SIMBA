import pandas as pd


def od_matrix(path_seq_file):
    df_mk = pd.read_csv(path_seq_file, sep=";", header=0, keep_default_na=False)
    max_seq = max(df_mk["Id_seq"]) + 1

    labels = df_mk.iloc[:, 3].drop_duplicates().values.tolist()
    labels.remove('')

    print(labels)

    dict_label = {x: i for i, x in enumerate(labels)}

    seq_mk = [[x for x in df_mk[df_mk["Id_seq"] == i].iloc[:, 3].values.tolist()]
              for i in range(1, max_seq)]

    dict_flow = {x: [0]*len(labels) for x in labels}

    for l in seq_mk:
        l = list(filter(lambda a: a != '', l))
        if len(l) > 1:
            for i, x in enumerate(l[:-1]):
                dict_flow[x][dict_label[l[i+1]]] += 1
    pd.DataFrame.from_dict(dict_flow).to_csv("./Data/flow.csv", index=False, sep=";")
    return 0


def main():

    path_seq_file = ""
    #od_matrix(path_seq_file)


if __name__ == '__main__':
    main()
