import numpy as np


def get_data(file_name):
    input_file = open(file_name, 'r').read().splitlines()

    party = 0
    name = 1
    constituency = 2
    constituency_vote = 3
    constituency_percent = 4
    vote_1 = 5
    vote_2 = 6
    vote_3 = 7
    vote_4 = 8
    vote_5 = 9
    vote_6 = 10
    vote_7 = 11
    vote_8 = 12

    number_mps = 649

    data = [[None for i in range(13)] for j in range(number_mps)]
    current_row = -1

    for i in range(len(input_file)):
        line = input_file[i]
        if '<div class="int-cell int-cell--party int-color--' in line:
            data[current_row][party] = input_file[i+1]
        elif line == '<div class="int-cell int-cell--name">':
            data[current_row][name] = input_file[i+1]
        elif line == '<div class="int-cell int-cell--const">':
            data[current_row][constituency] = input_file[i+1].replace(", ", " - ")
            constit_vote = input_file[i+3]
            constit_vote = constit_vote.split(">")[1].split(" ")
            if constit_vote[0] != '--':
                data[current_row][constituency_vote] = constit_vote[0]
                data[current_row][constituency_percent] = int(constit_vote[1][:-1])
        elif line == '<div class="gv-vote-history">':
            data[current_row][vote_1] = get_vote(input_file[i+1])
            data[current_row][vote_2] = get_vote(input_file[i+4])
            data[current_row][vote_3] = get_vote(input_file[i+7])
            data[current_row][vote_4] = get_vote(input_file[i+10])
            data[current_row][vote_5] = get_vote(input_file[i+13])
            data[current_row][vote_6] = get_vote(input_file[i+16])
            data[current_row][vote_7] = get_vote(input_file[i+19])
            data[current_row][vote_8] = get_vote(input_file[i+22])
        elif line == '<div class="int-row int-row--mp" style="cursor:auto">':
            current_row += 1

    return data


def get_data2(file_name):
    input_file = open(file_name, 'r').read().splitlines()

    party = 0
    name = 1
    constituency = 2
    constituency_vote = 3
    constituency_percent = 4
    vote_1 = 5
    vote_2 = 6
    vote_3 = 7
    vote_4 = 8

    number_vars = 9

    number_mps = 649

    data = [[None for i in range(number_vars)] for j in range(number_mps)]
    current_row = -1

    for i in range(len(input_file)):
        line = input_file[i]
        if '<div class="int-cell int-cell--party int-color--' in line:
            data[current_row][party] = input_file[i+1]
        elif line == '<div class="int-cell int-cell--name">':
            data[current_row][name] = input_file[i+1]
        elif line == '<div class="int-cell int-cell--const">':
            data[current_row][constituency] = input_file[i+1].replace(", ", " - ")
            constit_vote = input_file[i+3]
            constit_vote = constit_vote.split(">")[1].split(" ")
            if constit_vote[0] != '--':
                data[current_row][constituency_vote] = constit_vote[0]
                data[current_row][constituency_percent] = int(constit_vote[1][:-1])
        elif line == '<div class="gv-vote-history">':
            data[current_row][vote_1] = get_vote(input_file[i+1])
            data[current_row][vote_2] = get_vote(input_file[i+4])
            data[current_row][vote_3] = get_vote(input_file[i+7])
            data[current_row][vote_4] = get_vote(input_file[i+10])
        elif '<div class="int-row int-row--mp" style="cursor:auto">' in line:
            current_row += 1

    return data



def get_vote(line):
    if "for" in line:
        return "For"
    elif "did-not-vote" in line:
        return "Abstain"
    else:
        return "Against"


columns = ["Party", "Name", "Constituency", "ConstituencyVote", "Constituency%",
           "Vote1", "Vote2", "Vote3", "Vote4", "Vote5", "Vote6", "Vote7", "Vote8"]
columns2 = ["Party", "Name", "Constituency", "ConstituencyVote", "Constituency%",
           "Vote1", "Vote2", "Vote3", "Vote4"]
data = get_data("MP vote HTML.txt")


np_data = np.array(data)
np_data = np.insert(np_data, 0, np.array(columns), 0)
np.savetxt("raw_data.csv", np_data, fmt='%s', delimiter=",")

data2 = get_data2("MP Second Vote HTML.txt")
np_data2 = np.array(data2)
np_data2 = np.insert(np_data2, 0, np.array(columns2), 0)
np.savetxt("raw_data2.csv", np_data2, fmt='%s', delimiter=",")