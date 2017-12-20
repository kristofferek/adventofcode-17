def day5b():
    with open("inputs/5.txt") as file:
        nums = [int(line.strip()) for line in file]
    count = 0
    pos = 0
    length = len(nums)

    while (pos>=0 and pos<length):
        oldPos = pos
        value = nums[oldPos]
        pos += value

        if value < 3: nums[oldPos] = value + 1
        else: nums[oldPos] = value - 1
        count += 1

    print count

#day5b()

def day6(arr):
    prevLists = []
    count = 0

    while arr not in prevLists:
        prevLists.append(list(arr))
        value = max(arr)
        maxIndex = arr.index(value)
        arr[maxIndex] = 0
        i = maxIndex+1

        while value > 0:
            if i >= len(arr):
                i=0
            arr[i] = arr[i]+1
            i=i+1
            value = value - 1

        count = count + 1

    first = prevLists.index(arr)
    return count,(len(prevLists)-first)

inputL = [2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14]
#ans6a, ans6b = day6(inputL)
#print ans6a
#print ans6b

def day7():
    ans = 0
    children = dict()
    values = dict()
    all_kids = set()
    total = set()
    with open("inputs/7b.txt") as file:
        lines = [line for line in file]
    for line in lines:
        a = list(line.replace(',','').strip().split())
        val = a[0]
        values[val] = int(a[1].replace('(','').replace(')',''))
        kids = a[3:]
        children[val] = kids
        total.add(val)
        for kid in kids:
            all_kids.add(kid)
    ans = (total - all_kids).pop()
    print ans

    def calc_kids_weights(root):
        kid_weights = []
        for kid in children[root]:
            kid_weights.append(calc_weight(kid))
        return kid_weights


    def check_bal(root):
        if children[root] == []:
            return True
        kid_weights = calc_kids_weights(root)
        return len(set(kid_weights)) == 1

    def unbalanced_kid(root):
        kid_weights = calc_kids_weights(root)
        for kid in children[root]:
            curr_weight = calc_weight(kid)
            if kid_weights.count(curr_weight) == 1:
                return kid

    def calc_weight(root):
        tot = values[root]
        for kid in children[root]:
            tot += calc_weight(kid)
        return tot

    ans_parent = ans
    while not check_bal(ans):
        ans_parent = ans
        ans = unbalanced_kid(ans)
    another_kid = children[ans_parent][0]
    if another_kid == ans:
        another_kid = children[ans_parent][1]
    print values[ans] - calc_weight(ans) + calc_weight(another_kid)

#day7()

def day8():
    with open("inputs/8.txt") as file:
        lines = [line.strip().split() for line in file]

    regs = set()
    for line in lines:
        regs.add(line[0])

    values = []
    for reg in regs:
        values.append(0)

    def reg_index(regName):
        for index,reg in enumerate(regs):
            if reg == regName:
                return index

    def calc_expr(vL, operand, vR):
        result = {
            '<': vL<vR,
            '>': vL>vR,
            '==': vL==vR,
            '<=': vL<=vR,
            '>=': vL>=vR,
            '!=': vL!=vR
        }[operand]
        return result

    highs = set()
    for line in lines:
        leftVal = values[reg_index(line[4])]
        if calc_expr(leftVal, line[5], int(line[6])):
            updateVal = int(line[2])
            reg_to_update = reg_index(line[0])
            if line[1] == 'inc':
                values[reg_to_update] += updateVal
            if line[1] == 'dec':
                values[reg_to_update] -= updateVal
            highs.add(max(values))

    print 'a:', max(values)
    print 'b:', max(highs)

#day8()

def day10a():
    with open('inputs/10.txt') as f:
        for line in f:
            lengths = line.strip().split(',')

    seq = range(0,256)
    skip_size = 0
    current = 0
    for elem in lengths:
        l = int(elem)
        seq = reverse_sublist(seq, current, (current+l))
        current += l+skip_size
        current = current % (len(seq))
        skip_size += 1
    print seq[0]*seq[1]

#day10a()

import operator as op

def day10b():
    with open('inputs/10.txt') as f:
        for line in f:
            lengths = []
            for char in line.strip():
                lengths.append(ord(char))
            lengths += [17, 31, 73, 47, 23]

    seq = range(0,256)
    skip_size = 0
    current = 0
    for it in range(0,64):
        for elem in lengths:
            l = int(elem)
            seq = reverse_sublist(seq, current, (current+l))
            current += l+skip_size
            current = current % (len(seq))
            skip_size += 1

    ascii_hash = ''
    chunks_of_16 = [seq[i:i + 16] for i in xrange(0, len(seq), 16)]
    for chunk in chunks_of_16:
        xor_res = reduce(op.xor, chunk)
        hex_res = hex(xor_res)[2:]
        if len(hex_res)==1: hex_res = '0'+hex_res
        ascii_hash += hex_res
    print ascii_hash

#day10b()

# Day 10 helper
def reverse_sublist(lst,start,end):
    size = len(lst)
    if end>=size:
        wrapped_rev = (lst[start:size] + lst[0:(end%size)])[::-1]
        lst[start:size] = wrapped_rev[0:(size)-start]
        lst[0:(end%size)] = wrapped_rev[(size)-start:len(wrapped_rev)]
    else:
        lst[start:end] = lst[start:end][::-1]
    return lst

# Solved using 3-D coordinate system
# Illustration: http://keekerdc.com/wp-content/uploads/2011/03/HexGridLandscapeTriCoordinates.gif
def day11():
    with open('inputs/11.txt') as f:
        for line in f:
            dirs = line.strip().split(',')

    my_pos = (0,0,0)
    prev_max_dists = []

    for step in dirs:
        my_pos = {
            'sw': tuple(map(op.add, my_pos, (-1,0,1))),
            'nw': tuple(map(op.add, my_pos, (-1,1,0))),
            'n':  tuple(map(op.add, my_pos, (0,1,-1))),
            'ne': tuple(map(op.add, my_pos, (1,0,-1))),
            'se': tuple(map(op.add, my_pos, (1,-1,0))),
            's':  tuple(map(op.add, my_pos, (0,-1,1)))
        }[step]
        prev_max_dists.append(max(tuple(map(abs, my_pos))))

    print 'a:', max(tuple(map(abs, my_pos)))
    print 'b:', max(prev_max_dists)

#day11()

from collections import deque
# All , are removed from inputs/12.txt
def day12():
    with open("inputs/12.txt") as file:
        connections = [line.strip().replace('<->', '').split() for line in file]

    explored = set()
    nodes_to_explore = deque([])

    def explore_start_node(start, start_children):
        explored.add(int(start))
        map(nodes_to_explore.append, start_children)

        while nodes_to_explore:
            node = int(nodes_to_explore.popleft())
            if not node in explored:
                explored.add(node)
                map(nodes_to_explore.append, connections[node][1:])

    explore_start_node(connections[0][0], connections[0][1:])
    print 'a:', len(explored)

    groups = 1
    for conn in connections:
        if not int(conn[0]) in explored:
            explore_start_node(conn[0], conn[1:])
            groups += 1

    print 'b:', groups

#day12()

def day13():
    with open("inputs/13.txt") as file:
        firewall = [line.strip().split(': ') for line in file]

    result = 0
    for depth in firewall:
        scanner_cycle = (int(depth[1])-1)*2
        if int(depth[0]) % scanner_cycle == 0:
            result += int(depth[0]) * int(depth[1])
    print 'a:', result

    delay = -1
    caught = True
    while caught:
        delay += 1
        caught = False
        for depth in firewall:
            scanner_cycle = (int(depth[1])-1)*2
            if (int(depth[0])+delay) % scanner_cycle == 0:
                caught = True
                break
    print 'b:', delay

day13()
