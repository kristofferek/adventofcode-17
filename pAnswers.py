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

    print max(values)
    print max(highs)

day8()
