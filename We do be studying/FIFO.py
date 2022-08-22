def fifo(n, reference_list):
    memory = [-1 for i in range(n)]
    i = 0
    while reference_list:
        page = reference_list.pop(0)
        if page not in memory:
            memory[i % n] = page
            i += 1
            print(memory)
    return memory


print(fifo(4, [1, 2, 3, 3, 4, 5, 1]))