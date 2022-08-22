"""Узнав, что ДНК не является случайной строкой, только что поступившие в Институт биоинформатики студенты группы
информатиков предложили использовать алгоритм сжатия, который сжимает повторяющиеся символы в строке. Кодирование
осуществляется следующим образом: s = 'aaaabbсaa' преобразуется в 'a4b2с1a2', то есть группы одинаковых символов
исходной строки заменяются на этот символ и количество его повторений в этой позиции строки. Напишите программу,
которая считывает строку, кодирует её предложенным алгоритмом и выводит закодированную последовательность на
стандартный вывод. Кодирование должно учитывать регистр символов.

"""
dna = str(input())
count = 0
dnanew = dna[0]
for i in range(len(dna) - 1):
    if dna[i] == dna[i + 1]:
        count += 1
    else:
        dnanew = dnanew + str(count + 1)
        count = 0
        dnanew = dnanew + dna[i + 1]
    i += 1
dnanew = dnanew + str(count + 1)
print(dnanew)