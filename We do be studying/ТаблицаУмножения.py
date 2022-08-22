"""Напишите программу, на вход которой даются четыре числа a, b, c и d, каждое в своей строке. Программа должна
вывести фрагмент таблицы умножения для всех чисел отрезка [a; b][a;b] на все числа отрезка [c;d][c;d].
Числа a, b, c и d являются натуральными и не превосходят 10, a<=b, c<=d
Для разделения элементов внутри строки используйте '\t' — символ табуляции.
Заметьте, что левым столбцом и верхней строкой выводятся сами числа из заданных отрезков — заголовочные столбец и
строка таблицы.

"""
a = int(input())
b = int(input())
c = int(input())
d = int(input())
if c != d:
    for i in range(c, d + 1):
        print('\t', i, '\t', end='')
    print()
    for j in range(a, b + 1):
        print(j, end='')
        for i in range(c, d + 1):
            print('\t', i * j, '\t', end='')
        print()
else:
    print('\t', c)
    for j in range(a, b + 1):
        print(j, '\t', j * c, '\t', end='')
        print()
