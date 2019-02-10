#!/usr/bin/env python3

from string import ascii_lowercase, digits
from copy import deepcopy

# Program działa następująco
# Poprawność danej funkcji jest sprawdzana funkcją check
# Z wejściowego stringa budowane jest drzewo z funkcją build_prefix
# Funkcja reduce_tree próbuje zredukować drzewo do mniejszego przy pomocy danych regół ( rules )
# Na wejściowym drzewie ( przed redukcja ) jest wykonywana metodą Quine-McCluskeya
# Wyjściowe drzewo jest ponownie poddawane redukcji z reduce_tree
# Najmniejsze drzewo otrzymane kiedykolwiek jest wypisywane na wyjściu po zamianie na string funkcją tree_to_string


all_ops = "~>&|/^"
uops = ["~"]
bops = [">", "&|/", "^"]  # Malejąco według priorytetu
pr = {'>': 1, '&': 2, '|': 2, '/': 2, '^': 3, '~': 4}  #pomocne przy tworzeniu stringów
operators = {'^': lambda a, b: a ^ b,
             '&': lambda a, b: a & b,
             '|': lambda a, b: a | b,
             '/': lambda a, b: 1-(a & b),
             '>': lambda a, b: (1-a) | b,
             '~': lambda a: 1-a}
variables = ''
min_len = 1000
min_exp = ''
rules = [
    [['~', ['~', '1']], ['1']],
    [['>', '1', '1'], ['T']],
    [['>', 'F', '1'], ['1']],
    [['^', '1', '1'], ['F']],
    [['&', '1', '1'], ['1']],
    [['|', '1', '1'], ['1']],
    [['&', 'F', '1'], ['F']],
    [['&', 'T', '1'], ['1']],
    [['|', 'F', '1'], ['1']],
    [['|', 'T', '1'], ['T']],
    [['|', ['^', '1', '2'], '1'], ['|', '1', '2']],
    [['&', ['^', '1', '2'], '1'], ['&', '1', ['~', '2']]],
    [['|', ['&', '1', '2'], ['&', '1', '3']], ['&', '1', ['|', '2', '3']]],
    [['~', ['&', '1', '2']], ['/', '1', '2']],
    [['/', '1', '2'], ['~', ['&', '1', '2']]],
    [['|', ['/', '1', '2'], '1'], ['T']],
    [['&', ['/', '1', '2'], '1'], ['&', '1', ['~', '2']]],
    [['/', ['/', '1', '2'], ['/', '1', '2']], ['&', '1', '2']],
    [['|', ['~', '1'], ['~', '2']], ['~', ['&', '1', '2']]],
    [['&', ['~', '1'], ['~', '2']], ['~', ['|', '1', '2']]],
    [['~', ['&', '1', '2']], ['|', ['~', '1'], ['~', '2']]],
    [['~', ['|', '1', '2']], ['&', ['~', '1'], ['~', '2']]],
    [['|', ['&', '1', ['~', '2']], ['&', ['~', '1'], '2']], ['^', '1', '2']],
    [['^', '1', '2'], ['|', ['&', '1', ['~', '2']], ['&', ['~', '1'], '2']]],
    [['|', ['|', '1', '2'], '3'], ['|', ['|', '2', '3'], '1']],
    [['|', ['~', '1'], '2'], ['>', '1', '2']],
    [['|', ['|', '1', '2'], '3'], ['|', ['|', '2', '3'], '1']]
]


def check(exp):
    # Sprawdza poprawnosc wyrazenia danego stringiem
    value_expected = True

    brackets_opened = 0
    for num, c in enumerate(exp):
        if brackets_opened < 0:
            return False
        if c == '(' and value_expected:
            brackets_opened += 1
        elif c == ')' and not value_expected:
            brackets_opened -= 1
        elif c in ''.join(uops) and value_expected:
            pass
        elif c in ''.join(bops) and not value_expected:
            value_expected = True
        elif c in variables and value_expected:
            value_expected = False
        elif c in 'TF' and value_expected:
            value_expected = False
        else:
            return False
    if brackets_opened < 0:
        return False
    if value_expected:
        return False
    return True


def ball(w, op):
    ln = 0
    for i in range(len(w) - 1, -1, -1):
        if w[i] == "(":
            ln += 1
        elif w[i] == ")":
            ln -= 1
        elif w[i] in op and ln == 0:
            return i, w[i]
    return -1, ''


def get_variables(w):
    ws = [letter for letter in w if letter in ascii_lowercase]
    return sorted(set(ws))


def build_prefix(exp):
    # Buduje drzewo z operacjami w oparciu o danego stringa

    while exp[0] == '(' and exp[-1] == ')' and check(exp[1:-1]):
        exp = exp[1:-1]

    # Poszukiwany jest którykolwiek z operatorów według najmniejszego priorytetu
    # eq_ops to grupy operatorów o rownym priorytecie
    for eq_ops in bops:
        b, op = ball(exp, eq_ops)
        if b != -1:
            return [op, build_prefix(exp[0:b]), build_prefix(exp[b+1:])]
    for eq_ops in uops:
        b, op = ball(exp, eq_ops)
        if b != -1:
            return [op, build_prefix(exp[1:])]
    return exp


def latch(a, b):
    # Część metody Quine McCluskeya
    # z zajęć
    for n in range(0, len(a)):
        if a[n] == '-' and b[n] != '-':
            return False
        if b[n] == '-' and a[n] != '-':
            return False
    x = ''.join(map(lambda p: p[0] if p[0] == p[1] and p[0] != '-' else "-", zip(a, b)))
    return x if x.count('-') == 1 + a.count('-') else False


def reduce(s):
    # Część metody Quine McCluskeya

    # Lista na wyjściu zawiera wszystkie implikanty wraz z informacją które pokrywa
    # i czy został użyty przy tworzeniu nowego implikanta
    changes = True
    while changes:
        wynik = []
        changes = False
        for a in s:
            for b in s:
                if a > b:
                    l = latch(a,b)
                    if l != False:
                        s[a][1] = True
                        s[b][1] = True
                        if l not in s:
                            changes = True
                            wynik.append([l, s[a][0] + s[b][0]])
        for w in wynik:
            s[w[0]] = [w[1], False]
    return s


def gen_01(n):
    # generuje ciągi zerojedynkowe długośći n
    # z zajęć
    if n == 0:
        yield ""
    else:
        for x in gen_01(n-1):
            yield "0"+x
            yield "1"+x


def eval_tree(tree, dvalues):
    # Ewaluuje funkcję wyrażoną drzewem w oparciu o słownik par zmienna-wartość
    c = tree[0]
    if c in dvalues.keys():
        return int(dvalues[c])

    # Poniżej c na pewno jest tuplem z operatorem na pozycji 0
    if c in ''.join(uops):
        return operators[c](eval_tree(tree[1], dvalues))
    else:
        return operators[c](eval_tree(tree[1], dvalues), eval_tree(tree[2], dvalues))


def gen_table(exp):
    # Generuje kazdą możliwą wartość zmiennych przy pomocy gen_01
    # i zipowana z lista zmiennych w slownik
    ones = set()
    for values in gen_01(len(variables)):
        d = dict(zip(variables, values))
        d['T'] = 1
        d['F'] = 0
        if eval_tree(exp, d) == 1:
            ones.add(values)
    return ones


def get_min_implicants(implicants, table):
    # Część metody Quine McCluskey
    # Wybor takich implikantow zeby koncowy string byl minimalnego rozmiaru
    # Operuje wyłącznie na implikantach prostych. Działa rekurencyjnie
    if len(table) == 0:
        return 'F'
    mini_len = 999
    mini_str = ''

    def gmi_inner(unmarked, sofar=''):
        # unmarked -- implikanty wymagające pokrycia
        # sofar -- funkcja do tej pory
        nonlocal mini_len, mini_str
        if len(unmarked) == 0:
            if mini_len > len(sofar):
                mini_len = len(sofar)
                mini_str = sofar
            return
        for i in implicants:
            if bool(set(unmarked) & set(i[1])):
                part = ''
                if sofar != '':
                    part += '|('
                for variable_num, c in enumerate(i[0]):
                    if c != '-':
                        if part != '' and part[-1] in variables:
                            part += '&'
                        if c == '0':
                            part += '~' + variables[variable_num]
                        elif c == '1':
                            part += variables[variable_num]
                if part != '' and part[0] == '|':
                    part += ')'

                gmi_inner([x for x in unmarked if x not in i[1]], sofar + part)
    gmi_inner([x for x in range(0, len(table))])
    if mini_str != '':
        return mini_str
    return 'T'


def matches(node, pattern, captures={}):
    # Matches sprawdza czy dany węzeł jest zgodny z regułą
    # podmieniając cyfry na zamienniki z captures kiedy trzeba.
    # Dodaje nowy zamiennik jeżeli cyfra się jeszcze nie pojawiła.
    if node[0] in all_ops and node[0] == pattern[0]:
        for num, child in enumerate(node[1:]):
            if not matches(child, pattern[num+1], captures):
                break
        else:
            return True

        # Dla symetrycznych operacji pattern jest sprawdzany jeszcze raz z zamienionymi miejscami potomkami
        if node[0] in '^&|/':
            for num, child in enumerate(node[1:][::-1]):
                if not matches(child, pattern[num+1], captures):
                    return False
            return True
        else:
            return False
    elif isinstance(pattern[0], str) and pattern[0] in digits:  # jeżeli capture
        if pattern[0] in captures.keys():
            return matches(captures[pattern[0]], node)
        else:  # jeżeli nie było jeszcze to dodaj do słownika
            captures[pattern[0]] = node
            return True
    elif isinstance(pattern, str) and isinstance(node, str):  # jeżeli zmienna albo T/F
        return pattern[0] == node[0]
    return False


def tree_to_string(tree):
    # Zamiana funkcji z drzewa na string
    f = ''
    if tree[0] in ''.join(bops):
        s = [tree[0], tree_to_string(tree[1]), tree_to_string(tree[2])]

        for x in [1, 2]:  # dla obu potomków
            # dodawanie nawiasów na podstawie priorytetów ich operacji
            if isinstance(tree[x][0], str) and tree[x][0] in all_ops:
                if x == 2 and pr[tree[x][0]] <= pr[s[0]]:
                    s[x] = '(' + s[x] + ')'

                # nawias jest pomijany jeżeli i tak będziemy wykonywali operacje od lewej strony
                if x == 1 and pr[tree[x][0]] < pr[s[0]]:
                    s[x] = '(' + s[x] + ')'

        f = str(s[1])+s[0]+str(s[2])
    elif tree[0] in ''.join(uops):
        s1 = tree_to_string(tree[1])
        if tree[1][0] in ''.join(bops):  # dodajemy nawias jeżeli negacja dotyczy wyniku operacji
            s1 = '(' + s1 + ')'
        f = tree[0] + s1
    else:
        f = str(tree[0])
    while f[0] == '(' and f[-1] == ')' and check(f[1:-1]):
        f = f[1:-1]
    return f


def deep_replace(tree, captures):
    # Podmienia cyfry w drzewie na odpowiednie węzły
    if isinstance(tree, list):
        for num, node in enumerate(tree):
            if isinstance(node, str) and node in digits:
                tree[num] = captures[node]
            else:
                deep_replace(node, captures)


def apply_rules(node):
    # Aplikuje każdą znaną i pasującą regułę do danego węzła
    # Generuje nowe możliwe węzły
    global rules
    yield node
    for rule in rules:

        # Matches modyfikuje captures!
        captures = {}
        if matches(node, rule[0], captures):
            new_node = deepcopy(rule[1])
            deep_replace(new_node, captures)

            # wyciągam jedyny element z listy jeżeli się da
            if len(new_node) == 1:
                yield new_node[0]
            else:
                yield new_node


def reduce_tree(tree):
    # Funkcja usiłuje zredukować dane drzewo
    # Mogłaby pracować bardzo długo, więc ograniczona jest przez:
    # tree_set -- nie dodaje do kolejki drzew już spotkanych
    # total -- każde wywołanie nie wyprodukuje więcej niż total drzew
    # org_len -- drzewa których wynikowe stringi są dłuższe o 1.7 raza są odrzucane

    # Podmienia pomotków na możliwe zamienniki
    # Po zakończeniu pozostawia drzewo nienaruszonym

    org_len = len(tree_to_string(tree))
    org_self = deepcopy(tree)
    tree_set = set()
    total = 0

    def is_new(tree):
        # Funkcja pomocnicza do ograniczenia czasu działania funkcji
        nonlocal total, org_len
        total = total + 1
        if total > 10:
            return False
        s = tree_to_string(tree)
        if len(s) > 1.7 * org_len:
            return False
        h = hash(s)
        if h not in tree_set:
            tree_set.add(h)
            return True
        return False

    # Nowe możliwe drzewa są dodawane do kolejki trees
    if isinstance(tree, list):
        trees = [tree]
        while trees:
            local_tree = trees.pop()

            if local_tree[0] in ''.join(bops):
                for new_child2 in reduce_tree(local_tree[2]):  # dla każdego przekształcenia drugiego dziecka
                    local_tree[2] = new_child2
                    for new_child1 in reduce_tree(local_tree[1]):  # dla każdego przekształcenia pierwszego dziecka
                        local_tree[1] = new_child1

                        for new_tree in apply_rules(local_tree):  # dla każdego przekształcenia siebie
                            if is_new(new_tree):
                                trees.insert(0, new_tree)
                                yield new_tree

            elif local_tree[0] in ''.join(uops):
                for new_child1 in reduce_tree(local_tree[1]):
                    local_tree[1] = new_child1

                    for new_tree in apply_rules(local_tree):
                        if is_new(new_tree):
                            trees.append(new_tree)
                            yield new_tree
            else:
                yield local_tree

    yield org_self


def shortest_search(gen):
    # Wyciaga z generatora/listy mozliwe drzewa i aktualizuje globalne minimum jezeli trzeba
    global min_len
    global min_exp
    for exp in gen:
        s = tree_to_string(exp)
        if len(s) < min_len:
            min_len = len(s)
            min_exp = s


def main():
    global min_exp, min_len, variables

    line = input()
    variables = get_variables(line)
    if not check(line):
        print("ERROR")
        return
    min_exp = ''
    min_len = 1000
    b = build_prefix(line)

    shortest_search(reduce_tree(b))

    # Metoda Quine-McCluskey
    table = gen_table(b)
    g = {x: [[num], False] for num, x in enumerate(table)}
    r = reduce(g)
    important_implicants = [[k, v[0]] for k, v in r.items() if not v[1]]
    mi = get_min_implicants(important_implicants, table)
    if len(mi) < min_len:
        min_len = len(mi)
        min_exp = mi

    # reduce_tree jeszcze raz
    shortest_search(reduce_tree(build_prefix(mi)))
    print(min_exp)

    #if set(table) != set(gen_table(build_prefix(min_exp))):
        #print("LOGIC ERROR")


if __name__ == "__main__":
    main()
