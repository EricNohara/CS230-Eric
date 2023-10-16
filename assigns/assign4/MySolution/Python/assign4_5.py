# let
# string_fset_at
# (cs: string)(i0: int)(c0: char) =
# string_tabulate
# (string_length(cs))
# (
# fun i ->
# if i <> i0 then string_get_at(cs)(i) else c0)
# ;;
# (* ****** ****** *)
#
# let
# alphabet =
# string_tabulate(26)(fun i -> chr(ord('a') + i));;
#
# (* ****** ****** *)
#
# let
# list_of_buddies
# (word: string): string list =
# let n0 =
# string_length(word) in
# list_make_fwork
# (
# fun work ->
# int1_foreach(n0)
# (
# fun i0 ->
# let c0 =
# string_get_at(word)(i0) in
# string_foreach(alphabet)
# (fun c1 -> if c1 <> c0 then work(string_fset_at(word)(i0)(c1)))))
# ;;
# (* ****** ****** *)
from MyPython import *
import sys
sys.path.append("./../../../../../classlib/Python")


def string_tabulate(length, fun):
    string_init = ""
    for i in range(length):
        string_init += fun(i)
    return string_init


def string_length(str):
    return len(str)


def string_get_at(str, i):
    return str[i]


def list_make_fwork(fwork):
    list = []
    fwork(lambda x: list.append(x))
    return list


def string_foreach(cs, work):
    length = string_length(cs)
    int1_foreach(length, lambda i0: work(string_get_at(cs, i0)))


def string_fset_at(cs, i0, c0):
    return string_tabulate(string_length(cs), lambda i:
                           string_get_at(cs, i) if i != i0 else c0)


def alphabet():
    return string_tabulate(26, lambda i: chr(ord('a') + i))


def list_of_buddies(word):
    n0 = string_length(word)
    list = list_make_fwork(lambda work:
                           int1_foreach(n0, lambda i0:
                                        string_foreach(alphabet(), lambda c1: work(string_fset_at(word, i0, c1)) if (c1 != string_get_at(word, i0)) else '')))
    return list
