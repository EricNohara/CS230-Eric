# Assign4-6:
#
# HX-2023-10-06: 30 points (bonus)
#
# (*
# //
# Please implement the following function
# that enumerates all the pairs (i, j) of natural
# numbers satisfying $i <= j$; a pair (i1, j1) must
# be enumerated ahead of another pair (i2, j2) if the
# following condition holds:
#   i1*i1*i1 + j1*j1*j1 < i2*i2*i2 + j2*j2*j2
# //
# let
# theNatPairs_cubesum(): (int * int) stream = fn () =>
# //
# *)
#
# def theNatPairs_cubesum(): # please give your implementation

from MyPython import *
import sys
sys.path.append("./../../../../classlib/Python")


def 


def stream_merge(str1, str2, fun):
    if str1 == strcon_nil() and str2 == strcon_nil():
        return strcon_nil()
    elif str1 == strcon_nil():
        return str2
    elif str2 == strcon_nil():
        return str1
    elif fun(str1, str2):
        return strcon_cons(str1, str2)
    else:
        return strcon_cons(str2, str1)


def theNatPairs_cubesum():
    i = 0
    j = 0
    while True:
        print((i, j))
        yield (i, j)

        if j == i:
            i = 0
            j += 1

        else:
            i += 1


# let theNatPairs: (int*int) stream = fun () ->
#   let rec generate_pair i j =
#     if i = 0 && j = 0 then StrCons ((i, j), fun () -> generate_pair i (j+1))
#     else if j = 0 then StrCons ((i, j), fun () -> generate_pair 0 (i+1))
#     else StrCons ((i, j), fun () -> generate_pair (i+1) (j-1))
#   in generate_pair 0 0
