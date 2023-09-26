# Assign2-6: 20 points
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly
#
# let
# string_merge
# (cs1: string)(cs2: string): string =
# let n1 = string_length(cs1)
# and n2 = string_length(cs2) in
# let rec
# foreach(i1: int)(i2:int)(work) =
# if
# i1 < n1
# then (
# if
# i2 < n2
# then
#   let c1 = string_get_at(cs1)(i1)
#   and c2 = string_get_at(cs2)(i2) in
#   if c1 <= c2
#   then (work(c1); foreach(i1+1)(i2+0)(work))
#   else (work(c2); foreach(i1+0)(i2+1)(work))
# else
#   int1_foreach(n1-i1)
#     (fun i -> work(string_get_at(cs1)(i1+i)))
# ) else (
#   int1_foreach(n2-i2)
#     (fun i -> work(string_get_at(cs2)(i2+i)))
# )
# in
#   string_make_fwork(foreach(0)(0))
# ;;

def int1_foreach(r, f):
    for i in range(r):
        f(i)

def string_make_fwork(f):
    result = []
    def work(c):
        result.append(c)
    f(work)
    return ''.join(result)

def string_get_at(str, i):
    return str[i]

def string_length(str):
    return len(str)

def string_merge(str1, str2):
    length1 = string_length(str1)
    length2 = string_length(str2)

    def foreach(i1, i2, work):
        if i1 < length1:
            if i2 < length2:
                char1 = string_get_at(str1, i1)
                char2 = string_get_at(str2, i2)
                if char1 <= char2:
                    work(char1)
                    foreach(i1+1, i2, work)
                else:
                    work(char2)
                    foreach(i1, i2+1, work)
            else:
                int1_foreach(length1-i1, lambda i: work(string_get_at(str1, i1+i)))
        else:
            int1_foreach(length2-i2, lambda i: work(string_get_at(str2, i2+i)))

    return string_make_fwork(lambda work: foreach(0, 0, work))