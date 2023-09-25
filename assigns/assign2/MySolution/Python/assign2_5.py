# Assign2-5: 20 points
# Please implement in Python a function
# of the name fnlist_make_fwork that corresponds
# to the function list_make_fwork in the library
# MyOCaml.ml

import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def fnlist_make_fwork(fwork):
    eList = []

    fwork(lambda x: eList.append(x))

    result = fnlist_nil()
    
    for el in fnlist_reverse(eList):
        result = fnlist_cons(el, result)

    return result
