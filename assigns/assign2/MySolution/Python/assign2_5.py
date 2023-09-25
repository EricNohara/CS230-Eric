# Assign2-5: 20 points
# Please implement in Python a function
# of the name fnlist_make_fwork that corresponds
# to the function list_make_fwork in the library
# MyOCaml.ml

import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def fnlist_make_fwork(fwork):
    result = fnlist_nil()

    def work(x0):
        return fnlist_cons(x0, result)

    fwork(work)
    return result
