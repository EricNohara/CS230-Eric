########################################################################
#
# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python
#
########################################################################


class MyList:
    ctag = -1

    def get_ctag(self):
        return self.ctag


class MyNil(MyList):
    def __init__(self):
        self.ctag = 0


class MyCons(MyList):
    def __init__(self, hd, tl):
        self.ctag = 1
        self.hd = hd
        self.tl = tl


class MySnoc(MyList):
    def __init__(self, init, last):
        self.ctag = 2
        self.init = init
        self.last = last


class MyReverse(MyList):
    def __init__(self, list):
        self.ctag = 3
        self.list = list


class MyAppend2(MyList):
    def __init__(self, list1, list2):
        self.ctag = 4
        self.list1 = list1
        self.list2 = list2


def mylist_foreach(xs, work):
    if xs.get_ctag() == 0:
        return
    elif xs.get_ctag() == 1:
        work(xs.hd)
        xs = xs.tl
    elif xs.get_ctag() == 2:
        mylist_foreach(xs.init, work)
        work(xs.last)
        return
    elif xs.get_ctag() == 3:
        mylist_foreach(xs.list, work)
        return
    elif xs.get_ctag() == 4:
        mylist_foreach(xs.list1, work)
        mylist_foreach(xs.list2, work)
        return


def mylist_rforeach(xs, work):
    # just call the foreach function with the reversed list
    mylist_foreach(mylist_reverse(xs), work)

# constructors


def mylist_nil():
    return MyNil()


def mylist_cons(hd, tl):
    return MyCons(hd, tl)


def mylist_snoc(init, last):
    return MySnoc(init, last)


def mylist_reverse(list):
    return MyReverse(list)


def mylist_append2(list1, list2):
    return MyAppend2(list1, list2)
