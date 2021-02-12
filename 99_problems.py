# 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)

def last(list):
    return list[-1]

assert(last([ "a","b","c", "d" ]) == "d")

# 2. Find the last but one (last and penultimate) elements of a list. (easy)

def last_two(list):
    return list[-2:]

assert(last_two(["a","b","c", "d" ]) == ["c", "d"])

# 3. Find the k'th element of a list. (easy)

def at(k, list):
    return list[k - 1]


# 4. Find the number of elements of a list. (easy)

def list_length(list):
    return len(list)


# 5. Reverse a list. (easy)

def list_reverse(l):
    return list(reversed(l))
