# with open('inpex.txt', 'r') as f:
with open('input.txt', 'r') as f:
    lines = f.read().splitlines()

class DoublyLinkedListNode:
    def __init__(self, value):
        self.value = value
        self.next = None
        self.prev = None
    def __repr__(self):
        return str(self.value)

def to_list(node, count):
    l = [0] * count
    for i in range(count):
        l[i] = node.value
        node = node.next
    return l


def prev_n(node, n):
    for _ in range(n): node = node.prev
    return node
def next_n(node, n):
    for _ in range(n): node = node.next
    return node

def move_right_n(node, n):
    if n == 0: return
    original_prev = node.prev
    original_next = node.next
    original_prev.next = original_next
    original_next.prev = original_prev

    target_prev = next_n(node, n)
    target_next = target_prev.next
    target_prev.next = node
    target_next.prev = node
    node.prev = target_prev
    node.next = target_next

def move_left_n(node, n):
    if n == 0: return
    original_prev = node.prev
    original_next = node.next
    original_prev.next = original_next
    original_next.prev = original_prev

    target_next = prev_n(node, n)
    target_prev = target_next.prev
    target_prev.next = node
    target_next.prev = node
    node.prev = target_prev
    node.next = target_next

key = 811589153
code = [int(x)*key for x in lines]
codelist = [] # list to preserve original order
for i,x in enumerate(code):
    dll = DoublyLinkedListNode(x)
    if i != 0:
        dll.prev = codelist[i-1]
        codelist[i-1].next = dll
    codelist.append(dll)
print('doubly linked list done')

# wrap around
codelist[0].prev = codelist[-1]
codelist[-1].next = codelist[0]

zeronode = None
for node in codelist:
    if node.value == 0: 
        zeronode = node
        break

length = len(codelist)
for j in range(10):
    for i, code in enumerate(codelist):
        # reporting
        print(j, i, '/', length)
        # end reporting
        val = code.value
        if val >= 0: 
            n = val % (length - 1)
            move_right_n(code, n)
        else : 
            n = abs(val) % (length - 1)
            move_left_n(code, n)

l = to_list(zeronode, len(codelist))
v1 = l[1000 % len(l)]
v2 = l[2000 % len(l)]
v3 = l[3000 % len(l)]
sum = v1 + v2 + v3

# print(l)
print(sum)
