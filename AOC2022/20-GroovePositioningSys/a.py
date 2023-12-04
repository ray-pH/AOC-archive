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


def move_right(node):
    original_prev = node.prev
    original_next = node.next
    original_prev.next = original_next
    original_next.prev = original_prev
    node.next = original_next.next
    node.prev = original_next
    original_next.next.prev = node
    original_next.next = node

def move_left(node):
    original_prev = node.prev
    original_next = node.next
    original_prev.next = original_next
    original_next.prev = original_prev
    node.prev = original_prev.prev
    node.next = original_prev
    original_prev.prev.next = node
    original_prev.prev = node

def move_right_n(node, n): 
    for _ in range(n): move_right(node)
def move_left_n(node, n): 
    for _ in range(n): move_left(node)


code = [int(x) for x in lines]
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
for i, code in enumerate(codelist):
    # reporting
    print(i, '/', length)
    # end reporting
    val = code.value
    if val >= 0: move_right_n(code, val)
    else       : move_left_n(code, abs(val))

l = to_list(zeronode, len(codelist))
v1 = l[1000 % len(l)]
v2 = l[2000 % len(l)]
v3 = l[3000 % len(l)]
sum = v1 + v2 + v3

# print(l)
print(sum)
