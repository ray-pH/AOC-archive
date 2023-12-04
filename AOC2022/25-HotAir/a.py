snafu_value = { '2' : 2, '1' : 1, '0' : 0, '-' : -1, '=' : -2, }
isnafu_value = {2 : '2', 1 : '1', 0 : '0', -1 : '-', -2 : '='}

def snafu_to_decimal(snafustr):
    snafuval = [snafu_value[c] for c in snafustr]
    snafuval = reversed(snafuval)
    return sum(5**i * v for i, v in enumerate(snafuval))

def decimal_to_base5(decimal):
    if decimal == 0:
        return '0'
    digits = []
    while decimal:
        digits.append(decimal % 5)
        decimal //= 5
    return list(reversed(digits))

def base5_to_snafu(base5):
    rev = list(reversed(base5))
    rev.append(0)
    for i in range(len(rev)):
        if rev[i] >= 3:
            rev[i] -= 5
            rev[i+1] += 1
    return ''.join(isnafu_value[c] for c in reversed(rev)).lstrip('0')

def decimal_to_snafu(decimal):
    return base5_to_snafu(decimal_to_base5(decimal))


# ------------------------------------

# with open('inpex.txt', 'r') as f:
with open('input.txt', 'r') as f:
    lines = f.read().splitlines()

decimals = [snafu_to_decimal(line) for line in lines]
value = sum(decimals)
snafu = decimal_to_snafu(value)

print(decimals)
print(value)
print(snafu)
