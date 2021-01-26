{-# python

import hashlib as hl

res = ['_', '_', '_', '_', '_', '_', '_', '_']
counter = 0
salt = "cxdnnyjw"
found = 0
while found < 8:
    g = salt + str(counter)
    hashed = hl.md5(g.encode('utf-8')).hexdigest()
    if hashed[0:5] == "00000" and hashed[5] in '01234567':
        pos = int(hashed[5])
        if res[pos] == '_':
            res[pos] = hashed[6]
            found += 1
    counter += 1
print("".join(res))

#-}