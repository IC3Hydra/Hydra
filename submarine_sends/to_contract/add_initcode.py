#!/usr/bin/env python
import sys

# See initcode_header.easm for assembly
# Compiled EVM bytecode: 58600c8038038082843982f3
payload = sys.argv[1]
assert len(payload) % 2 == 0
print '58600c8038038082843982f3{}'.format(payload)
