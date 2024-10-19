#!/usr/bin/env python3
import sys
target_path = sys.argv[1]


with open(target_path, 'r') as f: nock_code = f.read()

code = f'''
from nock_runner import run_nock

main= (run_nock {nock_code})
'''

with open('out.bend', 'w') as f: f.write(code)

import os

os.system("bend run-c out.bend")

