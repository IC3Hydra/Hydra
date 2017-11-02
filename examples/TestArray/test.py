
from ethereum.tools import tester as t

s = t.Chain()

from ethereum.slogging import get_logger
log_tx = get_logger('eth.pb.tx')
log_tx.setLevel("DEBUG")
log_msg = get_logger('eth.pb.msg')
log_msg.setLevel("TRACE")
"""
with open("test_array.sol") as fd:
    code_sol = fd.read()
    c_sol = s.contract(code_sol, language='solidity')


ret = c_sol.test()
print(ret)
"""

with open("test_array.se") as fd:
    code_se = fd.read()
    c_se = s.contract(code_se, language='serpent')


ret = c_se.test()

print(ret)
"""
with open("test_array.vy") as fd:
    code_vy = fd.read()
    c_vy = s.contract(code_vy, language='viper')

ret = c_vy.test()
print(ret)
"""


