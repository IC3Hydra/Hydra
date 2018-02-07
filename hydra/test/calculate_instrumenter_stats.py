import os

BLOCKDATA_PATH = "blocks/"
BAD_OPCODES = ['CODESIZE', 'CODECOPY', 'EXTCODESIZE', 'EXTCODECOPY', 'CREATE', 'CALLCODE', 'DELEGATECALL', 'SUICIDE', 'STATICCALL', 'RETURNDATACOPY', 'RETURNDATASIZE'] # from Instrumentation.hs

error_map = {}

num_failures = 0
num_tx_failures = 0
num_successes = 0
num_tx_successes = 0
num_failures_containing_opcode = {}
num_tx_failures_containing_opcode = {}
average_opcodes_in_containing_contracts = {}

for opcode in BAD_OPCODES:
    num_failures_containing_opcode[opcode] = 0
    num_tx_failures_containing_opcode[opcode] = 0
    average_opcodes_in_containing_contracts[opcode] = [0, 0]


for filename in os.listdir(BLOCKDATA_PATH):
    if filename.endswith(".errors"):
        path = os.path.join(BLOCKDATA_PATH, filename)
        errors = open(path).read().strip().splitlines()
        for error in errors:
            error = error.split("~")
            error[0] = int(error[0])
            assert len(error) == 2

            contract_errors = {}
            for op in BAD_OPCODES:
                num_ops = error[1].count('"' + op)
                contract_errors[op] = num_ops

            error_map[error[0]] = contract_errors

for filename in os.listdir(BLOCKDATA_PATH):
    if not filename.endswith(".errors") and not "ids" in filename:
        path = os.path.join(BLOCKDATA_PATH, filename)
        exec("dict = " + open(path).read().strip()) # this is hacky but ah well it works
        for id in dict:
            id = int(id)
            status = dict[id]
            if status[1] == True or (error_map[id]['EXTCODESIZE'] == sum(error_map[id].values())):
                #print("YES")
                # instrumentation succeeded
                if status[1] == True:
                    assert not id in error_map
                else:
                    assert id in error_map
                num_successes += 1
                num_tx_successes += status[0]
            else:
                # instrumentation failed
                #print("NO")
                assert id in error_map
                num_failures += 1
                num_tx_failures += status[0]
                for opcode in BAD_OPCODES:
                    num_opcodes_in_fail = error_map[id][opcode]
                    if num_opcodes_in_fail > 0:
                        num_failures_containing_opcode[opcode] += 1
                        num_tx_failures_containing_opcode[opcode] += status[0]
                        average_opcodes_in_containing_contracts[opcode][0] += num_opcodes_in_fail
                        average_opcodes_in_containing_contracts[opcode][1] += 1

print("Num failures", num_failures)
print("Num tx failures", num_tx_failures)
print("Num successes", num_successes)
print("Num tx successes", num_tx_successes)
print("Num failures for each opcode", num_failures_containing_opcode)
print("Num tx failures for each opcode", num_tx_failures_containing_opcode)
print("Average opcodes in offending contract", [(opcode, float(average_opcodes_in_containing_contracts[opcode][0]) / average_opcodes_in_containing_contracts[opcode][1]) for opcode in average_opcodes_in_containing_contracts])


