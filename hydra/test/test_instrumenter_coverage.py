from utils.pyethereum_test_utils import PyEthereumHydraDeployment
from ethereum import utils
from ethereum.tools import tester
import subprocess

import web3
w3 = web3.Web3(web3.IPCProvider('/Users/floriantramer/Library/Ethereum/geth.ipc'))


def crawl(end_block):

    t = tester
    s = t.Chain()
    s.head_state.gas_limit = 10**80

    dep = PyEthereumHydraDeployment(s, t.k0, t.a0, None, None)
    MC_address = utils.decode_hex("1234567890")
    unique_codes = {}
    unique_instrumentable = 0

    all_contracts = 0
    all_instrumentable = 0

    for i in range(end_block, 0, -1):
        print("block {} ({}/{} unique instrumented, {}/{} total instrumented)".format(i, unique_instrumentable, len(unique_codes), all_instrumentable, all_contracts))

        block = w3.eth.getBlock(i)

        transactions = block['transactions']
        for t in transactions:
            t = w3.eth.getTransaction(t)

            if t is None:
                continue

            addr = t['to']
            if addr is None:
                continue

            code = w3.eth.getCode(addr)

            if code != "0x":
                all_contracts += 1

                if (code,) not in unique_codes:
                    try:
                        dep.instrument_head(True, code[2:], None, MC_address, run_constructor=False, detect_swarm=True)
                        unique_codes[(code,)] = (1, True)
                        unique_instrumentable += 1
                        all_instrumentable += 1
                    except subprocess.CalledProcessError as e:
                        unique_codes[(code,)] = (1, True)

                else:
                    (count, status) = unique_codes[(code,)]
                    unique_codes[(code,)] = (count+1, status)
                    if status:
                        all_instrumentable += 1

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("end", type=int, help="end block")

    args = parser.parse_args()
    crawl(args.end)
