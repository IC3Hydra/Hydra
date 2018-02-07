import requests
from utils.pyethereum_test_utils import PyEthereumHydraDeployment
from ethereum import utils
from ethereum.tools import tester
import time
import subprocess

URL = 'https://mainnet.infura.io/'
#URL = 'http://localhost:8545/'


def get_block(block_id):

    data = '{' \
           '"jsonrpc":"2.0", ' \
           '"method":"eth_getBlockByNumber", ' \
           '"params":["' + '0x{:x}'.format(block_id) + '", true], ' \
           '"id":1' \
           '}'

    #time.sleep(0.05)
    r = requests.post(URL, data=data)
    try:
        return r.json()['result']
    except KeyError:
        print(r.json())
        return r.json()['result']


def get_code(addr):
    data = '{' \
           '"jsonrpc":"2.0", ' \
           '"method":"eth_getCode", ' \
           '"params":["' + addr + '", "latest"], ' \
           '"id":1' \
           '}'

    #time.sleep(0.05)
    r = requests.post(URL, data=data)
    try:
        return r.json()['result']
    except KeyError:
        print(r.json())
        return "0x"


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

        block = get_block(i)

        transactions = block['transactions']
        for t in transactions:
            addr = t['to']
            if addr is None:
                continue

            code = get_code(addr)

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
    parser.add_argument("key")

    args = parser.parse_args()

    URL += args.key

    crawl(args.end)
