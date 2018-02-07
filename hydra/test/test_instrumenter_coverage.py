import requests
from utils.pyethereum_test_utils import PyEthereumHydraDeployment
from ethereum import utils
from ethereum.tools import tester
import time
import threading
import os

import web3
w3 = web3.Web3(web3.IPCProvider('/home/debian/geth_temp_fast/geth.ipc'))

lock = threading.Lock()
latest_block = -1

def crawl(start_block, end_block, blockpath):

    block_file_out_path = blockpath + os.sep + str(start_block) + "_" + str("end_block")

    t = tester
    s = t.Chain()
    s.head_state.gas_limit = 10**80

    dep = PyEthereumHydraDeployment(s, t.k0, t.a0, None, None)
    MC_address = utils.decode_hex("1234567890")
    unique_codes = {}
    instrumentable = 0

    for i in range(start_block, end_block + 1):
        block = w3.eth.getBlock(i, full_transactions=True)
        transactions = block['transactions']
        for t in transactions:
            addr = t['to']
            if addr is None:
                continue

            code = w3.eth.getCode(addr)
            if code != "0x":
                if (code,) not in unique_codes:
                    instrumented_code = dep.instrument_head(True, code[2:], None, MC_address, run_constructor=False, detect_swarm=True, write_stderr_to=block_file_out_path + ".errors")
                    if len(instrumented_code) == 0:
                        unique_codes[(code,)] = (1, False)
                    else:
                        unique_codes[(code,)] = (1, True)
                        instrumentable += 1

                else:
                    (count, status) = unique_codes[(code,)]
                    unique_codes[(code,)] = (count+1, status)
        outsummary = "block {} ({}/{} instrumented)\n".format(i, instrumentable, len(unique_codes))
        print(outsummary)
    open(block_file_out_path, "w").write(str(unique_codes))


def do_crawl(blockspath, batchsize):
    global latest_block
    while True:
        # lock and grab a batch
        # run the batch
        lock.acquire()
        last_block = latest_block
        latest_block -= batchsize
        lock.release()
        crawl(last_block - batchsize + 1, last_block, blockspath)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("end", type=int, help="end block")
    parser.add_argument("blockspath", type=str, help="block data files prefix")
    parser.add_argument("numthreads", type=int, help="number of threads to use")
    parser.add_argument("batchsize", type=int, help="number of blocks to batch together (higher=more caching, slower writes to disk)")

    args = parser.parse_args()

    latest_block = args.end

    for i in range(args.numthreads):
        threading.Thread(target=do_crawl, args=[args.blockspath, args.batchsize], daemon=True).start()

    while latest_block > 0: # nasty polling loop
        time.sleep(5)
