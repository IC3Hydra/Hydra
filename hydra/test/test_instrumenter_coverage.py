import requests
from utils.pyethereum_test_utils import PyEthereumHydraDeployment
from ethereum import utils
from ethereum.tools import tester
import time
import threading
import os

import web3
w3 = web3.Web3(web3.IPCProvider('/home/debian/geth_temp_fast/geth.ipc'))

latest_block_lock = threading.Lock()
latest_block = -1

results_cache_lock = threading.Lock()
results_cache = {}

ids_lock = threading.Lock()
ids = {}
new_id = 0

def get_id(code, blockpath):
    global ids, ids_lock, new_id
    ids_lock.acquire()
    if code in ids:
        ids_lock.release()
        return ids[code]
    ids[code] = new_id
    assigned_id = new_id
    new_id += 1
    with open(blockpath + os.sep + "ids", "a") as f:
        f.write(str(assigned_id) + "~" + str(code) + "\n")
    ids_lock.release()
    return assigned_id

def crawl(start_block, end_block, blockpath, thread_id):
    global results_cache, results_cache_lock

    block_file_out_path = blockpath + os.sep + str(start_block) + "_" + str(end_block)

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
                code_id = get_id(code, blockpath)
                results_cache_lock.acquire()
                if code_id not in results_cache:
                    results_cache[code_id] = "processing"
                    results_cache_lock.release()
                    instrumented_code = dep.instrument_head(True, code[2:], None, MC_address, run_constructor=False, detect_swarm=True, write_stderr_to=block_file_out_path + ".errors", code_id=code_id)
                    results_cache_lock.acquire()
                    if len(instrumented_code) == 0:
                        results_cache[code_id] = False
                    else:
                        results_cache[code_id] = True
                while results_cache[code_id] == "processing":
                    # wait if another thread is processing results
                    results_cache_lock.release()
                    time.sleep(1)
                    results_cache_lock.acquire()
                result = results_cache[code_id]
                results_cache_lock.release()
                if code_id not in unique_codes:
                    if result:
                        unique_codes[code_id] = (1, True)
                        instrumentable += 1
                    else:
                        unique_codes[code_id] = (1, False)

                else:
                    (count, status) = unique_codes[code_id]
                    unique_codes[code_id] = (count+1, status)
        outsummary = "block {} ({}/{} instrumented [thread {}, {}% complete])".format(i, instrumentable, len(unique_codes), thread_id, int(((float(i) - start_block) * 100) / (end_block - start_block)))
        print(outsummary)
    open(block_file_out_path, "w").write(str(unique_codes))


def do_crawl(blockspath, batchsize, thread_id):
    global latest_block, latest_block_lock
    while True:
        # lock and grab a batch
        # run the batch
        latest_block_lock.acquire()
        last_block = latest_block
        latest_block -= batchsize
        latest_block_lock.release()
        crawl(last_block - batchsize + 1, last_block, blockspath, thread_id)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("end", type=int, help="end block")
    parser.add_argument("blockspath", type=str, help="block data files prefix")
    parser.add_argument("numthreads", type=int, help="number of threads to use")
    parser.add_argument("batchsize", type=int, help="number of blocks to batch together (higher=larger files, slower writes to disk)")

    args = parser.parse_args()

    latest_block = args.end

    for i in range(args.numthreads):
        threading.Thread(target=do_crawl, args=[args.blockspath, args.batchsize, i], daemon=True).start()

    while latest_block > 0: # nasty polling loop
        time.sleep(5)
